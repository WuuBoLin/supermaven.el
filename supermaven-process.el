;;; supermaven-process.el --- Supermaven process management -*- lexical-binding: t; -*-

;;; Commentary:

;; This file handles the Supermaven binary process and communication.

;;; Code:

(require 'json)
(require 'supermaven-binary)
(require 'supermaven-completion)
(require 'supermaven-logger)
(require 'supermaven-config)
(require 'supermaven-state)

;; Forward declarations
(declare-function supermaven--process-completion "supermaven-completion")
(declare-function supermaven--send-message "supermaven-process")

;; Process variables
(defvar supermaven--process nil
  "The Supermaven process.")

(defvar supermaven--process-buffer " *supermaven*"
  "Buffer for Supermaven process output.")

(defvar supermaven--message-queue nil
  "Queue of messages to be processed.")

(defvar supermaven--changed-documents (make-hash-table :test 'equal)
  "Hash table of changed documents.")

(defvar supermaven--retry-count 0
  "Number of process restart attempts.")

(defvar supermaven--last-message-time 0
  "Time of last message sent.")

(defvar supermaven--startup-successful nil
  "Whether the process has successfully started and communicated.")

(defvar supermaven--output-buffer ""
  "Buffer for accumulating partial output.")

(defconst supermaven--message-delay 0.05
  "Minimum delay between messages in seconds.")

(defconst supermaven--max-retries 3
  "Maximum number of restart attempts.")

(defconst supermaven--hard-size-limit 10e6
  "Maximum size of buffer text to process.")

;; Safe logging function for timer/sentinel contexts
(defun supermaven--safe-log (level message)
  "Safely log MESSAGE at LEVEL, avoiding timer/sentinel issues."
  (condition-case err
      (supermaven-log level message)
    (error
     (message "Supermaven log error: %s (original: %s)" err message))))

;; Core process functions
(defun supermaven--process-running-p ()
  "Check if Supermaven process is running."
  (and supermaven--process
       (process-live-p supermaven--process)))

(defun supermaven--start-process ()
  "Start the Supermaven process."
  (supermaven-log-info "Starting Supermaven process...")
  (if (supermaven--process-running-p)
      (supermaven-log-debug "Process already running and healthy")
    (condition-case err
        (progn
          (supermaven-log-debug "Ensuring binary is available")
          (supermaven--ensure-binary)
          (unless supermaven-binary-path
            (error "Supermaven binary path not set"))
          
          (supermaven-log-debug (format "Creating process with binary: %s" supermaven-binary-path))
          (let ((process-buffer (get-buffer-create supermaven--process-buffer)))
            (with-current-buffer process-buffer
              (let ((inhibit-read-only t))
                (erase-buffer))
              (setq buffer-read-only t))
            
            (setq supermaven--process
                  (make-process
                   :name "supermaven"
                   :buffer process-buffer
                   :command (list supermaven-binary-path "stdio")
                   :filter #'supermaven--process-filter
                   :sentinel #'supermaven--process-sentinel
                   :noquery t)
                  supermaven--output-buffer "")
            
            (setq supermaven--retry-count 0
                  supermaven--startup-successful nil)
            
            (supermaven-log-info "Supermaven process created, initializing communication...")
            ;; Wait a moment before sending greeting to ensure process is ready
            (run-with-timer 0.5 nil #'supermaven--send-greeting)
            (supermaven-log-trace "Process initialization timer started")))
      (error
       (supermaven-log-error (format "Failed to start Supermaven: %s" err))
       (when (< supermaven--retry-count supermaven--max-retries)
         (cl-incf supermaven--retry-count)
         (supermaven-log-warn (format "Retrying Supermaven start (attempt %d/%d)" 
                                            supermaven--retry-count supermaven--max-retries))
         (run-with-timer 3 nil #'supermaven--start-process))
       (when (>= supermaven--retry-count supermaven--max-retries)
         (supermaven-log-error "Maximum retry attempts reached, giving up"))))))

(defun supermaven--stop-process ()
  "Stop the Supermaven process."
  (when (supermaven--process-running-p)
    (supermaven-log-info "Stopping Supermaven process...")
    (delete-process supermaven--process)
    (supermaven-log-debug "Process terminated"))
    (setq supermaven--process nil
        supermaven--message-queue nil
        supermaven--startup-successful nil
        supermaven--output-buffer "")
    (when-let ((buf (get-buffer supermaven--process-buffer)))
      (kill-buffer buf)
      (supermaven-log-trace "Process buffer cleaned up"))
    (supermaven-log-info "Supermaven process stopped"))

(defun supermaven--send-message (message)
  "Send MESSAGE to the Supermaven process."
  (when (supermaven--process-running-p)
    (let ((now (float-time)))
      (when (> (- now supermaven--last-message-time) supermaven--message-delay)
        (setq supermaven--last-message-time now)
        (condition-case err
            (let* ((json-false :json-false)
                   (json-null nil)
                   (json-string (json-encode message)))
              (supermaven-log-debug (format "Sending message: %s" json-string))
              (process-send-string supermaven--process (concat json-string "\n")))
          (error
           (supermaven-log-error (format "Failed to send message: %s" err))))))))

(defun supermaven--process-filter (proc output)
  "Process filter for OUTPUT from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert output)
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc)))))
    
    ;; Process the output for complete messages
    (setq supermaven--output-buffer (concat supermaven--output-buffer output))
    (supermaven--process-complete-messages)))

(defun supermaven--process-complete-messages ()
  "Process any complete messages in the output buffer."
  (while (string-match "\\(.+\\)\n" supermaven--output-buffer)
    (let ((line (match-string 1 supermaven--output-buffer)))
      (setq supermaven--output-buffer (substring supermaven--output-buffer (match-end 0)))
      (supermaven--process-line line))))

(defun supermaven--process-line (line)
  "Process a complete LINE from the Supermaven binary."
  (supermaven-log-trace (format "Processing line: %s" line))
  (cond
   ;; Handle SM-MESSAGE format from Supermaven binary
   ((string-match "^SM-MESSAGE \\(.+\\)$" line)
    (let ((message-text (match-string 1 line)))
      (condition-case err
          (progn
            (supermaven-log-debug (format "Received SM-MESSAGE: %s" message-text))
            ;; Parse JSON string into alist before handling
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-false nil)
                   (json-null nil)
                   (parsed-message (json-read-from-string message-text)))
              (supermaven--handle-message parsed-message)))
        (error
         (supermaven-log-error 
          (format "Error processing SM-MESSAGE: %s\nMessage: %s" 
                  err message-text))))))
   ;; Handle plain JSON (fallback)
   ((string-match "^{" line)
          (condition-case err
        (progn
          (supermaven-log-debug (format "Received JSON line: %s" line))
          ;; Parse JSON string into alist before handling
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-false nil)
                 (json-null nil)
                 (parsed-message (json-read-from-string line)))
            (supermaven--handle-message parsed-message)))
            (error
       (supermaven-log-error 
        (format "Error processing JSON line: %s\nLine: %s" 
                err line)))))
   ;; Log unknown lines for debugging
   (t
    (supermaven-log-warn (format "Unknown line format received: %s" line)))))

(defun supermaven--process-sentinel (proc event)
  "Handle process state changes for PROC with EVENT."
  (condition-case err
      (let ((status (process-status proc))
            (event-desc (string-trim event)))
        (supermaven-log-info (format "Supermaven process %s" event-desc))
        
        (when (memq status '(exit signal))
          (setq supermaven--process nil
                supermaven--startup-successful nil
                supermaven--output-buffer "")
          
          ;; Only restart if it was a successful startup that then failed
          ;; AND we haven't exceeded retry limits
          (when (and supermaven--startup-successful
                     (< supermaven--retry-count supermaven--max-retries)
               supermaven-auto-start)
            (supermaven-log-error "Process exited unexpectedly, attempting restart...")
            (cl-incf supermaven--retry-count)
            (run-with-timer 5 nil #'supermaven--start-process))))
    (error
     (supermaven-log-error (format "Error in process sentinel: %s" err)))))

;; Message handling
(defun supermaven--handle-message (json-obj)
  "Handle incoming JSON-OBJ from Supermaven process."
  (supermaven-log-debug (format "Handling message: %s" json-obj))
  
  (let ((kind (alist-get 'kind json-obj)))
    (cond
     ;; Handle completion response - Updated to match actual Supermaven format
     ((equal kind "response")
      (let ((state-id-str (alist-get 'stateId json-obj))  ; Note: stateId not state_id
            (items (alist-get 'items json-obj))           ; Note: items not completion_items
            (response-type (alist-get 'responseType json-obj)))
        (cond
         ;; Handle completion items
         ((and state-id-str items)
          (let ((state-id (string-to-number state-id-str)))
            (supermaven-log-info (format "Received completion response for state %s with %d items" 
                                                state-id (length items)))
            ;; Mark startup as successful on first response
            (setq supermaven--startup-successful t)
            ;; Forward to completion module for processing
            (when (fboundp 'supermaven--process-completion)
              (supermaven-log-debug "Forwarding completion to completion module")
              (supermaven--process-completion state-id items))
            (unless (fboundp 'supermaven--process-completion)
              (supermaven-log-error "supermaven--process-completion function not available"))))
         ;; Handle greeting response
         ((equal response-type "greeting")
          (supermaven-log-info "Received greeting from Supermaven - connection established")
          (setq supermaven--process-ready t
                supermaven--startup-successful t)
          ;; Trigger any waiting actions after greeting
          (run-hooks 'supermaven-after-greeting-hook)
          (supermaven-log-debug "Greeting hooks executed")))))
     
     ;; Handle activation response
     ((equal kind "activation_response")
      (supermaven-log-info "Supermaven Pro activated successfully")
      (setq supermaven--startup-successful t)
      (run-hooks 'supermaven-after-activation-hook))
     
     ;; Handle activation request
     ((equal kind "activation_request")
      (let ((url (alist-get 'activateUrl json-obj)))
        (setq supermaven-activate-url url
              supermaven--startup-successful t)
        (supermaven-log-info 
         (format "Activation required. Visit %s to activate Supermaven Pro, or use M-x supermaven-use-free for free tier" url))))
     
     ;; Handle metadata
     ((equal kind "metadata")
      (let ((value (alist-get 'value json-obj)))
        (puthash 'metadata value supermaven--server-state)
        (setq supermaven--startup-successful t)
        (supermaven-log-trace (format "Stored metadata: %s" value))))
     
     ;; Handle service tier
     ((equal kind "service_tier")
      (let ((value (alist-get 'value json-obj)))
        (puthash 'service-tier value supermaven--server-state)
        (setq supermaven--startup-successful t)
        (supermaven-log-info (format "Supermaven service tier: %s" value))))
     
     ;; Unknown message type
     (t
      (supermaven-log-warn (format "Received unknown message type: %s" kind))))))

(defun supermaven--send-greeting ()
  "Send initial greeting to Supermaven."
  (when (supermaven--process-running-p)
    (supermaven-log-debug "Sending greeting message to establish connection")
    (supermaven--send-message
     `((kind . "greeting")
       (allowGitignore . :json-false)))
    (supermaven-log-trace "Greeting message sent"))
  (unless (supermaven--process-running-p)
    (supermaven-log-error "Cannot send greeting - process not running")))

(defun supermaven--submit-state-update ()
  "Submit pending document updates to Supermaven."
  (let (updates)
    (maphash (lambda (_path doc)
               (push `((kind . "file_update")
                      (path . ,(alist-get 'path doc))
                      (content . ,(alist-get 'content doc)))
                     updates))
             supermaven--changed-documents)
    (clrhash supermaven--changed-documents)
    (when updates
      (supermaven--send-message
       `((kind . "state_update")
         (newId . ,(number-to-string supermaven--current-state-id))
         (updates . ,(vconcat updates)))))))

(defun supermaven--update-completion-state (state-id items)
  "Update completion state with STATE-ID and ITEMS."
  ;; Skip the problematic state management and directly process completion
  (supermaven--process-completion state-id items))

(defun supermaven--initialize-process-manager ()
  "Initialize the process management system."
  (setq supermaven--process nil
        supermaven--message-queue nil
        supermaven--changed-documents (make-hash-table :test 'equal)
        supermaven--retry-count 0
        supermaven--last-message-time 0
        supermaven--startup-successful nil
        supermaven--output-buffer ""))

(defun supermaven--cleanup-process ()
  "Clean up process resources."
  (supermaven--stop-process)
  (setq supermaven--message-queue nil
        supermaven--changed-documents (make-hash-table :test 'equal)
        supermaven--retry-count 0
        supermaven--startup-successful nil
        supermaven--output-buffer ""))

(provide 'supermaven-process)

;;; supermaven-process.el ends here