;;; supermaven-completion.el --- Improved completion handling -*- lexical-binding: t; -*-

;;; Commentary:

;; Core completion functionality for Supermaven with robust timing management.

;;; Code:

(require 'cl-lib)
(require 'supermaven-state)
(require 'supermaven-logger)
(require 'supermaven-config)

;; Forward declarations
(declare-function supermaven--send-message "supermaven-process")
(declare-function supermaven--process-running-p "supermaven-process")

;; Variables
(defvar-local supermaven--overlay nil
  "Current completion overlay.")

(defvar-local supermaven--keymap-overlay nil
  "Overlay used to surround point and make completion keymap activate.")

(defvar-local supermaven--real-posn nil
  "Posn information without overlay.
To work around posn problems with after-string property.")

(defconst supermaven--hard-size-limit 10e6
  "Maximum size of buffer text to process.")

(defvar-local supermaven--current-completion nil
  "Current completion text.")

(defvar-local supermaven--current-state-id nil
  "Current completion state ID.")

(defvar supermaven--post-command-timer nil
  "Timer for triggering completions after commands.")

(defvar-local supermaven--last-doc-version 0
  "The document version of the last completion.")

(defvar-local supermaven--doc-version 0
  "The document version of the current buffer.")

(defvar-local supermaven--completion-states (make-hash-table :test 'equal)
  "Hash table storing completion states by ID.")

(defvar-local supermaven--last-completion-time 0
  "Time of last completion request.")

(defvar-local supermaven--last-completion-position nil
  "Position of last completion request.")

;; New timing control variables
(defvar-local supermaven--pending-request-id nil
  "ID of the currently pending request.")

(defvar-local supermaven--request-counter 0
  "Counter for generating unique request IDs.")

(defvar-local supermaven--request-positions (make-hash-table :test 'equal)
  "Hash table mapping request IDs to their positions.")

(defvar-local supermaven--last-typing-time 0
  "Time of last typing activity.")

(defvar-local supermaven--typing-speed 0
  "Current typing speed (chars per second).")

(defvar-local supermaven--last-char-count 0
  "Character count at last measurement.")

(defcustom supermaven-idle-delay 0.1
  "Time in seconds to wait before starting completion.
Complete immediately if set to 0.
Disable idle completion if set to nil."
  :type '(choice
          (number :tag "Seconds of delay")
          (const :tag "Idle completion disabled" nil))
  :group 'supermaven)

(defcustom supermaven-minimum-prefix-length 2
  "Minimum number of characters to type before triggering completion.
Set to 0 to always trigger completion."
  :type 'integer
  :group 'supermaven)

(defcustom supermaven-max-completion-frequency 0.5
  "Minimum time in seconds between completion requests.
Prevents too frequent completion requests."
  :type 'number
  :group 'supermaven)

(defcustom supermaven-completion-triggers
  '("." "(" "[" "{" " " ":" "=" ">" "<" "/" "\\")
  "List of characters that should trigger completion immediately.
These bypass the idle delay when typed."
  :type '(repeat string)
  :group 'supermaven)

(defcustom supermaven-fast-typing-threshold 5.0
  "Typing speed threshold (chars/sec) above which to delay completions more.
When typing faster than this, completions are delayed to avoid interruption."
  :type 'number
  :group 'supermaven)

(defcustom supermaven-fast-typing-delay-multiplier 2.0
  "Multiplier for idle delay when typing fast.
The idle delay is multiplied by this value when typing speed exceeds threshold."
  :type 'number
  :group 'supermaven)

(defcustom supermaven-disable-predicates nil
  "A list of predicate functions with no argument to disable Supermaven.
Supermaven will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'supermaven)

(defcustom supermaven-enable-predicates '(supermaven--buffer-changed)
  "A list of predicate functions with no argument to enable Supermaven.
Supermaven will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'supermaven)

(defface supermaven-overlay-face
  '((t :foreground "#888888" :background nil))
  "Face for Supermaven overlay - more visible gray.")

(defvar supermaven-completion-map
  (let ((map (make-sparse-keymap)))
    ;; Essential completion commands only
    (define-key map (kbd "TAB") #'supermaven-accept-completion)
    (define-key map (kbd "C-j") #'supermaven-accept-completion-by-word)
    (define-key map (kbd "C-g") #'supermaven-dismiss-completion)
    (define-key map (kbd "ESC") #'supermaven-dismiss-completion)
    map)
  "Keymap active when Supermaven completion overlay is visible.")

;; Utility functions
(defun supermaven--buffer-changed ()
  "Return non-nil if the buffer has changed since last completion."
  (not (= supermaven--last-doc-version supermaven--doc-version)))

(defmacro supermaven--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
                    (if (functionp pred) (funcall pred) t))
                  ,enable)
        (cl-notany (lambda (pred)
                     (if (functionp pred) (funcall pred) nil))
                   ,disable)))

(defun supermaven--satisfy-trigger-predicates ()
  "Return t if all trigger predicates are satisfied."
  (supermaven--satisfy-predicates supermaven-enable-predicates supermaven-disable-predicates))

;; Typing speed tracking
(defun supermaven--update-typing-speed ()
  "Update the current typing speed measurement."
  (let* ((now (float-time))
         (time-diff (- now supermaven--last-typing-time))
         (char-diff (- (point) supermaven--last-char-count)))
    (when (> time-diff 0.1) ; Update every 0.1 seconds minimum
      (setq supermaven--typing-speed (/ (float char-diff) time-diff)
            supermaven--last-typing-time now
            supermaven--last-char-count (point)))))

(defun supermaven--get-adjusted-idle-delay ()
  "Get idle delay adjusted for typing speed."
  (let ((base-delay (or supermaven-idle-delay 0.1)))
    (if (> supermaven--typing-speed supermaven-fast-typing-threshold)
        (* base-delay supermaven-fast-typing-delay-multiplier)
      base-delay)))

(defun supermaven--should-trigger-completion ()
  "Check if completion should be triggered based on context."
  (and supermaven-mode
       (not (supermaven--should-ignore-buffer))
       (supermaven--process-running-p)
       ;; Check minimum prefix length
       (let ((prefix (supermaven--get-prefix-at-point)))
         (or (>= (length prefix) supermaven-minimum-prefix-length)
             ;; Or if we just typed a trigger character
             (and (> (point) 1)
                  (member (buffer-substring-no-properties (1- (point)) (point))
                          supermaven-completion-triggers))))
       ;; Check frequency throttling
       (let ((now (float-time)))
         (> (- now supermaven--last-completion-time) 
            supermaven-max-completion-frequency))
       ;; Check if position changed significantly
       (not (equal (point) supermaven--last-completion-position))))

(defun supermaven--overlay-visible ()
  "Return whether the overlay is visible."
  (and (overlayp supermaven--overlay)
       (overlay-buffer supermaven--overlay)))

(defun supermaven--get-or-create-keymap-overlay ()
  "Make or return the local keymap overlay."
  (unless (overlayp supermaven--keymap-overlay)
    (setq supermaven--keymap-overlay (make-overlay 1 1 nil nil t))
    (overlay-put supermaven--keymap-overlay 'keymap supermaven-completion-map)
    (overlay-put supermaven--keymap-overlay 'priority 101))
  supermaven--keymap-overlay)

(defun supermaven--get-overlay ()
  "Create or get overlay for Supermaven."
  (unless (overlayp supermaven--overlay)
    (setq supermaven--overlay (make-overlay 1 1 nil nil t))
    (overlay-put supermaven--overlay 'keymap-overlay (supermaven--get-or-create-keymap-overlay)))
  supermaven--overlay)

(defun supermaven--set-overlay-text (ov completion)
  "Set overlay OV with COMPLETION text."
  ;; Position from current point to end of line (like copilot.el)
  (move-overlay ov (point) (line-end-position))
  
  ;; Set overlay position for keymap activation
  (move-overlay (overlay-get ov 'keymap-overlay) (point) (min (point-max) (+ 1 (point))))
  
  ;; Handle completion display properly (following copilot.el pattern)
  (let* ((p-completion (propertize completion 'face 'supermaven-overlay-face)))
    (if (eolp)
        (progn
          ;; At end of line - use after-string with cursor property
          (overlay-put ov 'after-string "") ; Clear first to ensure posn is correct
          (setq supermaven--real-posn (cons (point) (posn-at-point)))
          (put-text-property 0 1 'cursor t p-completion)
          (overlay-put ov 'display "")
          (overlay-put ov 'after-string p-completion))
      ;; Not at end of line - use display property
      (overlay-put ov 'display (substring p-completion 0 1))
      (overlay-put ov 'after-string (substring p-completion 1)))
    (overlay-put ov 'completion completion)
    (overlay-put ov 'start (point))
    ;; Store the request ID that created this overlay
    (overlay-put ov 'request-id supermaven--current-state-id)))

(defun supermaven--is-quality-completion (completion)
  "Check if COMPLETION is worth showing to the user."
  (and completion
       (not (string-empty-p completion))
       ;; Filter out single character completions unless they're special
       (or (> (length completion) 1)
           (member completion '(";" "," ")" "]" "}")))
       ;; Filter out completions that are just whitespace
       (not (string-match-p "\\`[[:space:]]+\\'" completion))
       ;; Filter out completions that are too short relative to context
       (let ((prefix (supermaven--get-prefix-at-point)))
         (or (string-empty-p prefix)
             (> (length completion) 2)))))

(defun supermaven--display-completion (completion request-id)
  "Display COMPLETION text for REQUEST-ID."
  (supermaven-log-debug (format "Attempting to display completion for request %s: '%s'" 
                               request-id completion))

  
  ;; Only proceed if validation passes
  (when (and 
         ;; Validate this is still the current request
         (equal request-id supermaven--pending-request-id)
         ;; Validate position hasn't changed significantly
         (let ((expected-pos (gethash request-id supermaven--request-positions)))
           (or (not expected-pos)
               (equal (point) expected-pos))))
    
    ;; Clear any existing overlay
    (supermaven-clear-overlay)
    
    ;; Display if quality completion
    (when (and (supermaven--is-quality-completion completion)
               (not (supermaven--should-ignore-buffer)))
      (supermaven-log-info (format "Displaying completion overlay: '%s'" completion))
      (let ((ov (supermaven--get-overlay)))
        (supermaven--set-overlay-text ov completion)
        (setq supermaven--current-completion completion
              supermaven--current-state-id request-id)
        (supermaven-log-trace "Completion overlay created successfully")))
    
    ;; Log if completion was rejected
    (unless (supermaven--is-quality-completion completion)
      (supermaven-log-debug (format "Completion rejected (low quality): '%s'" completion)))
    (when (supermaven--should-ignore-buffer)
      (supermaven-log-debug "Completion not displayed - buffer should be ignored"))))

(defun supermaven-clear-overlay ()
  "Clear Supermaven overlay."
  (interactive)
  (when (supermaven--overlay-visible)
    (supermaven-log-trace "Clearing completion overlay")
    (delete-overlay supermaven--overlay)
    (delete-overlay supermaven--keymap-overlay)
    (setq supermaven--current-completion nil
          supermaven--current-state-id nil
          supermaven--real-posn nil)
    (supermaven-log-debug "Completion overlay cleared")))

(defun supermaven-dismiss-completion ()
  "Dismiss current completion and prevent immediate re-triggering."
  (interactive)
  (supermaven-clear-overlay)
  ;; Set last completion time to prevent immediate re-trigger
  (setq supermaven--last-completion-time (float-time)
        supermaven--last-completion-position (point))
  (message "Supermaven: Completion dismissed"))

(defun supermaven--clear-completion ()
  "Clear current completion."
  (supermaven-clear-overlay))

(defun supermaven--get-completion-text ()
  "Get current completion text."
  supermaven--current-completion)

(defun supermaven--to-next-word (text)
  "Get text up to next word boundary in TEXT."
  (when text
    (let ((word-end (string-match "\\W" text)))
      (if word-end
          (substring text 0 word-end)
        text))))

;; Add posn advice like copilot.el
(defun supermaven--posn-advice (&rest args)
  "Remap posn if in supermaven-mode with ARGS."
  (when supermaven-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and supermaven--real-posn
                 (eq pos (car supermaven--real-posn)))
        (cdr supermaven--real-posn)))))

;; Handle typing through overlay (like copilot.el)
(defun supermaven--self-insert (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue. COMMAND is the command that triggered."
  (when (and (eq command 'self-insert-command)
             (supermaven--overlay-visible))
    (let* ((ov supermaven--overlay)
           (completion (overlay-get ov 'completion)))
      ;; The char just inserted is the next char of completion
      (when (and completion 
                 (not (string-empty-p completion))
                 (eq last-command-event (elt completion 0)))
        (if (= (length completion) 1)
            ;; If there is only one char in the completion, clear it
            (supermaven-clear-overlay)
          ;; Otherwise, update the overlay to show remaining completion
          (supermaven--set-overlay-text ov (substring completion 1)))
        t))))  ; Return t to indicate we handled it

;; Interactive commands
(defun supermaven-accept-completion ()
  "Accept current completion."
  (interactive)
  (when (supermaven--overlay-visible)
    (let* ((completion (overlay-get supermaven--overlay 'completion))
           (start (overlay-get supermaven--overlay 'start)))
      (supermaven-log-info (format "Accepting completion: '%s'" completion))
      (supermaven-clear-overlay)
      ;; Go to start position and insert completion
      (goto-char start)
      ;; Don't delete anything, just insert the completion
      (insert completion)
      (supermaven-log-debug "Completion text inserted successfully")
      t)))  ; Return t to indicate success

(defun supermaven-accept-completion-by-word ()
  "Accept next word of completion."
  (interactive)
  (when-let* ((completion (supermaven--get-completion-text)))
    (let* ((word (supermaven--to-next-word completion))
           (start (overlay-get supermaven--overlay 'start)))
      (supermaven-log-info (format "Accepting word: '%s'" word))
      (supermaven-clear-overlay)
      (goto-char start)
      (insert word)
      ;; Show remaining completion if any
      (let ((remaining (substring completion (length word))))
        (when (not (string-empty-p remaining))
          (supermaven-log-debug (format "Showing remaining completion: '%s'" remaining))
          ;; Use run-at-time to avoid blocking
          (run-at-time 0 nil (lambda () 
                              (when (and (eq (point) (+ start (length word)))
                                         (not (supermaven--overlay-visible)))
                                (supermaven--display-completion remaining supermaven--current-state-id))))))
      (supermaven-log-trace "Word acceptance completed")
      t)))

;; Document version tracking
(defun supermaven--on-change (&rest _args)
  "Handle buffer changes."
  (cl-incf supermaven--doc-version)
  ;; Update typing speed
  (supermaven--update-typing-speed))

;; Completion triggering
(defun supermaven--post-command ()
  "Handle post-command for completion triggering."
  (when (and this-command
             (not (and (symbolp this-command)
                       (or (string-prefix-p "supermaven-" (symbol-name this-command))
                           ;; Handle self-insert special case
                           (supermaven--self-insert this-command)
                           ;; Also ignore self-insert of certain characters
                           (and (eq this-command 'self-insert-command)
                                (member (char-to-string last-command-event)
                                        '("\n" "\r")))))))
    ;; Only clear overlay if we didn't handle it in self-insert
    (unless (supermaven--self-insert this-command)
      ;; Clear overlay on certain commands that indicate user wants to dismiss
      (when (or (memq this-command '(keyboard-quit abort-recursive-edit))
                ;; Clear if moving away from completion position
                (and (supermaven--overlay-visible)
                     (not (eq (point) (overlay-get supermaven--overlay 'start)))))
        (supermaven-clear-overlay)))
    
    ;; Cancel pending timer
    (when supermaven--post-command-timer
      (cancel-timer supermaven--post-command-timer)
      (setq supermaven--post-command-timer nil))
    
    ;; Check if we should trigger completion
    (when (and (numberp supermaven-idle-delay)
               (supermaven--should-trigger-completion))
      ;; Check for immediate trigger characters
      (let* ((immediate-trigger 
              (and (> (point) 1)
                   (member (buffer-substring-no-properties (1- (point)) (point))
                           supermaven-completion-triggers)))
             (adjusted-delay (if immediate-trigger 
                                0.05
                              (supermaven--get-adjusted-idle-delay))))
        (setq supermaven--post-command-timer
              (run-with-idle-timer adjusted-delay
                                   nil
                                   #'supermaven--post-command-debounce
                                   (current-buffer)
                                   (point)))))))

(defun supermaven--post-command-debounce (buffer position)
  "Complete in BUFFER at POSITION after debounce."
  (when (and (buffer-live-p buffer)
             (equal (current-buffer) buffer)
             (equal (point) position)  ; Ensure cursor hasn't moved
             supermaven-mode
             (supermaven--satisfy-trigger-predicates)
             (supermaven--should-trigger-completion))
    (supermaven-log-debug "Auto-triggering completion after debounce")
    (setq supermaven--last-completion-time (float-time)
          supermaven--last-completion-position position)
    (supermaven-complete)))

;;;###autoload
(defun supermaven-complete ()
  "Request completion at current point."
  (interactive)
  (supermaven-log-debug "supermaven-complete called")
  (when (and (not (supermaven--should-ignore-buffer))
             (buffer-file-name))
    (supermaven-log-debug "Sending completion request")
    (setq supermaven--last-doc-version supermaven--doc-version)

    ;; Cancel any pending request
    (when supermaven--pending-request-id
      (supermaven-log-debug (format "Cancelling pending request %s" supermaven--pending-request-id))
      (remhash supermaven--pending-request-id supermaven--completion-states)
      (remhash supermaven--pending-request-id supermaven--request-positions))
    
    (supermaven--request-completion))
  (unless (buffer-file-name)
    (supermaven-log-warn "Completion not triggered - buffer has no file"))
  (when (supermaven--should-ignore-buffer)
    (supermaven-log-debug "Completion not triggered - buffer should be ignored")))

;; Make request processing non-blocking with proper state tracking
(defun supermaven--request-completion ()
  "Send completion request to Supermaven."
  (when (supermaven--process-running-p)
    ;; Generate unique request ID
    (cl-incf supermaven--request-counter)
    (let* ((request-id supermaven--request-counter)
           (buffer (current-buffer))
           (buffer-path (buffer-file-name))
           (content (buffer-substring-no-properties (point-min) (point-max)))
           (prefix (supermaven--get-prefix-at-point))
           (cursor-pos (max 0 (1- (point))))
           (current-position (point)))
      
      ;; Store request metadata
      (setq supermaven--pending-request-id request-id)
      (puthash request-id current-position supermaven--request-positions)
      
      (when (and buffer-path
                 (< (length content) supermaven--hard-size-limit))
        ;; Use run-at-time to make it non-blocking
        (run-at-time 0 nil
                     (lambda ()
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (supermaven-log-debug (format "Processing completion request %s - file: %s, position: %d" 
                                                        request-id buffer-path current-position))
                           ;; Store the state for accumulating completion items
                           (puthash request-id 
                                    (list :prefix prefix 
                                          :completion-items nil 
                                          :position current-position
                                          :doc-version supermaven--doc-version)
                                    supermaven--completion-states)
                           ;; Send updates directly
                           (let ((updates `(((kind . "file_update")
                                            (path . ,buffer-path)
                                            (content . ,content))
                                           ((kind . "cursor_update")
                                            (path . ,buffer-path)
                                            (offset . ,cursor-pos)))))
                             (supermaven-log-trace (format "Sending state update for request %s" request-id))
                             (supermaven--send-state-update request-id updates))))))))))

(defun supermaven--get-prefix-at-point ()
  "Get prefix at current point for completion."
  ;; Get the current word/identifier at point
  (let* ((start (save-excursion
                  (skip-chars-backward "a-zA-Z0-9_.-")
                  (point)))
         (end (point))
         (prefix (if (< start end)
                    (buffer-substring-no-properties start end)
                  "")))
    ;; If we're at a trigger character, include context before it
    (when (and (> (point) 1)
               (member (buffer-substring-no-properties (1- (point)) (point))
                       supermaven-completion-triggers))
      (setq prefix (concat prefix (buffer-substring-no-properties (1- (point)) (point)))))
    prefix))

(defun supermaven--send-state-update (state-id updates)
  "Send state update with STATE-ID and UPDATES."
  (when (buffer-file-name)
    (let ((message `((kind . "state_update")
                    (newId . ,(number-to-string state-id))
                    (updates . ,(vconcat updates)))))
      (supermaven-log-debug (format "Sending state update message: %s" message))
      (supermaven--send-message message))))

;; Completion processing with validation
(defun supermaven--process-completion (state-id items)
  "Process completion ITEMS for STATE-ID and display them."
  (supermaven-log-debug (format "Processing completion items for state %s: %s" state-id items))
  
  ;; Only proceed if validation passes
  (when (and 
         ;; Validate this is still relevant
         (gethash state-id supermaven--completion-states)
         ;; Check if document version matches  
         (let ((state-info (gethash state-id supermaven--completion-states)))
           (when state-info
             (let ((request-doc-version (plist-get state-info :doc-version)))
               (= request-doc-version supermaven--doc-version)))))
    (supermaven-log-trace "Completion validation passed")
    
    (when-let ((state-info (gethash state-id supermaven--completion-states)))
      ;; Accumulate completion items (like supermaven-nvim does)
      (let ((existing-items (plist-get state-info :completion-items)))
        (plist-put state-info :completion-items (append existing-items items))
        (puthash state-id state-info supermaven--completion-states)
        
        ;; Extract completion text and show it
        (let* ((all-items (plist-get state-info :completion-items))
               (completion-text (supermaven--extract-completion-text all-items)))
          (supermaven-log-debug (format "Extracted completion text: '%s'" completion-text))
          
          (when (and completion-text
                     (not (string-empty-p completion-text)))
            ;; Display with request ID for validation
            (supermaven--display-completion completion-text state-id)))))
  (unless (gethash state-id supermaven--completion-states)
    (supermaven-log-warn (format "Completion state %s not found - request may be stale" state-id)))))

(defun supermaven--extract-completion-text (items)
  "Extract completion text from ITEMS."
  (when items
    (let ((text ""))
      (dolist (item items)
        (let ((kind (alist-get 'kind item))
              (item-text (alist-get 'text item "")))
          (supermaven-log-trace (format "Processing item: kind=%s text='%s'" kind item-text))
          (when (string= kind "text")
            (setq text (concat text item-text)))))
      (supermaven-log-debug (format "Final extracted text: '%s'" text))
      text)))

;; Disable company mode integration to prevent interference
(defun supermaven-completion-at-point ()
  "Completion at point function for Supermaven - DISABLED to prevent interference."
  ;; Return nil to disable company-mode integration
  nil)

;; Disable company backend to prevent interference  
(defun company-supermaven (command &optional arg &rest _ignored)
  "Company backend for Supermaven - DISABLED to prevent interference."
  ;; Return nil for all commands to disable
  nil)



;; Setup and cleanup
(defun supermaven--initialize-completion ()
  "Initialize completion system."
  (add-hook 'after-change-functions #'supermaven--on-change nil t)
  (add-hook 'post-command-hook #'supermaven--post-command nil t)
  ;; Add posn advice like copilot.el
  (advice-add 'posn-at-point :before-until #'supermaven--posn-advice))

(defun supermaven--setup-company ()
  "Set up company-mode integration - DISABLED."
  ;; Disabled to prevent interference
  nil)

(defun supermaven--cleanup-completion ()
  "Clean up completion system."
  (remove-hook 'after-change-functions #'supermaven--on-change t)
  (remove-hook 'post-command-hook #'supermaven--post-command t)
  ;; Remove posn advice
  (advice-remove 'posn-at-point #'supermaven--posn-advice)
  (supermaven--clear-completion)
  (when supermaven--post-command-timer
    (cancel-timer supermaven--post-command-timer)
    (setq supermaven--post-command-timer nil))
  (setq supermaven--current-completion nil
        supermaven--current-state-id nil
        supermaven--doc-version 0
        supermaven--last-doc-version 0
        supermaven--completion-states (make-hash-table :test 'equal)
        supermaven--last-completion-time 0
        supermaven--last-completion-position nil
        supermaven--real-posn nil
        supermaven--pending-request-id nil
        supermaven--request-counter 0
        supermaven--request-positions (make-hash-table :test 'equal)
        supermaven--last-typing-time 0
        supermaven--typing-speed 0
        supermaven--last-char-count 0))



(provide 'supermaven-completion)

;;; supermaven-completion.el ends here