;;; supermaven-document.el --- Document handling -*- lexical-binding: t; -*-

;;; Commentary:

;; Handles document changes and updates.

;;; Code:

(require 'cl-lib)
(require 'supermaven-state)
(require 'supermaven-process)
(require 'supermaven-config)
(require 'supermaven-logger)

;; Forward declarations
(declare-function supermaven--send-message "supermaven-process")
(declare-function supermaven--process-running-p "supermaven-process")
(declare-function supermaven--send-state-update "supermaven-completion")

;; Variables
(defvar-local supermaven--pending-update-timer nil
  "Timer for pending document update.")

(defvar-local supermaven--last-sent-content ""
  "Last content sent to server to detect actual changes.")

(defvar-local supermaven--last-update-time 0
  "Time of last document update sent.")

(defconst supermaven--hard-size-limit 10e6
  "Maximum size of buffer text to process.")

(defcustom supermaven-document-update-delay 0.5
  "Delay in seconds before updating document after changes.
Increased delay reduces server load when typing fast."
  :type 'number
  :group 'supermaven)

(defcustom supermaven-ignore-filetypes 
  '(image-mode archive-mode doc-view-mode pdf-view-mode)
  "List of major modes to ignore for document tracking."
  :type '(repeat symbol)
  :group 'supermaven)

(defcustom supermaven-condition nil
  "Additional condition function to determine if Supermaven should be active.
If non-nil, this function is called with no arguments and should return
non-nil if Supermaven should be disabled in the current buffer."
  :type '(choice (const :tag "No additional condition" nil)
                 function)
  :group 'supermaven)

;; Core functions
(defun supermaven--should-ignore-buffer ()
  "Check if current buffer should be ignored."
  (or (not (buffer-file-name))
      (member major-mode supermaven-ignore-filetypes)
      (string-match-p "^\\*" (buffer-name))  ; Ignore special buffers
      (string-match-p "^\\s-*$" (buffer-name)) ; Ignore buffers with only whitespace names
      (and supermaven-condition
           (funcall supermaven-condition))))

(defun supermaven--handle-buffer-change (&rest _)
  "Handle buffer content changes with debouncing."
  (when (and supermaven-mode
             (buffer-file-name)
             (not (supermaven--should-ignore-buffer))
             (supermaven--process-running-p))
    (supermaven-log-trace (format "Buffer change detected in: %s" (buffer-file-name)))
    ;; Cancel any pending update
    (when supermaven--pending-update-timer
      (supermaven-log-trace "Cancelling pending document update timer")
      (cancel-timer supermaven--pending-update-timer)
      (setq supermaven--pending-update-timer nil))
    
    ;; Schedule new update with delay
    (supermaven-log-debug (format "Scheduling document update with %fs delay" supermaven-document-update-delay))
    (setq supermaven--pending-update-timer
          (run-at-time supermaven-document-update-delay nil
                       #'supermaven--send-document-update-if-changed
                       (current-buffer)))))

(defun supermaven--send-document-update-if-changed (buffer)
  "Send document update for BUFFER if content actually changed."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and supermaven-mode
                 (supermaven--process-running-p)
                 (not (supermaven--should-ignore-buffer)))
        (let ((current-content (buffer-substring-no-properties (point-min) (point-max))))
          (supermaven-log-trace (format "Checking if document update needed for: %s (length: %d)" 
                                       (buffer-file-name) (length current-content)))
          ;; Only send if content actually changed
          (when (and (not (string= current-content supermaven--last-sent-content))
                     ;; And enough time has passed since last update
                     (> (- (float-time) supermaven--last-update-time) 0.3)
                     ;; And content is not too large
                     (< (length current-content) supermaven--hard-size-limit))
            (supermaven-log-debug "Document content changed, sending update")
            (supermaven--send-document-update)
            (setq supermaven--last-sent-content current-content
                  supermaven--last-update-time (float-time)))
          
          ;; Log why update was skipped
          (when (string= current-content supermaven--last-sent-content)
            (supermaven-log-trace "Document update skipped - content unchanged"))
          (when (<= (- (float-time) supermaven--last-update-time) 0.3)
            (supermaven-log-trace "Document update skipped - too soon since last update"))
          (when (>= (length current-content) supermaven--hard-size-limit)
            (supermaven-log-warn (format "Document update skipped - content too large (%d bytes)" 
                                        (length current-content)))))))))

(defun supermaven--send-document-update ()
  "Send current document state to Supermaven."
  (when-let* ((file-path (buffer-file-name))
              (content (buffer-substring-no-properties (point-min) (point-max))))
    (supermaven-log-info (format "Sending document update for: %s" file-path))
    (supermaven-log-debug (format "Document content length: %d characters" (length content)))
    ;; Send file update message
    (let ((updates `(((kind . "file_update")
                     (path . ,file-path)
                     (content . ,content)))))
      ;; Use a simple state ID for document updates
      (when (fboundp 'supermaven--send-state-update)
        (supermaven-log-trace "Sending state update via supermaven--send-state-update")
        (supermaven--send-state-update 0 updates))
      (unless (fboundp 'supermaven--send-state-update)
        (supermaven-log-error "supermaven--send-state-update function not available")))))

;; Initialization and cleanup
(defun supermaven--initialize-document ()
  "Initialize document tracking."
  (when (buffer-file-name)
    (supermaven-log-debug (format "Initializing document tracking for: %s" (buffer-file-name)))
    (add-hook 'after-change-functions #'supermaven--handle-buffer-change nil t)
    (setq supermaven--last-sent-content (buffer-substring-no-properties (point-min) (point-max))
          supermaven--last-update-time 0
          supermaven--pending-update-timer nil)
    (supermaven-log-trace "Document tracking hooks installed"))
  (unless (buffer-file-name)
    (supermaven-log-debug "Document tracking skipped - buffer has no file")))

(defun supermaven--cleanup-document ()
  "Clean up document tracking."
  (supermaven-log-debug (format "Cleaning up document tracking for: %s" 
                               (or (buffer-file-name) "unnamed buffer")))
  (remove-hook 'after-change-functions #'supermaven--handle-buffer-change t)
  (when supermaven--pending-update-timer
    (supermaven-log-trace "Cancelling pending document update timer")
    (cancel-timer supermaven--pending-update-timer)
    (setq supermaven--pending-update-timer nil))
  (setq supermaven--last-sent-content ""
        supermaven--last-update-time 0)
  (supermaven-log-trace "Document tracking cleanup completed"))

;; Public API
(defun supermaven-document-force-update ()
  "Force an immediate document update."
  (interactive)
  (when (and supermaven-mode
             (buffer-file-name)
             (not (supermaven--should-ignore-buffer))
             (supermaven--process-running-p))
    (supermaven-log-info "Forcing immediate document update")
    (supermaven--send-document-update)
    (message "Supermaven: Document update sent"))
  (unless supermaven-mode
    (supermaven-log-warn "Force update failed - supermaven-mode not enabled"))
  (unless (buffer-file-name)
    (supermaven-log-warn "Force update failed - buffer has no file"))
  (when (supermaven--should-ignore-buffer)
    (supermaven-log-warn "Force update failed - buffer should be ignored"))
  (unless (supermaven--process-running-p)
    (supermaven-log-warn "Force update failed - process not running")))

(provide 'supermaven-document)

;;; supermaven-document.el ends here