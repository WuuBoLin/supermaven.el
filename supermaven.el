;;; supermaven.el --- Supermaven for Emacs -*- lexical-binding: t; -*-

;; Author: WuuBoLin <me@wuubolin.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (json "1.5"))
;; Keywords: convenience, completion, tools
;; URL: https://github.com/WuuBoLin/supermaven.el

;;; Commentary:

;; Supermaven is an AI-powered code completion plugin for Emacs.

;;; Code:

(require 'cl-lib)
(require 'json)

;; Load dependencies
(require 'supermaven-config)
(require 'supermaven-logger)
(require 'supermaven-util)
(require 'supermaven-binary)
(require 'supermaven-state)
(require 'supermaven-process)
(require 'supermaven-completion)
(require 'supermaven-document)

;; Forward declarations
(declare-function supermaven--send-message "supermaven-process")
(declare-function supermaven--process-running-p "supermaven-process")
(declare-function supermaven--start-process "supermaven-process")
(declare-function supermaven--stop-process "supermaven-process")
(declare-function supermaven--initialize-process-manager "supermaven-process")
(declare-function supermaven--clear-completion "supermaven-completion")
(declare-function supermaven--initialize-completion "supermaven-completion")
(declare-function supermaven--cleanup-completion "supermaven-completion")
(declare-function supermaven-complete "supermaven-completion")
(declare-function supermaven-clear-overlay "supermaven-completion")
(declare-function supermaven-accept-completion "supermaven-completion")
(declare-function supermaven-accept-completion-by-word "supermaven-completion")

(declare-function supermaven--initialize-document "supermaven-document")
(declare-function supermaven--cleanup-document "supermaven-document")

;; Core variables
(defvar supermaven--initialized nil
  "Whether Supermaven has been initialized.")

(defvar supermaven--state-manager nil
  "Placeholder for compatibility - not used.")

(defvar supermaven-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keep only essential admin commands
    (define-key map (kbd "C-c s r") #'supermaven-restart)
    (define-key map (kbd "C-c s s") #'supermaven-status)
    map)
  "Keymap for Supermaven mode.")

;; Core functions
(defun supermaven--initialize ()
  "Initialize Supermaven if not already initialized."
  (unless supermaven--initialized
    (supermaven-log-debug "Initializing Supermaven core components")
    (supermaven--ensure-binary)
    (supermaven--initialize-process-manager)
    ;; Note: completion initialization is done per-buffer in supermaven-mode
    (setq supermaven--initialized t)
    (supermaven-log-info "Supermaven initialization completed")))

(defun supermaven--cleanup ()
  "Clean up Supermaven resources."
  (supermaven-stop)
  (supermaven--cleanup-completion)
  (setq supermaven--initialized nil))

(defun supermaven--clear-overlays ()
  "Clear all Supermaven overlays in current buffer."
  (supermaven-clear-overlay))

(defun supermaven--should-ignore-buffer ()
  "Check if current buffer should be ignored."
  (or (not (buffer-file-name))
      (member major-mode supermaven-ignore-filetypes)
      (string-match-p "^\\*" (buffer-name))
      (and supermaven-condition
           (funcall supermaven-condition))))

;; Interactive commands
;;;###autoload
(defun supermaven-start ()
  "Start Supermaven."
  (interactive)
  (if (supermaven--process-running-p)
      (supermaven-log-debug "Supermaven process already running")
    (progn
      (supermaven-log-info "Starting Supermaven...")
      (supermaven--initialize)
      (supermaven--start-process)
      (supermaven-log-debug "Supermaven start sequence completed"))))

;;;###autoload
(defun supermaven-stop ()
  "Stop Supermaven."
  (interactive)
  (supermaven-log-info "Stopping Supermaven...")
  (supermaven--stop-process)
  (supermaven-log-debug "Supermaven stop sequence completed"))

;;;###autoload
(defun supermaven-restart ()
  "Restart Supermaven."
  (interactive)
  (supermaven-log-info "Restarting Supermaven...")
  (supermaven-stop)
  (supermaven-start)
  (supermaven-log-info "Supermaven restart completed"))

;;;###autoload
(defun supermaven-toggle ()
  "Toggle Supermaven on/off."
  (interactive)
  (if (supermaven--process-running-p)
      (supermaven-stop)
    (supermaven-start)))

;;;###autoload
(defun supermaven-status ()
  "Show Supermaven status."
  (interactive)
  (message "Supermaven is %s"
           (if (supermaven--process-running-p)
               "running" "not running")))

;;;###autoload
(defun supermaven-use-free ()
  "Switch to Supermaven Free version."
  (interactive)
  (supermaven-log-info "Switching to Supermaven Free version")
  (supermaven--send-message '((kind . "use_free_version"))))

;;;###autoload
(defun supermaven-use-pro ()
  "Switch to Supermaven Pro version."
  (interactive)
  (supermaven-log-info "Switching to Supermaven Pro version")
  (if-let ((url supermaven-activate-url))
      (progn
        (supermaven-log-debug (format "Opening activation URL: %s" url))
        (browse-url url))
    (supermaven-log-error "No activation URL available")))

;;;###autoload
(defun supermaven-logout ()
  "Log out of Supermaven."
  (interactive)
  (supermaven-log-info "Logging out of Supermaven")
  (supermaven--send-message '((kind . "logout"))))

;;;###autoload
(defun supermaven-show-log ()
  "Show Supermaven log buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Supermaven Log*")))
    (with-current-buffer buffer
      (view-mode 1)
      (goto-char (point-max)))
    (display-buffer buffer)))

;;;###autoload
(defun supermaven-toggle-log ()
  "Toggle Supermaven log buffer visibility."
  (interactive)
  (let ((log-buffer (get-buffer "*Supermaven Log*")))
    (if (and log-buffer (get-buffer-window log-buffer))
        (quit-window nil (get-buffer-window log-buffer))
      (supermaven-show-log))))

;;;###autoload
(defun supermaven-clear-log ()
  "Clear Supermaven log buffer."
  (interactive)
  (when-let ((buffer (get-buffer "*Supermaven Log*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))





;; Mode definitions
;;;###autoload
(define-minor-mode supermaven-mode
  "Toggle Supermaven mode."
  :init-value nil
  :lighter " Supermaven"
  :keymap supermaven-mode-map
  :group 'supermaven
  (if supermaven-mode
      (progn
        (supermaven-log-info (format "Enabling Supermaven mode in buffer: %s" 
                                   (or (buffer-file-name) (buffer-name))))
        (supermaven--initialize)
        (supermaven--initialize-completion)  ; Initialize completion per-buffer
        (supermaven--initialize-document)
        (when (and supermaven-auto-start
                   (not (supermaven--process-running-p)))
          (supermaven-log-debug "Auto-starting Supermaven process")
          (supermaven-start))
        (supermaven-log-debug "Supermaven mode enabled"))
    (supermaven-log-info (format "Disabling Supermaven mode in buffer: %s" 
                               (or (buffer-file-name) (buffer-name))))
    (supermaven--cleanup-completion)  ; Cleanup completion per-buffer
    (supermaven--cleanup-document)
    (supermaven--clear-overlays)
    (supermaven-log-debug "Supermaven mode disabled")))

;;;###autoload
(define-globalized-minor-mode global-supermaven-mode
  supermaven-mode
  (lambda ()
    (unless (or (minibufferp)
                (not (buffer-file-name))
                (member major-mode supermaven-ignore-filetypes))
      (supermaven-mode 1)))
  :group 'supermaven)

;; Setup function
;;;###autoload
(defun supermaven-setup ()
  "Set up Supermaven."
  (interactive)
  ;; Initialize
  (supermaven--initialize)
  
  ;; Set up completion
  (with-eval-after-load 'company
    (supermaven--setup-company))
  
  ;; Start if auto-start enabled
  (when supermaven-auto-start
    (supermaven-start)))

;; Register all commands
(mapc (lambda (cmd)
        (put cmd 'completion-predicate #'ignore))
      '(supermaven-start
        supermaven-stop
        supermaven-restart
        supermaven-toggle
        supermaven-status
        supermaven-use-free
        supermaven-use-pro
        supermaven-logout
        supermaven-show-log
        supermaven-toggle-log
        supermaven-clear-log))

(provide 'supermaven)

;;; supermaven.el ends here