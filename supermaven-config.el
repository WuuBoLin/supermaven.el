;;; supermaven-config.el --- Supermaven configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains configuration options for Supermaven.

;;; Code:

(defgroup supermaven nil
  "Supermaven settings."
  :group 'completion
  :prefix "supermaven-")

(defcustom supermaven-auto-start t
  "Whether to automatically start Supermaven when enabling the mode."
  :type 'boolean
  :group 'supermaven)

(defcustom supermaven-ignore-filetypes '()
  "List of filetypes to ignore."
  :type '(repeat string)
  :group 'supermaven)

(defcustom supermaven-disable-inline-completion nil
  "Whether to disable inline completion."
  :type 'boolean
  :group 'supermaven)

(defcustom supermaven-condition (lambda () nil)
  "Function to determine if Supermaven should be enabled."
  :type 'function
  :group 'supermaven)

(defcustom supermaven-log-level 'info
  "Log level for Supermaven."
  :type '(choice (const :tag "Off" off)
                (const :tag "Error" error)
                (const :tag "Warn" warn)
                                 (const :tag "Info" info)
                 (const :tag "Debug" debug)
                 (const :tag "Trace" trace))
  :group 'supermaven)

(defcustom supermaven-suggestion-color "#ffffff"
  "Color for completion suggestions."
  :type 'string
  :group 'supermaven)

(defcustom supermaven-suggestion-face 'shadow
  "Face to use for completion suggestions."
  :type 'face
  :group 'supermaven)

(defvar supermaven-dust-strings nil
  "List of dust strings used by Supermaven.")

(defvar supermaven-activate-url nil
  "URL for activating Supermaven Pro.")

(defvar supermaven--current-state-id 0
  "Current state ID for completions.")

(defvar supermaven--state-map (make-hash-table :test 'equal)
  "Map of state IDs to their completion states.")

(defvar supermaven--changed-documents (make-hash-table :test 'equal)
  "Map of changed documents.")

;; Default state initialized
(put 'supermaven-mode 'globalized-minor-mode t)
(setq-default supermaven-mode nil)

;; Function to reset configuration to defaults
(defun supermaven-reset-config ()
  "Reset Supermaven configuration to defaults."
  (interactive)
  (setq supermaven-auto-start t
        supermaven-ignore-filetypes '()
        supermaven-disable-inline-completion nil
        supermaven-condition (lambda () nil)
        supermaven-log-level 'info
        supermaven-suggestion-color "#ffffff"
        supermaven-suggestion-face 'shadow))

;; Configuration verification
(defun supermaven-verify-config ()
  "Verify Supermaven configuration is valid."
  (interactive)
  (let ((issues '()))
    (unless (member supermaven-log-level '(off error warn info debug trace))
      (push "Invalid log level" issues))
    (unless (functionp supermaven-condition)
      (push "Invalid condition function" issues))
    (if issues
        (progn
          (supermaven-log-error (format "Configuration issues found: %s" issues))
          nil)
      t)))

(provide 'supermaven-config)

;;; supermaven-config.el ends here