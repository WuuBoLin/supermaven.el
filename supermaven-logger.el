;;; supermaven-logger.el --- Supermaven logging -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains logging functions for Supermaven.

;;; Code:

(require 'supermaven-config)

(defconst supermaven-log-buffer-name "*Supermaven Log*"
  "Name of the Supermaven log buffer.")

(defun supermaven-log (level message)
  "Log MESSAGE at LEVEL."
  (when (>= (supermaven--log-level-to-number supermaven-log-level)
            (supermaven--log-level-to-number level))
    (let ((buffer (get-buffer-create supermaven-log-buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (was-read-only buffer-read-only))
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
          (insert (format "[%s] %s\n" (upcase (symbol-name level)) message))
          ;; Set buffer to read-only for viewing, but allow programmatic writes
          (unless was-read-only
            (setq buffer-read-only t)))))))

(defun supermaven-log-trace (message)
  "Log MESSAGE at TRACE level."
  (supermaven-log 'trace message))

(defun supermaven-log-debug (message)
  "Log MESSAGE at DEBUG level."
  (supermaven-log 'debug message))

(defun supermaven-log-info (message)
  "Log MESSAGE at INFO level."
  (supermaven-log 'info message))

(defun supermaven-log-warn (message)
  "Log MESSAGE at WARN level."
  (supermaven-log 'warn message))

(defun supermaven-log-error (message)
  "Log MESSAGE at ERROR level."
  (supermaven-log 'error message))

(defun supermaven--log-level-to-number (level)
  "Convert log LEVEL to a number for comparison."
  (pcase level
    ('off 0)
    ('error 1)
    ('warn 2)
    ('info 3)
    ('debug 4)
    ('trace 5)
    (_ 3)))

(defun supermaven-show-log ()
  "Show the Supermaven log buffer."
  (interactive)
  (let ((buffer (get-buffer-create supermaven-log-buffer-name)))
    (with-current-buffer buffer
      (view-mode 1)
      (goto-char (point-max)))
    (switch-to-buffer-other-window buffer)))

(defun supermaven-clear-log ()
  "Clear the Supermaven log buffer."
  (interactive)
  (let ((buffer (get-buffer supermaven-log-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer))))))

(provide 'supermaven-logger)

;;; supermaven-logger.el ends here
