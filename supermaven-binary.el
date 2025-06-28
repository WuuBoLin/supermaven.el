;;; supermaven-binary.el --- Supermaven binary management -*- lexical-binding: t; -*-

;;; Commentary:

;; This file handles downloading and managing the Supermaven binary.

;;; Code:

(require 'url)
(require 'json)
(require 'supermaven-logger)

(defvar supermaven--os-info nil
  "Cached system information.")

(defvar supermaven--arch-info nil
  "Cached architecture information.")

(defcustom supermaven-binary-path nil
  "Path to the Supermaven binary."
  :type 'string
  :group 'supermaven)

(defun supermaven--get-binary-version ()
  "Get the binary version string."
  "v20")

(defun supermaven--determine-platform ()
  "Get the platform identifier."
  (unless supermaven--os-info
    (setq supermaven--os-info
          (cond
           ((eq system-type 'darwin) "macosx")
           ((eq system-type 'gnu/linux) "linux")
           ((eq system-type 'windows-nt) "windows")
           (t (error "Unsupported platform: %s" system-type)))))
  supermaven--os-info)

(defun supermaven--determine-arch ()
  "Get the architecture identifier, defaulting to aarch64 if unknown."
  (unless supermaven--arch-info
    (let ((machine (car (split-string system-configuration "-"))))
      (setq supermaven--arch-info
            (cond
             ((or (string= machine "aarch64")
                  (string= machine "arm64")) "aarch64")
             ((string= machine "x86_64") "x86_64")
             (t (progn
                  (supermaven-log-warn 
                   (format "Unknown architecture %s, defaulting to aarch64" machine))
                  "aarch64"))))))
  supermaven--arch-info)

(defun supermaven--get-binary-path ()
  "Get the path where the binary should be stored."
  (let* ((platform (supermaven--determine-platform))
         (arch (supermaven--determine-arch))
         (version (supermaven--get-binary-version))
         (binary-name (if (eq system-type 'windows-nt) "sm-agent.exe" "sm-agent"))
         (data-home (or (getenv "XDG_DATA_HOME")
                       (expand-file-name ".local/share" (getenv "HOME"))))
         (binary-dir (expand-file-name
                     (format "supermaven/binary/%s/%s-%s"
                             version platform arch)
                     data-home)))
    (expand-file-name binary-name binary-dir)))

(defun supermaven--construct-download-url ()
  "Construct the download URL for the binary."
  (format "https://supermaven.com/api/download-path-v2?platform=%s&arch=%s&editor=neovim"
          (supermaven--determine-platform)
          (supermaven--determine-arch)))

(defun supermaven--download-binary (url output-path)
  "Download binary from URL to OUTPUT-PATH."
  (supermaven-log-info (format "Downloading Supermaven binary from: %s" url))
  (supermaven-log-debug (format "Target path: %s" output-path))
  (make-directory (file-name-directory output-path) t)
  (let ((temp-file (make-temp-file "supermaven-download-")))
    (supermaven-log-trace (format "Using temporary file: %s" temp-file))
    (unwind-protect
        (progn
          (supermaven-log-debug "Starting binary download...")
          (url-copy-file url temp-file t)
          (supermaven-log-debug "Download completed, installing binary")
          (rename-file temp-file output-path t)
          (set-file-modes output-path #o755)
          (supermaven-log-info (format "Binary successfully installed to: %s" output-path)))
      (when (file-exists-p temp-file)
        (supermaven-log-trace "Cleaning up temporary download file")
        (delete-file temp-file)))))

(defun supermaven--fetch-binary ()
  "Fetch the Supermaven binary."
  (let* ((binary-path (supermaven--get-binary-path))
         (url (supermaven--construct-download-url)))
    
    (supermaven-log-info (format "Fetching binary for system: %s-%s"
                                (supermaven--determine-platform)
                                (supermaven--determine-arch)))
    (supermaven-log-debug (format "API URL: %s" url))
    
    (let* ((url-request-method "GET")
           (response-buffer (url-retrieve-synchronously url t nil 30))
           download-url)
      
      (unless response-buffer
        (supermaven-log-error "Failed to retrieve download information from Supermaven API")
        (error "Failed to retrieve download information from Supermaven API"))
      
      (supermaven-log-debug "Processing API response")
      (unwind-protect
          (with-current-buffer response-buffer
            (goto-char (point-min))
            (re-search-forward "^$")
            (forward-char)
            (let* ((json-object-type 'hash-table)
                   (response (json-read)))
              (setq download-url (gethash "downloadUrl" response))
              (supermaven-log-trace (format "API response: %s" response))))
        (kill-buffer response-buffer))
      
      (unless download-url
        (supermaven-log-error "Failed to get download URL from Supermaven API")
        (error "Failed to get download URL from Supermaven API"))
      
      (supermaven-log-info (format "Received download URL: %s" download-url))
      (supermaven--download-binary download-url binary-path)
      (setq supermaven-binary-path binary-path)
      (supermaven-log-debug (format "Binary path set to: %s" supermaven-binary-path))
      binary-path)))

(defun supermaven--ensure-binary ()
  "Ensure the Supermaven binary is available and up to date."
  (let ((binary-path (supermaven--get-binary-path)))
    (supermaven-log-debug (format "Checking binary at: %s" binary-path))
    
    (when (and (not supermaven-binary-path)
               (file-exists-p binary-path)
               (file-executable-p binary-path))
      (setq supermaven-binary-path binary-path)
      (supermaven-log-info (format "Found existing binary at: %s" binary-path)))
    
    (when (not (file-exists-p binary-path))
      (supermaven-log-warn "Supermaven binary not found, downloading...")
      
      (condition-case err
          (progn
            (supermaven--fetch-binary)
            (supermaven-log-info (format "Successfully installed Supermaven binary to %s" binary-path)))
        (error
         (supermaven-log-error (format "Failed to fetch Supermaven binary: %s" (error-message-string err)))
         (signal (car err) (cdr err)))))
    
    (when (file-exists-p binary-path)
      (setq supermaven-binary-path binary-path)
      (supermaven-log-trace (format "Binary path variable set to: %s" supermaven-binary-path))
      
      (unless (file-executable-p binary-path)
        (supermaven-log-error (format "Binary at %s is not executable, attempting to fix..." binary-path))
        (condition-case err
            (progn
              (set-file-modes binary-path #o755)
              (supermaven-log-info "Binary permissions fixed"))
          (error
           (supermaven-log-error (format "Failed to fix binary permissions: %s" err))))))
    
    (if (file-exists-p binary-path)
        (progn
          (supermaven-log-debug "Binary verification completed successfully")
          binary-path)
      (progn
        (supermaven-log-error "Binary not available after ensure attempt")
        nil))))

(defun supermaven--initialize-binary-path ()
  "Initialize binary path if binary already exists."
  (unless supermaven-binary-path
    (let ((binary-path (supermaven--get-binary-path)))
      (when (and (file-exists-p binary-path)
                 (file-executable-p binary-path))
        (setq supermaven-binary-path binary-path)
        (when (fboundp 'supermaven-log-debug)
          (supermaven-log-debug (format "Initialized binary path at startup: %s" binary-path)))))))

(condition-case nil
    (supermaven--initialize-binary-path)
  (error nil))

(provide 'supermaven-binary)

;;; supermaven-binary.el ends here