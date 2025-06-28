;;; supermaven-util.el --- Supermaven utility functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Utility functions for Supermaven.

;;; Code:

(require 'cl-lib)

(defun supermaven-util-current-line ()
  "Get the current line number."
  (line-number-at-pos))

(defun supermaven-util-current-column ()
  "Get the current column number."
  (current-column))

(defun supermaven-util-buffer-string ()
  "Get the entire buffer as a string."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun supermaven-util-line-at-point ()
  "Get the current line as a string."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun supermaven-util-word-at-point ()
  "Get the word at point."
  (thing-at-point 'word t))

(defun supermaven-util-symbol-at-point ()
  "Get the symbol at point."
  (thing-at-point 'symbol t))

(defun supermaven-util-line-before-point ()
  "Get text from beginning of line to point."
  (buffer-substring-no-properties
   (line-beginning-position)
   (point)))

(defun supermaven-util-line-after-point ()
  "Get text from point to end of line."
  (buffer-substring-no-properties
   (point)
   (line-end-position)))

(defun supermaven-util-trim-string (str)
  "Remove whitespace from beginning and end of STR."
  (string-trim str))

(defun supermaven-util-starts-with-p (str prefix)
  "Check if STR starts with PREFIX."
  (and (>= (length str) (length prefix))
       (string= (substring str 0 (length prefix)) prefix)))

(defun supermaven-util-ends-with-p (str suffix)
  "Check if STR ends with SUFFIX."
  (and (>= (length str) (length suffix))
       (string= (substring str (- (length str) (length suffix))) suffix)))

(defun supermaven-util-split-lines (str)
  "Split STR into lines."
  (split-string str "\n"))

(defun supermaven-util-join-lines (lines)
  "Join LINES with newlines."
  (string-join lines "\n"))

(defun supermaven-util-file-path ()
  "Get current buffer file path."
  (buffer-file-name))

(defun supermaven-util-relative-file-path ()
  "Get current buffer file path relative to project root."
  (when-let ((file-path (buffer-file-name)))
    (if (and (boundp 'projectile-project-root)
             (fboundp 'projectile-project-root))
        (file-relative-name file-path (projectile-project-root))
      file-path)))

(defun supermaven-util-buffer-size ()
  "Get size of current buffer in characters."
  (buffer-size))

(defun supermaven-util-point-to-offset ()
  "Convert current point to character offset."
  (1- (point)))

(defun supermaven-util-offset-to-point (offset)
  "Convert character OFFSET to point."
  (1+ offset))

(defun supermaven-util-safe-substring (str start &optional end)
  "Safely get substring of STR from START to END."
  (when (and str (stringp str))
    (let ((len (length str))
          (start (max 0 (or start 0)))
          (end (min (length str) (or end (length str)))))
      (when (< start len)
        (substring str start end)))))

(defun supermaven-util-truncate-string (str max-length)
  "Truncate STR to MAX-LENGTH characters."
  (if (> (length str) max-length)
      (concat (substring str 0 (- max-length 3)) "...")
    str))

(defun supermaven-util-ensure-trailing-newline (str)
  "Ensure STR ends with a newline."
  (if (string-suffix-p "\n" str)
      str
    (concat str "\n")))

(defun supermaven-util-remove-trailing-newline (str)
  "Remove trailing newline from STR."
  (if (string-suffix-p "\n" str)
      (substring str 0 -1)
    str))

(defun supermaven-util-indent-string (str indent)
  "Indent each line of STR with INDENT spaces."
  (let ((indent-str (make-string indent ?\s)))
    (mapconcat (lambda (line)
                 (if (string-empty-p line)
                     line
                   (concat indent-str line)))
               (split-string str "\n")
               "\n")))

(defun supermaven-util-get-indentation ()
  "Get indentation of current line."
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun supermaven-util-line-empty-p ()
  "Check if current line is empty or contains only whitespace."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[[:space:]]*$")))

(defun supermaven-util-in-string-p ()
  "Check if point is inside a string."
  (nth 3 (syntax-ppss)))

(defun supermaven-util-in-comment-p ()
  "Check if point is inside a comment."
  (nth 4 (syntax-ppss)))

(defun supermaven-util-in-string-or-comment-p ()
  "Check if point is inside a string or comment."
  (or (supermaven-util-in-string-p)
      (supermaven-util-in-comment-p)))

(provide 'supermaven-util)

;;; supermaven-util.el ends here
