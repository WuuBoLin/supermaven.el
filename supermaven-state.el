;;; supermaven-state.el --- Improved state management -*- lexical-binding: t; -*-

;;; Commentary:

;; Core state management for Supermaven completions.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'supermaven-logger)

(defclass supermaven-completion-item ()
  ((kind :initarg :kind
         :type string
         :documentation "Type of completion item.")
   (text :initarg :text
         :type string
         :documentation "Completion text.")
   (dedent :initarg :dedent
           :initform ""
           :type string
           :documentation "Dedent information.")
   (delete :initarg :delete
           :initform nil
           :type (or null string)
           :documentation "Text to delete."))
  "A single completion item.")

(defclass supermaven-state ()
  ((id :initarg :id
       :type number
       :documentation "State ID.")
   (prefix :initarg :prefix
          :type string
          :documentation "Completion prefix.")
   (items :initarg :items
         :initform nil
         :type list
         :documentation "List of completion items.")
   (timestamp :initarg :timestamp
              :type number
              :documentation "When this state was created."))
  "Completion state information.")

(defvar supermaven--state-manager nil
  "Global state manager instance.")

(defclass supermaven-state-manager ()
  ((current-id :initform 0
              :type number
              :documentation "Current state ID.")
   (states :initform (make-hash-table :test 'eql)
          :documentation "Map of state IDs to states.")
   (changed-buffers :initform (make-hash-table :test 'equal)
                   :documentation "Map of changed buffer paths.")
   (last-state :initform nil
              :type (or null supermaven-state)
              :documentation "Last known state.")
   (dust-strings :initform nil
                :type list
                :documentation "List of dust strings."))
  "Manager for Supermaven states.")

(cl-defmethod supermaven-state-create ((manager supermaven-state-manager) prefix)
  "Create a new state with PREFIX."
  (with-slots (current-id states) manager
    (cl-incf current-id)
    (let ((state (make-instance 'supermaven-state
                               :id current-id
                               :prefix prefix
                               :timestamp (float-time))))
      (puthash current-id state states)
      (supermaven-state-purge-old manager)
      state)))

(cl-defmethod supermaven-state-get ((manager supermaven-state-manager) id)
  "Get state with ID."
  (with-slots (states) manager
    (gethash id states)))

(cl-defmethod supermaven-state-update ((manager supermaven-state-manager) id items)
  "Update state ID with completion ITEMS."
  (when-let ((state (supermaven-state-get manager id)))
    (oset state items
          (append (oref state items)
                 (mapcar #'supermaven-state--convert-item items)))))

(cl-defmethod supermaven-state-purge-old ((manager supermaven-state-manager))
  "Remove old states."
  (with-slots (states current-id) manager
    (maphash (lambda (id _)
               (when (< id (- current-id 50))
                 (remhash id states)))
             states)))

(cl-defmethod supermaven-state-record-buffer-change ((manager supermaven-state-manager)
                                                   buffer-path content)
  "Record that buffer at BUFFER-PATH has changed to CONTENT."
  (with-slots (changed-buffers) manager
    (puthash buffer-path
             (list :path buffer-path :content content :time (float-time))
             changed-buffers)))

(cl-defmethod supermaven-state-get-changes ((manager supermaven-state-manager))
  "Get all pending buffer changes."
  (with-slots (changed-buffers) manager
    (let (changes)
      (maphash (lambda (_path change)
                 (push `((kind . "file_update")
                        (path . ,(plist-get change :path))
                        (content . ,(plist-get change :content)))
                       changes))
               changed-buffers)
      (clrhash changed-buffers)
      (nreverse changes))))

(defun supermaven-state--convert-item (item)
  "Convert completion ITEM to internal format."
  (make-instance 'supermaven-completion-item
                :kind (or (alist-get 'kind item) "")
                :text (or (alist-get 'text item) "")
                :dedent (or (alist-get 'dedent item) "")
                :delete (alist-get 'delete item)))

(defun supermaven-state-initialize ()
  "Initialize the state manager."
  (setq supermaven--state-manager
        (make-instance 'supermaven-state-manager)))

(defun supermaven-state-cleanup ()
  "Clean up state manager."
  (when supermaven--state-manager
    (with-slots (states changed-buffers) supermaven--state-manager
      (clrhash states)
      (clrhash changed-buffers))
    (setq supermaven--state-manager nil)))

(provide 'supermaven-state)

;;; supermaven-state.el ends here