(require 'cl-macs)

(defclass org-roam-learn-node nil
  (id :initarg :id :reader org-roam-learn-node-get-id)
  (tags :initarg :tags :reader org-roam-learn-node-get-tags)
  (repetitions :reader org-roam-learn-node-get-repetitions
               :accessor org-roam-learn-node-set-repetitions)
  (certainty :reader org-roam-learn-node-get-certainty
             :accessor org-roam-learn-node-set-certainty))

(defun org-roam-learn-node-new (id tags)
  (let ((node (org-roam-learn-node :name name :tags tags)))
    (org-roam-learn-node-set-repetitions 0)
    (org-roam-learn-node-set-certainty 0.)
    node))

(defun org-roam-learn-node-stringify-tags (node)
  "Get the tags of NODE and return one string."
  (cl-assert (org-roam-learn-node-p node))
  (mapconcat #'identity (org-roam-learn-get-tags node) ":"))

(provide 'org-roam-learn-node)
