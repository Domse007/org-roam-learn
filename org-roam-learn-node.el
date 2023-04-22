(require 'cl-macs)

(cl-defstruct org-roam-learn-node
  (id nil :read-only t :type 'string)
  (tags nil :read-only t :type 'list)
  (repetitions 0 :read-only nil :type 'number)
  (certainty 0 :read-only nil :type 'number))

(defun org-roam-learn-node-certainty-inc (node cert)
  "Increment the certainty of NODE by CERT."
  (cl-assert (org-roam-learn-node-p node))
  (setf (org-roam-learn-node-certainty node)
        (+ (org-roam-learn-node-certainty node) cert)))

(defun org-roam-learn-node-certainty-dec (node cert)
  "Decrement the certainty of NODE by CERT."
  (cl-assert (org-roam-learn-node-p node))
  (setf (org-roam-learn-node-certainty node)
        (- (org-roam-learn-node-certainty node) cert)))

(defun org-roam-learn-node-repetitions-inc (node)
  "Increment the number of times NODE was learned."
  (cl-assert (org-roam-learn-node-p node))
  (setf (org-roam-learn-node-repetitions node)
        (1+ (org-roam-learn-node-repetitions node))))

(defun org-roam-learn-node-stringify-tags (tags)
  "Get the tags of NODE and return one string."
  (mapconcat #'identity tags ":"))

(defun org-roam-learn-node-unstring-tags (s)
  "Take a stringified tags list and return a list."
  (unless (string-equal s "") (split-string s ":")))

(provide 'org-roam-learn-node)
