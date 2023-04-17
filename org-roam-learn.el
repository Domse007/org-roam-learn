(require 'org-roam-learn-node)
(require 'org-roam-learn-db)

(require 'org-roam-node)
(require 'org-roam)

(defgroup org-roam-learn nil
  "An extension to org-roam to learn nodes."
  :lighter " orl")

(defcustom org-roam-learn-selector (lambda (l) (seq-random-elt))
  "The function that decides which node should be opened. It takes a list of
org-roam-learn-node and must return one of them as the result."
  :type 'function)

(defvar org-roam-learn--last-tag nil
  "The last tag that was used.")

(defun org-roam-learn-init ()
  (org-roam-learn-db-init))

(defun org-roam-learn-add ()
  (interactive)
  (let* ((nodes (org-roam-node-list))
	 (selected (completing-read "Node: " nodes nil t))
	 (node (org-roam-learn-node-new (org-roam-node-file selected)
					(org-roam-node-tags selected))))
    (org-roam-learn-db-insert node)))

(defun org-roam-learn-learn ()
  (interactive)
  (let* ((tags (org-roam-learn-db-get-defined-tags))
	 (selected-tag (completing-read "Tag: " tags nil t))
	 (queried (org-roam-learn-db-get-entries-where-tag selected-tag))
	 (calculated (apply org-roam-learn-selector queried)))
    (setq org-roam-learn--last-tag calculated)
    (org-roam-id-open (org-roam-learn-node-get-id node))))

(defun org-roam-learn-next ()
  (interactive)
  (if org-roam-learn--last-tag
      (let ((queried (org-roam-learn-db-get-entries-where-tag selected-tag))
	    (calculated (apply org-roam-learn-selector queried)))
	(org-roam-id-open (org-roam-learn-node-get-id calculated)))
    (error "There was no previous tag selected")))

(provide 'org-roam-learn)
