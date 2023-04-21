(require 'emacsql)
(require 'emacsql-sqlite)
(require 'cl-macs)
(require 'org-roam-learn-node)

(defcustom org-roam-learn-db-file (expand-file-name "var/org-roam-learn.db"
						    user-emacs-directory)
  "The location of the db file."
  :type 'string)

(defvar org-roam-learn--db nil)

(defconst org-roam-learn--default-schema
  [:create-table entries ([(id text :primary-key :unique)
			   (tags text)
			   (repetitions integer)
			   (certainty integer)])]
  "The schema used in the db. This should match the org-roam-learn-node class")

(defun org-roam-learn-db-init ()
  "If the file does not exist, create it."
  (if (file-directory-p org-roam-learn-db-file)
      (unless org-roam-learn--db
	(setq org-roam-learn--db (emacsql-sqlite org-roam-learn-db-file)))
    (progn (make-empty-file org-roam-learn-db-file t)
	   (setq org-roam-learn--db (emacsql-sqlite org-roam-learn-db-file))
	   (emacsql org-roam-learn--db org-roam-learn--default-schema))))

(defun org-roam-learn-db-insert (node)
  "Insert NODE into the db. NODE must be of type org-roam-learn-node."
  (cl-assert (and (org-roam-learn-node-p node) org-roam-learn--db))
  (let ((id (org-roam-learn-node-id node))
	(tags (org-roam-learn-node-stringify-tags
	       (org-roam-learn-node-tags node)))
	(repetitions (org-roam-learn-node-repetitions node))
	(certainty (org-roam-learn-node-certainty node)))
    (emacsql org-roam-learn--db [:insert :into entries :values $v1]
	     (vector id tags repetitions certainty))))

(defun org-roam-learn-db-get-entries (tags-constraint
				      repetitions-constraint
				      certainty-constraint)
  "Get a list of entries where CONSTRAINT-FN returns t. The function
CONSTRAINT-FN takes the parameters tags, repetitions and certainty."
  (cl-assert org-roam-learn--db)
  (emacsql org-roam-learn--db
	   [:select [id repetitions] :from entries
		    :where (and (string-search tags-constraint tags)
				(>= repetitions-constraint repetitions)
				(>= certainty-constraint certainty))]))

(defun org-roam-learn-db-get-entries-where-tag (tag-constraint)
  (cl-assert org-roam-learn--db)
  (emacsql org-roam-learn--db
	   [:select [id repetitions certainty] :from entries
		    :where (string-search tag-constraint tags)]))

(defun org-roam-learn-db-get-defined-tags ()
  "Get all tags that are registerd in the db."
  (cl-assert org-roam-learn--db)
  (let* ((tags (emacsql org-roam-learn--db [:select [tags] :from entries]))
	 (actual-tags (car (nth 2 tags))))
    (split-string actual-tags ":")))

(provide 'org-roam-learn-db)
