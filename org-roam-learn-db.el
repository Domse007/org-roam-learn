(require 'emacsql)
(require 'emacsql-sqlite)
(require 'dash)
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

(defun org-roam-learn-db-get-entries-where-tag (tag-constraint)
  (cl-assert org-roam-learn--db)
  (message "constr: %s" (format "%%%s%%" tag-constraint))
  (let* ((result (emacsql org-roam-learn--db
			  [:select [id tags repetitions certainty]
				   :from entries ]))
	 ;; Very hacky... Should be a constraint in the query, but oh well...
	 (filtered (-filter (lambda (e)
			      (when (string-match tag-constraint (nth 1 e))
				e))
			    result)))
    (mapcar (lambda (l) (make-org-roam-learn-node
			 :id (nth 0 l)
			 :tags (org-roam-learn-node-unstring-tags (nth 1 l))
			 :repetitions (nth 2 l)
			 :certainty (nth 3 l)))
	    filtered)))

(defun org-roam-learn-db-get-defined-tags ()
  "Get all tags that are registerd in the db."
  (cl-assert org-roam-learn--db)
  (let* ((tags (emacsql org-roam-learn--db [:select [tags] :from entries]))
	 (splitten (mapcar (lambda (i) (let ((c (car i)))
					 (org-roam-learn-node-unstring-tags c)))
			   tags))
	 (flatten (-flatten splitten))
	 (non-nil (-non-nil flatten))
	 (distinct (-distinct non-nil)))
    distinct))

(provide 'org-roam-learn-db)
