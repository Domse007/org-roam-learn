;; (org-end-of-subtree)

(require 'org-roam)

(defvar org-roam-learn-overlays '()
  "List of all overlays that are applied by `org-roam-learn'")

(defcustom org-roam-registered-tags '()
  "List of tags that are used by `org-roam-learn'")

(defun org-roam-learn-node (node)
  "Learn a NODE. This will hide the buffer contents and only show the title of
the NODE. To reveal the node use `org-roam-learn-reveal'."
  (org-roam-node-visit node)
  (let ((dest-pos)
        (node-name (org-roam-node-title node))
        (node-pos (org-roam-node-point node)))
    (save-excursion
      (goto-char node-pos)
      (goto-char (re-search-forward node-name))
      (next-line)
      ;; (goto-char (point-min))
      ;; (setq dest-pos (re-search-forward "#\\+\\(TITLE\\|Title\\|title\\):"
      ;;   				nil t))
      ;; (goto-char dest-pos)
      (beginning-of-line)
      (save-excursion
        (let* ((start-pos (point))
               (end-pos (progn (org-end-of-subtree)
                               (point)))
               (overlay (make-overlay start-pos end-pos (current-buffer))))
          (setq dest-pos start-pos)
          (overlay-put overlay 'display "...")
          (push overlay org-roam-learn-overlays))))
    (goto-char (1+ dest-pos))
    (recenter-top-bottom 1)))

(defun org-roam-learn-reveal ()
  "Remove the overlay from the current buffer."
  (interactive)
  (dolist (overlay org-roam-learn-overlays)
    (when (string-equal (buffer-name (current-buffer))
			(buffer-name (overlay-buffer overlay)))
      (delete-overlay overlay)
      (message "Revealing..."))))

(defun org-roam-learn-reveal-all ()
  "Remove all overlays that are currently applied by this package."
  (interactive)
  (unless org-roam-learn-overlays
    (dolist (overlay dk/roam-learn-overlays)
      (delete-overlay overlay))))

(defun org-roam-learn-select-random-node (nodes)
  "Take a random node from NODES and return it."
  (seq-random-elt nodes))

(defun org-roam-learn-matches-tag (node tag)
  "Check if NODE has TAG as one of it's members."
  (member tag (org-roam-node-tags node)))

(defun org-roam-learn-get-nodes-matching-tag (tag)
  (let* ((nodes (org-roam-node-list))
	 (matched-nodes (cl-remove-if-not
		         (lambda (node) (org-roam-learn-matches-tag node tag))
                         nodes)))
    matched-nodes))

(defun org-roam-learn ()
  "Select a tag and learn a random node."
  (interactive)
  (let* ((tag (completing-read "Tag: " org-roam-registered-tags nil t))
	 (nodes (org-roam-learn-get-nodes-matching-tag tag))
	 (result (org-roam-learn-select-random-node nodes)))
    (org-roam-learn-node result)))

(defun org-roam-learn-random-by-tag (tag)
  "Visit a random node that has TAG."
  (let ((nodes (org-roam-learn-get-nodes-matching-tag tag))
	(node (org-roam-learn-select-random-node nodes)))
    (org-roam-learn-node node)))

(provide 'org-roam-learn)
