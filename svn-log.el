;;; -*- emacs-lisp -*-

(eval-when-compile
  (defvar svn-log-repository-root nil))

(defconst svn-log-version-regexp
  "^------------------------------------------------------------------------\nr\\([0-9]+\\) "
  "* ")

(defvar svn-log-show-path-count 5
  "* ")

(defvar svn-log-font-lock-keywords
  '(("^\\(------------------------------------------------------------------------\n\\)\\(r[0-9]+\\) "
     (1 'header-line)
     (2 'mode-line))))

(defvar svn-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'svn-log-next)
    (define-key map "p" 'svn-log-previous)
    (define-key map "t" 'svn-log-toggle-show-paths)
    (define-key map "g" 'svn-log-goto-version)

    (define-key map [return] 'svn-log-find-file)
    map)
  "* ")

(defvar svn-log-font-lock-keywords
  '((svn-log-version-regexp . font-lock-string-face)))

(defun svn-select-from-buffer (func &optional key prompt)
  (let ((back (make-overlay (point-min) (point-max))))
    (unwind-protect
        (progn
          (overlay-put back 'face '(foreground-color . "red"))
          (select-from-buffer
           (or prompt "Select & Type [return]")
           (or key [return])
           func svn-version-regexp 0 "green"))
      (delete-overlay back))))

(defun svn-log-current-version ()
  (save-excursion
    (beginning-of-line 2)
    (if (search-backward-regexp svn-log-version-regexp nil t)
        (match-string-no-properties 1))))

(defun svn-log-next-file (&optional previous)
  (let ((func (if previous
                  'previous-single-property-change
                'next-single-property-change)))

  (let (to tmp)
    (save-excursion
      (save-match-data
        (when (setq tmp (funcall func (point) 'svn-path))
          (goto-char tmp)
          (or (looking-at " +[A-Z] +")
              (setq tmp (funcall func (point) 'svn-path)))
          (when tmp
            (goto-char tmp)
            (looking-at " +[A-Z] +")
            (setq to (1- (match-end 0)))))))
    (if to (goto-char to)))))

(defun svn-log-next-1 ()
  (let (c1 c2)
    (save-excursion
      (save-match-data
        (setq c1 (if (search-forward-regexp svn-log-version-regexp nil t)
                     (line-beginning-position)))))
    (save-excursion
      (while (and (svn-log-next-file)
                  (or (assoc (get-text-property (point) 'invisible)
                             buffer-invisibility-spec)
                      (prog1 nil
                        (setq c2 (point)))))))
    (cond
     ((and c1 c2) (goto-char (min c1 c2)))
     (c1          (goto-char c1))
     (c2          (goto-char c2))
     (t           nil))))

(defun svn-log-previous-1 ()
  (let (c1 c2)
    (save-excursion
      (save-match-data
        (setq c1 (if (search-backward-regexp svn-log-version-regexp nil t)
                     (line-beginning-position 2)))))
    (save-excursion

      (while (and (progn
                    (beginning-of-line)
                    (svn-log-next-file t))
                  (or (assoc (get-text-property (point) 'invisible)
                             buffer-invisibility-spec)
                      (prog1 nil
                        (setq c2 (point)))))))
    (cond
     ((and c1 c2) (goto-char (max c1 c2)))
     (c1          (goto-char c1))
     (c2          (goto-char c2))
     (t           nil))))

;;; interactive functions

(defun svn-log-next (arg)
  (interactive "p")

  (while (and (< 0 arg) (svn-log-next-1))
    (setq arg (1- arg)))
  arg)

(defun svn-log-previous (arg)
  (interactive "p")

  (while (and (< 0 arg) (svn-log-previous-1))
    (setq arg (1- arg)))
  arg)

(defun svn-log-toggle-show-paths (version)
  (interactive (list (string-to-number (svn-log-current-version))))
  (if (assoc version buffer-invisibility-spec)
      (remove-from-invisibility-spec (cons version t))
    (add-to-invisibility-spec (cons version t)))
  (redraw-display))

(defun svn-log-find-file ()
  (interactive)
  (let ((path (get-text-property (point) 'svn-path))
        (version (svn-log-current-version)))
    (when path
      (let ((ver-file (concat (file-name-nondirectory path) ".~" version "~"))
            (repository-root svn-log-repository-root))
        (with-current-buffer
            (generate-new-buffer ver-file)
          (if (zerop (svn-call-process
                      "cat" (svn-make-version-option version)
                      (concat repository-root path)))
              (progn
                (setq buffer-read-only t)
                (buffer-disable-undo)
                (set-buffer-modified-p nil)
                (display-buffer (current-buffer)))
            (kill-buffer nil)
            nil))))))

(defun svn-log-goto-version (version)
  (interactive "nVersion: ")
  (let (to)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (if (search-forward-regexp
             (format "^------------------------------------------------------------------------\nr%d " version)
             nil t)
            (setq to (line-beginning-position 1)))))
    (if to
        (goto-char to)
      (if (called-interactively-p 'interactive)
          (message "Version %d not found." version))
      nil)))

(defun svn-log-url ()
  (concat svn-log-repository-root
          (get-text-property (point) 'svn-path)))

(defun svn-log-mode (repository-root &optional target)
  "* "
  (if svn-debug
      (message "svn-log-mode(%s)" repository-root))
  (kill-all-local-variables)
  (widen)

  (setq major-mode 'svn-log-mode
        mode-name "Log for Subversion"

        buffer-read-only t
        font-lock-defaults (list 'svn-log-font-lock-keywords)
        buffer-invisibility-spec nil)

  (set (make-local-variable 'svn-log-repository-root) repository-root)

  (set-buffer-modified-p nil)
  (buffer-disable-undo (current-buffer))
  (use-local-map svn-log-mode-map)

  (let (buffer-read-only count version)

    (goto-char (point-min))

    (while (zerop (svn-log-next 1))
      (setq version (string-to-number (svn-log-current-version)))
      (add-to-invisibility-spec (cons version t))

      (forward-line 1)

      (when (looking-at "^.*:$")
        (setq count 0)
        (forward-line 1)
        (while (looking-at " +[A-Z] +\\(.*\\)")
          (put-text-property (point) (line-end-position 1)
                             'svn-path (match-string-no-properties 1))
          (setq count (1+ count))
          (forward-line 1))

        (and (<= 0 svn-log-show-path-count)
             (< svn-log-show-path-count count)
             (put-text-property
              (line-beginning-position (1+ (- svn-log-show-path-count count)))
              (line-end-position 0)
              'invisible version)))))

  (let ((version (svn-get-prop target 'commit-version)))
    (or (and (numberp version)
             (svn-log-goto-version version))
        (goto-char (point-min))))
  (svn-minor-mode 1))

(defun svn-log-get-URL-s ()
  (list (svn-log-url)))

(defun svn-log-get-URL-REV-s ()
  (let ((revision (svn-log-current-version)))
    (if revision
        (list (concat (svn-log-url) "@" revision)))))

(svn-set-minor-mode-config
 'svn-log-mode
 '(("URL-s" . svn-log-get-URL-s)
   ("URL-REV-s" . svn-log-get-URL-REV-s)
   ("VERSION"  . svn-log-current-version)
   ))

(provide 'svn-log)
