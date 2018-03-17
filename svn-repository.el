;;; -*- emacs-lisp -*-

(eval-when-compile
  (defvar hl-line-overlay nil))

(require 'svn)
(require 'folder-mode)

(defvar svn-repository-urls nil
  "* よく使う SVN リポジトリのリスト")

(defconst svn-repository-regexp-revision "\\([0-9]+\\)")
(defconst svn-repository-regexp-author "\\([a-zA-Z0-9_@.?]+\\)")
(defconst svn-repository-regexp-size "\\([0-9]+\\)")
(defconst svn-repository-regexp-manth "\\([^\s]+\\)")

(defconst svn-repository-regexp-time-or-year
  "\\([0-2 ][0-9]:[0-5 ][0-9]\\| [0-9 ][0-9][0-9][0-9]\\)")
(defconst svn-repository-regexp-date
  (concat "\\("
          svn-repository-regexp-manth " [0-3 ][0-9] "
          svn-repository-regexp-time-or-year
          "\\)"))
(defconst svn-repository-regexp-flag "O?")

(defconst svn-repository-list-regexp-type-dir
  (concat "^ *"
          svn-repository-regexp-revision ;;; 1
          " +"
          svn-repository-regexp-author ;;; 2
          " +"
          svn-repository-regexp-date ;;; 3
          " +\\(.+\\)/"))            ;;; 6

(defconst svn-repository-list-regexp-type-file
  (concat "^ *"
          svn-repository-regexp-revision ;;; 1
          " +"
          svn-repository-regexp-author ;;; 2
          " +"
          svn-repository-regexp-flag
          " +"
          svn-repository-regexp-size ;;; 3
          " +"
          svn-repository-regexp-date ;;; 4
          " +\\(.+\\)"))             ;;; 7

(defvar svn-repository-prop-obarray (make-vector 17 0))

(defvar svn-repository-info-short-format
  '((revision 6 t)
    (" ")))

(defvar svn-repository-info-long-format
  '((revision 6 t)
    (" ")
    (author 8)
    (" ")
    (date 12 t)
    (" ")
    (size 8 t)
    (" ")))

(defvar svn-repository-debug nil "* debug")

(defun svn-repository-set-prop (url property value)
  (if svn-repository-debug
      (message "SET [%s] [%s] [%s]" url property value))
  (put (intern url svn-repository-prop-obarray) property value))

(defun svn-repository-get-prop (url property)
  (if svn-repository-debug
      (message "GET [%s] [%s]" url property))
  (get (intern-soft url svn-repository-prop-obarray) property))

(defun svn-repository-symbol-convert (url symbol)
  (cond
   ((memq symbol '(author date))
    (svn-repository-get-prop url symbol))
   ((eq symbol 'revision)
    (let ((n (svn-repository-get-prop url symbol)))
      (if n (number-to-string n))))
   ((eq symbol 'size)
    (or (svn-repository-get-prop url 'size) ""))
   (t
    (symbol-name symbol))))

(defun svn-repository-convert-item (url form)
  (let ((item (nth 0 form))
        (size (nth 1 form))
        (opt (nth 2 form))
        value)

    (setq value (or (cond
                     ((null item) nil)
                     ((stringp item) item)
                     ((numberp item) (number-to-string item))
                     ((symbolp item) (svn-repository-symbol-convert url item))
                     (t (format "%s" item)))
                    ""))

    (if (numberp size)
        (setq value (folder-truncate-string value size opt)))

    value))

(defun svn-repository-format-internal (url form)
  (mapconcat (lambda (item) (svn-repository-convert-item url item)) form ""))

(defun svn-repository-info-short (url)
  (svn-repository-format-internal url svn-repository-info-short-format))

(defun svn-repository-info-long (url)
  (svn-repository-format-internal url svn-repository-info-long-format))

(defun svn-repository-get-child (base-url)
  (with-temp-buffer
    (save-match-data
      (if (zerop (svn-call-process "list" '("-v") base-url))
          (let (dirs files url)
            (goto-char (point-min))
            (while (not (eobp))
              (cond
               ((looking-at svn-repository-list-regexp-type-dir)
                (setq url
                      (if (string= (match-string 6) ".")
                          base-url
                        (concat base-url "/" (match-string 6))))
                (svn-repository-set-prop url 'revision
                                         (string-to-number (match-string 1)))
                (svn-repository-set-prop url 'author (match-string 2))
                (svn-repository-set-prop url 'date (match-string 3))
                (svn-repository-set-prop url 'node-kind 'directory)
                (or (string= (match-string 6) ".")
                    (setq dirs (cons (match-string 6) dirs))))

               ((looking-at svn-repository-list-regexp-type-file)
                (setq url (concat base-url "/" (match-string 7)))
                (svn-repository-set-prop url 'revision
                                         (string-to-number (match-string 1)))
                (svn-repository-set-prop url 'author (match-string 2))
                (svn-repository-set-prop url 'size (match-string 3))
                (svn-repository-set-prop url 'date (match-string 4))
                (svn-repository-set-prop url 'node-kind 'file)
                (setq files (cons (match-string 7) files)))
               (t
                (message (buffer-substring (line-beginning-position)
                                           (line-end-position)))))

              (forward-line 1))
            (append (mapcar
                     (lambda (f)
                       (folder-make-info (concat base-url "/" f) t f))
                     (sort dirs 'string<))
                    (mapcar
                     (lambda (f)
                       (folder-make-info (concat base-url "/" f) nil f))
                     (sort files 'string<))))))))

(defun svn-repository-process-cat (url)
  (or (eq (svn-repository-get-prop url 'node-kind) 'file)
      (error "Not a file: %s" url))
  (svn-cat (list url)))

(defun svn-repository-process-log (url)
  (svn-command "log" (list url) '("-l" "1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun svn-repository (url)
  (interactive (list (svn-completing-read "URL: " svn-repository-urls)))

  (save-match-data
    (if (string-match "^[ \t\n\r]+" url)
        (setq url (substring url (match-end 0))))
    (if (string-match "[ \t\n\r]+$" url)
        (setq url (substring url 0 (match-beginning 0))))
    (if (string-match "/$" url)
        (setq url (substring url 0 (match-beginning 0)))))

  (with-temp-buffer
    (or (zerop (svn-call-process "info" nil url))
        (error "Can't access repository: %s" url)))

  (or (member url svn-repository-urls )
      (setq svn-repository-urls (cons url svn-repository-urls)))

  (let ((buffer-name (format "*SVN Repository %s*" url)))

    (if (and (get-buffer buffer-name)
             (y-or-n-p "Use old buffer? "))
        (switch-to-buffer buffer-name)

      (switch-to-buffer (generate-new-buffer buffer-name))
      (kill-all-local-variables)

      (setq major-mode 'svn-repository-mode
            mode-name "SVN Repository")

      ;; top の バージョン番号が表示されない件の対処
      ;; ToDo: 美しくない

      (svn-repository-get-child url)

      (folder-setup (folder-make-info url t url)
                    '(svn-repository-get-child)
                    '(svn-repository-info-short
                      svn-repository-info-long)
                    '(svn-repository-process-cat
                      svn-repository-process-log
                      )
                    )

      (use-local-map folder-mode-map)

      (set (make-local-variable 'hl-line-overlay) (make-overlay 1 1))
      (overlay-put hl-line-overlay 'face 'underline)

      (svn-minor-mode 1)

      (hl-line-mode 1)
      (buffer-disable-undo)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t
            truncate-lines t))))

(defun svn-repository-get-URL-s ()
  (let ((URLs (folder-get-path)))
    (if (listp URLs) URLs (list URLs))))

(svn-set-minor-mode-config
 'svn-repository-mode
 '(("URL-s" . svn-repository-get-URL-s)))


(provide 'svn-repository)

