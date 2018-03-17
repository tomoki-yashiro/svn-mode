;;; -*- emacs-lisp -*-

(require 'svn)
(require 'file-explorer)

(defvar svn-explorer-debug nil
  "* ")

(defvar svn-explorer-info-short-format
  '((status 8)
    (" ")
    (commit-version 5 t)
    (" ")))

(defvar svn-explorer-info-long-format
  '((status 8)
    (" ")
    (working-version 5 t)
    (" ")
    (commit-user 11)
    (" ")
    (commit-version 5 t)
    (" ")))

;;; 自動的に更新するので、直接操作してはいけない。
(defvar svn-explorer-buffers nil)

(defvar svn-explorer-working-version-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3] 'svn-explorer-mouse-diff)
    map))

(defvar svn-explorer-local-map-alist
  (list
   (cons 'working-version svn-explorer-working-version-map)
   (cons 'commit-version svn-explorer-working-version-map)))

(defgroup svn-explorer-faces nil
  "Fontification colors."
  :prefix "svn-explore-"
  :group 'svn)

(defface svn-explorer-added-face
  '((((class color) (background light))
     (:background "pink" :weight bold))
    (t
     (:weight bold)))
  "* 追加されたファイルのフェイス"
  :group 'svn-explorer-faces)

(defface svn-explorer-deleted-face
  '((((class color) (background light))
     (:background "tan" :strike-through t :bold t))
    (t
     (:bold t)))
  "* 削除されたファイルのフェイス"
  :group 'svn-explorer-faces)

(defface svn-explorer-modified-face
  '((((class color) (type tty))
     (:foreground "red" :bold t))
    (((class color) (background light))
     (:background "light sky blue" :bold t))
    (((class color) (background dark))
     (:foreground "OrangeRed" :bold t))
    (t
     (:bold t)))
  "* 修正されたファイルのフェイス"
  :group 'svn-explorer-faces)

(defface svn-explorer-removed-face
  '((((class color) (background light))
     (:background "sandy brown" :strike-through t :bold t))
    (t
     (:bold t)))
  "* 削除されたファイルのフェイス"
  :group 'svn-explorer-faces)

(defface svn-explorer-updated-face
  '((((class color) (background light))
     (:background "pale green" :bold t))
    (t
     (:bold t)))
  "* 更新されたファイルのフェイス"
  :group 'svn-explorer-faces)

(defface svn-explorer-conflict-face
  '((((class color) (background light))
     (:background "tomato" :bold t))
    (t
     (:bold t)))
  "* コンフリクトしたファイルのフェイス"
  :group 'svn-explorer-faces)

(defface svn-explorer-not-under-control-face
  '((((class color) (background light))
     (:background "snow1" :foreground "snow4" :bold t))
    (t
     (:bold t)))
  "* 管理外ファイルのフェイス"
  :group 'svn-explorer-faces)

(defvar svn-explorer-font-lock-keywords
  (append
   '(("^ A .*\n"    . 'svn-explorer-added-face)
     ("^ D .*\n"    . 'svn-explorer-deleted-face)
     ("^ M .*\n"    . 'svn-explorer-modified-face)
     ("^ R .*\n"    . 'svn-explorer-removed-face)
     ("^ [PU] .*\n" . 'svn-explorer-updated-face)
     ("^ C .*\n"    . 'svn-explorer-conflict-face)
     ("^ \\? .*\n"  . 'svn-explorer-not-under-control-face)
     ("^  M .*\n"    . 'svn-explorer-modified-face)
     ("^  C .*\n"    . 'svn-explorer-conflict-face)
     ("^        \\*.*\n" . 'svn-explorer-updated-face))
   file-explorer-font-lock-keywords))

(defun svn-create-version-menu (versions)
  (list
   (cons "Revision"
         (mapcar (lambda (v) (cons v v)) versions))))

(defun svn-explorer-mouse-diff (event)
  (interactive "e")
  (folder-mouse-goto event)
  (let* ((file (folder-current-path))
         (menu (cons "Diff with"
                     (svn-create-version-menu (svn-get-versions file))))
         (version (x-popup-menu t menu)))
    (if version
        (svn-ediff (list file) version))))

(defun svn-explorer-mouse-update (event)
  (interactive "e")
  (folder-mouse-goto event)
  (let* ((file (folder-current-path))
         (menu (cons "Update to"
                     (svn-create-version-menu (svn-get-versions file))))
         (version (x-popup-menu t menu)))
    (if version
        (svn-update (list file) version))))

(defun svn-explorer-change-status (files)
  (if svn-explorer-debug
      (message "svn-explorer-change-status(%s)" files))
  (mapc (lambda (buffer)
          (when (buffer-live-p buffer)
            (set-buffer buffer)
            (folder-update-path (mapcar 'directory-file-name files))))
        svn-explorer-buffers))

(defun svn-explorer-regist-buffer ()
  (let ((buffer (current-buffer)))
    (or (memq buffer svn-explorer-buffers)
        (setq svn-explorer-buffers (cons buffer svn-explorer-buffers)))
    (or (local-variable-p 'kill-buffer-hook)
        (make-local-variable 'kill-buffer-hook))
    (add-hook 'kill-buffer-hook
              `(lambda ()
                 (setq svn-explorer-buffers
                       (delq ,buffer svn-explorer-buffers))) nil t)))

(defun svn-explorer-symbol-convert (symbol f)
  (cond
   ((memq symbol '(working-version commit-version))
    (let ((n (svn-get-prop f symbol)))
      (if n (number-to-string n))))
   ((memq symbol '(commit-user))
    (svn-get-prop f symbol))
   ((eq symbol 'status)
    (or (svn-get-prop f 'status) "?"))
   (t
    (symbol-name symbol))))

(defun svn-explorer-convert-item (file form)
  (save-match-data
    (let ((item (nth 0 form))
          (size (nth 1 form))
          (opt (nth 2 form))
          value)

      (setq value (or (cond
                       ((null item) nil)
                       ((stringp item) item)
                       ((numberp item) (number-to-string item))
                       ((functionp item) (funcall item file))
                       ((symbolp item) (svn-explorer-symbol-convert item file))
                       (t (format "%s" item)))
                      ""))

      (if (numberp size)
          (setq value (folder-truncate-string value size opt)))

      (and (symbolp item)
           (not (null item))
           (assoc-default item svn-explorer-local-map-alist)
           (string-match "[^ ]+\\( +[^ ]+\\)*" value)
           (add-text-properties
            (match-beginning 0) (match-end 0)
            (list 'local-map (assoc-default item svn-explorer-local-map-alist)
                  'mouse-face 'highlight)
            value)
           )
      value)))

(defun svn-explorer-format-internal (file form)
  (mapconcat (lambda (item) (svn-explorer-convert-item file item)) form ""))

(defun svn-explorer-info-short (file)
  (svn-explorer-format-internal file svn-explorer-info-short-format))

(defun svn-explorer-info-long (file)
  (svn-explorer-format-internal file svn-explorer-info-long-format))

(defun svn-get-child (dir)
  (setq dir (file-name-as-directory dir))

  (let (cdirs cfiles udirs ufiles)
    (mapc (lambda (file)
            (svn-set-prop (concat dir file) 'status " ")
            (if (file-directory-p (concat dir file))
                (setq cdirs (cons file cdirs))
              (setq cfiles (cons file cfiles))))
          (svn-get-svn-files dir))
    (save-match-data
      (mapc (lambda (file)
              (or (member file cdirs)
                  (member file cfiles)
                  (string-match "~[0-9]+\\(\\.[0-9]+\\)*\\.?~$" file)
                  (string-match "\\.\\(orig\\|rej\\)$" file)
                  (if (file-directory-p (concat dir file))
                      (or (string= ".svn" file)
                          (setq udirs (cons file udirs)))
                    (setq ufiles (cons file ufiles)))))
            (directory-files dir nil "^\\([^.]\\|\\.[^.#]\\|\\.\\..\\)" t)))
    (file-explorer-make-info dir (append (sort cdirs 'string<)
                                         (sort cfiles 'string<)
                                         (sort udirs 'string<)
                                         (sort ufiles 'string<)))))

(defun svn-explorer (top)
  (interactive "DDir: ")
  (file-explorer top "Subversion"
                 '(svn-get-child)
                 '(svn-explorer-info-short svn-explorer-info-long))
  (setq font-lock-defaults (list 'svn-explorer-font-lock-keywords))
  (font-lock-mode 1))

(defun svn-explorer-update-dir (dir)
  (setq dir (file-name-as-directory dir))

  (when (svn-is-under-svn dir)

    ;; 必要なら svn-minor-mode にする
    (unless svn-minor-mode
      (svn-minor-mode t)
      (svn-explorer-regist-buffer))
    ;; ステータス更新
    (svn-update-status (list dir))))

(add-hook 'svn-change-status-hook 'svn-explorer-change-status)
(add-hook 'folder-open-folder-hook 'svn-explorer-update-dir)

(svn-set-minor-mode-config
 'file-explorer-mode
 '(("WCPATH-s" . file-explorer-get-files)
))

(provide 'svn-explorer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
