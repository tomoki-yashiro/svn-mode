;;; -*- emacs-lisp -*-
;;;

;;; ToDo
;;; ・ドキュメント充実
;;; ・バージョンを指定する部分で日付の指定ができない。
;;; ・再帰的な update した時に、状態の更新が再帰的にできない。
;;; ・svn-import 等、未テストな関数がある。
;;; ・バージョンの一覧を取得する処理に時間がかかる。キャッシュした方が良いかも。

(autoload 'svn-blame-mode "svn-blame")
(autoload 'svn-update-mode "svn-update")
(autoload 'svn-log-mode "svn-log")
(autoload 'svn-explorer "svn-explorer" nil t)
(autoload 'svn-repository "svn-repository" nil t)

;;; defcustom すべき項目

(defvar svn-client-command "svn"
  "* SVN client command.")

(defvar svn-commit-type nil
  "* specify how to input commit message.
nil means not specified.
'log-edit means using log-edit module.
'read-string means read-string function.")

(defvar svn-option-list-max 50)

(defvar svn-diff-switches
  (if diff-switches
      (let ((switches)
            (switch (split-string diff-switches)))
        (while switch
          (setq switches (append switches (list "-x" (car switch)))
                switch (cdr switch)))
        switches))
  "* ")

(defconst svn-version-regexp "\\([1-9][0-9]*\\)"
  "")

(defconst svn-command-config
  '("add"
    "blame"
    "cat"
    "changelist"
    "checkout"
    "cleanup"
    "commit"
    "copy"
    "delete"
    "diff"
    "export"
    "help"
    "import"
    "info"
    "list"
    "lock"
    "log"
    "merge"
    "mergeinfo"
    "mkdir"
    "move"
    "propdel"
    "propedit"
    "propget"
    "proplist"
    "propset"
    "resolve"
    "resolved"
    "revert"
    "status"
    "switch"
    "unlock"
    "update"))

(defconst svn-status-regexp
  "^\\([ACDMRX!~I ][CM ][L ][+ ][S ][K ]\\) +\\([0-9]+\\|-\\) +\\([0-9]+\\|\\?\\) +\\([^ ]+\\) +\\(.*\\)"
  "* ")

(defconst svn-status-u-regexp
  "^\\([ACDMRX!~I ][CM ][L ][+ ][S ][KOBT ] [* ]\\) +\\([0-9]+\\|-\\)? +\\([0-9]+\\|\\?\\)? +\\([^ ]*\\) +\\(.*\\)"
  "* ")

;;; バッファローカルでなくてはならない
(eval-when-compile
  (load "ediff" t)
  (defvar svn-get-URL-s-function nil)
  (defvar svn-get-URL-REV-s-function nil)
  (defvar svn-get-WCPATH-s-function nil)
  (defvar svn-get-WCPATH-REV-s-function nil)
  (defvar svn-get-VERSION-function nil)
  (defvar svn-mode-line nil)
  (defvar svn-process nil))

(make-variable-buffer-local 'svn-minor-mode)
(put 'svn-minor-mode 'permanent-local t)
(defvar svn-minor-mode nil)

;;;
(defvar svn-file-prop-obarray (make-vector 17 0))
(defvar svn-process-prop-obarray (make-vector 17 0))
(defvar svn-change-status-hook nil)
(defvar svn-option-list nil)
(defvar svn-debug nil "* Non-nil ならば svn のデバッグが有効になる。")
(defvar svn-update-queue nil)
(defvar svn-update-process nil)
(defvar svn-completion-function nil)

(defvar svn-version-0-1 nil)
(defvar svn-version-1-2 nil)
(defvar svn-version-0-2 nil)
(defvar svn-version-1   nil)

(defvar svn-sentinel-list
  '(("add" . svn-update-sentinel)
    ("blame" . svn-blame-sentinel)
                                        ;("cat" . nil)
                                        ;("changelist" . nil)
    ("checkout" . svn-update-sentinel)
                                        ;("cleanup" . nil)
    ("commit" . svn-commit-sentinel)
    ("copy" . svn-process-sentinel-default)
    ("delete" . svn-update-sentinel)
    ("diff" . svn-diff-sentinel)
                                        ;("export" . nil)
                                        ;("help" . nil)
                                        ;("import" . nil)
                                        ;("info" . nil)
                                        ;("list" . nil)
                                        ;("lock" . nil)
    ("log" . svn-log-sentinel)
                                        ;("merge" . nil)
                                        ;("mergeinfo" . nil)
                                        ;("mkdir" . nil)
    ("move" . svn-process-sentinel-default)
                                        ;("propdel" . nil)
                                        ;("propedit" . nil)
                                        ;("propget" . nil)
                                        ;("proplist" . nil)
    ("propset" . svn-process-sentinel-default)
                                        ;("resolve" . nil)
                                        ;("resolved" . nil)
    ("revert" . svn-update-sentinel)
    ("status" . svn-status-sentinel)
                                        ;("switch" . nil)
                                        ;("unlock" . nil)
    ("update" . svn-update-sentinel)
    ))

(defvar svn-prefix-map
  (let ((map (make-sparse-keymap)))

    ;; リポジトリを直接変更する可能性のあるコマンド
    ;; 属性関連を除く
    (define-key map "C" 'svn-copy)
    (define-key map "D" 'svn-delete)
    (define-key map "I" 'svn-import)
    (define-key map "K" 'svn-mkdir)
    (define-key map "L" 'svn-lock)
    (define-key map "M" 'svn-move)
    (define-key map "U" 'svn-unlock)

    ;; 属性関連のコマンド
    (define-key map "pD" 'svn-propdel-revprop)
    (define-key map "pE" 'svn-propedit-revprop)
    (define-key map "pG" 'svn-propget-revprop)
    (define-key map "pL" 'svn-proplist-revprop)
    (define-key map "pS" 'svn-propset-revprop)
    (define-key map "pd" 'svn-propdel)
    (define-key map "pe" 'svn-propedit)
    (define-key map "pg" 'svn-propget)
    (define-key map "pl" 'svn-proplist)
    (define-key map "ps" 'svn-propset)

    (define-key map "a" 'svn-add)
    (define-key map "b" 'svn-blame)
    (define-key map "c" 'svn-commit)
    (define-key map "d" 'svn-diff)
    (define-key map "e" 'svn-export)
    (define-key map "f" 'svn-mergeinfo) ;
    (define-key map "g" 'svn-cleanup)
    (define-key map "h" 'svn-help)
    (define-key map "i" 'svn-info)
    (define-key map "j" 'svn-list)      ;
    ;; (define-key map "k" 'svn-)
    (define-key map "l" 'svn-log)
    (define-key map "m" 'svn-merge)
    (define-key map "n" 'svn-changelist)
    (define-key map "o" 'svn-checkout)
    ;;(define-key map "p" 'svn)
    ;;(define-key map "q" 'svn)
    (define-key map "r" 'svn-resolve)
    (define-key map "s" 'svn-status)
    (define-key map "t" 'svn-cat)       ;
    (define-key map "u" 'svn-update)
    (define-key map "v" 'svn-version)
    (define-key map "w" 'svn-switch)
    ;;(define-key map "x" 'svn-)
    ;;(define-key map "y" 'svn-)
    (define-key map "z" 'svn-revert)

    (define-key map "!" 'svn-command)

    (define-key map "=" 'svn-diff-with-head)
    (define-key map "-" 'svn-diff-with-previous-version)
    (define-key map "|" 'svn-ediff)
    (define-key map "%" 'svn-resolve-conflicts)

    (define-key map [return] 'svn-find-file)

    ;;; process 関連
    (define-key map "/" 'svn-kill-process)
    (define-key map ">" 'svn-show-output)
    (define-key map "<" 'svn-kill-output-buffer)
    map))

(defvar svn-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cs" svn-prefix-map)
    map))

(defvar svn-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'svn-minibuffer-complete)

    (set-keymap-parent map minibuffer-local-map)
    map))

;;; property 関連

(defun svn-file-prop-sym-name (file)
  (directory-file-name (expand-file-name (or file "."))))

(defun svn-set-prop (file property value)
  "Set per-file SVN PROPERTY for FILE to VALUE."
  (setq file (svn-file-prop-sym-name file))
  (if svn-debug
      (message "SET [%s] [%s] [%s]" file property value))
  (put (intern (svn-file-prop-sym-name file) svn-file-prop-obarray)
       property value))

(defun svn-get-prop (file property)
  "Set per-file SVN PROPERTY for FILE to VALUE."
  (get (intern-soft (svn-file-prop-sym-name file) svn-file-prop-obarray)
       property))

(defun svn-delete-prop (file)
  (if svn-debug
      (message "DELETE [%s]" (svn-file-prop-sym-name file)))
  (unintern (svn-file-prop-sym-name file) svn-file-prop-obarray))

(defun svn-delete-prop-under (file)
  (svn-delete-prop file)

  ;; ディレクトリが削除された場合、その下のファイルも削除
  (let* ((name (concat (svn-file-prop-sym-name file) "/"))
         (len (length name)))
    (mapcar (lambda  (arg)
              (setq arg (symbol-name arg))
              (if (and (< len (length arg))
                       (string= name (substring arg 0 len)))
                  (unintern name svn-file-prop-obarray))
              (message "%s" arg))
            svn-file-prop-obarray)))

(defun svn-process-sym-name (process)
  (format "svn-process-%d" (process-id process)))

(defun svn-create-process-sym (process)
  (intern (svn-process-sym-name process) svn-process-prop-obarray))

(defun svn-get-process-sym (process)
  (intern-soft (svn-process-sym-name process) svn-process-prop-obarray))

(defun svn-delete-process-sym (process)
  (unintern (svn-process-sym-name process) svn-process-prop-obarray))

;;; input 関連

(defun svn-read-string (prompt &optional rest)
  (let ((str (apply 'read-string prompt rest)))
    (if (string= "" str) nil str)))

(defun svn-read-string-1 (prompt &rest rest)
  (let (str)
    (while (not (setq str (apply 'svn-read-string prompt rest))))
    str))

(defun svn-completing-read (prompt list &rest args)
  (let ((str (apply 'completing-read prompt list args)))
    (if (string= "" str) nil str)))

(defun svn-read-files (prompt &optional initial-input history
                              default-value inherit-input-method)
  (let ((svn-completion-function 'svn-try-complete-file))
    (split-string
     (read-from-minibuffer prompt initial-input svn-minibuffer-map
                           nil history default-value initial-input))))

(defun svn-minibuffer-get-current-input ()
  (let ((top (next-single-property-change (point-min) 'read-only)))
    (if top
        (buffer-substring top (point))
      "")))

(defun svn-get-last-arg (line)
  (save-match-data
    (let ((ifs "[ \t\n\r]+"))
      (if (string-match (concat "^" ifs) line)
          (setq line (substring line (match-end 0))))
      (while (string-match ifs line)
        (setq line (substring line (match-end 0))))
      line)))

(defun svn-try-complete-file (path)
  (setq path (svn-get-last-arg path))
  (let (dir file collection)
    (save-match-data
      (if (string-match ".*/" path)
          (setq dir (substring path 0 (match-end 0))
                file (substring path (match-end 0)))
        (setq dir default-directory
              file path)))
    (if (setq collection
              (condition-case nil
                  (mapcar (lambda (f)
                            (list
                             (if (file-directory-p (concat dir f))
                                 (file-name-as-directory f)
                               f)))
                          (delete "." (delete ".." (directory-files dir))))
                (file-error nil)))
        (let ((add (try-completion file collection)))
          (if (and (stringp add)
                   (progn
                     (setq add (substring add (length file)))
                     (zerop (length add))))
              (all-completions file collection)
            add)))))

(defun svn-minibuffer-complete ()
  (interactive)
  (let ((comp (funcall (or svn-completion-function 'ignore)
                       (svn-minibuffer-get-current-input)))
        (buffer "*Completions*"))
    (cond
     ((eq comp t)
      ;; 完全に一致
      (if (buffer-live-p (get-buffer buffer))
          (kill-buffer buffer)))
     ((eq comp nil)
      ;; 一致する候補なし
      (ding)
      (message "No Completions."))
     ((stringp comp)
      (insert comp))
     ((listp comp)
      (with-output-to-temp-buffer buffer
        (setq tab-width (/ (window-width) 2))
        (princ "Possible completions are:\n")
        (let (odd)
          (while comp
            (princ (format "%s" (car comp)))
            (princ (if odd "\n" "\t"))
            (setq odd (not odd)
                  comp (cdr comp)))))))))

(defun svn-file-name-directory (file)
  (directory-file-name
   (if (file-directory-p file)
       file
     (or (file-name-directory file) "."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 引数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun svn-read-URL()
  (svn-read-string-1 "URL: "))

(defun svn-make-version-option (arg1 &optional arg2)
  (if arg1 (list (concat "-r" arg1 (if arg2 (concat ":" arg2))))))

(defun svn-read-option (&optional default)
  (prog1
      (split-string
       (read-string "Option: "
                    (mapconcat 'identity default " ")
                    '(svn-option-list . 0)))
    (cond
     ((zerop svn-option-list-max)
      (setq svn-option-list nil))
     ((< 0 svn-option-list-max)
      (let ((lg (nthcdr (1- svn-option-list-max) svn-option-list)))
        (and (cdr lg) (setcdr lg nil)))))))

(defun select-from-buffer (prompt key callback regexp index face)
  (let ((ver (make-overlay 0 0)))
    (unwind-protect
        (save-excursion
          (overlay-put ver 'face (cons 'background-color face))
          (let ((hilight-ver
                 (lambda ()
                   (save-excursion
                     (save-match-data
                       (beginning-of-line 1)
                       (if (looking-at regexp)
                           (move-overlay ver (match-beginning 0) (match-end 0))
                         (move-overlay ver 0 0))
                       (match-string-no-properties index)))))
                (f0 (vconcat key))
                last-match f current-prefix-arg)

            (setq last-match (funcall hilight-ver))
            (while (not (equal f0 (setq f (read-key-sequence-vector prompt))))
              (save-restriction
                (save-selected-window
                  (call-interactively (key-binding f))
                  (setq last-match (funcall hilight-ver)))))
            (if (functionp callback)
                (funcall callback)
              last-match)))
      (delete-overlay ver))))

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

(defun svn-get-WCPATH-s ()
  (if (functionp svn-get-WCPATH-s-function)
      (funcall svn-get-WCPATH-s-function)))

(defun svn-get-WCPATH-REV-s ()
  (if current-prefix-arg
      (mapcar
       (lambda (target)
         (let ((ver (svn-read-version target)))
           (if ver (concat target "@" ver) target)))
       (svn-get-WCPATH-s))
    (or (if (functionp svn-get-WCPATH-REV-s-function)
            (funcall svn-get-WCPATH-REV-s-function))
        (svn-get-WCPATH-s))))

(defun svn-get-URL-s ()
  (if (functionp svn-get-URL-s-function)
      (funcall svn-get-URL-s-function)))

(defun svn-get-URL-REV-s ()
  (if current-prefix-arg
      (mapcar
       (lambda (target)
         (let ((ver (svn-read-version target)))
           (if ver (concat target "@" ver) target)))
       (svn-get-URL-s))
    (or (if (functionp svn-get-URL-REV-s-function)
            (funcall svn-get-URL-REV-s-function))
        (svn-get-URL-s))))

(defun svn-get-VERSION ()
  (if (functionp svn-get-VERSION-function)
      (funcall svn-get-VERSION-function)))

(defun svn-get-TARGET-s ()
  (or (svn-get-WCPATH-s)
      (svn-get-URL-s)))

(defun svn-get-TARGET-REV-s ()

  (if current-prefix-arg
      (mapcar
       (lambda (target)
         (let ((ver (svn-read-version target)))
           (if ver (concat target "@" ver) target)))
       (or (svn-get-WCPATH-s)
           (svn-get-URL-s)))

    (or (svn-get-WCPATH-REV-s)
        (svn-get-URL-REV-s)
        (svn-get-WCPATH-s)
        (svn-get-URL-s))))

(defun svn-get-NEWURL ()
  (svn-read-URL))

(defun svn-get-NEWPATH ()
  (let ((path (read-directory-name "Path: ")))
    (if (string= path "") nil path)))

(defun svn-get-NEWTARGET (&optional prompt initial-input)
  (or prompt
      (setq prompt "Target: "))
  (let ((target (read-string prompt initial-input)))
    (if (string= target "") nil target)))

(defun svn-get-NEWTARGET-s (&optional initial-input)
  (let (targets target n)
    (setq n 1)
    (while (setq target (svn-get-NEWTARGET
                         (format "Target[%d]: " n))))
    (setq targets (cons target targets)
          n (1+ n))
    targets))

(defun svn-get-revision ()
  (let ((rev (read-string "Revision: ")))
    (if (string= rev "") nil rev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun svn-get-command-args (command option files)
  " command option files から start-process に渡すリストを作る。"
  (cons svn-client-command (append (list command) option files)))

(defun svn-mode-line-update()
  " モードラインを更新する。"
  (save-excursion
    (mapc
     (lambda (buffer)
       (set-buffer buffer)
       (when svn-minor-mode
         (setq svn-mode-line
               (concat
                (and (processp svn-process)
                     (not (eq 'exit (process-status svn-process)))
                     (concat ":" (process-name svn-process)
                             " " (symbol-name (process-status svn-process))))
                (if svn-update-queue
                    (format ":updating(%d)" (length svn-update-queue)))))))
     (buffer-list)))
  (force-mode-line-update t))

(defun svn-process-live-p (process)
  " process が生きているかチェックする。"
  (and (processp process)
       (memq (process-status process) '(run stop open closed))))

(defun svn-call-process (command option &optional file)
  "* Call svn process"
  (let ((cmd (svn-get-command-args command option (list file))))
    (if svn-debug
        (message "svn-call-process %s" (list command option file)))
    (prog1 (apply 'call-process (car cmd) nil t nil (cdr cmd))
      (goto-char (point-min)))))

(defun svn-start-process-no-buffer (name command &optional option files)
  (if svn-debug
      (message "[%s] %s (dir=%s)"
               name (svn-get-command-args command option files)
               default-directory))
  (apply 'start-process name nil (svn-get-command-args command option files)))

(defun svn-start-process (command &optional option files callback)
  "* Start svn process"
  (if (svn-process-live-p svn-process)
      (error "%s is runnning." (process-name svn-process)))

  (let ((buffer-name (format "*svn-%s*" command)))
    (let (buffer)
      (and (processp svn-process)
           (buffer-live-p (setq buffer (process-buffer svn-process)))
           (delete-windows-on buffer))
      (and (buffer-live-p (setq buffer (get-buffer buffer-name)))
           (not (eq buffer (current-buffer)))
           (kill-buffer buffer)))

    (prog1
        (setq svn-process (svn-start-process-no-buffer
                           (concat "svn " command) command option files))
      (svn-mode-line-update)
      (message "%s start..." (process-name svn-process))

      (set-process-buffer svn-process (generate-new-buffer buffer-name))
      (set-process-sentinel
       svn-process
       `(lambda (proc change)
          (message "%s start...%s"
                   (process-name proc)
                   (replace-regexp-in-string "[\r\n]+" "" change))

          ;; モードラインの更新
          (svn-mode-line-update)

          (unless (svn-process-live-p proc)
            ;; 終了した
            (let ((buffer (process-buffer proc)))

              (if svn-debug
                  (message "svn buffer size: %d" (buffer-size buffer)))

              (if (buffer-live-p buffer)
                  (with-current-buffer buffer
                    ;; ポイントを先頭に
                    (goto-char (point-min))

                    ;; callback が指定されていたら実行
                    ,(if (functionp callback)
                         (list callback command
                               (list 'quote option) (list 'quote files)))))

              (if svn-debug
                  (message "svn buffer-live-p %s" (buffer-live-p buffer)))
              (if (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (cond
                     ((= 0 (buffer-size buffer))
                      (kill-buffer buffer))
                     ;; すぐに消えてしまっって(?)確認できないケースがある。
                     ;;((>= 2 (count-lines (point-min) (point-max)))
                     ;; (let ((msg (buffer-substring (point-min) (point-max))))
                     ;;   (kill-buffer buffer)
                     ;;   ;; 文字列の最後が改行コードだったら削除する
                     ;;   (if (eq 10 (aref msg (1- (length msg))))
                     ;;       (setq str (substring msg 0 (1- (length msg)))))
                     ;;   (message str)))
                     (t 
                      (let ((w (display-buffer buffer t)))
                        (setq buffer-read-only t)
                        (set-buffer-modified-p nil)
                        (set-window-point w (point))
                        (set-window-start w (point))
                        (font-lock-mode t)
                        (save-restriction
                          (narrow-to-region (point) (point-max))
                          (shrink-window-if-larger-than-buffer w))))))))))))))

(defun svn-start-command (command targets &optional option callback)
  (if svn-debug
      (message "svn-start-command(%s %s %s)" command targets option))
  (or callback
      (setq callback
            (let ((list (assoc command svn-sentinel-list)))
              (and list (cdr list)))))
  (svn-start-process command option targets callback))

(defun svn-get-versions (target)
  (if svn-debug
      (message "svn-get-versions(%s)" target))

  (let (process)
    (setq process
          (svn-start-process-no-buffer
           "svn get versions" "log" '("-q" "--stop-on-copy") (list target)))
    (svn-create-process-sym process)
    (set-process-filter
     process
     (svn-process-create-line-filter
      (lambda (sym line state)
        (save-match-data
          (if (string-match (concat "^r" svn-version-regexp) line)
              (put sym 'revisions
                   (cons (match-string-no-properties 1 line)
                         (get sym 'revisions))))))))
    (while (accept-process-output process))
    (prog1
        (get (svn-get-process-sym process) 'revisions)
      (svn-delete-process-sym process))))

(defun svn-start-update ()
  (interactive)
  ;; 処理中だったら、何もしない
  ;;(message "SAU process %s" svn-update-process)
  (unless (processp svn-update-process)
    ;; 処理中じゃなかった。

    (svn-mode-line-update)

    ;; キューから1つ取り出す
    (let ((dir (caar svn-update-queue)))
      (setq svn-update-queue (cdr svn-update-queue))

      (if svn-debug
          (message "svn-start-update (%s)" dir))

      (when dir
        ;; キューから取り出されば、処理開始

        (if (file-exists-p dir)

            (let ((default-directory (file-name-as-directory dir)))
              (setq svn-update-process
                    (svn-start-process-no-buffer
                     "svn auto update" "status" '("-Nv") nil))
              (put (svn-create-process-sym svn-update-process)
                   'dir default-directory)

              (set-process-filter
               svn-update-process
               (svn-process-create-line-filter 'svn-update-status-line-filter))
              (set-process-sentinel
               svn-update-process
               '(lambda (process change)
                  (unwind-protect
                      (run-hook-with-args
                       'svn-change-status-hook
                       (get (svn-get-process-sym process) 'param))
                    (svn-delete-process-sym process)
                    (setq svn-update-process nil)
                    (run-at-time 0 nil 'svn-start-update)))))

          (message "Directory not exist %s" dir)
          (run-at-time 0 nil 'svn-start-update))))))

(defun svn-update-status (files)
  (if svn-debug
      (message "svn-update-status %s" files))
  (mapc
   (lambda (file)
     (if (file-exists-p file)
         (progn
           (setq file (expand-file-name file))
           (or (file-directory-p file)
               (setq file (directory-file-name (file-name-directory file))))

           (or (assoc file svn-update-queue)
               (setq svn-update-queue (cons (list file) svn-update-queue))))

       (svn-delete-prop-under file)))
   files)

  ;; 処理開始
  (run-at-time 0 nil 'svn-start-update))

(defun svn-find-file-noselect (file version)
  (if (stringp version)

      ;; version 指定がある場合
      (let ((ver-file (concat file ".~" version "~")))
        (save-match-data
          (if (string-match (concat "^" svn-version-regexp "$") version)

              ;; version が数字の場合
              (if (file-readable-p ver-file)
                  (find-file-noselect ver-file)

                ;; version 指定で取得
                (with-current-buffer
                    (generate-new-buffer (file-name-nondirectory ver-file))
                  (if (zerop (svn-call-process
                              "cat" (svn-make-version-option version) file))
                      (prog1 (current-buffer)
                        (write-file ver-file)
                        (set-file-modes (buffer-file-name)
                                        (logand (file-modes (buffer-file-name))
                                                (lognot 146))))
                    (kill-buffer nil)
                    nil))))

          ;; version が数字でない場合
          (with-current-buffer
              (generate-new-buffer (file-name-nondirectory ver-file))
            (if (zerop (svn-call-process
                        "cat" (svn-make-version-option version) file))
                (prog1 (current-buffer)
                  (setq buffer-read-only t))
              (kill-buffer nil)
              nil))))

    ;; version 指定なしの場合
    ;; local に無いかチェックする
    (or (file-readable-p file)
        (svn-call-process "update" '("-N", "--ignore-externals") file))

    (if (file-readable-p file)
        (find-file-noselect file))))

(defun svn-get-svn-files (dir)
  (with-temp-buffer
    (save-match-data
      (condition-case nil
          (let ((default-directory
                  (file-name-as-directory (expand-file-name dir))))
            (if (zerop (svn-call-process "status" '("-Nv") "."))
                (let (dirs files file)
                  (while (not (eobp))
                    (when (looking-at svn-status-regexp)
                      (setq file (match-string-no-properties 5))

                      (or (string-equal "." file)
                          (if (file-directory-p file)
                              (setq dirs (cons file dirs))
                            (setq files (cons file files)))))

                    (forward-line 1))

                  (append (sort dirs 'string<)
                          (sort files 'string<)))))
        (error nil)))))

(defun svn-get-repository-root (files)
  (let ((process (svn-start-process-no-buffer
                  "svn get repository root" "info" nil (list (car files)))))
    (svn-create-process-sym process)
    (set-process-filter
     process
     (svn-process-create-line-filter
      (lambda (sym line state)
        (or state
            (save-match-data
              (if (or (string-match "^Repository Root: *\\(.*\\)" line)
                      (string-match "^リポジトリのルート: *\\(.*\\)" line))
                  (put sym 'result (match-string-no-properties 1 line))))))))
    (while (accept-process-output process))
    (prog1
        (get (svn-get-process-sym process) 'result)
      (svn-delete-process-sym process))))

(defun svn-get-mime-type (file &optional rev)
  (with-temp-buffer
    (and (zerop (svn-call-process
                 "propget"
                 (if rev
                     (list "--strict" "svn:mime-type" "-r" rev)
                   (list "--strict" "svn:mime-type"))
                 file))
         (< 0 (buffer-size))
         (buffer-substring (point-min) (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process sentinels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun svn-file-separator-change (&rest ignore)
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (while (search-forward "\\" nil t)
        (replace-match "/" nil t)))))

(defun svn-process-sentinel-default (command option files)
  (if files
      (svn-update-status files)))

(defun svn-commit-sentinel (command option files)
  (svn-file-separator-change)
  (save-excursion
    (save-match-data
      (let (file files)
        (while (not (eobp))
          (cond
           ((looking-at "Sending +\\(.*\\)")
            (setq files (cons (match-string-no-properties 1) files)))
           ((looking-at "Adding +\\(.*\\)")
            (setq files (cons (match-string-no-properties 1) files)))
           ;; 削除する場合のメッセージは不明
           ((looking-at "Removing in \\([^;]+\\);")
            (setq file (match-string-no-properties 1))
            (svn-delete-prop file)
            (setq files (cons file files))))
          (forward-line 1))
        (svn-update-status files)))))

(defun svn-update-sentinel (command option files)
  (svn-file-separator-change)

  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "[A-Z] +")
            (setq files (cons (buffer-substring-no-properties
                               (match-end 0)
                               (line-end-position))
                              files)))
        (forward-line 1))))

  (if files
      (svn-update-status files))
  (svn-update-mode))

(defun svn-status-sentinel (command option files)
  (svn-file-separator-change)
  (if files
      (svn-update-status files))
  (svn-update-mode))

(defun svn-diff-sentinel (&rest ignore)
  (diff-mode))

(defun svn-blame-sentinel (command option files)
  (svn-blame-mode files))

(defun svn-log-sentinel (command option targets)
  (svn-log-mode (svn-get-repository-root targets) (car targets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process filters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-process-create-line-filter (func)
  `(lambda (proc string)
     (let ((sym (svn-get-process-sym proc))
           line)
       (setq string (concat (get sym 'rest) string))

       (save-match-data
         (while (string-match "\r?\n" string)
           (setq line (substring string 0 (match-beginning 0))
                 string (substring string (match-end 0)))
           (if svn-debug
               (message "Filter (%s %s) %s" sym (get sym 'param) line))
           (put sym 'param (,func sym line (get sym 'param)))))

       (put sym 'rest string))))

;; 管理外ファイルの対応必要
(defun svn-update-status-line-filter (sym line param)
  ;;(message "SAU %s" line)
  (save-match-data
    (cond
     ((string-match "^svn: warning:" line)
      nil)
     ((string-match "^Status against revision:" line)
      nil)
     ((string-match "^--- " line)       ; change list
      nil)
     ((string-match "^ *$" line)
      nil)
     ((string-match "^        >" line)
      nil)
     ((string-match "^    X   " line)   ; svn:externals
      nil)

     ((string-match "^\\? +\\(.*\\)" line)
      (svn-delete-prop (match-string-no-properties 1 line)))
     ((string-match "^       \\* +.*" line)
      ;; added by another
      )

     ((string-match svn-status-u-regexp line)
      (let ((default-directory (get sym 'dir))
            (file (match-string-no-properties 5 line)))
        (setq file
              (if (string= file ".")
                  default-directory
                (concat default-directory file)))
        (svn-set-prop file 'status (match-string-no-properties 1 line))
        (svn-set-prop file 'working-version
                      (string-to-number
                       (or (match-string-no-properties 2 line) "0")))
        (svn-set-prop file 'commit-version
                      (string-to-number
                       (or (match-string-no-properties 3 line) "0")))
        (svn-set-prop file 'commit-user (match-string-no-properties 4 line))
        (setq param (cons file param))))
     (t
      (message "Unknown line \"%s\"" line)
      ;;(error "Unknown line \"%s\"" line)
      )))

  param)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; intractive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun svn-stop-update ()
  (interactive)
  (if (processp svn-update-process)
      (delete-process svn-update-process))
  (setq svn-update-process nil))

(defun svn-kill-process ()
  "* Kill svn process."
  (interactive)
  (or (processp svn-process)
      (error "No process to kill"))
  (let ((status (process-status svn-process))
        (buffer (process-buffer svn-process))
        (name   (process-name   svn-process)))
    (when (svn-process-live-p svn-process)
      (message "Killing %s" name)
      (kill-process svn-process))
    (message "%s status is %s" name status)))

(defun svn-show-output (&optional show)
  "* 実行中 process の出力バッファを表示する。"
  (interactive "P")
  (or (processp svn-process)
      (error "No process"))
  (let ((buffer (process-buffer svn-process)))
    (or (buffer-live-p buffer)
        (error "No process buffer for %s" svn-process))
    (if (if show
            (not (< 0 (prefix-numeric-value show)))
          (get-buffer-window buffer))
        (delete-windows-on buffer)
      (shrink-window-if-larger-than-buffer (display-buffer buffer t)))))

(defun svn-kill-output-buffer()
  ""
  (interactive)
  (save-match-data
    (mapc (lambda (buffer)
            (and (string-match "^\\*svn-+[a-z]+\\*$" (buffer-name buffer))
                 (buffer-live-p buffer)
                 (not (svn-process-live-p (get-buffer-process buffer)))
                 (progn
                   (delete-windows-on buffer)
                   (kill-buffer buffer))))
          (buffer-list))))

(defun svn-find-file (TARGET-s version)
  "* FILE の VERSION を表示する。"
  (interactive
   (let ((targets (svn-get-TARGET-s)))
     (list targets
           (if current-prefix-arg
               (svn-read-version targets)
             (or (svn-get-VERSION)
                 (svn-read-version targets))))))
  (mapcar (lambda (file)
            (let ((buffer (svn-find-file-noselect file version)))
              (or buffer
                  (error "File not found %s (%s)" file version))
              (display-buffer buffer)))
          TARGET-s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-command (command TARGET-s &optional option)
  (interactive
   (let (command files option)
     (setq command (svn-completing-read
                    "Command: " svn-command-config nil t)
           files (svn-read-files "Files: " (svn-get-TARGET-s))
           option (svn-read-option))
     (list command files option)))
  (svn-start-command command TARGET-s option))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn add
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-add (NEWPATH-s)
  "* svn add"
  ;; add はローカルファイルを指定するので、対象ファイルを
  ;; svn-get-WCPATH-s で取得する。
  (interactive (list (svn-get-WCPATH-s)))
  (or NEWPATH-s
      (error "NEWPATHs not specified"))
  (svn-start-command "add" NEWPATH-s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn blame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-blame (TARGET-REV-s &optional verbose)
  (interactive (list (svn-get-TARGET-REV-s)
                     (if current-prefix-arg (y-or-n-p "Verbose?"))))
  (or TARGET-REV-s
      (error "TARGET not specified"))
  (svn-start-command "blame" TARGET-REV-s (if verbose '("-v"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn cat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-cat (TARGET-REV-s)
  (interactive (list (svn-get-TARGET-REV-s)))
  (or TARGET-REV-s
      (error "TARGET not specified"))
  (mapc (lambda (TARGET-REV)
          (let ((mime-type (svn-get-mime-type TARGET-REV)))
            (or (eq mime-type nil)
                (let ((case-fold-search t))
                  (string-match "^text/" mime-type))
                (error "Not a text file: %s" mime-type))))
        TARGET-REV-s)
  (svn-start-command "cat" TARGET-REV-s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn changelist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-changelist (LISTNAME TARGET-s)
  (interactive (list (svn-read-string "changelist: ")
                     (svn-get-TARGET-s)))
  (or TARGET-s
      (error "TARGET not specified"))
  (or LISTNAME
      (setq LISTNAME "--remove"))
  (svn-start-command "changelist" TARGET-s (list LISTNAME)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn checkout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-checkout (URL-REV-s NEWPATH &optional ignore-externals)
  (interactive (list (svn-get-URL-REV-s)
                     (svn-get-NEWPATH)
                     (if current-prefix-arg (y-or-n-p "Ignore externals?"))))
  (or URL-REV-s
      (error "URL not specified"))
  (svn-start-command "checkout"
                     (append URL-REV-s (list NEWPATH))
                     (if ignore-externals '("--ignore-externals"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn cleanup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-cleanup (WCPATH-s)
  (interactive (list (svn-get-WCPATH-s)))
  (svn-start-command "cleanup" WCPATH-s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn commit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-commit (WCPATH-s &optional mes)
  "* svn commit"
  (interactive (list (svn-get-TARGET-REV-s)))

  (cond
   ((stringp mes)
    (svn-start-command "commit" WCPATH-s (list "-m" mes)))

   ((eq svn-commit-type 'log-edit)
    (require 'log-edit)
    (let ((wc (current-window-configuration))
          (parent (current-buffer)))
      (log-edit `(lambda()
                   (interactive)
                   (let ((mes (buffer-string)))
                     (kill-buffer nil)
                     (set-window-configuration ',wc)
                     (when (buffer-live-p ',parent)
                       (set-buffer ',parent)
                       (svn-commit ',WCPATH-s mes))))
                nil
                `(lambda () ',WCPATH-s)
                (generate-new-buffer "*svn-commit-message*"))
      (turn-on-auto-fill)
      (momentary-string-display
       (concat "Log Message for:\n\t"
               (mapconcat 'identity WCPATH-s "\n\t") "\n")
       (point-min))))

   ((eq svn-commit-type 'read-string)
    (svn-commit WCPATH-s (read-string "Message: ")))

   ((stringp svn-commit-type)
    (svn-start-command "commit" WCPATH-s (list "-F" svn-commit-type)))

   (t
    (svn-start-command "commit" WCPATH-s (list "--force-interactive")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn copy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-copy (TARGET-REV-s NEWTARGET)
  (interactive
   (let ((target (svn-get-TARGET-REV-s)))
     (or target
         (error "SOURCE not specified"))
     (if (< 1 (length target))
         (error "Plural number of SOURCES not supported"))
     (list target
           (svn-get-NEWTARGET
            (format "Copy %s to: " (car target))))))
  (or TARGET-REV-s
      (error "SOURCE not specified"))
  (if (< 1 (length TARGET-REV-s))
      (error "Plural number of SOURCES not supported"))
  (or NEWTARGET
      (error "DST not specified"))
  (svn-start-command "copy" (append TARGET-REV-s (list NEWTARGET))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn delete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-delete (TARGET-s)
  (interactive (list (svn-get-TARGET-s)))
  (or TARGET-s
      (error "TARGET not specified"))
  (svn-start-command "delete" TARGET-s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn diff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-diff (TARGET-s &optional N M diff-switches)
  "* svn diff"
  (interactive
   (if current-prefix-arg
       (let* ((targets (svn-get-TARGET-s))
              (vers (svn-read-version2 targets)))
         (list targets (car vers) (cdr vers)
               (split-string
                (read-string "Diff Switch: "
                             (mapconcat (lambda (x) x)
                                        svn-diff-switches " ")))))
     (list (svn-get-TARGET-s))))
  (svn-start-command "diff" TARGET-s
                     (append (or diff-switches svn-diff-switches)
                             (svn-make-version-option N M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-export (URL-REV-s NEWPATH &optional ignore-externals)
  (interactive (list (svn-get-URL-REV-s)
                     (svn-get-NEWPATH)
                     (if current-prefix-arg (y-or-n-p "Ignore externals?"))))
  (or URL-REV-s
      (error "URL not specified"))
  (svn-start-command "export"
                     (append URL-REV-s (list NEWPATH))
                     (if ignore-externals '("--ignore-externals"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn help
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-help (command)
  "* help"
  (interactive (list (svn-completing-read "Command for help: "
                                          svn-command-config nil t)))
  (svn-start-command "help" (list command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn import
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-import (WCPATH-s NEWURL)
  (interactive (list (svn-get-WCPATH-s)
                     (svn-get-NEWURL)))
  (or WCPATH-s
      (error "PATH not specified"))
  (if (< 1 (length WCPATH-s))
      (error "Plural number of PATHs not supported"))
  (or NEWURL
      (error "URL not specified"))
  (svn-start-command "import" (append WCPATH-s (list NEWURL))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-info (TARGET-REV-s &optional recursive)
  (interactive (list (svn-get-TARGET-REV-s)
                     (if current-prefix-arg
                         (y-or-n-p "Recursive?"))))
  (svn-start-command "info" TARGET-REV-s (if recursive '("-R"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-list (TARGET-REV-s &optional verbose recursive)
  (interactive (list (svn-get-TARGET-REV-s)
                     (if current-prefix-arg (y-or-n-p "Verbose?"))
                     (if current-prefix-arg (y-or-n-p "Recursive?"))))
  (svn-start-command "list" TARGET-REV-s
                     (let (opotions)
                       (if verbose (setq opotions (cons "-v" opotions)))
                       (if recursive (setq opotions (cons "-R" opotions)))
                       opotions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-lock (TARGET-s)
  (interactive (list (svn-get-TARGET-s)))
  (or TARGET-s
      (error "TARGET not specified"))
  (svn-start-command "lock" TARGET-s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-log (TARGET-s &optional revision)
  "* svn log"
  (interactive
   (let ((targets (svn-get-TARGET-s))
         version)
     (if (< 1 (length targets))
         (error "TARGET must be a single target"))
     (setq version
           (if current-prefix-arg
               (svn-read-version targets)
             (svn-get-VERSION)))
     (list targets version)))
  (if (< 1 (length TARGET-s))
      (error "TARGET must be a single target"))
  (svn-start-command "log" TARGET-s
                     (append '("-v" "--stop-on-copy")
                             (if revision (list "-r" revision)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn merge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-merge (TARGET-REV-1 TARGET-REV-2 WCPATH-s)
  (interactive
   (let ((TARGET-REV-s (svn-get-TARGET-REV-s)))
     (list (car TARGET-REV-s)
           (cdr TARGET-REV-s)
           (svn-get-WCPATH-s))))
  (or TARGET-REV-1
      (error "TARGET1 not specified"))
  (or TARGET-REV-2
      (error "TARGET2 not specified"))
  (or WCPATH-s
      (error "WCPATH not specified"))
  (svn-start-command "merge" (list TARGET-REV-1 TARGET-REV-2 WCPATH-s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn mergeinfo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-mergeinfo (TARGET-REV-1 TARGET-REV-2)
  (interactive
   (let ((TARGET-REV-s (svn-get-TARGET-REV-s)))
     (list (car TARGET-REV-s)
           (cdr TARGET-REV-s))))
  (or TARGET-REV-1
      (error "TARGET1 not specified"))
  (or TARGET-REV-2
      (error "TARGET2 not specified"))
  (svn-start-command "mergeinfo" (list TARGET-REV-1 TARGET-REV-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn mkdir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-mkdir (NEWTARGET-s)
  (interactive (list (svn-get-NEWTARGET-s)))
  (or NEWTARGET-s
      (error "TARGET not specified"))
  (svn-start-command "mkdir" NEWTARGET-s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn move
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-move (TARGET-s NEWTARGET)
  (interactive (list (svn-get-TARGET-s)
                     (svn-get-NEWTARGET)))
  (or TARGET-s
      (error "SRC not specified"))
  (or NEWTARGET
      (error "DST not specified"))
  (svn-start-command "move" (append TARGET-s (list NEWTARGET))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn propdel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-propdel (propname WCPATH-s)
  (interactive (list (read-string "Property Name: ")
                     (svn-get-WCPATH-s)))
  (or WCPATH-s
      (error "WCPATH not specified"))
  (svn-start-command "propdel" WCPATH-s (list propname)))

(defun svn-propdel-revprop (revision propname TARGET-s)
  (interactive (list (or (svn-get-VERSION)
                         (svn-get-revision))
                     (read-string "Property Name: ")
                     (svn-get-TARGET-s)))
  (or revision
      (error "Revision not specified"))
  (or TARGET-s
      (error "TARGET not specified"))
  (svn-start-command "propdel" TARGET-s
                     (list propname "--revprop" "-r" revision)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn propedit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun svn-propedit (propname TARGET-s)
  (interactive (list (read-string "Property Name: ")
                     (svn-get-TARGET-s)))
  (or TARGET-s
      (error "TARGET not specified"))
  (svn-start-command "propedit" TARGET-s (list propname)))

(defun svn-propedit-revprop (revision propname TARGET-s)
  (interactive (list (or (svn-get-VERSION)
                         (svn-get-revision))
                     (read-string "Property Name: ")
                     (svn-get-TARGET-s)))
  (or revision
      (error "Revision not specified"))
  (or TARGET-s
      (error "TARGET not specified"))
  (svn-start-command "propedit" TARGET-s
                     (list propname "--revprop" "-r" revision)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn propget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-propget (propname TARGET-REV-s)
  (interactive (list (read-string "Property Name: ")
                     (svn-get-TARGET-REV-s)))
  (or TARGET-REV-s
      (error "TARGET not specified"))
  (svn-start-command "propget" TARGET-REV-s (list propname "-v")))

(defun svn-propget-revprop (revision propname TARGET-s)
  (interactive (list (or (svn-get-VERSION)
                         (svn-get-revision))
                     (read-string "Property Name: ")
                     (svn-get-TARGET-s)))
  (or revision
      (error "Revision not specified"))
  (or TARGET-s
      (error "TARGET not specified"))
  (svn-start-command "propget" TARGET-s
                     (list propname "-v" "--revprop" "-r" revision)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn proplist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-proplist (TARGET-REV-s &optional recursive)
  (interactive (list (svn-get-TARGET-REV-s)
                     (if current-prefix-arg
                         (y-or-n-p "Recursive?"))))
  (or TARGET-REV-s
      (error "TARGET not specified"))
  (svn-start-command "proplist" TARGET-REV-s
                     (append '("-v")
                             (if recursive '("-R")))))

(defun svn-proplist-revprop (revision TARGET-s)
  (interactive (list (or (svn-get-VERSION)
                         (svn-get-revision))
                     (svn-get-TARGET-s)))
  (or revision
      (error "Revision not specified"))
  (or TARGET-s
      (error "TARGET not specified"))
  (svn-start-command "proplist" TARGET-s
                     (list "-v" "--revprop" "-r" revision)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn propset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-propset (propname propvalue WCPATH-s)
  (interactive (list (read-string "Property Name: ")
                     (read-string "Property Value: ")
                     (svn-get-WCPATH-s)))
  (or WCPATH-s
      (error "WCPATH not specified"))
  (svn-start-command "propset" WCPATH-s (list propname propvalue)))

(defun svn-propset-revprop (revision propname propvalue TARGET-s)
  (interactive (list (or (svn-get-VERSION)
                         (svn-get-revision))
                     (read-string "Property Name: ")
                     (read-string "Property Value: ")
                     (svn-get-TARGET-s)))
  (or revision
      (error "Revision not specified"))
  (or TARGET-s
      (error "TARGET not specified"))
  (svn-start-command "propset" TARGET-s
                     (list propname propvalue "--revprop" "-r" revision)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn resolve
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-resolve (accept WCPATH-s &optional recursive)
  (interactive
   (list (completing-read "Accept: "
                          '("base" "working"
                            "mine-ocnflict" "theirs-conflict"
                            "mine-full" "theirs-full"))
         (svn-get-WCPATH-s)
         (if current-prefix-arg (y-or-n-p "Recursive?"))))
  (or WCPATH-s
      (error "WCPATH not specified"))
  (svn-start-command "resolve" WCPATH-s
                     (append (list "--accept" accept)
                             (if recursive '("-R")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn resolved
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-resolved (WCPATH-s &optional recursive)
  (interactive (list (svn-get-WCPATH-s)
                     (if current-prefix-arg (y-or-n-p "Recursive?"))))
  (warn "svn resolved command is Not recommended.")
  (svn-start-command "resolved" WCPATH-s (if recursive '("-R"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn revert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-revert (WCPATH-s &optional recursive)
  (interactive (list (svn-get-WCPATH-s)
                     (if current-prefix-arg (y-or-n-p "Recursive?"))))
  (or WCPATH-s
      (error "WCPATH not specified"))
  (svn-start-command "revert" WCPATH-s (if recursive '("-R"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-status (WCPATH-s &optional non-recursive verbose show-updates)
  (interactive (list (svn-get-WCPATH-s)
                     (if current-prefix-arg (y-or-n-p "Non Recursive?"))
                     (if current-prefix-arg (y-or-n-p "Verbose?"))
                     (if current-prefix-arg (y-or-n-p "Show Updates?"))))
  (or WCPATH-s
      (error "WCPATH not specified"))
  (svn-start-command "status" WCPATH-s
                     (let ((options '("--ignore-externals")))
                       (if non-recursive (setq options (cons "-N" options)))
                       (if verbose       (setq options (cons "-v" options)))
                       (if show-updates  (setq options (cons "-u" options)))
                       options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn switch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-switch ()
  (error "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn unlock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-unlock (TARGET-s)
  (interactive (list (svn-get-TARGET-s)))
  (or TARGET-s
      (error "TARGET not specified"))
  (svn-start-command "unlock" TARGET-s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun svn-update (WCPATH-s &optional rev)
  (interactive (list (svn-get-WCPATH-s)
                     (if current-prefix-arg (svn-get-revision))))
  (let ((option '("--ignore-externals")))
    (if rev (setq option (append option (list "-r" rev))))
    (svn-start-command "update" WCPATH-s option)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; svn version
(defun svn-version ()
  "* svn version"
  (interactive)
  (svn-start-command "--version" nil))

;;;;; diff
(defun svn-diff-with-head (TARGET-s version)
  "* svn diff with branch top"
  (interactive (list (svn-get-TARGET-s)
                     (or (svn-get-VERSION)
                         (svn-read-version (svn-get-TARGET-s)))))
  (svn-start-command "diff" TARGET-s
                     (append svn-diff-switches
                             (list "-r"
                                   (if version
                                       (concat version ":HEAD")
                                     "HEAD")))))

(defun svn-diff-with-previous-version (TARGET-s version)
  (interactive
   (let ((targets (svn-get-TARGET-s)))
     (list targets
           (if current-prefix-arg
               (svn-read-version (svn-get-TARGET-s) nil t)
             (or (svn-get-VERSION)
                 (let ((version (svn-get-prop (car targets) 'commit-version)))
                   (if (numberp version)
                       (number-to-string version)))
                 (svn-read-version (svn-get-TARGET-s) nil t))))))
  (svn-start-command "diff" TARGET-s
                     (append svn-diff-switches (list "-c" version))))

(defun svn-ediff (TARGET-s rev1 &optional rev2)
  ""
  (interactive
   (if current-prefix-arg
       (let* ((targets (svn-get-TARGET-s))
              (vers (svn-read-version2 targets)))
         (list targets (car vers) (cdr vers)))
     (list (svn-get-TARGET-s)
           (or (svn-get-VERSION) "BASE"))))

  (let ((file (car TARGET-s)))
    (if (file-regular-p file)
        (let ((buffer1 (svn-find-file-noselect file rev1))
              (buffer2 (svn-find-file-noselect file rev2))
              (config (current-window-configuration)))

          (set-buffer (ediff-buffers buffer1 buffer2))

          (set (make-local-variable 'ediff-quit-hook)
               `(lambda ()
                  (ediff-cleanup-mess)
                  ,(if rev1 (list 'kill-buffer buffer1))
                  ,(if rev2 (list 'kill-buffer buffer2))
                  (set-window-configuration ,config)))))))

(defun svn-resolve-conflicts (WCPATH-s)
  (interactive (list (svn-get-WCPATH-s)))
  (or (= 1 (length WCPATH-s))
      (error "Number of files must be 1 for this function."))
  (if (file-directory-p (car WCPATH-s))
      (error "File must not be a directory."))

  (let (file-mine file-base)

    ;; svn info を実行して、マージするファイルの情報を取得する
    (with-temp-buffer
      (save-match-data
        (or (zerop (svn-call-process "info" nil (car WCPATH-s)))
            (error "svn info fail."))

        (if (search-forward-regexp
             "^Conflict Previous Working File: +" nil t)
            (setq file-mine (buffer-substring-no-properties
                             (match-end 0) (line-end-position))))
        (if (search-forward-regexp
             "^Conflict Current Base File: +" nil t)
            (setq file-base (buffer-substring-no-properties
                             (match-end 0) (line-end-position))))))

    ;; 結果のチェック
    (or (and file-mine file-base)
        (error "%s is not conflicted." (car WCPATH-s)))

    (let ((file (expand-file-name (car WCPATH-s))))
      (let ((config (current-window-configuration))
            (default-directory (file-name-directory file)))

        ;; 結果のチェック
        (or (file-exists-p file-mine)
            (error "%s is not exist." file-mine))
        (or (file-exists-p file-base)
            (error "%s is not exist." file-base))

        (set-buffer (ediff-merge-files file-mine file-base))

        (set (make-local-variable 'ediff-quit-hook)
             `(lambda ()
                (let ((buffer-A ediff-buffer-A)
                      (buffer-B ediff-buffer-B)
                      (buffer-C ediff-buffer-C))

                  (ediff-cleanup-mess)

                  (find-file ,file)
                  (erase-buffer)
                  (insert-buffer buffer-C)

                  (delete-other-windows)
                  (kill-buffer buffer-A)
                  (kill-buffer buffer-B)
                  (kill-buffer buffer-C)

                  (when (y-or-n-p "Save buffer? ")
                    (save-buffer)
                    (set-window-configuration ,config)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for minor-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(or (assq 'svn-minor-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(svn-minor-mode svn-mode-line)
                                 minor-mode-alist)))

(or (assq 'svn-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'svn-minor-mode svn-minor-mode-map)
                minor-mode-map-alist)))

(defvar svn-minor-mode-config nil)

(defun svn-default-get-files ()
  (file-relative-name buffer-file-name))

(defun svn-collect-version (target &optional no-aliases)
  (let (versions)
    (cond
     ((stringp target)
      (setq versions (svn-get-versions target)))
     ((listp target)
      (mapc (lambda (_target)
              (let ((vers (svn-get-versions _target)))
                (while vers
                  (if (not (member (car vers) versions))
                      (setq versions (cons (car vers) versions)))
                  (setq vers (cdr vers)))))
            target)))
    (or no-aliases
        (setq versions (append versions '("HEAD" "BASE" "COMMITED" "PREV"))))
    versions))

(defun svn-read-version (target &optional prompt no-aliases)
  (or prompt
      (setq prompt (format "Version for %s : " target)))
  (svn-completing-read prompt (svn-collect-version target no-aliases)))

(defun svn-read-version2 (target &optional prompt no-aliases)
  (or prompt
      (setq prompt (format "Version for %s : " target)))
  (let ((versions (svn-collect-version target no-aliases))
        r1 r2)
    (if (setq r1 (svn-completing-read prompt versions))
        (setq r2 (svn-completing-read prompt (delete r1 versions))))
    (cons r1 r2)))

(defun svn-default-get-WCPATH-s ()
  (list (file-relative-name buffer-file-name)))

(defvar svn-minor-mode-default-config
  '(("WCPATH-s" . svn-default-get-WCPATH-s)))

(defun svn-set-minor-mode-config (mode funcs)
  (let ((pair (assoc mode svn-minor-mode-config)))
    (if pair (delq pair svn-minor-mode-config)))
  (setq svn-minor-mode-config
        (cons (cons mode funcs) svn-minor-mode-config)))

(defun svn-minor-mode (&optional arg)
  ""
  (interactive "P")
  (setq svn-minor-mode
        (if (null arg)
            (not svn-minor-mode)
          (if (< 0 (prefix-numeric-value arg)) t nil)))

  (if svn-debug
      (message "svn-minor-mode %s in %s" svn-minor-mode (current-buffer)))
  (if svn-minor-mode
      (let ((config (or (assoc major-mode svn-minor-mode-config)
                        svn-minor-mode-default-config)))
        (if svn-debug
            (message "config %s" config))

        (set (make-local-variable 'svn-get-URL-REV-s-function)
             (assoc-default "URL-REV-s" config))
        (set (make-local-variable 'svn-get-WCPATH-REV-s-function)
             (assoc-default "WCPATH-REV-s" config))
        (set (make-local-variable 'svn-get-URL-s-function)
             (assoc-default "URL-s" config))
        (set (make-local-variable 'svn-get-WCPATH-s-function)
             (assoc-default "WCPATH-s" config))
        (set (make-local-variable 'svn-get-VERSION-function)
             (assoc-default "VERSION" config))
        (set (make-local-variable 'svn-mode-line) nil)
        (set (make-local-variable 'svn-process) nil))

    (kill-local-variable 'svn-get-URL-REV-s-function)
    (kill-local-variable 'svn-get-WCPATH-REV-s-function)
    (kill-local-variable 'svn-get-URL-s-function)
    (kill-local-variable 'svn-get-WCPATH-s-function)
    (kill-local-variable 'svn-get-VERSION-function)

    (kill-local-variable 'svn-mode-line)
    (kill-local-variable 'svn-process))
  svn-minor-mode)

;;; default

(defun svn-directory-p (dir)
  (with-temp-buffer
    (svn-call-process "info" nil dir)))

(defun svn-is-under-svn (file)
  " Subversion で管理されているファイル(or ディレクトリ)かをチェックする。
主に、svn-minor-mode を有効にするかどうかを判断するために使われる。"
  (cond
   ((file-directory-p file)
    (svn-directory-p file))
   ((file-regular-p file)
    ;; ファイルの場合管理ファイルの中に自身のファイルがあるかどうか
    (member (file-name-nondirectory file)
            (svn-get-svn-files (file-name-directory file))))
   (t nil)))

(defun svn-find-file-hook ()
  ""
  (and buffer-file-name
       (svn-is-under-svn buffer-file-name)
       (svn-minor-mode 1)
       (svn-update-status (list buffer-file-name))))
(add-hook 'find-file-hooks 'svn-find-file-hook)

(defun svn-after-save ()
  "* Subversion で管理しているファイルをセーブした時に状態を更新する。"
  (and buffer-file-name
       svn-minor-mode
       (svn-update-status (list buffer-file-name))))
(add-hook 'after-save-hook 'svn-after-save)

;;; dired

(require 'dired)

(defun svn-dired-get-files ()
  (or (condition-case nil
          (dired-get-marked-files t)
        (error nil))
      (save-excursion
        (dired-next-subdir 0)
        (list (file-relative-name (directory-file-name (dired-get-subdir)))))))

(svn-set-minor-mode-config
 'dired-mode
 '(("WCPATH-s" . svn-dired-get-files)))

(defun svn-dired-after-readin-hook ()
  (save-excursion
    (goto-char (point-min))
    (let ((subdir (dired-get-subdir)))
      (when subdir
        (and (not svn-minor-mode)
             (svn-is-under-svn subdir)
             (svn-minor-mode 1))
        (if svn-minor-mode
            (svn-update-status (list subdir)))))))
(add-hook 'dired-after-readin-hook 'svn-dired-after-readin-hook)

(provide 'svn)
