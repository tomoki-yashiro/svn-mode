;;; -*- emacs-lisp -*-

(defvar svn-update-font-lock-keywords
  '(("^ *A .*\n" . font-lock-keyword-face)
    ("^ *D .*\n" . font-lock-comment-face)
    ("^ *[U ][U ] .*\n" . font-lock-type-face)
    ("^ *C .*\n" . font-lock-warning-face)
    ("^ *G .*\n" . font-lock-function-name-face)
    ("^ *[M ][M ] .*\n" . 'svn-explorer-modified-face)
    ("^\\? .*\n" . 'svn-explorer-not-under-control-face)))

(defvar svn-update-move-direction 1)

(defvar svn-update-initial-buffer-invisibility-spec '(??)
  "* ")

(defvar svn-update-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "@" 'svn-update-show-all)
    (define-key map "A" 'svn-update-toggle-invisible)
    (define-key map "C" 'svn-update-toggle-invisible)
    (define-key map "D" 'svn-update-toggle-invisible)
    (define-key map "G" 'svn-update-toggle-invisible)
    (define-key map "M" 'svn-update-toggle-invisible)
    (define-key map "U" 'svn-update-toggle-invisible)
    (define-key map "m" 'svn-update-mark)
    (define-key map "n" 'svn-update-next)
    (define-key map "p" 'svn-update-previous)
    (define-key map "u" 'svn-update-unmark)
    (define-key map "?" 'svn-update-toggle-invisible)

    (define-key map [return] 'svn-update-find-file)

    (define-key map [mouse-1] 'svn-update-select-mouse)
    (define-key map [mouse-2] 'svn-update-find-file-mouse)

    map)
  "* ")

(defun svn-update-file (&optional point)
  (save-excursion
    (if point (goto-char point))
    (beginning-of-line 1)
    (save-match-data
      (if (looking-at "[A-Z?!~+* ]+ ")
;;      (if (looking-at ".......")
          (file-relative-name
           (buffer-substring-no-properties
            (match-end 0) (line-end-position 1)))))))

(define-derived-mode svn-update-mode fundamental-mode "Update for Subversion "
  ""
  (setq buffer-read-only t
        buffer-invisibility-spec nil
        font-lock-defaults '(svn-update-font-lock-keywords t nil nil nil))

  (widen)
  (buffer-disable-undo (current-buffer))
  (set-buffer-modified-p nil)
  (hl-line-mode 1)
  (svn-minor-mode 1)

  (let (buffer-read-only x)
    (save-excursion
      (save-match-data

        (goto-char (point-min))
        (while (not (eobp))
          (cond
           ((looking-at "[A-Z][A-Z ] +")
            (setq x (char-after))
            (put-text-property (match-end 0) (line-end-position)
                               'mouse-face 'highlight))
           ((looking-at " [A-Z] +")
            (setq x (char-after (1+ (point))))
            (put-text-property (match-end 0) (line-end-position)
                               'mouse-face 'highlight))
           ((looking-at "?")
            (setq x (char-after)))
           (t
            (setq x nil)))

          (put-text-property (point) (line-beginning-position 2) 'invisible x)
          (forward-line 1)))))
  (setq buffer-invisibility-spec svn-update-initial-buffer-invisibility-spec)

  ;; 見えない所にカーソルがあれば、見える所まで移動。
  (while (and (member (get-text-property (point) 'invisible)
                      buffer-invisibility-spec)
              (not (eobp)))
    (forward-line 1)
  (svn-update-select)))

(defun svn-update-select ()
  (beginning-of-line 1)
  (if (looking-at "[A-Z ][A-Z ] ")
      (forward-char 1)))

(defun svn-update-next (n)
  (interactive "p")
  (let ((d (if (< 0 n) 1 -1))
        (pos (point)))
    (setq svn-update-move-direction d)
    (while (and (not (zerop n))
                (zerop (forward-line d)))
      (if (and (looking-at "[A-Z ][A-Z ] ")
               (not (member (get-text-property (point) 'invisible)
                            buffer-invisibility-spec)))
          (setq n (- n d)
                pos (point))))
    (goto-char pos))
  (svn-update-select)
  n)

(defun svn-update-previous (n)
  (interactive "p")
  (- (svn-update-next (- n))))

(defun svn-update-find-file ()
  (interactive)
  (let ((file (svn-update-file)))
    (if file
        (find-file-other-window file)
      (message "File not found at this line"))))

(defun svn-update-mark-p ()
   (catch 'found
     (let ((ols (overlays-at (point))))
       (while ols
         (if (eq (overlay-get (car ols) 'face)
                 'explorer-mark-file-face)
             (throw 'found (car ols)))
         (setq ols (cdr ols))))))

(defun svn-update-mark (n)
  (interactive "p")
  (if (< n 0)
      (svn-update-unmark (- n))
    (let (ol)
      (while (< 0 n)
        (setq n (1- n))
        (or (svn-update-mark-p)
            (progn
              (setq ol (make-overlay (line-beginning-position 1)
                                     (line-end-position 1)))
              (overlay-put ol 'evaporate t)
              (overlay-put ol 'face 'explorer-mark-file-face)))
        (svn-update-next svn-update-move-direction)))))

(defun svn-update-unmark (n)
  (interactive "p")
  (if (< n 0)
      (svn-update-mark (- n))
    (let (ol)
      (while (< 0 n)
        (setq n (1- n))
        (if (setq ol (svn-update-mark-p))
            (delete-overlay ol))
        (svn-update-next svn-update-move-direction)))))

(defun svn-update-show-all ()
  (interactive)
  (setq buffer-invisibility-spec nil)
  (redraw-display))

(defun svn-update-toggle-invisible (char)
  (interactive (list (aref (this-command-keys-vector) 0)))
  (setq buffer-invisibility-spec
        (if (member char buffer-invisibility-spec)
            (delete char buffer-invisibility-spec)
          (cons char buffer-invisibility-spec)))
  (redraw-display))

(defun svn-update-select-mouse (event)
  (interactive "e")
  (goto-char (posn-point (event-end event)))
  (svn-update-select))

(defun svn-update-find-file-mouse (event)
  (interactive "e")
  (svn-update-select-mouse event)
  (svn-update-find-file))

;;; svn-minor-mode

(defun svn-update-marked-file ()
  (let (files)
    (save-excursion
      (mapc (lambda (ol)
              (if (eq (overlay-get ol 'face) 'explorer-mark-file-face)
                  (setq files (cons (svn-update-file (overlay-start ol))
                                    files))))
            (overlays-in (point-min) (point-max))))
    (or files
        (list (svn-update-file)))))

(svn-set-minor-mode-config
 'svn-update-mode
 '(("WCPATH-s" . svn-update-marked-file)))

(provide 'svn-update)

