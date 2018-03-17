;;; -*- emacs-lisp -*-

;;; ToDo
;;; ・複数ファイルを扱えるようにする

(eval-when-compile
  (require 'svn)
  (defvar svn-blame-files nil)
  (defvar svn-blame-font-lock-keywords nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for blame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar svn-blame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'svn-blame-next-change)
    (define-key map "p" 'svn-blame-previous-change)
;;    (define-key map "N" 'svn-blame-next-file)
;;    (define-key map "P" 'svn-blame-previous-file)
    (define-key map [return] 'svn-blame-find-file)
    map)
  "* ")

(defun svn-blame-def-face (color)
  (let ((sym (intern (concat "svn-blame-face-" color))))
    (eval
     `(progn
        (copy-face 'default ',sym)
        (set-face-background ',sym ,color)
        (defvar ,sym ',sym)))))

(defvar svn-blame-faces
  (mapcar 'svn-blame-def-face
          '("honeydew1" "honeydew2" "honeydew3"
            "LightSkyBlue1" "LightSkyBlue2" "LightSkyBlue3"
            "DeepSkyBlue1" "DeepSkyBlue2" "DeepSkyBlue3"
            "turquoise1" "turquoise2" "turquoise3"
            "SeaGreen1" "SeaGreen2" "SeaGreen3"
            "OliveDrab1" "OliveDrab2" "OliveDrab3"
            "khaki1" "khaki2" "khaki3"
            "yellow1" "yellow2" "yellow3"
            "Goldenrod1" "Goldenrod2""Goldenrod3"
            "LightPink1" "LightPink2" "LightPink3"
            "DeepPink1" "DeepPink2" "DeepPink3"
            "tan1" "tan2" "tan3"
            "tomato1" "tomato2" "tomato3"
            "plum1" "plum2" "plum3"))
  "* ")

(defun svn-blame-mode (files)
  "* Blame for Subversion"

  (if svn-debug
      (message "svn-blame-mode(%s)" files))
  (kill-all-local-variables)
  (widen)

  (setq major-mode 'svn-blame-mode
        mode-name "Blame for Subversion"

        buffer-read-only t
        buffer-invisibility-spec nil)

  (set (make-local-variable 'svn-blame-font-lock-keywords) nil)
  (set (make-local-variable 'svn-blame-files) files)

  (set-buffer-modified-p nil)
  (buffer-disable-undo (current-buffer))
  (use-local-map svn-blame-mode-map)

  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((faces svn-blame-faces)
            reg buffer-read-only)
        (while (not (eobp))
          (if (looking-at (concat "^ *" svn-version-regexp))
              (progn
                (setq reg (concat "^ *" (match-string 1) " .*\n"))
                (or (cdr (assoc reg svn-blame-font-lock-keywords))
                    (setq svn-blame-font-lock-keywords
                          (cons (cons reg (or (car faces) 'fringe))
                                svn-blame-font-lock-keywords)
                          faces (cdr faces)))

                (while (looking-at reg)
                  (forward-line 1)))
            (forward-line 1))))))

  (setq font-lock-defaults '(svn-blame-font-lock-keywords t nil nil nil))
  (set-buffer-modified-p nil)

  (svn-minor-mode 1))

(defun svn-blame-version ()
  (if (looking-at (concat " *" svn-version-regexp))
      (match-string-no-properties 1)))

(defun svn-blame-goto-revision (revision)
  ""
  (interactive)
  (let* ((reg (concat "^ *" (regexp-quote revision) " "))
         (point (re-search-forward reg nil t)))
    (if point
        (goto-char point))))

(defun svn-blame-next-change (n)
  ""
  (interactive "p")
  (let ((d (if (< 0 n) 1 -1))
        (version (svn-blame-version))
        (pos (point))
        (skip nil))
    (while (and (not (zerop n))
                (zerop (forward-line d)))
      (setq skip (svn-blame-version))
      (if skip
          (if (not (equal version skip))
              (setq n (- n d)
                    pos (point)
                    version skip))
        (setq version nil)))
    (goto-char pos))
  n)

(defun svn-blame-previous-change (n)
  ""
  (interactive "p")
  (- (svn-blame-next-change (- n))))

;;(defun svn-blame-next-file (n)
;;  ""
;;  (interactive "p")
;;  (let ((regexp "^Annotations for +\\(.+\\)")
;;        (pos (point))
;;        (func (if (< 0 n) 're-search-forward 're-search-backward))
;;        (d (if (< 0 n) 1 -1)))
;;    (forward-line 1)
;;    (if (re-search-backward regexp nil t)
;;        (setq pos (point))
;;      (if (re-search-forward regexp nil t)
;;          (setq pos (line-beginning-position 1))))
;;    (while (and (not (zerop n))
;;                (progn
;;                  (forward-line d)
;;                  (funcall func regexp nil t)))
;;      (beginning-of-line 1)
;;      (setq n (- n d)
;;            pos (point)))
;;    (goto-char pos))
;;  n)

;;(defun svn-blame-previous-file (n)
;;  ""
;;  (interactive "p")
;;  (- (svn-blame-next-file (- n))))

(defun svn-blame-find-file ()
  (interactive)
  (find-file-other-window (car svn-blame-files)))

;;; svn-minor-mode

(defun svn-blame-get-WCPATH-s ()
  svn-blame-files)

(defun svn-blame-get-WCPATH-REV-s ()
  (let ((version (svn-blame-version)))
    (mapcar (lambda (file) (concat file "@" version)) svn-blame-files)))

(svn-set-minor-mode-config
 'svn-blame-mode
 '(("WCPATH-s" . svn-blame-get-WCPATH-s)
   ("WCPATH-REV-s" . svn-blame-get-WCPATH-REV-s)
   ("VERSION" . svn-blame-version)
))

(provide 'svn-blame)
