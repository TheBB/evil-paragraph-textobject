(require 'cl-lib)
(require 'evil)

(defun evil-paragraph--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun evil-paragraph--empty-line-p ()
  (string= "" (evil-indent--chomp (thing-at-point 'line))))

(defun evil-paragraph--not-empty-line-p ()
  (not (evil-paragraph--empty-line-p)))

(defun evil-paragraph--last-line-p (pt)
  (save-excursion
    (goto-char pt)
    (forward-line 1)
    (= pt (point))))

(defun evil-paragraph--seek (start direction before skip predicate)
  "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds. Skips one line in the beginning if skip is truthy."
  (save-excursion
    (goto-char start)
    (goto-char (point-at-bol))
    (let ((bnd (if (> 0 direction)
                   (point-min)
                 (point-max)))
          (pt (point)))
      (when skip (forward-line direction))
      (loop while (and (/= (point) bnd) (funcall predicate))
            do (progn
                 (when before (setq pt (point-at-bol)))
                 (forward-line direction)
                 (unless before (setq pt (point-at-bol)))))
      pt)))

(evil-define-text-object evil-paragraph-a-paragraph (&optional count beg end type)
  "Text object describing a paragraph."
  :type line
  (save-excursion
    (let ((source (point))
          begin end nend)

      ;; Seek forward to find the first non-empty line
      (setq source (evil-paragraph--seek source 1 nil nil 'evil-paragraph--empty-line-p))

      ;; Seek backward to find the beginning of the paragraph
      (setq begin (evil-paragraph--seek source -1 t nil 'evil-paragraph--not-empty-line-p))

      ;; Seek forward to find the end of the paragraph
      (setq end (evil-paragraph--seek source 1 t nil 'evil-paragraph--not-empty-line-p))

      ;; Seek forward to find the beginning of the next paragraph
      (setq nend (evil-paragraph--seek end 1 t t 'evil-paragraph--empty-line-p))

      ;; If there's no next paragraph, seek backward until the end of the previous one
      (when (= nend end)
        (setq begin (evil-paragraph--seek begin -1 t t 'evil-paragraph--empty-line-p)))
      (evil-range begin nend 'line))))

;;;###autoload
(eval-after-load 'evil
  '(progn
     (autoload 'evil-paragraph-a-paragraph "evil-paragraph-textobject" nil t)
     (define-key evil-outer-text-objects-map "p" 'evil-paragraph-a-paragraph)))

(provide 'evil-paragraph-textobject)
