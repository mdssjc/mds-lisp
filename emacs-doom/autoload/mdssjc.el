;;; autoload/mdssjc.el -*- lexical-binding: t; -*-


;;;###autoload
(defun mds/gtd ()
  "Internal function."
  (interactive "")
  (find-file "~/Documents/GTD/dashboard.org"))

;;;###autoload
(defun mds/gtd-projects ()
  "Internal function."
  (interactive "")
  (find-file "~/Documents/GTD/projects.org"))



(defun ispell-dict (lang)
  (setq ispell-complete-word-dict (concat (expand-file-name user-emacs-directory) "modules/~\.config/doom/mdssjcsjc/dict/" lang ".dic"))
  (ispell-change-dictionary lang))

;;;###autoload
(defun ispell-pt-br ()
  (interactive)
  (ispell-dict "pt_BR"))

;;;###autoload
(defun ispell-en-us ()
  (interactive)
  (ispell-dict "en_US"))

;;;###autoload
(defun ispell-en-gb ()
  (interactive)
  (ispell-dict "en_GB"))



;; TODO: avaliar a remoção, pois está sem uso.
(defun avy-goto-char-in-line-side (char p1 p2 )
  (avy-with avy-goto-char
    (avy--generic-jump
     (regexp-quote (string char))
     avy-all-windows
     avy-style
     p1
     p2)))

;;;###autoload
(defun avy-goto-char-in-line-right (char)
  "Jump to the currently visible CHAR in the current line."
  (interactive (list (read-char "char: " t)))
  (avy-goto-char-in-line-side char (point) (line-end-position)))

;;;###autoload
(defun avy-goto-char-in-line-left (char)
  "Jump to the currently visible CHAR in the current line."
  (interactive (list (read-char "char: " t)))
  (avy-goto-char-in-line-side char (line-beginning-position) (point)))
