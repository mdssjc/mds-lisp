;;; config.el -*- lexical-binding: t; -*-


;;
;; Plugins
;;

(def-package! avy
  :commands (avy-goto-char-2
             avy-goto-line
             avy-goto-char-timer
             avy-goto-line-above
             avy-goto-line-below
             avy-goto-char-in-line
             avy-goto-char-in-line-left
             avy-goto-char-in-line-right)
  :config
  (setq avy-all-windows nil
        avy-background  t))

(def-package! rg
  :commands (rg-dwim rg-kill-saved-searches rg-list-searches rg-project
                     rg rg-save-search rg-save-search-as-name rg-literal)
  :config
  (setq rg-group-result t
        rg-show-columns t))

(def-package! ripgrep
  :commands ripgrep-regexp)

(def-package! multiple-cursors
  :commands (multiple-cursors-mode
             mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-next-like-this-symbol
             mc/mark-previous-like-this-symbol
             mc/mark-all-dwim
             mc/mark-all-in-region-regexp)
  :config
  (multiple-cursors-mode))

(def-package! ialign
  :commands ialign)

(def-package! move-dup
  :commands md/move-lines-up md/move-lines-down md/duplicate-up md/duplicate-down)



;;
;; Config
;;

(load! "+bindings")


(after! company
  (setq company-minimum-prefix-length 1
        company-show-numbers t
        company-selection-wrap-around t))

(after! magit
  (setq magit-diff-refine-hunk 'all))

(after! markdown-mode
  (remove-hook 'markdown-mode-hook #'auto-fill-mode))



;;
;; ORG - GTD
;;

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(d)" "BLOCKED(b)" "REVIEW(r)"
                                      "|" "DONE(e)" "ARCHIVED(a)" "CANCELED(c)"))
        org-todo-keyword-faces '(("TODO"     . org-warning)
                                 ("DOING"    . "yellow")
                                 ("BLOCKED"  . "red")
                                 ("REVIEW"   . "orange")
                                 ("DONE"     . "green")
                                 ("ARCHIVED" . "blue")
                                 ("CANCELED" . "red1"))
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-tags-column -77
        ;; Agenda
        org-agenda-files '("~/Documents/GTD/someday-maybe.org"
                           "~/Documents/GTD/calendar.org"
                           "~/Documents/GTD/projects.org")
        org-agenda-custom-commands '(("W" "Weekly Review"
                                      ((agenda "" ((org-agenda-ndays 7)))
                                       (stuck "")
                                       (todo "DOING")
                                       (org-agenda-entry-types '(:deadline))
                                       (org-deadline-warning-days 30))))
        ;; Refile
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        completing-read-function 'ivy-completing-read
        org-refile-targets '(("~/Documents/GTD/trash.org"         :maxlevel . 1)
                             ("~/Documents/GTD/someday-maybe.org" :maxlevel . 1)
                             ("~/Documents/GTD/reference.org"     :maxlevel . 1)
                             ("~/Documents/GTD/projects.org"      :maxlevel . 3)
                             ("~/Documents/GTD/next-action.org"   :maxlevel . 2)
                             ("~/Documents/GTD/waiting-for.org"   :maxlevel . 1))
        ;; Templates - Capture
        org-capture-templates '(("n" "Notes" entry
                                 (file+headline +org-default-notes-file "Notes")
                                 "* %i%?"
                                 :prepend t :kill-buffer t)
                                ("t" "Inbox" entry
                                 (file+headline "~/Documents/GTD/inbox.org" "Things")
                                 "* %i%?\nCreate in %U"
                                 :prepend t :kill-buffer t)))
  ;; Encrypt all entries before saving
  ;; (org-crypt-use-before-save-magic)
  ;; (setq org-tags-exclude-from-inheritance '("crypt"))
  ;; (let ((secrets-path "~/.config/doom/modules/private/mdssjc/secrets/secrets.el"))
  ;;   (if (file-exists-p secrets-path)
  ;;       (progn (load secrets-path)
  ;;              ;; GPG key to use for encryption
  ;;              (setq org-crypt-key user-password))
  ;;     (message "Use mds-secrets-template.el as the basis for the secrets.el file.")))
  )
