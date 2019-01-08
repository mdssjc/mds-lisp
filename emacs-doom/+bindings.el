;;; +bindings.el -*- lexical-binding: t; -*-


(map! "C-'"         #'imenu
      "M-SPC"       #'cycle-spacing

      "C-M-<up>"    #'mc/mark-previous-like-this
      "C-M-<down>"  #'mc/mark-next-like-this
      "C-M-<left>"  #'mc/mark-previous-like-this-symbol
      "C-M-<right>" #'mc/mark-next-like-this-symbol
      "C-M-<"       #'mc/mark-all-dwim
      "C-M->"       #'mc/mark-all-in-region-regexp

      "s-\\ \\"      #'align-regexp
      "s-\\ SPC"     #'ialign
      :n "g \\ \\"   #'align-regexp
      :n "g \\ SPC"  #'ialign

      ;; Org-mode
      "<f9> SPC"    #'mds/gtd
      "<f9> 1"      #'mds/gtd-projects
      "<f9> b"      #'org-switchb
      "<f9> g"      #'org-clock-goto
      "<f9> l"      #'org-store-link

      (:mode prog-mode
       "M-<up>"     #'md/move-lines-up
       "M-<down>"   #'md/move-lines-down
       "S-M-<up>"   #'md/duplicate-up
       "S-M-<down>" #'md/duplicate-down)

      (:map evil-window-map
        "<up>"    #'evil-window-up
        "<down>"  #'evil-window-down
        "<left>"  #'evil-window-left
        "<right>" #'evil-window-right)

      (:after evil-easymotion
        :map evilem-map
          "<up>"    #'evilem-motion-previous-line
          "<down>"  #'evilem-motion-next-line
          "<left>"  #'evilem-motion-backward-word-begin
          "<right>" #'evilem-motion-forward-word-begin))


;;  (map! :leader
;;     (:desc "search" :prefix "/"
;;       (:desc "Rg" :prefix "r"
;;         :desc "rg-dwim"                :n "d"  #'rg-dwim
;;         :desc "rg-kill-saved-searches" :n "k"  #'rg-kill-saved-searches
;         :desc "rg-list-searches"       :n "l"  #'rg-list-searches
; ;        :desc "rg-project"             :n "p"  #'rg-project
; ;        :desc "rg"                     :n "r"  #'rg
;         :desc "rg-save-search"         :n "s"  #'rg-save-search
;         :desc "rg-save-search-as-name" :n "S"  #'rg-save-search-as-name
;         :desc "rg-list-searches"       :n "t"  #'rg-literal)
;       :desc "Ripgrep"    :n "SPC" #'ripgrep-regexp
;       :desc "Counsel-Rg" :n "."   #'counsel-rg
;       :desc "Deadgrep"   :n "t"   #'deadgrep)
;     (:desc "macro" :prefix "0"
;       :desc "Universal Argument" #'universal-argument
;       :desc "Start Macro"        #'kmacro-start-macro
;       :desc "End Macro"          #'kmacro-end-macro
;       :desc "End And Call Macro" #'kmacro-end-and-call-macro
;;       :desc "Kmacro Bind To Key" #'kmacro-bind-to-key
;       :desc "Name Last Macro"    #'kmacro-name-last-macro
;       :desc "Insert Macro"       #'insert-kbd-macro)
;     (:desc "syntactic" :prefix "1"
;;;       :desc "cleanup" :n "c" #'whitespace-cleanup
;       (:desc "Ispell" :prefix "s"
;         :desc "pt-br" :n "p" #'ispell-pt-br
;         :desc "en-us" :n "e" #'ispell-en-us
;         :desc "en-gb" :n "g" #'ispell-en-gb))
;     (:desc "semantic" :prefix "2"
;       (:desc "Language Tool" :prefix "l"
;         :desc "check"                          :n "c"   #'langtool-check
;         :desc "check-done"                     :n "d"   #'langtool-check-done
;         :desc "correct-buffer"                 :n "b"   #'langtool-correct-buffer
;         :desc "switch-default-language"        :n "s"   #'langtool-switch-default-language
;         :desc "show-message-at-point"          :n "."   #'langtool-show-message-at-point)
;       (:desc "Google-This" :prefix "g"
;         :desc "this-search"                    :n "RET" #'google-this-search
;         :desc "this-region"                    :n "SPC" #'google-this-region
;         :desc "this"                           :n "t"   #'google-this
;         :desc "this-noconfirm"                 :n "n"   #'google-this-noconfirm
;         :desc "this-lucky-search"              :n "g"   #'google-this-lucky-search
;         :desc "this-lucky-and-insert-url"      :n "i"   #'google-this-lucky-and-insert-url
;         :desc "this-word"                      :n "w"   #'google-this-word
;         :desc "this-symbol"                    :n "s"   #'google-this-symbol
;         :desc "this-line"                      :n "l"   #'google-this-line
;         :desc "this-error"                     :n "e"   #'google-this-error
;         :desc "this-forecast"                  :n "f"   #'google-this-forecast
;         :desc "this-cpp-reference"             :n "r"   #'google-this-cpp-reference
;         :desc "this-maps"                      :n "m"   #'google-this-maps
;         :desc "this-ray"                       :n "a"   #'google-this-ray
;         :desc "maps"                           :n "m"   #'google-maps
;         :desc "this-translate-query-or-region" :n "c"   #'google-this-translate-query-or-region)
;       (:desc "Google-Translate" :prefix "t"
;         :desc "translate-at-point"             :n "."   #'google-translate-at-point
;         :desc "translate-smooth-translate"     :n "RET" #'google-translate-smooth-translate
;         :desc "translate-query-translate"      :n "SPC" #'google-translate-query-translate)))
;;;
