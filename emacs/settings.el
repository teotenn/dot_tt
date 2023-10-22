(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(eval-when-compile 
  (add-to-list 'load-path "~/.emacs.d/elpa/")
  (require 'use-package))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; A diferentiation for termux
(setq tt/is-termux (string-match-p
		    (rx (* nonl) "com.termux" (* nonl))
		    (getenv "HOME")))

;; Stop the welcome screen
(setq inhibit-startup-screen t)
;; Hide toolbar, scrollbar and menu
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Allows mark-type-delete
(delete-selection-mode t)

;; Starting file
(setq initial-buffer-choice
      (lambda ()
	(if (buffer-file-name)
	    (current-buffer)
	  (find-file "~/Code/personal_config/org/brujula.org"))))

;; enable column numbers
(setq column-number-mode t)

;; show-paren-mode
(show-paren-mode 1)

;; Save bookmars upon adding each
(setq bookmark-save-flag 1)

;; always allow 'y' instead of 'yes'.
(setq use-short-answers t)

					; default to utf-8 for all the things
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Backup files in ~/tmp/
(setq temporary-file-directory "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Changelog on Linux systems
(if (eq system-type 'gnu/linux)
    (progn
      (setq add-log-full-name "Manuel Teodoro")
      (setq add-log-mailing-address "teotenn@proton.me")
      (setq change-log-default-name "CHANGELOG")))

(add-hook 'change-log-mode-hook
	  (lambda ()
	    (make-local-variable 'tab-width)
	    (make-local-variable 'left-margin)
	    (setq tab-width   2
		  left-margin 2)))

;; Shell warning indentation off
(advice-add 'sh-set-shell :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'message) #'ignore))
                (apply orig-fun args))))

;; Extra code
(add-to-list 'load-path "~/Code/dot_tt/emacs/scripts/")

;; Make eww default browser
(setq browse-url-browser-function 'eww-browse-url)

;; From emacs 29.1
(setq show-paren-context-when-offscreen 'overlay)

;; personal function for windows
(defun tt/wrap ()
  "Shortcut to open neotree directly on wrapper"
  (interactive)
  (if (eq system-type 'windows-nt)
      (neotree-dir "c:/Users/teodorm3/Documents/Wrapper")
    (message "tt/wrap is available only on Windows")))

;; Personal registers
(set-register ?w '(buffer . "workflow.org"))

;; Select font
(defun tt/set-font-if-found (family font size)
  "If the font is installed, sets it globally for the session,
   given a `family' name, `set-frame-font' the `font' by its name and `size'"
  (let ((selected-font (format "%s %s" font size)))
  (find-font (font-spec :name family))
  (set-frame-font selected-font)))

;; My pre-selected font
(tt/set-font-if-found "Jetbrains" "Jetbrains Mono" 10)

(defun tt/switch-font (arg)
  "Switches fonts from a pre-defined list of `arg' size, or 12 by default.

   The list and details of the fonts has to be defined within the function
   based on personal choice."
  (interactive "P")
  (let* ((list-fonts '("Jetbrains" "Montserrat" "Monospace"))
	 (font-size (or arg 10))
	 (selected-font (ido-completing-read "Select font: " list-fonts)))
    (cond
     ((< font-size 7)
      (message "Size selected is too small!"))
     ((string= selected-font "Jetbrains")
      (tt/set-font-if-found "Jetbrains" "Jetbrains Mono" font-size))
     ((string= selected-font "Montserrat")
      (tt/set-font-if-found "Montserrat" "Montserrat" font-size))
     ((string= selected-font "Monospace")
      (tt/set-font-if-found "Cascadia Mono" "Monospace" font-size)))))

;; Keybindings to toggle 
(defvar toggle-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'display-line-numbers-mode)
    (define-key map "t" 'tool-bar-mode)
    (define-key map "s" 'scroll-bar-mode)
    (define-key map "m" 'menu-bar-mode)
    (define-key map "f" 'tt/switch-font)
    (define-key map "d" 'neotree-toggle)
    map)
  "Key map for toggling")
(global-set-key (kbd "<f9>") toggle-keymap)

;; tt-edit-mode
(require 'tt-edit-mode)
(global-set-key (kbd "<f12>") tt-edit-mode-keymap)

;; Some interesting moving through buffers
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; Dictionaries
(use-package flyspell
  :defer t
  :if (eq system-type 'windows-nt)
  :init
  (setenv "DICPATH" (concat (getenv "HOME") "/Library/Spelling"))
  (setq ispell-program-name "C:\\Users\\teodorm3\\Bin\\Hunspell\\bin\\hunspell.exe"))

(use-package flyspell
  :defer t
  :if (and (eq system-type 'gnu/linux)
	   (not tt/is-termux))
  :config
  (setq ispell-program-name "aspell"))

(use-package flyspell
 :defer t
 :if tt/is-termux
 :config
 (setq ispell-program-name (executable-find "hunspell")))

;; Check on the go for all text-based modes (org, md, etc)
(add-hook 'text-mode-hook 'flyspell-mode)
(setq ispell-list-command "--list")

(use-package magit)

;; Config for windows
(if (eq system-type 'windows-nt)
    (use-package ssh-agency))
(if (eq system-type 'windows-nt)
    (setenv "SSH_ASKPASS" "git-gui--askpass"))

(use-package cheatsheet
  :config
  (cheatsheet-add-group 'Info
			'(:key "C-x l" :description "count-lines-page"))
  (cheatsheet-add-group 'Consoles
			'(:key "M-r" :description "Back search")
			'(:key "C-c C-l" :description "list previous commands")
			'(:key "C-c RET" :description "copy NOT execute cmd"))
  (cheatsheet-add-group 'R
			'(:key "C-c <F5>" :description "shiny run_app()")
			'(:key "C-c C-z" :description "move console-script"))
  (cheatsheet-add-group 'Move
			'(:key "M-g i" :description "i menu")
			'(:key "C-c m" :description "imenu-list-smart-toggle")
			'(:key "C-x o" :description "other-window"))
  (cheatsheet-add-group 'Edit
			'(:key "M-h" :description "mark-parragraph")
			'(:key "C-M-h" :description "mark function")
			'(:key "M-y" :description "yank-pop")
			'(:key "C-x C-o" :description "delete-blank-lines")
			'(:key "C-x n" :description "narrow menu")
			'(:key "C-x w" :description "widen")
			'(:key "C-c c" :description "tt/copy-symbol-at-point")
			'(:key "M-u" :description "make-upcase-at-point")
			'(:key "C-x C-u" :description "upcase-region")))
(global-set-key (kbd "C-c s") 'cheatsheet-show)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "SlateBlue1"))))
   '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "chartreuse4"))))
   '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "medium orchid"))))
   '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "HotPink1"))))
   '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "SystemHilight"))))
   '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray55"))))
   '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "firebrick1"))))
   '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "chartreuse2"))))
   '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "purple3"))))
   ))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"
	  "~/Code/dot_tt/emacs/snippets"
	  ))
  :config
  (yas-global-mode 1))

;; csv-mode is not default anymore
(use-package csv-mode)

;; load screenshot script
;; cloned from https://github.com/tecosaur/screenshot
;; Require pckgs <transient> and <posframe>
(use-package transient)
(use-package posframe)

(defun tt/load-screenshot()
  (interactive)
  (load "screenshot.el"))

;; Lisp interpreter (for slime and sly)
;; (use-package slime
;;   :if (eq system-type 'windows-nt)
;;   :ensure nil
;;   :disabled)

;; (use-package slime
;;   :if (eq system-type 'gnu/linux)
;;   :init
;;   (setq inferior-lisp-program "sbcl"))

;; neotree
(use-package neotree
  :config
  (setq neo-theme 'icons))

;; htmlize to improve rendering of source code blocks
(use-package htmlize)

;; all the icons
(use-package all-the-icons
  :if (display-graphic-p))

(use-package imenu-list
  :bind (("C-c m" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t))

;; Flymake
(setq tt/lintr-linters
      "lintr::linters_with_defaults(
	   line_length_linter = line_length_linter(120),
           linters = object_name_linter(styles = c('dotted.case', 'lowercase', 'snake_case'))
	 )"
      )

(use-package flymake
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;; ESS ------------------------
;; R on windows
(if (eq system-type 'windows-nt)
    (setq inferior-ess-r-program "C:/Users/teodorm3/Bin/R-4.1.2/bin/R.exe"))

;; Personal functions for ess
(defun tt-inferior-ess-keymap ()
  "Define a keymap for ESS inferior processes to call prev and next command
   with C-up and C-down respectively"
  (setq-local ansi-color-for-comint-mode 'filter)
  (define-key inferior-ess-mode-map [\C-up]
	      'comint-previous-matching-input-from-input)
  (define-key inferior-ess-mode-map [\C-down]
	      'comint-next-matching-input-from-input)
  (define-key inferior-ess-mode-map [\C-x \t]
	      'comint-dynamic-complete-filename))

(defun tt-r-ess-init ()
  "Sends variable `tt-r-profile' to an ESS process"
  (let ((proc (ess-get-process)))
    (ess-send-string proc tt-r-profile)))

(defvar tt-r-profile "
options(help_type = \"text\")\n
utils::assignInNamespace(\"q\",
  function(save = \"no\", status = 0, runLast = TRUE) 
    {.Internal(quit(save, status, runLast))}, 
  \"base\")
")

(defun tt/shiny-run-app ()
  "Executes shiny <run_app()> in the inferior-ess-r process."
  (interactive)
  (let ((proc (ess-get-process)))
    (ess-send-string proc "run_app()")))

;; ESS config
(use-package ess
  :init
  (setq ess-style 'RStudio)
  :hook ((inferior-ess-mode . tt-inferior-ess-keymap)
	 (ess-r-post-run . tt-r-ess-init))
  :bind (("C-c <f5>" . tt/shiny-run-app))
  :config
  (setq ess-r-flymake-linters tt/lintr-linters)
  (setq ess-use-flymake nil)
  (setq ess-eval-visibly-p t) ; ESS process (print all)
  (setq ess-ask-for-ess-directory nil)
  ;; Syntax highlights
  (setq ess-R-font-lock-keywords
	'((ess-R-fl-keyword:keywords . t)
	  (ess-R-fl-keyword:constants . t)
	  (ess-R-fl-keyword:modifiers . t)
	  (ess-R-fl-keyword:fun-defs . t)
	  (ess-R-fl-keyword:assign-ops . t)
	  (ess-R-fl-keyword:%op% . t)
	  (ess-fl-keyword:fun-calls . t)
	  (ess-fl-keyword:numbers . t)
	  (ess-fl-keyword:operators)
	  (ess-fl-keyword:delimiters)
	  (ess-fl-keyword:=)
	  (ess-R-fl-keyword:F&T . t)))
  )

;; Flycheck for syntax. Not global
;;(setq lintr-modifier-function "with_defaults(line_length_linter=NULL)")

;; (use-package flycheck
;;   :config
;;   (setq flycheck-lintr-linters tt/lintr-linters))

(use-package flycheck
  :if (eq system-type 'windows-nt)
  :init
  (setq flycheck-r-lintr-executable "C:\\Users\\teodorm3\\Bin\\R-4.2.1\\bin\\x64\\R.exe")
  :config
  (setq flycheck-lintr-linters "linters_with_defaults(line_length_linter = line_length_linter(120))"))

;; R markdown
(use-package polymode)
(use-package poly-R)
(use-package poly-markdown)
(use-package quarto-mode)

;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

 ;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
 ;;(autoload 'r-mode "ess-site" "(Autoload)" t)

;; Add chunk
(defun rmarkdown-new-chunk (name)
  "Insert a new R chunk."
  (interactive "sChunk name: ")
  (insert "\n```{r " name "}\n")
  (save-excursion
    (newline)
    (insert "```\n")
    (previous-line)))
;; Map it to C-c `
(define-key markdown-mode-map "\C-c`" 'rmarkdown-new-chunk)

(use-package company
  :config
  ;; Turn on company-mode globally:
  (add-hook 'after-init-hook 'global-company-mode)
;; More customization options for company:
(setq company-selection-wrap-around t
      ;; Align annotations to the right tooltip border:
      company-tooltip-align-annotations t
      ;; Idle delay in seconds until completion starts automatically:
      company-idle-delay 0.45
      ;; Completion will start after typing two letters:
      company-minimum-prefix-length 3
      ;; Maximum number of candidates in the tooltip:
      company-tooltip-limit 10))

(use-package company-quickhelp
  :custom
  ;; Load company-quickhelp globally:
  (company-quickhelp-mode)
  ;; Time before display of documentation popup:
  (setq company-quickhelp-delay nil))

(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

(use-package org
  :ensure nil
  :bind
  ("M-q" . toggle-truncate-lines)
  ("C-c a" . org-agenda)
  :config
  (setq org-agenda-files '("~/Code/personal_config/org/"))
  ;; Settags closer (default is -80)
  (setq org-tags-column -40)
  ;; src blocks
  (setq org-src-fontify-natively t
	org-src-window-setup 'current-window
	org-src-strip-leading-and-trailing-blank-lines t
	org-src-preserve-indentation t
	org-src-tab-acts-natively t)
  ;; org clock format
  (setq org-duration-format (quote h:mm))
  (setq org-ellipsis " ≫"))

;; --- ORG BABEL ---
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (emacs-lisp . t))
 )

(if (eq system-type 'gnu/linux)
    (setq org-babel-python-command "python3"))

(if (eq system-type 'windows-nt)
    (setq org-babel-R-command "C:/Users/teodorm3/Bin/R-4.2.1/bin/x64/R --slave --no-save"))

;; Bullets
(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '("✙" "✤" "✚" "✜" "✛" "✢" "✣" "✥" "✠" "☥")))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package org-tempo
  :ensure nil
  :config
  ;; clocktable
  (add-to-list 'org-structure-template-alist '("CT" . ": clocktable :scope subtree :maxlevel 4 :block today"))
  ;; other
  ;; (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("p" . "src python"))
  (add-to-list 'org-structure-template-alist '("pexport" . "src python :session :results output :exports both"))
  (add-to-list 'org-structure-template-alist '("pnoeval" . "src python :exports code :eval no"))
  (add-to-list 'org-structure-template-alist '("phide" . "src python :session :exports none"))
  ;; elisp
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  ;; R
  (add-to-list 'org-structure-template-alist '("r" . "src R"))
  (add-to-list 'org-structure-template-alist '("rtibble" . "src R :session :results table :colnames yes :exports both"))
  (add-to-list 'org-structure-template-alist '("rplot" . "src R :session :file figure-N.png :results value graphics file :results output :exports both"))
  (add-to-list 'org-structure-template-alist '("rexport" . "src R :session :results output :exports both")))

(use-package org-transclusion)

;;(setq python-shell-interpreter "python3")

(use-package elpy
  :if (eq system-type 'gnu/linux)
  :init
  (setq elpy-rpc-python-command "python3")
  :config
  (elpy-enable)
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

(use-package jedi)

;; Auto formatting help
;; Requires to install python "black"
;; Use it by calling M-x blacken-buffer
(use-package blacken)

;; Jupyter and iPython
(use-package ein
  :hook (ein:connect-mode-hook . ein:jedi-setup))

;; All the icons
(use-package all-the-icons)

;; Modus Themes ---
(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
	modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

  ;; Maybe define some palette overrides, such as by using our presets
  ;; (setq modus-themes-common-palette-overrides
  ;;       modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi-tinted :no-confirm)
  (define-key toggle-keymap (kbd "z") #'modus-themes-toggle)
  (define-key toggle-keymap (kbd "Z") #'modus-themes-select))

(use-package counsel
  :after ivy
  :config (counsel-mode))
(use-package ivy
  :defer 0.1
  :diminish
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))


(setq ivy-initial-inputs-alist nil)

(use-package smex)
(smex-initialize)

(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
    '((swiper                     . ivy-posframe-display-at-point)
      (complete-symbol            . ivy-posframe-display-at-point)
      (counsel-M-x                . ivy-display-function-fallback)
      (counsel-esh-history        . ivy-posframe-display-at-window-center)
      (counsel-describe-function  . ivy-display-function-fallback)
      (counsel-describe-variable  . ivy-display-function-fallback)
      (counsel-find-file          . ivy-display-function-fallback)
      (counsel-recentf            . ivy-display-function-fallback)
      (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
      (dmenu                      . ivy-posframe-display-at-frame-top-center)
      (nil                        . ivy-posframe-display))
    ivy-posframe-height-alist
    '((swiper . 20)
      (dmenu . 20)
      (t . 10)))
  :config
  (ivy-posframe-mode 1)) ; 1 enables posframe-mode, 0 disables it.

(use-package which-key
  :config
  (which-key-mode))

(use-package time
  :ensure nil
  :config
  (setq display-time-format "%b/%e %H:%M ")
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  (add-hook 'after-init-hook #'display-time-mode))


;(load "prot-modeline.el")
(require 'prot-modeline)
(load "tt-modeline.el")

(setq mode-line-compact nil) ; Emacs 28

(setq-default mode-line-format
              '("%e"
                prot-modeline-kbd-macro
                prot-modeline-narrow
                prot-modeline-input-method
                prot-modeline-buffer-status
                " "
                prot-modeline-buffer-identification
                "  "
                prot-modeline-major-mode
                prot-modeline-process
                "  "
                prot-modeline-vc-branch
                "  "
		mode-line-position
                "  "
                prot-modeline-align-right
		(:eval (custom-modeline-region-info))
		" "
                prot-modeline-misc-info))

(prot-modeline-subtle-mode 1)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(setq dired-listing-switches "-aBhl --group-directories-first")

(use-package all-the-icons-dired
  :defer t
  :hook   (dired-mode . all-the-icons-dired-mode))

(require 'dired-ranger)

;; My keybindings
(defvar tt-dired-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'dired-ranger-copy)
    (define-key map "p" 'dired-ranger-paste)
    (define-key map "m" 'dired-ranger-move)
    map)
  "Key map for dired extensions")

(define-key dired-mode-map (kbd "C-c t") tt-dired-keymap)

;; Using garbage magic hack.
 (use-package gcmh
   :config
   (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive (setq comp-async-report-warnings-errors nil)
