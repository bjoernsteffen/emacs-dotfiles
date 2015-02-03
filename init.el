;; My Emacs Settings

; Size of Frame
(add-to-list 'default-frame-alist '(height . 90))
(add-to-list 'default-frame-alist '(width . 115))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 800))

(when window-system
  (setq frame-title-format '("" "Emacs - %b"))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (menu-bar-mode 1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(scroll-bar-mode -1)

(setq visible-bell nil
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      save-place-file (concat user-emacs-directory "places"))

(set-face-attribute 'default nil :font "Monaco-12")

(defvar my-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(defvar my-snippets-dir (expand-file-name "snippets" user-emacs-directory)
  "This folder stores all the snippets for yasnippets.")

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'thingatpt)
(require 'dash)
(require 'time-date)

;; savehist keeps track of some history
(setq history-length 1000) ; Store more history
(use-package savehist ; Save minibuffer history
             :init (savehist-mode t)
             :config (setq savehist-save-minibuffer-history t
                           savehist-autosave-interval 180
                           savehist-additional-variables '(search ring regexp-search-ring)))

;;; Customization interface
(defconst steffenb-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")
(use-package cus-edit
             :defer t
             :config
             (setq custom-file steffenb-custom-file
                   custom-buffer-done-kill nil ; Kill when existing
                   custom-buffer-verbose-help nil ; Remove redundant help text
                   ;; Show me the real variable name
                   custom-unlispify-tag-names nil
                   custom-unlispify-menu-entries nil)
             :init (load steffenb-custom-file 'no-error 'no-message))

;;; Environment fixup
(use-package exec-path-from-shell
             :ensure t
             :if (and (eq system-type 'darwin) (display-graphic-p))
             :init
             (progn
               (exec-path-from-shell-initialize))

;;; OS X support
(use-package ns-win ; OS X window support
             :defer t
             :if (eq system-type 'darwin)
             :config
             (setq ns-pop-up-frames nil ; Don't pop up new frames from the
                                        ; workspace
                   mac-option-modifier nil
                   mac-command-modifier 'meta 
                   mac-right-command-modifier 'none
                   mac-right-option-modifier 'none
                   mac-function-modifier 'hyper
                   x-select-enable-clipboard t))


;; CUA selection mode for tables
(cua-selection-mode t)
(setq cua-auto-tabify-rectangles nil)

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; nice scrolling
(setq scroll-margin 5
      redisplay-dont-pause t)
;      scroll-conservatively 100000
;      scroll-preserve-screen-position 1)
      
(setq standard-indent 2)

;; Zenburn Theme
(use-package zenburn-theme
             :init (load-theme 'zenburn t))

;; Ido
(use-package ido
             :init (progn
                     (ido-mode)
                     (ido-everywhere))
             :config
             (setq ido-enable-flex-matching t ; Match characters if string doesn't match
                   ido-create-new-buffer 'always ; Create a new buffer if nothing matches
                   ido-use-filename-at-point 'guess
                   ;; Visit buffers and files in the selected window
                   ido-default-file-method 'selected-window
                   ido-default-buffer-method 'selected-window
                   ; ido-use-virual-buffers t
                   ido-use-faces nil)) ; Prefer flx ido faces
(use-package ido-ubiquitous ; IDO everywhere, really!
             :ensure t
             :init (ido-ubiquitous-mode))
(use-package flx-ido ; Flex matching for IDO
             :ensure t
             :init (flx-ido-mode))
(use-package ido-vertical-mode ; Vertical interface for IDO
             :ensure t
             :init (ido-vertical-mode))
(use-package smex ; Better M-x
             :ensure t
             :bind (([remap execute-extended-command] . smex)
                    ("M-X" . smex-major-mode-commands)))


;; IBuffer
(use-package ibuffer ; Better buffer list
             :bind (([remap list-buffers] . ibuffer))
             ;; Show VC Status in ibuffer
             :config (setq ibuffer-formats
                           '((mark modified read-only vc-status-mini " "
                                   (name 18 18 :left :elide)
                                   " "
                                   (size 9 -1 :right)
                                   " "
                                   (mode 16 16 :left :elide)
                                   " "
                                   (vc-status 16 16 :left)
                                   " "
                                   filename-and-process)
                             (mark modified read-only " "
                                   (name 18 18 :left :elide)
                                   " "
                                   (size 9 -1 :right)
                                   " "
                                   (mode 16 16 :left :elide)
                                   " " filename-and-process)
                             (mark " "
                                   (name 16 -1)
                                   " " filename))))
(use-package ibuffer-vc ; Group buffers by VC project and status
             :ensure t
             :defer t
             :init (add-hook 'ibuffer-hook
                             (lambda ()
                               (ibuffer-vc-set-filter-groups-by-vc-root)
                               (unless (eq ibuffer-sorting-mode 'alphabetic)
                                 (ibuffer-do-sort-by-alphabetic)))))

; Windmove
(use-package windmove ; Move between windows with Shift+Arrow
             :bind (((kbd "S-<left>") . windmove-left)
                    ((kbd "S-<right>") . windmove-right)
                    ((kbd "S-<up>") . windmove-up)
                    ((kbd "S-<down>") . windmove-down)))

(use-package ediff-wind
             :defer t
             :config
             ;; Prevent Ediff from spamming the frame
             (setq ediff-window-setup-function #'ediff-setup-windows-plain
                   ;ediff-split-window-function #'split-window-horizontally
                   diff-switches "-u"))

(use-package desktop ; Save buffers, windows and frames
             :init (desktop-save-mode)
             :config (progn
                       ;; Don't autosave desktops, it's too expensive. Desktops aren't
                       ;; that precious, and Emacs will save the desktop on exit anyway.
                       (setq desktop-auto-save-timeout nil)
                       (dolist (mode '(magit-mode git-commit-mode))
                         (add-to-list 'desktop-modes-not-to-save mode))))

(use-package tramp ; Access remote files
             :defer t
             :config
             ;; Store auto-save files locally
             (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

;; Dired
(use-package dired ; Edit directories
             :defer t
             :config
             (progn
               (require 'dired-x)
               (setq dired-auto-revert-buffer t ; Revert on re-visiting
                     ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
                     ;; uses human-readable sizes, and `-F' appends file-type classifiers
                     ;; to file names (for better highlighting)
                     dired-listing-switches "-alhF"
                     dired-dwim-target t
                     dired-ls-F-marks-symlinks t ; -F marks links with @
                     ;; Inhibit prompts for simple recursive operations
                     dired-recursive-copies 'always)))
(use-package dired-x ; Additional tools for Dired
             :defer t
             :config
             (when (eq system-type 'darwin)
               ;; OS X bsdtar is mostly compatible with GNU Tar
               (setq dired-guess-shell-gnutar "tar")))

(use-package ignoramus ; Ignore uninteresting files everywhere
             :ensure t
             :defer t
             :idle (ignoramus-setup))

(use-package bookmark ; Bookmarks for Emacs buffers
             :bind (("C-c l b" . list-bookmarks))
             ;; Save bookmarks immediately after a bookmark was added
             :config (setq bookmark-save-flag 1))

(use-package recentf ; Save recently visited files
             :defer t
             :idle (recentf-mode)
             :config
             (setq recentf-max-saved-items 200
                   recentf-max-menu-items 15
                   ;; Cleanup recent files only when Emacs is idle, but not when the mode
                   ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
                   ;; idles often enough to have the recent files list clean up regularly
                   recentf-auto-cleanup 300
                   recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                                         "/elpa/.*\\'" ; Package files
                                         ;; And all other kinds of boring files
                                         #'ignoramus-boring-p)))
(use-package saveplace ; Save point position in files
             :config (setq-default save-place t))

(setq view-read-only t) ; View read-only files

(use-package autorevert ; Auto-revert buffers of changed files
             :init
             (progn
               (global-auto-revert-mode)
               (setq auto-revert-check-vc-info t)))

(use-package image-file ; Visit images as images
             :init (auto-image-file-mode))

(use-package launch ; Open files in external programs
             :ensure t
             :defer t
             :idle (global-launch-mode))

(set-default 'indent-tabs-mode nil)
(set-default 'require-final-newline t)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)


(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package subword ; Subword/superword editing
             :defer t
             :diminish subword-mode)

(use-package adaptive-wrap ; Choose wrap prefix automatically
             :ensure t
             :defer t
             :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
             :ensure t
             :defer t
             :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
             ;; Keep the fringe
             :config (setq visual-fill-column-disable-fringe nil))

(use-package expand-region ; Expand region by semantic units
             :ensure t
             :bind (("C-=" . er/expand-region)))


(bind-key [remap just-one-space] #'cycle-spacing)


(use-package outline ; Navigate outlines in buffers
             :defer t
             :init (dolist (hook '(text-mode-hook prog-mode-hook))
                     (add-hook hook #'outline-minor-mode))
             :diminish outline-minor-mode)

(use-package hl-line ; Highlight the current line
             :init (global-hl-line-mode 1))

(use-package paren ; Highlight paired delimiters
             :init (show-paren-mode)
             :config (setq show-paren-when-point-inside-paren t
                           show-paren-when-point-in-periphery t))
(use-package rainbow-delimiters ; Highlight delimiters by depth
             :ensure t
             :defer t
             :init (dolist (hook '(text-mode-hook prog-mode-hook))
                     (add-hook hook #'rainbow-delimiters-mode)))

(use-package hi-lock ; Custom regexp highlights
             :init (global-hi-lock-mode))

;;; Skeletons, completion and expansion
;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates. Cycle instead.
(setq completion-cycle-threshold 5)
(use-package hippie-exp ; Powerful expansion and completion
             :bind (([remap dabbrev-expand] . hippie-expand))
             :config
             (setq hippie-expand-try-functions-list
                   '(try-expand-dabbrev
                     try-expand-dabbrev-all-buffers
                     try-expand-dabbrev-from-kill
                     try-complete-file-name-partially
                     try-complete-file-name
                     try-expand-all-abbrevs
                     try-expand-list
                     try-complete-lisp-symbol-partially
                     try-complete-lisp-symbol)))

(use-package company ; Graphical (auto-)completion
             :ensure t
             :defer t
             :idle (global-company-mode)
             :config
             (progn
               ;; Use Company for completion
               (bind-key [remap completion-at-point] #'company-complete company-mode-map)
               (setq company-tooltip-align-annotations t
                     ;; Easy navigation to candidates with M-<n>
                     company-show-numbers t))
             :diminish company-mode)
(use-package company-math ; Completion for Math symbols
             :ensure t
             :defer t
             :init
             ;; Add backend for math characters
             (with-eval-after-load 'company
               (add-to-list 'company-backends 'company-math-symbols-unicode)
               (add-to-list 'company-backends 'company-math-symbols-latex)))

;;; Spelling and syntax checking
(use-package ispell ; Spell checking
             :defer t
             :config
             (setq ispell-program-name "aspell"
                   ispell-extra-args '("--sug-mode=fast")
                   ispell-dictionary "en_US" ; Default dictionnary
                   ispell-silently-savep t ; Don't ask when saving the private dict
                                        ; ispell-choices-win-default-height 5
                   ))
(use-package flyspell ; On-the-fly spell checking
             :bind (("C-c j" . flyspell-check-previous-higlighted-word))
             :init
             (progn
               (dolist (hook '(text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode))
             :config
             (progn
               (setq flyspell-use-meta-tab nil
                     ;; Make Flyspell less chatty
                     flyspell-issue-welcome-flag nil
                     flyspell-issue-message-flag nil)
               ;; Free C-M-i for completion
               (define-key flyspell-mode-map "\M-\t" nil))
             :diminish flyspell-mode)

(add-hook 'text-mode-hook 'turn-on-flyspell)
(global-set-key (kbd "C-c j") 'flyspell-check-previous-highlighted-word)


(use-package flycheck ; On-the-fly syntax checking
             :ensure t
             :bind (("C-c l e" . list-flycheck-errors)
                    ("C-c T f" . flycheck-mode))
             :init (global-flycheck-mode)
             :config
             (progn
               (setq flycheck-completion-system 'ido)
               ;; Use italic face for checker name
               (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic))
             :diminish flycheck-mode)
(use-package flycheck-pos-tip ; Show Flycheck messages in popups
             :ensure t
             :defer t
             :init
             (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; Misc

(use-package gist)

(use-package smart-mode-line)

(use-package volatile-highlights)

;; Git
(use-package git-commit-mode)
(use-package git-rebase-mode)
(use-package git-commit-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)
(use-package gitattributes-mode)
(use-package magit)

;; Diff-hl
(use-package diff-hl)

;; LaTeX
(use-package auctex
             :ensure auctex
             :defer t
             :config
             (progn
               (setq TeX-parse-self t ; Parse documents to provide completion
                                        ; for packages, etc.
                     TeX-auto-save t ; Automatically save style information
                     TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
                     ;TeX-electric-math '("\\(" "\\)")
                     ;; Don't insert magic quotes right away.
                     ;TeX-quote-after-quote t
                     ;; Don't ask for confirmation when cleaning
                     TeX-clean-confirm nil
                     ;; Provide forward and inverse search with SyncTeX
                     TeX-source-correlate-mode t
                     TeX-source-correlate-method 'synctex
                     )
               (setq-default TeX-master nil ; Ask for the master file
                             TeX-PDF-mode t)))
(use-package tex-buf                    ; TeX buffer management
             :ensure auctex
             :defer t
             ;; Don't ask for confirmation when saving before processing
             :config (setq TeX-save-query nil))

(use-package tex-style                  ; TeX style
             :ensure auctex
             :defer t
             :config
             ;; Enable support for csquotes
             (setq LaTeX-csquotes-close-quote "}"
                   LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; TeX folding
             :ensure auctex
             :defer t
             :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                   ; TeX mode
             :ensure auctex
             :defer t
             :config
             (font-lock-add-keywords 'latex-mode
                                     `((,(rx "\\"
                                             symbol-start
                                             "fx" (1+ (or (syntax word) (syntax symbol)))
                                             symbol-end)
                                        . font-lock-warning-face))))
(use-package latex                      ; LaTeX editing
             :ensure auctex
             :defer t
             :config
             (progn
               ;; Teach TeX folding about KOMA script sections
               (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                                         (,(rx (0+ space) "\\subsection*{") 3)
                                         (,(rx (0+ space) "\\subsubsection*{") 4)
                                         (,(rx (0+ space) "\\minisec{") 5))
                     ;; No language-specific hyphens please
                     LaTeX-babel-hyphen nil)

               (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)))    ; Easy math input

(use-package bibtex                     ; BibTeX editing
             :defer t
             :config
             (progn
               ;; Run prog mode hooks for bibtex
               (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
               
               ;; Use a modern BibTeX dialect
               (bibtex-set-dialect 'biblatex)))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (progn
    ;; Plug into AUCTeX
    (setq reftex-plug-into-AUCTeX t
          ;; Automatically derive labels, and prompt for confirmation
          reftex-insert-label-flags '(t t)
          reftex-cite-prompt-optional-args t
          reftex-ref-style-default-list '("Default" "Cleveref" "Subcaption")
          reftex-label-alist
          ;; Additional label definitions for RefTeX.
          '(("definition" ?d "def:"  "~\\ref{%s}" t ("definition" "def."))
          ("lemma" ?l "lem:" "~\\ref{%s}" t ("lemma" "lem."))
          ("lemma*" ?l "lem:" "~\\ref{%s}" t ("lemma" "lem."))
          ("theorem" ?h "thm:" "~\\ref{%s}" t ("theorem" "th."))
          ("theorem*" ?h "thm:" "~\\ref{%s}" t ("theorem" "th."))
          ("observation" ?o "obs:" "~\\ref{%s}" t ("observation" "obs."))
          ("observation*" ?o "obs:" "~\\ref{%s}" t ("observation" "obs."))
          ("corollary" ?c "cor:" "~\\ref{%s}" t ("corollary" "cor."))
          ("corollary*" ?c "cor:" "~\\ref{%s}" t ("corollary" "cor."))
          ("algorithm" ?a "alg:" "~\\ref{%s}" t ("algorithm" "alg."))
          ("example" ?x "ex:" "~\\ref{%s}" t ("example" "ex."))
          ))

    ;; Provide basic RefTeX support for biblatex
    (unless (assq 'biblatex reftex-cite-format-builtin)
      (add-to-list 'reftex-cite-format-builtin
                   '(biblatex "The biblatex package"
                              ((?\C-m . "\\cite[][]{%l}")
                               (?c . "\\cite[][]{%l}")
                               (?C . "\\Cite[][]{%l}")
                               (?f . "\\footcite[][]{%l}")
                               (?p . "\\parencite[][]{%l}")
                               (?P . "\\Parencite[][]{%l}")
                               (?t . "\\textcite[][]{%l}")
                               (?T . "\\Textcite[][]{%l}")
                               (?a . "\\autocite[][]{%l}")
                               (?A . "\\Autocite[][]{%l}")
                               (?s . "\\smartcite[][]{%l}")
                               (?S . "\\Smartcite[][]{%l}")
                               (?n . "\\nocite{%l}")
                               (?b . "\\blockcquote[]{%l}{}")
                               (?F . "\\fullcite[]{%l}")
                               (?x . "[]{%l}")
                               (?X . "{%l}"))))
      (setq reftex-cite-format 'biblatex)))
  :diminish reftex-mode)


(add-hook 'TeX-language-de-hook
	  (lambda () (ispell-change-dictionary "de_CH")))
(add-hook 'TeX-language-en-hook
       (lambda () (ispell-change-dictionary "en_US")))
;; Python
(use-package python
             :mode ("\\.py\\'" . python-mode)
             :interpreter ("python" . python-mode))

;; Lua
(use-package lua
             :mode ("\\.lua\\'" . lua-mode)
             :interprete ("lua" . lua-mode))

(use-package markdown-mode
             :mode ("\\.md\\'" . markdown-mode))

(use-package csv-mode
             :mode "\\.csv\\'")















(set-default 'imenu-auto-rescan t)

;; Finding Files At Point
(require 'ffap)
;; (defvar ffap-c-commment-regexp "^/\\*+"
;;   "Matches an opening C-style comment, like \"/***\".")

;; (defadvice ffap-file-at-point (after avoid-c-comments activate)
;;   "Don't return paths like \"/******\" unless they actually exist.

;; This fixes the bug where ido would try to suggest a C-style
;; comment as a filename."
;;   (ignore-errors
;;     (when (and ad-return-value
;;                (string-match-p ffap-c-commment-regexp
;;                                ad-return-value)
;;                (not (ffap-file-exists-string ad-return-value)))
;;       (setq ad-return-value nil))))



;; mode line settings
(global-visual-line-mode 1); Proper line wrapping
;(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;(setq-default truncate-lines t)

(global-hl-line-mode 1); Highlight current row
;(global-linum-mode t) ; line numbers
(column-number-mode t)

; (global-show-newlines-mode t)
; (indicate-buffer-boundaries 'left)

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

(require 'diff-hl)
(global-diff-hl-mode)


;; use zenburn as the default theme


;; delete the selection with a keypress
(delete-selection-mode t)

;; show-paren-mode: subtle highlighting of matching parens (global-mode)
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; shows what changes when yanking, undoing
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)



(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; Automatically reverting buffers


(random t) ;; Seed the random-number generator

;; Save Session across Sessions
(desktop-save-mode 1)
(setq desktop-dirname my-savefile-dir)



;; Recent files mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
 

;; Backup Files
(setq make-backup-files t
      vc-make-backup-files t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 1
      version-control t
      backup-by-copying t
      ; backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      )

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart pairing for all
(electric-pair-mode t)

;; diminish keeps the modeline tidy
(require 'diminish)

;; Hippie expand: at times perhaps too hip
(eval-after-load 'hippie-exp
  '(progn
     (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
       (delete f hippie-expand-try-functions-list))
     
     ;; Add this back in at the end of the list.
     (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)))

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))

;; Calendar and Time settings
(setq calendar-week-start-day 1)
(setq european-calendar-style t)
(setq calendar-time-display-form
      '(24-hours ":" minutes
		 (if time-zone " (") time-zone (if time-zone ")")))
(setq calendar-time-zone +100
      calendar-standard-time-zone-name "CET"
      calendar-daylight-time-zone-name "CEST")


;; Key bindings
(progn

  ;; Completion that uses many different methods to find options.
  (global-set-key (kbd "M-/") 'hippie-expand)
  ;; Font size
  ;(define-key global-map (kbd "C-+") 'text-scale-increase)
  ;(define-key global-map (kbd "C--") 'text-scale-decrease)

  ;; Use regex searches by default.
  ;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  ;; (global-set-key (kbd "\C-r") 'isearch-backward-regexp)
  ;; (global-set-key (kbd "M-%") 'query-replace-regexp)
  ;; (global-set-key (kbd "C-M-s") 'isearch-forward)
  ;; (global-set-key (kbd "C-M-r") 'isearch-backward)
  ;; (global-set-key (kbd "C-M-%") 'query-replace)

  ;; Jump to a definition in the current file. (Protip: this is awesome.)
  (global-set-key (kbd "C-x C-i") 'imenu)

  ;; File finding
  (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

  ;; Window switching. (C-x o goes to the next window)
  (windmove-default-keybindings) ;; Shift+direction
  (global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
  (global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

  ;; Start eshell or switch to it if it's active.
  (global-set-key (kbd "C-x m") 'eshell)

  ;; Start a new eshell even if one is active.
  (global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

  ;; Help should search more than just commands
  (define-key 'help-command "a" 'apropos)
  )

;; EShell

(require 'em-hist)
(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-buffer-shorthand t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
(load "~/.emacs.d/eshell-customizations.el")


;; ;;;###autoload
(eval-after-load 'esh-opt
   '(progn
      (require 'em-prompt)
      (require 'em-term)
      (require 'em-cmpl)
      (setenv "PAGER" "cat")
;; ;     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
;;      (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
;;                '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
 
      (add-to-list 'eshell-visual-commands "ssh")
      (add-to-list 'eshell-visual-commands "tail")
      (add-to-list 'eshell-command-completions-alist
                   '("gunzip" "gz\\'"))
      (add-to-list 'eshell-command-completions-alist
                   '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))

;; ;;;###autoload
;; (when (not (functionp 'eshell/rgrep))
;;   (defun eshell/rgrep (&rest args)
;;     "Use Emacs grep facility instead of calling external grep."
;;     (eshell-grep "rgrep" args t)))

;; ;;;###autoload
;; (defun eshell/extract (file)
;;   (let ((command (some (lambda (x)
;;                          (if (string-match-p (car x) file)
;;                              (cadr x)))
;;                        '((".*\.tar.bz2" "tar xjf")
;;                          (".*\.tar.gz" "tar xzf")
;;                          (".*\.bz2" "bunzip2")
;;                          (".*\.rar" "unrar x")
;;                          (".*\.gz" "gunzip")
;;                          (".*\.tar" "tar xf")
;;                          (".*\.tbz2" "tar xjf")
;;                          (".*\.tgz" "tar xzf")
;;                          (".*\.zip" "unzip")
;;                          (".*\.Z" "uncompress")
;;                          (".*" "echo 'Could not extract the file:'")))))
;;     (eshell-command-result (concat command " " file))))

;; (defface esk-eshell-error-prompt-face
;;   '((((class color) (background dark)) (:foreground "red" :bold t))
;;     (((class color) (background light)) (:foreground "red" :bold t)))
;;   "Face for nonzero prompt results"
;;   :group 'eshell-prompt)

;; (add-hook 'eshell-after-prompt-hook
;;           (defun esk-eshell-exit-code-prompt-face ()
;;             (when (and eshell-last-command-status
;;                        (not (zerop eshell-last-command-status)))
;;               (let ((inhibit-read-only t))
;;                 (add-text-properties
;;                  (save-excursion (beginning-of-line) (point)) (point-max)
;;                  '(face esk-eshell-error-prompt-face))))))

;; (defun esk-eshell-in-dir (&optional prompt)
;;   "Change the directory of an existing eshell to the directory of the file in
;; the current buffer or launch a new eshell if one isn't running. If the
;; current buffer does not have a file (e.g., a *scratch* buffer) launch or raise
;; eshell, as appropriate. Given a prefix arg, prompt for the destination
;; directory."
;;   (interactive "P")
;;   (let* ((name (buffer-file-name))
;;          (dir (cond (prompt (read-directory-name "Directory: " nil nil t))
;;                     (name (file-name-directory name))
;;                     (t nil)))
;;          (buffers (delq nil (mapcar (lambda (buf)
;;                                       (with-current-buffer buf
;;                                         (when (eq 'eshell-mode major-mode)
;;                                           (buffer-name))))
;;                                     (buffer-list))))
;;          (buffer (cond ((eq 1 (length buffers)) (first buffers))
;;                        ((< 1 (length buffers)) (ido-completing-read
;;                                                 "Eshell buffer: " buffers nil t
;;                                                 nil nil (first buffers)))
;;                        (t (eshell)))))
;;     (with-current-buffer buffer
;;       (when dir
;;         (eshell/cd (list dir))
;;         (eshell-send-input))
;;       (end-of-buffer)
;;       (pop-to-buffer buffer))))


(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode); Enable Flyspell program mode for emacs lisp mode, which highlights all misspelled words in comments and strings.


;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")




;; load yasnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs my-snippets-dir)
;(yas-global-mode 1)

;; term-mode does not play well with yasnippet
(add-hook 'term-mode-hook (lambda ()
                            (yas-minor-mode -1)))

;; Dired
(require 'dired+)
;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)
;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-alhF")
;; Handle zip compression
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))
;; I use ibuffer for my C-x C-b binding instead of list-buffers. This
;; has the ability to group buffers. I tend to wind up with a lot of
;; dired buffers, so it's nice to group them together:
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
;(require 'dired-x)


;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq
 ;; use ido to look for branches
 magit-completing-read-function 'magit-ido-completing-read
 ;; don't put "origin-" in front of new branch names by default
 magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
 ;; open magit status in same window as current buffer
 ;magit-status-buffer-switch-function 'switch-to-buffer
 ;; highlight word/letter changes in hunk diffs
 magit-diff-refine-hunk t
 ;; ask me if I want to include a revision when rewriting
 magit-rewrite-inclusive 'ask
 ;; ask me to save buffers
 magit-save-some-buffers t
 ;; pop the process buffer if we're taking a while to complete
 magit-process-popup-time 10
 ;; ask me if I want a tracking upstream
 magit-set-upstream-on-push 'askifnotset
 )

;; clean up obsolete buffers automatically
(require 'midnight)

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" my-savefile-dir))

;; enable winner-mode to manage window configurations
(winner-mode +1)

;; IBuffer for buffer lsit
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

;; hi-lock-mode
(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy #'(lambda (dummy) t))
(defadvice hi-lock-set-pattern (around use-overlays activate)
  (let ((font-lock-fontified nil))
    ad-do-it))

;; Highlight Comment Annotations
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)
(add-hook 'text-mode-hook 'font-lock-comment-annotations)


(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map [(control tab)] 'outline-cycle)))

; use allout minor mode to have outlining everywhere.
(allout-mode)

;; Convenient printing
(require 'printing)
(pr-update-menus t)

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-save-query nil)

;; use pdflatex
(setq TeX-PDF-mode t)
(setq TeX-style-local "style/")
(defun my/LaTeX-mode-hook ()
  (local-set-key [remap next-error] nil)
  (local-set-key [remap previous-error] nil))

(add-hook 'LaTeX-mode-hook 'my/LaTeX-mode-hook)
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (define-key LaTeX-mode-map "\C-c!" 'TeX-next-error)))

(add-hook 'TeX-mode-hook 'flyspell-mode); Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.
(setq ispell-dictionary "american"); Default dictionary. To change do M-x ispell-change-dictionary RET.
(add-hook 'TeX-language-de-hook
	  (lambda () (ispell-change-dictionary "de_CH")))
(add-hook 'TeX-language-en-hook
       (lambda () (ispell-change-dictionary "american")))
; Flyspell-babel not working
; (autoload 'flyspell-babel-setup "flyspell-babel")
; (add-hook 'latex-mode-hook 'flyspell-babel-setup)
; (add-to-list 'flyspell-babel-command-alist ("hyphenquote" "hyphenquote"))
; (add-to-list 'flyspell-babel-command-alist ("foreignquote" "foreignquote"))
; (add-to-list 'flyspell-babel-environment-alist ("hyphenrules" "hyphenrules"))
;(setq LaTeX-babel-hyphen nil); Disable language-specific hyphen insertion.

(add-hook 'TeX-mode-hook 'font-lock-comment-annotations)
(add-hook 'TeX-mode-hook 'turn-on-auto-fill)

(add-hook 'TeX-mode-hook
      (lambda () (TeX-fold-mode 1))); Automatically activate TeX-fold-mode.

(add-hook 'text-mode-hook
      (lambda () (abbrev-mode 1)))

;; " expands into csquotes macros (for this to work babel must be loaded after csquotes).
(setq LaTeX-csquotes-close-quote "}"
      LaTeX-csquotes-open-quote "\\enquote{")

;; LaTeX-math-mode http://www.gnu.org/s/auctex/manual/auctex/Mathematics.html
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)

;;; RefTeX
;; Turn on RefTeX for AUCTeX http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
(add-hook 'TeX-mode-hook 'turn-on-reftex)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-add-environments
             '("definition" LaTeX-env-label)
             '("lemma" LaTeX-env-label)
             '("theorem" LaTeX-env-label)
             '("observation" LaTeX-env-label)
             '("corollary" LaTeX-env-label)
             '("algorithm" LaTeX-env-label) )
            (TeX-add-symbols
             '("cref" TeX-arg-ref)
             '("Cref" TeX-arg-ref)
             '("labelcref" TeX-arg-ref)
             '("labelcpageref" TeX-arg-ref)
             '("namecref" TeX-arg-ref)
             '("nameCref" TeX-arg-ref)
             '("namecrefs" TeX-arg-ref)
             '("nameCrefs" TeX-arg-ref)
             '("lcnamecref" TeX-arg-ref)
             '("lcnamecrefs" TeX-arg-ref)
             '("cpageref" TeX-arg-ref)
             '("Cpageref" TeX-arg-ref)
             '("subref" TeX-arg-ref))))

(eval-after-load
    "latex"
  '(TeX-add-style-hook
    "cleveref"
    (lambda ()
      (if (boundp 'reftex-ref-style-alist)
          (add-to-list
           'reftex-ref-style-alist
           '("Cleveref" "cleveref"
             (("\\cref" ?c)
              ("\\Cref" ?C)
              ("\\namecref" ?n)
              ("\\nameCref" ?N)
              ("\\labelcref" ?l)
              ("\\cpageref" ?d)
              ("\\Cpageref" ?D) )))))))

(eval-after-load
    "latex"
  '(TeX-add-style-hook
    "subcaption"
    (lambda ()
      (if (boundp 'reftex-ref-style-alist)
          (add-to-list
           'reftex-ref-style-alist
           '("Subcaption" "subcaption"
             (("\\subref" ?s))))
          (add-to-list
           'reftex-label-alist
           '("\subcaptionbox{}{}" ?f "fig:" "~\\ref{%s}" nil ("figure" "fig."))
           )
        ))))

             

(setq reftex-ref-style-default-list '("Default" "Cleveref" "Subcaption"))

(setq reftex-label-alist
        '(("definition" ?d "def:"  "~\\ref{%s}" t ("definition" "def."))
          ("lemma" ?l "lem:" "~\\ref{%s}" t ("lemma" "lem."))
          ("lemma*" ?l "lem:" "~\\ref{%s}" t ("lemma" "lem."))
          ("theorem" ?h "thm:" "~\\ref{%s}" t ("theorem" "th."))
          ("theorem*" ?h "thm:" "~\\ref{%s}" t ("theorem" "th."))
          ("observation" ?o "obs:" "~\\ref{%s}" t ("observation" "obs."))
          ("observation*" ?o "obs:" "~\\ref{%s}" t ("observation" "obs."))
          ("corollary" ?c "cor:" "~\\ref{%s}" t ("corollary" "cor."))
          ("corollary*" ?c "cor:" "~\\ref{%s}" t ("corollary" "cor."))
          ("algorithm" ?a "alg:" "~\\ref{%s}" t ("algorithm" "alg."))
          ))

;; RefTeX settings for biblatex
(eval-after-load 'reftex-vars; Is this construct really needed?
  '(progn
     (setq reftex-cite-format; Get ReTeX with biblatex, see http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
           '((?c . "\\cite[][]{%l}")
             (?C . "\\Cite[][]{%l}")
             (?f . "\\footcite[][]{%l}")
             (?p . "\\parencite[][]{%l}")
             (?P . "\\Parencite[][]{%l}")
             (?t . "\\textcite[][]{%l}")
             (?T . "\\Textcite[][]{%l}")
             (?a . "\\autocite[][]{%l}")
             (?A . "\\Autocite[][]{%l}")
             (?s . "\\smartcite[][]{%l}")
             (?S . "\\Smartcite[][]{%l}")
             (?n . "\\nocite{%l}")
             (?b . "\\blockcquote[]{%l}{}")))
     (setq reftex-cite-prompt-optional-args t)
     (setq reftex-plug-into-AUCTeX t)

     ;; Fontification (remove unnecessary entries as you notice them) http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00236.html http://www.gnu.org/software/auctex/manual/auctex/Fontification-of-macros.html
     (setq font-latex-match-reference-keywords
           '(
             ;; cleveref
             ("cref" "*{")
             ("Cref" "*{")
             ("namecref" "{")
             ("labelcref" "{")
             ("labelcpageref" "{")
             ("namecref" "{")
             ("nameCref" "{")
             ("namecrefs" "{")
             ("nameCrefs" "{")
             ("lcnamecref" "{")
             ("lcnamecrefs" "{")
             ("cpageref" "{")
             ("Cpageref" "{")
             ;; subcaption
             ("subref" "{")
             ;; biblatex
             ("printbibliography" "[{")
             ("addbibresource" "[{")
             ;; Standard commands
             ("cite" "[[{")
             ("Cite" "[[{")
             ("parencite" "[[{")
             ("Parencite" "[[{")
             ("footcite" "[[{")
             ("footcitetext" "[[{")
             ;; Style-specific commands
             ("textcite" "[[{")
             ("Textcite" "[[{")
             ("smartcite" "[[{")
             ("Smartcite" "[[{")
             ("cite*" "[[{")
             ("parencite*" "[[{")
             ("supercite" "{")
             ;; Qualified citation lists
             ("cites" "[{")
	     ("Cites" "[{")
	     ("parencites" "[{")
	     ("Parencites" "[{")
	     ("footcites" "[{")
	     ("footcitetexts" "[{")
	     ("smartcites" "[{")
	     ("Smartcites" "[{")
	     ("textcites" "[{")
	     ("Textcites" "[{")
	     ("supercites" "[{")
	     ;; Style-independent commands
	     ("autocite" "[[{")
	     ("Autocite" "[[{")
	     ("autocite*" "[[{")
	     ("Autocite*" "[[{")
	     ("autocites" "[{")
	     ("Autocites" "[{")
	     ;; Text commands
	     ("citeauthor" "[[{")
	     ("Citeauthor" "[[{")
	     ("citetitle" "[[{")
	     ("citetitle*" "[[{")
	     ("citeyear" "[[{")
	     ("citedate" "[[{")
	     ("citeurl" "[[{")
	     ;; Special commands
	     ("fullcite" "[[{")))

     (setq font-latex-match-textual-keywords
	   '(
	     ;; biblatex brackets
	     ("parentext" "{")
	     ("brackettext" "{")
	     ("hybridblockquote" "[{")
	     ;; Auxiliary Commands
	     ("textelp" "{")
	     ("textelp*" "{")
	     ("textins" "{")
	     ("textins*" "{")
	     ;; caption
	     ("subcaption" "[{")
	     ("subcaptionbox" "*[{[[")
	     ("captionof" "*[{{")))

     (setq font-latex-match-variable-keywords
	   '(
	     ;; amsmath
	     ("numberwithin" "{")
	     ;; enumitem
	     ("setlist" "[{")
	     ("setlist*" "[{")
	     ("newlist" "{")
	     ("renewlist" "{")
	     ("setlistdepth" "{")
	     ("restartlist" "{")))
     (setcdr (assoc 'caption reftex-default-context-regexps) "\\\\\\(rot\\|sub\\)?caption\\*?[[{]"); Recognize \subcaptions, e.g. reftex-citation
     ))


;; LaTeX Fill paragraph where each sentence starts on a new line.
(defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
  "Start each sentence on a new line."
  (let ((from (ad-get-arg 0))
        (to-marker (set-marker (make-marker) (ad-get-arg 1))))
    (while (< from (marker-position to-marker))
      (forward-sentence)
      ;; might have gone beyond to-marker---use whichever is smaller:
      (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
      ad-do-it
      (ad-set-arg 0 (setq from (point)))
      (unless (bolp)
        (LaTeX-newline)))))
(ad-activate 'LaTeX-fill-region-as-paragraph)

(setq bibtex-text-indentation 19)
(setq bibtex-contline-indentation 20)
(setq bibtex-align-at-equal-sign t)
(setq prelude-guru nil)

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq TeX-source-correlate-method 'synctex)

(setq TeX-view-program-selection '((output-pdf "Skim")))
(setq TeX-view-program-list
      '(("DVI Viewer" "open -a Skim %o")
        ("Preview" "open -a Preview %o")
	("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")
        ("HTML Viewer" "open %o")))

;; Enable TikZ Syntax highlightning
;(load "~/.emacs.d/auc-tikz-struct")

;; Maxima
(push "/usr/local/Cellar/maxima/5.28.0/share/maxima/5.28.0/emacs" load-path)
(autoload 'imaxima "imaxima" "Maxima frontend" t)
(autoload 'imath "imath" "Interactive Math mode" t)


(load custom-file)

(sml/setup)
(setq sml/theme 'dark)

;; Start Emacs Server
(use-package server ; The server of `emacsclient'
             :defer t
             :idle (server-start))


(provide 'init)
;;; init.el ends here
