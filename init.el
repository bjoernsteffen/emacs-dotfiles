;; My Emacs Settings

; Size of Frame
(add-to-list 'default-frame-alist '(height . 90))
(add-to-list 'default-frame-alist '(width . 120))
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
      mouse-yank-at-point t)

;; Backup Files
(setq make-backup-files t
      vc-make-backup-files t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 1
      version-control t
      backup-by-copying t
      )

;(set-face-attribute 'default nil :font "Monaco-12")

;; Please don't load outdated byte code
(setq load-prefer-newer t)


(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(require 'subr-x)
(require 'rx)
(require 'time-date)

(setq inhibit-default-init t)
(fset 'yes-or-no-p #'y-or-n-p)
(fset 'display-startup-echo-area-message #'ignore)

(use-package diminish :ensure diminish)

;; savehist keeps track of some history
(setq history-length 1000) ; Store more history
(use-package savehist ; Save minibuffer history
             :init (savehist-mode t)
             :config (setq savehist-save-minibuffer-history t
                           savehist-autosave-interval 180
                           savehist-additional-variables '(search ring regexp-search-ring)))


(use-package dynamic-fonts              ; Select best available font
  :ensure t
  :init
  (progn
    (setq dynamic-fonts-preferred-monospace-fonts
          '(
            ;; Best fonts
            "Source Code Pro"   ; https://github.com/adobe-fonts/source-code-pro
            "Anonymous Pro" ; http://www.marksimonson.com/fonts/view/anonymous-pro
            ;; Consolas and its free alternative.  Ok, but not my preference
            "Inconsolata"
            "Consolas"
            ;; Also still kind of ok
            "Fira Mono"
            ;; System fonts, as last resort
            "Menlo"
            "DejaVu Sans Mono"
            "Bitstream Vera Mono"
            "Courier New")
          dynamic-fonts-preferred-monospace-point-size (pcase system-type
                                                         (`darwin 13)
                                                         (_ 10))
          dynamic-fonts-preferred-proportional-fonts
          '(
            ;; Best, from
            ;; https://www.mozilla.org/en-US/styleguide/products/firefox-os/typeface/
            "Fira Sans"
            ;; System fonts, as last resort
            "Helvetica"
            "Segoe UI"
            "DejaVu Sans"
            "Bitstream Vera"
            "Tahoma"
            "Verdana"
            "Arial Unicode MS"
            "Arial")
          dynamic-fonts-preferred-proportional-point-size (pcase system-type
                                                            (`darwin 13)
                                                            (_ 10)))

    (dynamic-fonts-setup)))

(use-package unicode-fonts              ; Map Unicode blocks to fonts
  :ensure t
  :init (unicode-fonts-setup))

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
               (exec-path-from-shell-initialize)))

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
                   select-enable-clipboard t))


;; CUA selection mode for tables
(cua-selection-mode t)
(setq cua-auto-tabify-rectangles nil)

; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil) 
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; nice scrolling
(setq scroll-margin 5)
(setq scroll-conservatively 100000)
(setq mouse-wheel-progressive-speed nil)
(setq save-interprogram-paste-before-kill t)
;      scroll-preserve-screen-position 1)

(setq standard-indent 2)

;;; The mode line

(setq-default ;header-line-format
              ;'(which-func-mode ("" which-func-format " "))
              mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Some specific information about the current buffer: Indicate
                ;; the presence of structured editing, with Paredit or SHM
                (paredit-mode (:propertize " ()" face bold))
                (structured-haskell-mode (:propertize shm-lighter face bold))
                (structured-haskell-repl-mode (:propertize shm-lighter
                                                           face bold))
                (projectile-mode projectile-mode-line)
                (vc-mode vc-mode)
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (anzu-mode (:eval                  ; isearch pos/matches
                            (when (> anzu--total-matched 0)
                              (concat " " (anzu--update-mode-line)))))
                (multiple-cursors-mode mc/mode-line) ; Number of cursors
                ;; And the modes, which we don't really care for anyway
                " " mode-line-misc-info mode-line-modes mode-line-end-spaces)
              mode-line-remote
              '(:eval
                (when-let (host (file-remote-p default-directory 'host))
                  (propertize (concat "@" host) 'face
                              '(italic warning))))
              ;; Remove which func from the mode line, since we have it in the
              ;; header line
              mode-line-misc-info
              (assq-delete-all 'which-func-mode mode-line-misc-info))

;; Standard stuff
(line-number-mode)
(column-number-mode)

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :init (global-anzu-mode)
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

(use-package which-func                 ; Current function name in header line
  :defer t
  :config
  (setq ;which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))

;; Zenburn Theme
(use-package zenburn-theme
             :ensure t
             :init (load-theme 'zenburn t))

;; Ido
(use-package ido
             :init (progn
                     (ido-mode)
                     (ido-everywhere))
             :config
             (setq ido-enable-flex-matching t ; Match characters if string doesn't match
                   ido-everywhere t
                   ido-enable-tramp-completion t
                   ido-ignore-extensions t
                   ido-create-new-buffer 'always ; Create a new buffer if nothing matches
                   ido-use-filename-at-point 'guess
                   ido-file-extensions-order '(".tex" ".txt" ".py" ".el")
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
;(use-package ido-vertical-mode ; Vertical interface for IDO
;             :ensure t
;             :init (ido-vertical-mode))
(use-package smex ; Better M-x
             :ensure t
             :bind (([remap execute-extended-command] . smex)
                    ("M-X" . smex-major-mode-commands)))


(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

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
;; (use-package windmove ; Move between windows with Shift+Arrow
;;              :bind (( "S-<left>" . windmove-left)
;;                     ( "S-<right>" . windmove-right)
;;                     ( "S-<up>" . windmove-up)
;;                     ( "S-<down>" . windmove-down)))
(use-package winner                     ; Undo and redo window configurations
  :init (winner-mode))

(use-package ace-window
  :ensure t
  :config (setq aw-background nil)
  :bind (("H-w" . ace-window)))

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
;; I use ibuffer for my C-x C-b binding instead of list-buffers. This
;; has the ability to group buffers. I tend to wind up with a lot of
;; dired buffers, so it's nice to group them together:
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; Ranger
;; (use-package ranger
;;   :ensure t
;;   :config
;;   (progn 
;;   (setq ranger-cleanup-on-disable t)
;;   (setq ranger-width-parents 0.2)
;;   ))

(use-package ignoramus ; Ignore uninteresting files everywhere
             :ensure t
             :defer t
             :config (ignoramus-setup))

(use-package hardhat
  :ensure hardhat
  :config (progn (setq hardhat-mode-lighter nil)
		 (global-hardhat-mode 1)))

(use-package bookmark ; Bookmarks for Emacs buffers
             :bind (("H-b l" . list-bookmarks))
             ;; Save bookmarks immediately after a bookmark was added
             :config (setq bookmark-save-flag 1))

;; Visible Bookmarks
(use-package bm
  :ensure t
  :config (setq bm-highlight-style 'bm-highlight-only-fringe
                bm-marker 'bm-marker-right)
  :bind (("<right-fringe> <mouse-1>" . bm-toggle-mouse))
)

(use-package recentf ; Save recently visited files
             :config
	     (progn
             (setq recentf-max-saved-items 100
                   recentf-max-menu-items 5
                   ;; Cleanup recent files only when Emacs is idle, but not when the mode
                   ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
                   ;; idles often enough to have the recent files list clean up regularly
                   recentf-auto-cleanup 300
                   recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                                         "/elpa/.*\\'" ; Package files
                                         ;; And all other kinds of boring files
                                         #'ignoramus-boring-p))
	     (recentf-mode 1)))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

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
             :config (global-launch-mode))

(setq-default indent-tabs-mode nil)
;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)
(setq require-final-newline t
      indicate-empty-lines t)

(setq-default indicate-buffer-boundaries 'left)
;(set-default imenu-auto-rescan t)



(use-package electric                   ; Electric code layout
  :init (electric-layout-mode))

(use-package elec-pair                  ; Electric pairs
  :init (electric-pair-mode))


(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

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
             :disabled t
             :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
             ;; Keep the fringe
             :config (setq visual-fill-column-disable-fringe nil))

(use-package expand-region ; Expand region by semantic units
             :ensure t
             :bind (("C-=" . er/expand-region)))

(use-package easy-kill                  ; Easy killing and marking on C-w
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(bind-key [remap just-one-space] #'cycle-spacing)

(use-package outline ; Navigate outlines in buffers
             :defer t
             :init (dolist (hook '(text-mode-hook prog-mode-hook))
                     (add-hook hook #'outline-minor-mode))
             :diminish outline-minor-mode)

(use-package hl-line ; Highlight the current line
             :init (global-hl-line-mode 1))

(use-package paren ; Highlight paired delimiters
             :init (show-paren-mode 1)
             :config (setq show-paren-when-point-inside-paren t
                           show-paren-when-point-in-periphery t
                           show-paren-style 'parenthesis))
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
                     try-expand-line
                     try-complete-lisp-symbol-partially
                     try-complete-lisp-symbol)))

(use-package company ; Graphical (auto-)completion
             :ensure t
             :config
             (progn
               ;; Use Company for completion
               (bind-key [remap completion-at-point] #'company-complete company-mode-map)
               (setq company-tooltip-align-annotations t
                     ;; Easy navigation to candidates with M-<n>
                     company-show-numbers t)
	       (global-company-mode))
             :diminish company-mode)
(use-package company-math ; Completion for Math symbols
             :ensure t
             :defer t
             :init
             ;; Add backend for math characters
             (with-eval-after-load 'company
               (add-to-list 'company-backends 'company-math-symbols-unicode)
               (add-to-list 'company-backends 'company-math-symbols-latex)))


(use-package prog-mode
  :defer t
  :config
  (progn
    (use-package eldoc
      :diminish ""
      :defer t
      :init (add-hook 'prog-mode-hook 'eldoc-mode))
    (use-package hl-todo
      :ensure hl-todo
      :defer t
      :init (add-hook 'prog-mode-hook 'hl-todo-mode))
    (use-package hl-sexp
      :ensure hl-sexp
      :defer t
      :init
      (add-hook 'prog-mode-hook 'hl-sexp-mode))))

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
             :init
             (progn
               (dolist (hook '(text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode)
               (bind-key "H-." 'flyspell-check-previous-higlighted-word))
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

;; ** Syntax checking
(use-package flycheck
  :ensure flycheck
  :defer t
  :diminish ""
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (progn
    (setq flycheck-completion-system 'ido)
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (defvar flycheck-mode-line-lighter " *")
    (use-package flycheck-color-mode-line
      :ensure flycheck-color-mode-line
      :init (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))))

;; (use-package flycheck ; On-the-fly syntax checking
;;              :ensure t
;;              :bind (("C-c l e" . list-flycheck-errors)
;;                     ("C-c T f" . flycheck-mode))
;;              :init (global-flycheck-mode)
;;              :config
;;              (progn
;;                (setq flycheck-completion-system 'ido)
;;                ;; Use italic face for checker name
;;                (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic))
;;              :diminish flycheck-mode)
(use-package flycheck-pos-tip ; Show Flycheck messages in popups
             :ensure t
             :defer t
             :init
             (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; Misc

(use-package gist
  :ensure t)

(use-package volatile-highlights
  :ensure t)

;; LaTeX with AUCTeX
(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex
             :ensure auctex
             :defer t
             :bind (("M-g l" . TeX-error-overview))
             :config
             (progn
               (setq TeX-parse-self t ; Parse documents to provide completion
                                        ; for packages, etc.
                     TeX-auto-save t ; Automatically save style information
                     TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
                     TeX-electric-math (cons "\\(" "\\)")
                     ;; Don't insert magic quotes right away.
                     ;TeX-quote-after-quote t
                     ;; Don't ask for confirmation when cleaning
                     TeX-clean-confirm nil
                     ;; Provide forward and inverse search with SyncTeX
                     TeX-source-correlate-mode t
                     TeX-source-correlate-method 'synctex
                     TeX-source-correlate-start-server t
                     TeX-view-program-selection '((output-pdf "Skim") (output-dvi "DVI Viewer"))
                     TeX-view-program-list
                     '(("DVI Viewer" "open -a Skim %o")
                       ("Preview" "open -a Preview %o")
                       ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r -g %n %o %b")
                       ("HTML Viewer" "open %o")))
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
                '("subref" TeX-arg-ref))

               (setq font-latex-match-textual-keywords
                     '(
                       ;; biblatex brackets
                       ("parentext" "{")
                       ("brackettext" "{")
                       ("hybridblockquote" "[{")
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
               ;(setcdr (assoc 'caption reftex-default-context-regexps) "\\\\\\(rot\\|sub\\)?caption\\*?[[{]"); Recognize \subcaptions, e.g. reftex-citation
               (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)))    ; Easy math input

(use-package bibtex                     ; BibTeX editing
             :defer t
             :config
             (progn
               ;; Run prog mode hooks for bibtex
               (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

               ;; Use a modern BibTeX dialect
               (bibtex-set-dialect 'biblatex)
               (setq bibtex-text-indentation 19
                     bibtex-contline-indentation 20
                     bibtex-align-at-equal-sign t)
               ))

;; (use-package auctex-skim                ; Skim as viewer for AUCTeX
;;   :load-path "lisp/"
;;   :commands (auctex-skim-select)
;;   :init (with-eval-after-load 'tex
;;           (auctex-skim-select)))

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
          ("\subcaptionbox{}{}" ?f "fig:" "~\\ref{%s}" 1 ("figure" "fig."))
          )
          )
    (add-to-list
           'reftex-ref-style-alist
           '("Subcaption" "subcaption"
             (("\\subref" ?s))))
    (add-to-list
           'reftex-ref-style-alist
           '("Cleveref" "cleveref"
             (("\\cref" ?c)
              ("\\Cref" ?C)
              ("\\namecref" ?n)
              ("\\nameCref" ?N)
              ("\\labelcref" ?l)
              ("\\cpageref" ?d)
              ("\\Cpageref" ?D))))
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

    ;; Provide basic RefTeX support for biblatex
    (unless (assq 'biblatex reftex-cite-format-builtin)
      (add-to-list 'reftex-cite-format-builtin
                   '(biblatex "The biblatex package"
                              ((?\C-m . "\\cite[]{%l}")
                               (?c . "\\cite[]{%l}")
                               (?C . "\\Cite[]{%l}")
                               (?f . "\\footcite[]{%l}")
                               (?p . "\\parencite[]{%l}")
                               (?P . "\\Parencite[]{%l}")
                               (?t . "\\textcite[]{%l}")
                               (?T . "\\Textcite[]{%l}")
                               (?a . "\\autocite[]{%l}")
                               (?A . "\\Autocite[]{%l}")
                               (?s . "\\smartcite[]{%l}")
                               (?S . "\\Smartcite[]{%l}")
                               (?n . "\\nocite{%l}")
                               (?b . "\\blockcquote[]{%l}{}")
                               (?F . "\\fullcite[]{%l}"))))
      (setq reftex-cite-format 'biblatex)))
  :diminish reftex-mode)


(add-hook 'TeX-language-de-hook
          (lambda () (ispell-change-dictionary "de_CH")))
(add-hook 'TeX-language-en-hook
          (lambda () (ispell-change-dictionary "en_US")))

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

(use-package company-auctex
  :ensure t
  :config (company-auctex-init))

;;; Programming utilities
(use-package compile                    ; Compile from Emacs
  :config
  (progn
    (setq compilation-ask-about-save nil ; Just save before compiling
          compilation-always-kill t     ; Just kill old compile processes before
                                        ; starting the new one
          compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
          )))

(use-package iedit
  :ensure iedit
  :bind ("C-;" . iedit-mode))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init (progn
          (add-hook 'prog-mode-hook #'highlight-numbers-mode)
          (add-hook 'text-mode-hook #'highlight-numbers-mode)
          (add-hook 'LaTeX-mode #'highlight-numbers-mode)))

(use-package highlight-symbol
  :ensure highlight-symbol
  :defer t
  :diminish ""
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-symbol-mode)
    (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode))
  :config
  (progn
    (setq highlight-symbol-on-navigation-p nil
          highlight-symbol-idle-delay 1)
    (bind-key "C-%" 'highlight-symbol-query-replace highlight-symbol-nav-mode-map)))

(use-package paredit                    ; Balanced sexp editing
  :ensure t
  :defer t
  :disabled t
  :init
  (progn
    (dolist (hook '(eval-expression-minibuffer-setup-hook
                    emacs-lisp-mode-hook
                    inferior-emacs-lisp-mode-hook
                    clojure-mode-hook))
      (add-hook hook #'paredit-mode)))
  :config
  (progn
    ;; Free M-s.  There are some useful bindings in that prefix map.
    (define-key paredit-mode-map (kbd "M-s") nil)
    (define-key paredit-mode-map (kbd "M-S-<up>") #'paredit-splice-sexp))
  :diminish paredit-mode)

(use-package lisp-mode                  ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :config
  (progn
    (require 'ert)))

;; Python
(use-package python
  :defer t
  :config
  (progn
    ;; PEP 8 compliant filling rules, 79 chars maximum
    (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
    (add-hook 'python-mode-hook #'subword-mode)

    ;; Use a decent syntax and style checker
    (setq python-check-command "pylint"
          ;; Use IPython as interpreter
          python-shell-interpreter "python"
          python-shell-interpreter-args "-i")))
(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode))
(use-package company-anaconda           ; Python backend for Company
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-anaconda)))

;; Cucumber
(use-package feature-mode               ; Feature files for ecukes/cucumber
  :ensure t
  :defer t
  :config
  (progn
    ;; Add standard hooks for Feature Mode, since it is no derived mode
    (add-hook 'feature-mode-hook #'whitespace-mode)
    (add-hook 'feature-mode-hook #'whitespace-cleanup-mode)
    (add-hook 'feature-mode-hook #'flyspell-mode)))

;; Lua
(use-package lua
             :mode ("\\.lua\\'" . lua-mode)
             :interpreter ("lua" . lua-mode))

;; Markdown
(use-package markdown-mode
             :ensure t
             :mode ("\\.md\\'" . markdown-mode))

(use-package csv-mode
             :mode "\\.csv\\'")

;;; Version control
(use-package vc-hooks                   ; Simple version control
  :defer t
  ;; Always follow symlinks to files in VCS repos
  :config (setq vc-follow-symlinks t))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure diff-hl
  :defer t
  :init
  (progn
    ;; Highlight changes to the current file in the fringe
    (global-diff-hl-mode)
    ;; Highlight changed files in the fringe of Dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

    ;; Fall back to the display margin, if the fringe is unavailable
    (unless (display-graphic-p)
      (diff-hl-margin-mode))))

;; Magit
(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("H-g"   . magit-status))
  :config
  (progn
    (setq ;; ask me to save buffers
          magit-save-some-buffers t
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          ;; Except when you ask something useful…
          magit-set-upstream-on-push t
          ;; don't put "origin-" in front of new branch names by default
          magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
          ;; Use IDO for completion
          magit-completing-read-function #'magit-ido-completing-read
          ;; highlight word/letter changes in hunk diffs
          magit-diff-refine-hunk t
          ;; ask me if I want to include a revision when rewriting
          magit-rewrite-inclusive 'ask)
    ))

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :ensure t)

(use-package org
  :ensure t)

;; Calendar and Time settings
(use-package calendar                   ; Built-in calendar
  :config
  (setq calendar-week-start-day 1
        european-calendar-style t
        calendar-time-display-form
        '(24-hours ":" minutes
                   (if time-zone " (") time-zone (if time-zone ")"))
        calendar-time-zone +100
        calendar-standard-time-zone-name "CET"
        calendar-daylight-time-zone-name "CEST"
        ))


(bind-key "H-k" #'describe-personal-keybindings)

(use-package visual-line
  :init (global-visual-line-mode 1)
  :config (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(column-number-mode t)

(use-package delsel ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

;; shows what changes when yanking, undoing
(use-package volatile-highlights
  :ensure t
  :init (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package printing
             :config (pr-update-menus t))


(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; Key bindings
(progn
  ;; Help should search more than just commands
  (define-key 'help-command "a" 'apropos)
  )

;; EShell
(use-package eshell
  :bind ("H-e" . eshell)
  :config
  (progn
    ;; Scrolling
    (setq eshell-scroll-to-bottom-on-output t
          eshell-scroll-show-maximum-output t
          eshell-save-history-on-exit t
          eshell-buffer-shorthand t)
    (use-package esh-mode
      :defer t
      :config
      (progn
        (defun eshell/cds ()
          (eshell/cd (or (locate-dominating-file default-directory "src")
                         (locate-dominating-file default-directory ".git"))))
        (defun eshell/clear ()
          (interactive)
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point-max)))
          (eshell-send-input))
        (add-hook 'eshell-mode-hook
                  #'(lambda ()
                      (bind-key "C-l" 'eshell/clear eshell-mode-map)))))
    (use-package eshell-opt
      :config
      (use-package eshell-prompt-extras
        :ensure eshell-prompt-extras))
    (use-package em-term
      :defer t
      :config
      (setq eshell-visual-commands
            (append '("tmux" "screen" "ssh") eshell-visual-commands)))
    (use-package em-hist
      :defer t
      :config
      (setq eshell-hist-ignoredups t)))
  (use-package em-smart
    :config (progn
              (setq eshell-where-to-jump 'begin)
              (setq eshell-review-quick-commands nil)
              (setq eshell-smart-space-goes-to-end t))))

(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode); Enable Flyspell program mode for emacs lisp mode, which highlights all misspelled words in comments and strings.


(use-package smart-mode-line
  :ensure t
  :config (progn
            (sml/setup)
            (sml/apply-theme 'respectful)))

;; Start Emacs Server
(use-package server ; The server of `emacsclient'
             :defer t
             :config (server-start))

;;; init.el ends here
