;; My Emacs Settings

; Size of Frame
(add-to-list 'default-frame-alist '(height . 76))
(add-to-list 'default-frame-alist '(width . 115))

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar prelude-packages
  '(ace-jump-mode ack-and-a-half dash diminish elisp-slime-nav
    expand-region flycheck gist
    git-commit-mode gitconfig-mode gitignore-mode
    ido-ubiquitous dired+
    auctex
    magit melpa rainbow-mode
    smex volatile-highlights yasnippet zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  "Check if all packages in `prelude-packages' are installed."
  (every #'package-installed-p prelude-packages))

(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (mapc #'package-install
     (remove-if #'package-installed-p prelude-packages))))

(prelude-install-packages)

(defmacro prelude-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present. The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar prelude-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.php\\'" php-mode php-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (prelude-auto-install extension package mode))))
 prelude-auto-install-alist)

(defun prelude-ensure-module-deps (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'package-install (remove-if #'package-installed-p packages)))

(require 'thingatpt)
(require 'dash)


(defvar my-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(defvar my-snippets-dir (expand-file-name "snippets" user-emacs-directory)
  "This folder stores all the snippets for yasnippets.")
;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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

; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" my-savefile-dir))
;; activate it for all buffers
(setq-default save-place t)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" my-savefile-dir))
(savehist-mode +1)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)


;; Mac Path
(prelude-ensure-module-deps '(exec-path-from-shell))
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Mac key settings
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(setq x-select-enable-clipboard t)


;; nice scrolling
(setq scroll-margin 5)
;      scroll-conservatively 100000
;      scroll-preserve-screen-position 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
(setq standard-indent 2)


(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
;(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; ido-mode is like magic pixie dust!
(require 'ido-ubiquitous)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-save-directory-list-file (expand-file-name "ido.hist" my-savefile-dir)
      ido-max-prospects 10)
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)

;; smex, remember recently and most frequently used commands
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" my-savefile-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

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

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;; mode line settings
(global-visual-line-mode 1); Proper line wrapping
(global-hl-line-mode 1); Highlight current row
;(global-linum-mode t) ; line numbers
(column-number-mode t)

; (global-show-newlines-mode t)
; (indicate-buffer-boundaries 'left)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; use zenburn as the default theme
(load-theme 'zenburn t)

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

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=fast")) ; normal or fast or ultra

(add-hook 'text-mode-hook 'turn-on-flyspell)
(global-set-key (kbd "C-c j") 'flyspell-check-previous-highlighted-word)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; Automatically reverting buffers
(global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)

(random t) ;; Seed the random-number generator

;; Save Session across Sessions
(desktop-save-mode 1)
(setq desktop-dirname my-savefile-dir)

(setq-default fill-column 80)

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
      delete-old-versions t
      kept-new-versions 2
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


;; from emacs-starter-kid
(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'esk-add-watchwords)

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

  ;; This is a little hacky since VC doesn't support git add internally
  (eval-after-load 'vc
    (define-key vc-prefix-map "i"
      '(lambda () (interactive)
         (if (not (eq 'Git (vc-backend buffer-file-name)))
             (vc-register)
           (shell-command (format "git add %s" buffer-file-name))
           (message "Staged changes.")))))
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


;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" my-savefile-dir)
      bookmark-save-flag 1)

;; load yasnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs my-snippets-dir)
(yas-global-mode 1)

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

;; ediff 
(require 'ediff)
(setq diff-switches "-u")
; don't start another frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; (eval-after-load 'diff-mode
;;   '(progn
;;      (set-face-foreground 'diff-added "green4")
;;      (set-face-foreground 'diff-removed "red3")))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-add "green4")
;;      (set-face-foreground 'magit-diff-del "red3")))

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

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-save-query nil)

;; use pdflatex
(setq TeX-PDF-mode t)
(setq TeX-style-local "style/")
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (define-key LaTeX-mode-map "\C-c!" 'TeX-next-error)))

(add-hook 'TeX-mode-hook 'flyspell-mode); Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.
(setq ispell-dictionary "american"); Default dictionary. To change do M-x ispell-change-dictionary RET.
; (add-hook 'TeX-language-de-hook
;       (lambda () (ispell-change-dictionary "german")))
; (add-hook 'TeX-language-en-hook
;       (lambda () (ispell-change-dictionary "american")))
; Flyspell-babel not working
; (autoload 'flyspell-babel-setup "flyspell-babel")
; (add-hook 'latex-mode-hook 'flyspell-babel-setup)
; (add-to-list 'flyspell-babel-command-alist ("hyphenquote" "hyphenquote"))
; (add-to-list 'flyspell-babel-command-alist ("foreignquote" "foreignquote"))
; (add-to-list 'flyspell-babel-environment-alist ("hyphenrules" "hyphenrules"))
;(setq LaTeX-babel-hyphen nil); Disable language-specific hyphen insertion.

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

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
             '("namecref" TeX-arg-ref)
             '("nameCref" TeX-arg-ref)
             '("cpageref" TeX-arg-ref)
             '("Cpageref" TeX-arg-ref))))

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

(setq reftex-ref-style-default-list '("Default" "Cleveref"))

(setq reftex-label-alist
        '(("definition" ?d "def:"  "~\\ref{%s}" t ("definition" "def."))
          ("lemma" ?l "lem:" "~\\ref{%s}" t ("lemma" "lem."))
          ("theorem" ?h "thm:" "~\\ref{%s}" t ("theorem" "th."))
          ("observation" ?o "obs:" "~\\ref{%s}" t ("observation" "obs."))
          ("corollary" ?c "cor:" "~\\ref{%s}" t ("corollary" "cor."))
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
             ("namecref" "{")
             ("nameCref" "{")
             ("cpageref" "{")
             ("Cpageref" "{")
             ;; subcaption
             ("subref" "{")
             ;; biblatex
             ("printbibliography" "[{")
             ("addbibresource" "[{")
             ;; Standard commands
             ;; ("cite" "[{")
             ("Cite" "[{")
             ("parencite" "[{")
             ("Parencite" "[{")
             ("footcite" "[{")
             ("footcitetext" "[{")
             ;; Style-specific commands
             ("textcite" "[{")
             ("Textcite" "[{")
             ("smartcite" "[{")
             ("Smartcite" "[{")
             ("cite*" "[{")
             ("parencite*" "[{")
             ("supercite" "[{")
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
	     ("autocite" "[{")
	     ("Autocite" "[{")
	     ("autocite*" "[{")
	     ("Autocite*" "[{")
	     ("autocites" "[{")
	     ("Autocites" "[{")
	     ;; Text commands
	     ("citeauthor" "[{")
	     ("Citeauthor" "[{")
	     ("citetitle" "[{")
	     ("citetitle*" "[{")
	     ("citeyear" "[{")
	     ("citedate" "[{")
	     ("citeurl" "[{")
	     ;; Special commands
	     ("fullcite" "[{")))

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
	     ("subcaptionbox" "*[{")
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

;; Start Emacs Server
(server-start)

(provide 'init)
;;; init.el ends here
