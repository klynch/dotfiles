(require 'cl)
(defvar *emacs-load-start* (current-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In case we ever need to change the emacs directory location
(defvar dotemacs-dir "~/.emacs.d/")

;; We don't want the custom file cluttering our beautiful .emacs file
(setq custom-file (concat dotemacs-dir "emacs-custom.el"))
(load custom-file 'noerror)

(defun add-subdirs-to-load-path (dir)
  "Adds the subdirectories to the list if possible."
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (let* ((my-lisp-dir dir)
             (default-directory my-lisp-dir))
        (add-to-list 'load-path my-lisp-dir)
        (normal-top-level-add-subdirs-to-load-path))))

;; Where all of our custom extensions and what not go
(defvar site-lisp-dir (concat dotemacs-dir "site-lisp"))
(add-subdirs-to-load-path site-lisp-dir)

;; Let's not share our private stuff
(load "~/.emacs-private.el" 'noerror)

(if (fboundp 'image-load-path) nil (setq image-load-path '()))
(add-to-list 'image-load-path (concat dotemacs-dir "images/"))

;; We want to uniquify names instead of foo<1> foo<2>
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;; TODO reduce color theme to only the themes i want
(require 'color-theme)
(color-theme-tty-dark)

;; (color-theme-initialize)
;; (if window-system
;;     (color-theme-tango))
;; (if (not (window-system))
;;     (color-theme-tty-dark))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless window-system
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

;;TODO resize window based on screen size
;; http://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window

;; GROWL or its alternatives. must set directory first
(autoload 'todochiku-message "todochiku")
(autoload 'todochiku-icon "todochiku")
(autoload 'todochiku-in "todochiku" nil t)

;;TODO try setvar... see if its overrided?
;; (custom-set-variables
;;  '(todochiku-icons-directory (concat dotemacs-dir "todochiku-icons"))
;;  '(todochiku-message-too t)
;; )

(defvar todochiku-icons-directory (concat dotemacs-dir "todochiku-icons"))
(defvar todochiku-message-too t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(if (eq system-type 'darwin)
    (setq
     default-input-method "MacOSX"
     mac-command-modifier 'meta
     mac-option-modifier nil
     ns-pop-up-frames nil))

(autoload 'twit-show-recent-tweets              "twit" nil t)
(autoload 'twit-show-at-tweets                  "twit" nil t)
(autoload 'twit-show-friends                    "twit" nil t)
(autoload 'twit-show-followers                  "twit" nil t)
(autoload 'twit-follow-recent-tweets            "twit" nil t)
(autoload 'twit-post                            "twit" nil t)
(autoload 'twit-post-region                     "twit" nil t)
(autoload 'twit-post-buffer                     "twit" nil t)
(autoload 'twit-direct                          "twit" nil t)
(autoload 'twit-add-favorite                    "twit" nil t)
(autoload 'twit-remove-favorite                 "twit" nil t)
(autoload 'twit-add-friend                      "twit" nil t)
(autoload 'twit-remove-friend                   "twit" nil t)
(autoload 'twit-show-direct-tweets-with-account "twit" nil t)
(autoload 'twit-show-at-tweets-with-account     "twit" nil t)
(eval-after-load "twit"
  '(if window-system (setq twit-show-user-images t)))

(global-set-key "\C-cTT"  'twit-follow-recent-tweets) ; (s)how (T)weets
(global-set-key "\C-cTst" 'twit-show-recent-tweets)   ; (s)how (t)weets
(global-set-key "\C-cTsa" 'twit-show-at-tweets)       ; (s)how (a)t
(global-set-key "\C-cTsf" 'twit-show-friends)         ; (s)how (f)riends
(global-set-key "\C-cTsl" 'twit-show-followers)       ; (s)how fo(l)lowers
(global-set-key "\C-cTpp" 'twit-post)		              ; (p)ost
(global-set-key "\C-cTpr" 'twit-post-region)	        ; (p)ost (r)egion
(global-set-key "\C-cTpb" 'twit-post-buffer)	        ; (p)ost (b)uffer
(global-set-key "\C-cTpr" 'twit-direct)		            ; (p)ost (d)irect
(global-set-key "\C-cTfa" 'twit-add-favorite)	        ; (f)avorite (a)dd
(global-set-key "\C-cTfr" 'twit-remove-favorite)      ; (f)avorite (r)emove

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (if (file-newer-than-file-p "~/.emacs" "~/.emacs.elc")
      (byte-compile-file "~/.emacs"))
  (if (file-newer-than-file-p "~/.emacs-private.el" "~/.emacs-private.elc")
      (byte-compile-file "~/.emacs-private.el"))
  (byte-recompile-directory site-lisp-dir 0))

(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
   Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair))
    (self-insert-command 1)))

(defun indent-page ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (indent-region (mark) (point) nil)))

(autoload 'ascii-table         "ascii-table" nil t)
(autoload 'ascii-table-decimal "ascii-table" nil t)
(autoload 'ascii-table-octal   "ascii-table" nil t)
(autoload 'ascii-table-hex     "ascii-table" nil t)

(autoload 'count-words-region "count-words" nil t)
(autoload 'count-words-page   "count-words" nil t)

;;TODO add this to a mode that can be enabled / disabled
(autoload 'nuke-trailing-whitespace "whitespace" nil t)
(add-hook 'write-file-functions 'nuke-trailing-whitespace)

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defvar term-cmd (getenv "SHELL"))
(defvar term-cmd "/opt/local/bin/zsh")
(autoload 'term-check-proc "term")
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key Mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar k-minor-mode-map (make-keymap) "k-minor-mode keymap.")

;; Make the `Delete' key delete the char under the cursor
(define-key k-minor-mode-map [delete]        'delete-char)
(define-key k-minor-mode-map [kp-delete]     'delete-char)
(define-key k-minor-mode-map [insert]        'yank)
(define-key k-minor-mode-map [C-delete]      'undo)

;; Makes home/end go to beginning/end of line
(define-key k-minor-mode-map [end] 'end-of-line)
(define-key k-minor-mode-map [home] 'beginning-of-line)

;; Makes control + home/end go to beginning/end of buffer
(define-key k-minor-mode-map [C-end] 'end-of-buffer)
(define-key k-minor-mode-map [C-home] 'beginning-of-buffer)

;; Pager stuff
(autoload 'pager-row-up    "pager" nil t)
(autoload 'pager-page-up   "pager" nil t)
(autoload 'pager-row-down  "pager" nil t)
(autoload 'pager-page-down "pager" nil t)
(define-key k-minor-mode-map [M-up]          'pager-row-up)
(define-key k-minor-mode-map [M-down]        'pager-row-down)
(define-key k-minor-mode-map [prior]         'pager-page-up)
(define-key k-minor-mode-map [next]          'pager-page-down)
(define-key k-minor-mode-map [M-kp-8]        'pager-row-up)
(define-key k-minor-mode-map [M-kp-2]        'pager-row-down)
(define-key k-minor-mode-map (kbd "\M-v") 	 'pager-page-up)
(define-key k-minor-mode-map (kbd "\C-v")	   'pager-page-down)

(define-key k-minor-mode-map [f1]            'man-follow)
(define-key k-minor-mode-map [f2]            'visit-ansi-term)
(define-key k-minor-mode-map [f3]            nil)
(define-key k-minor-mode-map [f4]            nil)
(define-key k-minor-mode-map [f5]            'toggle-truncate-lines)
(define-key k-minor-mode-map [f6]            'nuke-trailing-whitespace)
(define-key k-minor-mode-map [f7]            'indent-page)

;; cycle through buffers
(define-key k-minor-mode-map [C-tab]         'bs-cycle-next)
(define-key k-minor-mode-map [S-tab]         'bs-cycle-previous)

;; window stuff
(define-key k-minor-mode-map [C-kp-enter]    'other-window)
(define-key k-minor-mode-map [C-kp-add]      'enlarge-window)
(define-key k-minor-mode-map [C-kp-subtract] 'shrink-window)

(define-key k-minor-mode-map (kbd "\C-x f")  'make-frame)
(define-key k-minor-mode-map (kbd "\C-x g")  'delete-frame)

(define-key k-minor-mode-map (kbd "\C-x p")  'align-regexp)

(define-key k-minor-mode-map (kbd "\C-c c")  'compile)
(define-key k-minor-mode-map (kbd "\C-c g")  'goto-line)
(define-key k-minor-mode-map (kbd "\C-c q")  'comment-or-uncomment-region)

(define-key k-minor-mode-map (kbd "\C-x t")  'toggle-truncate-lines)
(define-key k-minor-mode-map (kbd "\C-x r")  'toggle-read-only)
(define-key k-minor-mode-map (kbd "\C-x n")  'nuke-trailing-whitespace)

(define-key k-minor-mode-map (kbd "\C-c l")  'org-store-link)
(define-key k-minor-mode-map (kbd "\C-c a")  'org-agenda)

;; Mouse button 1 drags the scroll bar
(define-key k-minor-mode-map [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

;; Use the mouse wheel if available
(mouse-wheel-mode t)

(define-minor-mode k-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t
  " K"
  'k-minor-mode-map)

(k-minor-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Color syntax
(global-font-lock-mode t)

;; Column/line numbering in status line
(setq-default column-number-mode t)
(setq-default line-number-mode t)

;; Highlight matches from searches
(setq-default isearch-highlight t)
(setq-default query-replace-highlight t)
(setq-default search-highlight t)

;; Highlight marked block
(setq-default transient-mark-mode t)

;; Highlight paren matching
(show-paren-mode t)

;; Do not fold lines
(set-default 'truncate-lines t)
(set-default 'case-fold-search t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Behaviour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replace highlighted text with typed text
(delete-selection-mode t)

;; Set the line-wrapping width
(set-default 'fill-column 79)

;; Set the frame title
(setq-default frame-title-format (concat "%b - %F@" system-name))

;; Use spaces instead of tabs in general
(setq-default indent-tabs-mode nil)

;; Don't show the GNU splash screen
(setq-default inhibit-startup-message t)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop adding newlines to the end of files when moving down
(setq-default next-line-add-newlines nil)

;; Resize the mini-buffer  necessary
(setq-default resize-minibuffer-mode t)

;; Scroll just one line when point passes off the screen
(setq-default scroll-conservatively 7)

;; stop forcing me to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;; Stop that annoying beep
(setq visible-bell 'top-bottom)

(setq default-tab-width 2)
(setq tab-width 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'magit-status "magit" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CEDET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (concat dotemacs-dir "cedet/common"))
;;(require 'cedet)

(add-to-list 'load-path (concat dotemacs-dir "ecb"))
;;(load-file "~/.emacs.d/ecb/ecb.el")
;;(require 'ecb)

;;(require 'ecb-autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Company Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'company))

(autoload 'company-mode "company" nil t)
(setq company-backends nil)
(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(define-key k-minor-mode-map (kbd "\t")  'indent-or-complete)
(define-key k-minor-mode-map (kbd "\M-/")  'company-complete-common)

(defun add-company-backend (hook backends)
  "Adds a list of backends to the backendlist for a mode"
  (interactive)
  (add-hook 'hook (lambda ()
                    (set (make-local-variable 'company-backends) 'backend))))

(add-hook 'emacs-lisp-mode-hook (lambda () (set (make-local-variable 'company-backends) '(company-elisp))))
;;(add-company-backend 'emacs-lisp-mode-hook '(company-elisp))

(add-hook 'python-mode-hook (lambda () (set (make-local-variable 'company-backends) '(company-ropemacs company-pysmell))))
;;(add-company-backend 'python-mode-hook '(company-ropemacs company-pysmell))

;;TODO finish hooks
;;TODO automatically start
;;TODO bind TAB to YaSnippet -> Company Mode -> Indent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YaSnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar snippets-dir (concat dotemacs-dir "snippets"))

(require 'yasnippet)
(yas/initialize)
(yas/load-directory snippets-dir)

;;(add-to-list 'auto-mode-alist '(snippets-dir . snippet-mode))

;;(remove-hook 'write-file-functions 'nuke-trailing-whitespace t)

;; Enable versioning with default values (keep five last versions, I think!)
;;(setq version-control t)
;; Save all backup file in this directory.
;;(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C/C++ Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations for all modes in CC Mode.
(eval-when-compile (require 'cc-mode))
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq show-trailing-whitespace t)
            (c-toggle-electric-state 1)
            ;;            (c-toggle-hungry-state 1)
            ;;            (c-toggle-auto-newline 1)
            ))

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Set the indentation width for C-style programming languages
(setq-default c-basic-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ediff Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://www.emacswiki.org/emacs/EdiffMode
;; (setq ediff-split-window-function 'split-window-horizontally)
;; (setq ediff-split-window-function (lambda (&optional arg)
;;                                     (if (> (frame-width) 150)
;;                                         (split-window-horizontally arg)
;;                                       (split-window-vertically arg))))

;; ediff-make-wide-display-function
;; ediff-toggle-read-only-function

;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Text mode is happier than Fundamental mode ;-)
(setq default-major-mode 'text-mode)

;; Activate word-wrapping for text mode
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Allow tabs in text-mode
(add-hook 'text-mode-hook '(lambda () (setq indent-tabs-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MATLAB Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'matlab)
(add-to-list 'auto-mode-alist '("\\.m$'" . matlab-mode))
(setq matlab-shell-command-switches '("-nojvm" "-nodesktop" "-nodisplay" "-nosplash")
      matlab-shell-history-file "~/.matlab/R2009a/history.m")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-Complete Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'auto-complete-config)
;; (require 'auto-complete)
;; (global-auto-complete-mode t)

;; (setq ac-auto-start nil)
;; (global-set-key "\M-/" 'ac-start)
;; (define-key ac-complete-mode-map "\M-/" 'ac-stop)
;; ;;(define-key ac-complete-mode-map "\C-n" 'ac-next)
;; ;;(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;; (define-key ac-complete-mode-map "\t" 'ac-complete)
;; ;;(define-key ac-complete-mode-map "\r" nil)
;; (setq ac-dwim t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Latex Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-variable (quote latex-run-command) "pdflatex")
(set-variable (quote tex-dvi-view-command) "open")

(autoload 'tex-mode-flyspell-verify "flyspell" "" t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode) ;;LaTeX
(when (load "flyspell" t)
  (setq flyspell-issue-message-flag nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LISP Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parenthesis highlighting for LISP/SCHEME
(autoload 'cparen-activate "cparen" nil t)
;; (add-hook 'lisp-mode-hook 'cparen-activate)
;; (add-hook 'lisp-interaction-mode-hook 'cparen-activate)
;; (add-hook 'emacs-lisp-mode-hook 'cparen-activate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'rst-mode "rst-mode" "RST Mode." t)
(add-to-list 'auto-mode-alist '("\\.rst$"  . rst-mode))
(add-to-list 'auto-mode-alist '("\\.rest$" . rst-mode))

(autoload 'doctest-mode "doctest-mode" "doctest mode" t)
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))

(eval-when-compile (require 'python-mode))
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (set-variable 'py-indent-offset 4)
            ;; (set-variable 'py-smart-indentation nil)
            (set-variable 'indent-tabs-mode nil)
            (define-key py-mode-map (kbd "RET") 'newline-and-indent)
            (define-key py-mode-map [f8]        'flymake-mode)
            (define-key py-mode-map [f9]        'python-mode)
            (define-key py-mode-map [f10]       'doctest-mode)
            (define-key py-mode-map [f11]       'rst-mode)
            ;; (define-key py-mode-map [tab] 'yas/expand)
            ;; (setq yas/after-exit-snippet-hook 'indent-according-to-mode)
            ;; (smart-operator-mode-on)
            ))

(eval-when-compile (require 'flymake))
;; code checking via flymake
;; set code checker here from "epylint", "pyflakes"
(defvar pycodechecker "pyflakes")
(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("Makefile$"    . makefile-mode))

(autoload 'zencoding-mode "zencoding-mode" nil t)
(add-hook 'sgml-mode-hook 'zencoding-mode)

(autoload 'actr-mode "actr-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.actr$"     . actr-mode))
(add-to-list 'auto-mode-alist '("\\.act$"      . actr-mode))

(autoload 'stratego-mode "stratego" nil t)
(add-to-list 'auto-mode-alist '("\\.cltx$"     . stratego-mode))
(add-to-list 'auto-mode-alist '("\\.cr$"       . stratego-mode))
(add-to-list 'auto-mode-alist '("\\.r$"        . stratego-mode))
(add-to-list 'auto-mode-alist '("\\.ss$"       . stratego-mode))
(add-to-list 'auto-mode-alist '("\\.str$"      . stratego-mode))
(add-to-list 'auto-mode-alist '("\\.sdf$"      . stratego-mode))

(add-to-list 'auto-mode-alist '("\\.rake$"     . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$"  . ruby-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$"      . yaml-mode))

;; (add-hook 'yaml-mode-hook
;;           '(lambda ()
;;              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(add-to-list 'auto-mode-alist '("\\.stk$"      . scheme-mode))

(add-to-list 'auto-mode-alist '("\\.t2t$"      . text2tags-mode))

(add-to-list 'auto-mode-alist '("\\.spu.c$"    . c-mode))
(add-to-list 'auto-mode-alist '("\\.spuc$"     . c-mode))

(add-to-list 'auto-mode-alist '("\\.org$"      . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.txt$"  . org-mode))

(add-to-list 'auto-mode-alist '("/.?zsh.d/" . shell-script-mode))

(todochiku-message "Emacs"
                   (format ".emacs loaded in %ds"
                           (destructuring-bind (hi lo ms) (current-time)
                             (- (+ hi lo)
                                (+ (first *emacs-load-start*)
                                   (second *emacs-load-start*)))))
                   (todochiku-icon 'check))
