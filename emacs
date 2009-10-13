;;http://www.emacswiki.org/emacs/rlazoDotEmacs.el

;; Wrapper to make .emacs self-compiling
(defvar init-top-level t)
(if init-top-level
    (let ((init-top-level nil))
      (if (file-newer-than-file-p "~/.emacs" "~/.emacs.elc")
          (progn
            (load "~/.emacs")
            (byte-compile-file "~/.emacs"))
        (load "~/.emacs.elc")))

  (progn

    (add-to-list 'load-path "~/.emacs.d/site-lisp")

    (autoload 'nuke-trailing-whitespace "whitespace" nil t)
    (add-hook 'write-file-hooks 'nuke-trailing-whitespace)

    ;;     (require 'textmate)
    ;;     (tm/initialize)

    (require 'color-theme)
    (color-theme-tty-dark)

    ;; turn off the toolbar
    (if (>= emacs-major-version 21)
        (tool-bar-mode -1))

    ;; avoid hiding with M-h
    (setq mac-pass-command-to-system nil)

    ;; Parenthesis highlighting for LISP/SCHEME
    ;; (require 'cparen)
    ;; (cparen-activate)
    ;; (show-paren-mode t)

    ;; Adds color stuffs for a shell
    ;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
    ;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (require 'ascii-table)
    (require 'count-words)

    (defun electric-pair ()
      "If at end of line, insert character pair without surrounding spaces.
   Otherwise, just insert the typed character."
      (interactive)
      (if (eolp) (let (parens-require-spaces) (insert-pair))
        (self-insert-command 1)))

    ;; Indent the whole buffer
    (defun indent-page ()
      "Indent the entire buffer."
      (interactive)
      (save-excursion
        (push-mark (point-min) t t)
        (goto-char (point-max))
        (indent-region (mark) (point) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key Mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Make the `Delete' key delete the char under the cursor
    (global-set-key [delete]        'delete-char)
    (global-set-key [kp-delete]     'delete-char)
    (global-set-key [insert]        'yank)
    (global-set-key [C-delete]      'undo)

    ;; Makes home/end go to beginning/end of line
    (define-key global-map [end] 'end-of-line)
    (define-key global-map [home] 'beginning-of-line)

    ;; Makes control + home/end go to beginning/end of buffer
    (define-key global-map [C-end] 'end-of-buffer)
    (define-key global-map [C-home] 'beginning-of-buffer)

    (require 'pager)
    (global-set-key "\C-v"	   'pager-page-down)
    (global-set-key [next] 	   'pager-page-down)
    (global-set-key "\M-v" 	   'pager-page-up)
    (global-set-key [prior]	   'pager-page-up)
    (global-set-key [M-up]     'pager-row-up)
    (global-set-key [M-kp-8]   'pager-row-up)
    (global-set-key [M-down]   'pager-row-down)
    (global-set-key [M-kp-2]   'pager-row-down)

    (global-set-key [(f1)]     'man-follow)
    (global-set-key [(f6)]     'nuke-trailing-whitespace)
    (global-set-key [(f7)]     'indent-page)
    (global-set-key [(f8)]     'toggle-truncate-lines)

    ;; cycle through buffers
    (global-set-key [C-tab]    'bs-cycle-next)
    (global-set-key [S-tab]    'bs-cycle-previous)

    ;; window stuff
    (global-set-key [C-kp-enter]    'other-window)
    (global-set-key [C-kp-add]      'enlarge-window)
    (global-set-key [C-kp-subtract] 'shrink-window)

    (global-set-key (kbd "\C-x f") 'make-frame)
    (global-set-key (kbd "\C-x g") 'delete-frame)

    (global-set-key (kbd "\C-c c") 	'compile)
    (global-set-key (kbd "\C-c g") 	'goto-line)
    (define-key global-map (kbd "\C-c q") 'comment-or-uncomment-region)

    (global-set-key (kbd "\C-x t")  'toggle-truncate-lines)
    (global-set-key (kbd "\C-x r")  'toggle-read-only)
    (global-set-key (kbd "\C-x n")  'nuke-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mouse Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Mouse button 1 drags the scroll bar
    (define-key global-map [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

    ;; Use the mouse wheel if available
    (mouse-wheel-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Color syntax
    (global-font-lock-mode t)

    ;; Show scroll bar on right
    (set-scroll-bar-mode 'right)

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

    ;; Remove trailing whitespace on save
    ;;(add-hook 'write-file-hooks 'nuke-trailing-whitespace)

    ;; Replace highlighted text with typed text
    (delete-selection-mode t)

    ;; Set the line-wrapping width
    (set-default 'fill-column 79)

    ;; Set the frame title
    (setq-default frame-title-format (concat "%b - emacs@" system-name))

    ;; Use spaces instead of tabs in general
    (setq-default indent-tabs-mode nil)

    ;; Don't show the GNU splash screen
    (setq-default inhibit-startup-message t)

    ;; Always end a file with a newline
    (setq require-final-newline t)

    ;; Stop adding newlines to the end of files when moving down
    (setq-default next-line-add-newlines nil)

    ;; Resize the mini-buffer when necessary
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
;;; C/C++ Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Customizations for all modes in CC Mode.
    (add-hook 'c-mode-common-hook
              (lambda ()
                (setq-default show-trailing-whitespace t)

                (c-toggle-electric-state 1)
                (c-toggle-hungry-state 1)
                (c-toggle-auto-newline 1)
                ))

    (require 'google-c-style)
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)

    ;; Set the indentation width for C-style programming languages
    (setq-default c-basic-offset 2)

    (require 'textmate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Activate word-wrapping for text mode
    (add-hook 'text-mode-hook 'text-mode-hook-identify)
    (add-hook 'text-mode-hook 'turn-on-auto-fill)

    ;; Allow tabs in text-mode
    (add-hook 'text-mode-hook '(lambda () (setq indent-tabs-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (require 'matlab)
    (setq auto-mode-alist (cons '("\\.m$'" . matlab-mode) auto-mode-alist))
    (setq matlab-shell-command-switches
          '("-nojvm" "-nodesktop" "-nodisplay" "-nosplash"))
    (setq matlab-shell-history-file "~/.matlab/R2009a/history.m")

    (require 'actr-mode)
    (setq auto-mode-alist (cons '("\\.actr$" . actr-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.act$" . actr-mode) auto-mode-alist))

    ;; Stratego mode
    (autoload 'stratego-mode "stratego")
    (setq auto-mode-alist (cons '("\\.cltx$" . stratego-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.cr$" . stratego-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.r$" . stratego-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.ss$" . stratego-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.str$" . stratego-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.sdf$" . stratego-mode) auto-mode-alist))

    ;; STK (Scheme) mode
    (setq auto-mode-alist (cons '("\\.stk$" . scheme-mode) auto-mode-alist))

    ;; Text2Tags
    ;;    (setq auto-mode-alist (cons '("\.t2t$" . text2tags-mode) auto-mode-alist))

    ;; Cell Programming
    (setq auto-mode-alist (cons '("\\.spu.c$" . c-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.spuc$" . c-mode) auto-mode-alist))

    ))