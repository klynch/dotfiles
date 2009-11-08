(setq dotemacs-dir "~/.emacs.d")
(setq site-lisp-dir (concat dotemacs-dir "/site-lisp"))
(setq dotemacs-private "~/.emacs.private")

(if (file-exists-p dotemacs-private)
    (load dotemacs-private))

;; (add-to-list 'load-path site-lisp-dir)

;;TODO functionify this!
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir site-lisp-dir)
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

(if (fboundp 'image-load-path) nil (setq image-load-path '()))
(add-to-list 'image-load-path (concat dotemacs-dir "/images/"))

;;TODO add this to a mode that can be enabled / disabled
(autoload 'nuke-trailing-whitespace "whitespace" nil t)
(add-hook 'write-file-hooks 'nuke-trailing-whitespace)

;; TODO reduce color theme to only the themes i want
(require 'color-theme)
;;(require 'color-theme-tango)
(color-theme-tty-dark)

;; (color-theme-initialize)
;; (if window-system
;;     (color-theme-tango))
;; (if (not (window-system))
;;     (color-theme-tty-dark))

;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; GROWL or its alternatives. must set directory first
(setq todochiku-icons-directory (concat dotemacs-dir "/todochiku-icons"))
(require 'todochiku)
(message "Todochiku loaded")
;; TODO load these only if we are on a mac
(if (eq system-type 'darwin)
    (progn
      (setq default-input-method "MacOSX")
      (setq mac-pass-command-to-system nil)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)
      (setq x-select-enable-clipboard t)
      ;; We don't want to pop open new frames all the time
      (setq ns-pop-up-frames nil)))

;; (require 'textmate)
;; (tm/initialize)

;; Parenthesis highlighting for LISP/SCHEME
;; (require 'cparen)
;; (cparen-activate)
;; (show-paren-mode t)

;; Adds color stuff for a shell
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; TODO fix twitter faces
(require 'twit)
(setq twit-show-user-images t)

(autoload 'twit-show-recent-tweets "twit" nil t)
(autoload 'twit-show-at-tweets     "twit" nil t)
(autoload 'twit-show-friends 		"twit" nil t)
(autoload 'twit-show-followers 		"twit" nil t)

(autoload 'twit-follow-recent-tweets	"twit" nil t)

(autoload 'twit-post			"twit" nil t)
(autoload 'twit-post-region		"twit" nil t)
(autoload 'twit-post-buffer		"twit" nil t)
(autoload 'twit-direct			"twit" nil t)

(autoload 'twit-add-favorite		"twit" nil t)
(autoload 'twit-remove-favorite 	"twit" nil t)

(autoload 'twit-add-friend  		"twit" nil t)
(autoload 'twit-remove-friend 		"twit" nil t)

(autoload 'twit-show-direct-tweets-with-account "twit" nil t)
(autoload 'twit-show-at-tweets-with-account 	"twit" nil t)

;; (global-set-key "\C-cTT"  'twit-follow-recent-tweets) ; (s)how (T)weets
;; (global-set-key "\C-cTst" 'twit-follow-recent-tweets) ; (s)how (t)weets
;; (global-set-key "\C-cTsa" 'twit-show-at-tweets)       ; (s)how (a)t
;; (global-set-key "\C-cTsf" 'twit-show-at-tweets)       ; (s)how (f)riends
;; (global-set-key "\C-cTsl" 'twit-show-at-tweets)       ; (s)how fo(l)lowers

;; (global-set-key "\C-cTpp" 'twit-post)		      ; (p)ost
;; (global-set-key "\C-cTpr" 'twit-post-region)	      ; (p)post (r)egion
;; (global-set-key "\C-cTpb" 'twit-post-buffer)	      ; (p)post (b)uffer
;; (global-set-key "\C-cTpr" 'twit-direct)		      ; (p)post (d)irect
;; (global-set-key "\C-cTfa" 'twit-add-favorite)	      ; (f)avorite (a)dd
;; (global-set-key "\C-cTfr" 'twit-remove-favorite)      ; (f)avorite (r)emove

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (if (file-newer-than-file-p "~/.emacs" "~/.emacs.elc")
      (byte-compile-file "~/.emacs"))
  (byte-recompile-directory site-lisp-dir 0))

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
;;; YaSnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq snippets-dir (concat dotemacs-dir "/snippets"))

(require 'yasnippet)
(yas/initialize)
(yas/load-directory snippets-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C/C++ Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations for all modes in CC Mode.
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq show-trailing-whitespace t)

            (c-toggle-electric-state 1)
            (c-toggle-hungry-state 1)
            (c-toggle-auto-newline 1)
            ))

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Set the indentation width for C-style programming languages
(setq-default c-basic-offset 2)

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
(setq auto-mode-alist (cons '("\\.m$'" . matlab-mode) auto-mode-alist))
(setq matlab-shell-command-switches
      '("-nojvm" "-nodesktop" "-nodisplay" "-nosplash"))
(setq matlab-shell-history-file "~/.matlab/R2009a/history.m")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'python)
;; (require 'auto-complete)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

;; ;; Initialize Rope
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Auto-completion
;; ;;;  Integrates:
;; ;;;   1) Rope
;; ;;;   2) Yasnippet
;; ;;;   all with AutoComplete.el
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun prefix-list-elements (list prefix)
;;   (let (value)
;;     (nreverse
;;      (dolist (element list value)
;;       (setq value (cons (format "%s%s" prefix element) value))))))

;; (defvar ac-source-rope
;;   '((candidates
;;      . (lambda ()
;;          (prefix-list-elements (rope-completions) ac-target))))
;;   "Source for Rope")

;; (defun ac-python-find ()
;;   "Python `ac-find-function'."
;;   (require 'thingatpt)
;;   (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
;;     (if (null symbol)
;;         (if (string= "." (buffer-substring (- (point) 1) (point)))
;;             (point)
;;           nil)
;;       symbol)))

;; (defun ac-python-candidate ()
;;   "Python `ac-candidates-function'"
;;   (let (candidates)
;;     (dolist (source ac-sources)
;;       (if (symbolp source)
;;           (setq source (symbol-value source)))
;;       (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
;;              (requires (cdr-safe (assq 'requires source)))
;;              cand)
;;         (if (or (null requires)
;;                 (>= (length ac-target) requires))
;;             (setq cand
;;                   (delq nil
;;                         (mapcar (lambda (candidate)
;;                                   (propertize candidate 'source source))
;;                                 (funcall (cdr (assq 'candidates source)))))))
;;         (if (and (> ac-limit 1)
;;                  (> (length cand) ac-limit))
;;             (setcdr (nthcdr (1- ac-limit) cand) nil))
;;         (setq candidates (append candidates cand))))
;;     (delete-dups candidates)))

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;                  (auto-complete-mode 1)
;;                  (set (make-local-variable 'ac-sources)
;;                       (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
;;                  (set (make-local-variable 'ac-find-function) 'ac-python-find)
;;                  (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
;;                  (set (make-local-variable 'ac-auto-start) nil)))

;; ;;Ryan's python specific tab completion
;; (defun ryan-python-tab ()
;;   ; Try the following:
;;   ; 1) Do a yasnippet expansion
;;   ; 2) Do a Rope code completion
;;   ; 3) Do an indent
;;   (interactive)
;;   (if (eql (ac-start) 0)
;;       (indent-for-tab-command)))

;; (defadvice ac-start (before advice-turn-on-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) t))
;; (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) nil))

;; (define-key python-mode-map "\t" 'ryan-python-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("Makefile$" . makefile-mode))

;; Zen coding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;; ACTR
(require 'actr-mode)
(setq auto-mode-alist (cons '("\\.actr$" . actr-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.act$" . actr-mode) auto-mode-alist))

;; Stratego mode
(autoload 'stratego-mode "stratego" nil t)
(setq auto-mode-alist (cons '("\\.cltx$" . stratego-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cr$" . stratego-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.r$" . stratego-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ss$" . stratego-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.str$" . stratego-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sdf$" . stratego-mode) auto-mode-alist))

;; Ruby Mode
;; Rake files are ruby, too, as are gemspecs.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

;; STK (Scheme) mode
(setq auto-mode-alist (cons '("\\.stk$" . scheme-mode) auto-mode-alist))

;; Text2Tags
;;    (setq auto-mode-alist (cons '("\.t2t$" . text2tags-mode) auto-mode-alist))

;; Cell Programming
(setq auto-mode-alist (cons '("\\.spu.c$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.spuc$" . c-mode) auto-mode-alist))

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; ZSH configuration editing mode
(add-to-list 'auto-mode-alist '("/.?zsh.d/" . shell-script-mode))
