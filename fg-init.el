(require 'server)
(unless (server-running-p)
  (server-start))

(let ((default-directory "~/.emacs.d/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(set-variable 'inhibit-splash-screen t)
(set-variable 'inhibit-startup-message t)
(set-variable 'initial-scratch-message nil)
(show-paren-mode t)
(column-number-mode t)
(size-indication-mode t)
(set-variable 'cursor-type 'box)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq indent-tabs-mode nil)

(defvar running-on-home-p
  (string= (system-name) "Kyo.local"))

(defvar running-on-mswindows-p
  (memq system-type '(windows-nt ms-windows cygwin cygwin32)))

(defvar running-on-darwin-p
  (memq system-type '(darwin)))

(when running-on-darwin-p
  (setenv "PATH"
	  (concat "/Users/fgeller/bin:/usr/local/bin:/usr/local/sbin:"
		  (getenv "PATH")))
  (setq exec-path
	(append exec-path
		'("/Users/fgeller/bin" "/usr/local/bin" "/usr/local/sbin"))))

;; encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; mac's special keys. remap to allow entry of umlaut characters
(setq mac-command-modifier 'meta)

(global-unset-key "\C-l")
(defvar ctl-l-map (make-keymap)
     "Keymap for local bindings and functions, prefixed by (^L)")
(define-key global-map "\C-l" 'Control-L-prefix)
(fset 'Control-L-prefix ctl-l-map)

(define-key ctl-l-map "aa"		'anything)
(define-key ctl-l-map "af"		'anything-find-files)
(define-key ctl-l-map "ac"		'fg/anything-ac)
(define-key ctl-l-map "al"		'anything-locate)
(define-key ctl-l-map "caa"		'align)
(define-key ctl-l-map "car"		'align-regexp)
(define-key ctl-l-map "csb"		'hs-show-block)
(define-key ctl-l-map "csa"		'hs-show-all)
(define-key ctl-l-map "chb"		'hs-hide-block)
(define-key ctl-l-map "cha"		'hs-hide-all)
(define-key ctl-l-map "d"		'duplicate-line)
(define-key ctl-l-map "el"		'ediff-regions-linewise)
(define-key ctl-l-map "ew"		'ediff-regions-wordwise)
(define-key ctl-l-map "ef"		'ediff-files)
(define-key ctl-l-map "fn"		'find-name-dired)
(define-key ctl-l-map "G"		'rgrep)
(define-key ctl-l-map "gfn"		'flymake-goto-next-error)
(define-key ctl-l-map "gfp"		'flymake-goto-prev-error)
(define-key ctl-l-map "ha"		'apropos-command)
(define-key ctl-l-map "hb"		'describe-bindings)
(define-key ctl-l-map "hc"		'describe-key-briefly)
(define-key ctl-l-map "hd"		'apropos-documentation)
(define-key ctl-l-map "hf"		'describe-function)
(define-key ctl-l-map "hh"		'help-for-help)
(define-key ctl-l-map "hi"		'info)
(define-key ctl-l-map "hk"		'describe-key)
(define-key ctl-l-map "hl"		'view-lossage)
(define-key ctl-l-map "hm"		'describe-mode)
(define-key ctl-l-map "ho"		'icicle-describe-option-of-type)
(define-key ctl-l-map "hp"		'finder-by-keyword)
(define-key ctl-l-map "hq"		'help-quit)
(define-key ctl-l-map "hr"		'info-emacs-manual)
(define-key ctl-l-map "hs"		'describe-syntax)
(define-key ctl-l-map "hv"		'describe-variable)
(define-key ctl-l-map "hw"		'where-is)
(define-key ctl-l-map "h\S-tab" 'icicle-complete-keys)
(define-key ctl-l-map "k"		'kill-whole-line)
(define-key ctl-l-map "l"		'goto-line)
(define-key ctl-l-map "vg"		'magit-status)
(define-key ctl-l-map "vh"		'monky-status)
(define-key ctl-l-map "n"		'notmuch)
(define-key ctl-l-map "ma"		'auto-complete-mode)
(define-key ctl-l-map "mr"		'auto-revert-mode)
(define-key ctl-l-map "mw"		'whitespace-mode)
(define-key ctl-l-map "of"		'org-footnote-action)
(define-key ctl-l-map "q"		'query-replace)
(define-key ctl-l-map "Q"		'query-replace-regexp)
(define-key ctl-l-map "r"		'revert-buffer)
(define-key ctl-l-map "ui"		'ucs-insert)
(define-key ctl-l-map "U"		'browse-url)
(define-key ctl-l-map "xx"		'execute-extended-command)
(define-key ctl-l-map "xb"		'eval-buffer)
(define-key ctl-l-map "xe"		'eval-last-sexp)
(define-key ctl-l-map "xr"		'eval-region)
(define-key ctl-l-map "!"		'shell-command)
(define-key ctl-l-map "^"		'join-line)
(define-key ctl-l-map "%"		'goto-match-paren)
(define-key ctl-l-map "<"		'beginning-of-buffer)
(define-key ctl-l-map ">"		'end-of-buffer)
(define-key ctl-l-map "\C-c"	'calendar)
(define-key ctl-l-map "\C-l"	'redraw-display)
(define-key ctl-l-map "\C-n"	'linum-mode)
(define-key ctl-l-map "\C-q"	'fill-paragraph)
(define-key ctl-l-map "\C-r"	'isearch-backward-regexp)
(define-key ctl-l-map "\C-s"	'isearch-forward-regexp)

(global-set-key (kbd "<S-iso-lefttab>") 'indent-relative)
(global-set-key (kbd "<S-tab>") 'indent-relative)
(global-set-key (kbd "<backtab>") 'indent-relative)

;; this uses `forward-whitespace' from `thingatpt.el'
(defun fg/backward-whitespace ()
  (interactive)
  (forward-whitespace -1))
(global-set-key (kbd "M-F") 'forward-whitespace)
(global-set-key (kbd "M-B") 'fg/backward-whitespace)


(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "<S-iso-lefttab>") 'indent-relative)
     (define-key org-mode-map (kbd "<S-tab>") 'indent-relative)
     (define-key org-mode-map (kbd "<backtab>") 'indent-relative)))

;; ;; -- Display images in org mode
;; ;; enable image mode first
;; (iimage-mode)
;; ;; add the org file link format to the iimage mode regex
;; (add-to-list 'iimage-mode-image-regex-alist
;;   (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex "\\)\\]")  1))
;; ;;  add a hook so we can display images on load
;; (add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))
;; ;; function to setup images for display on load
;; (defun org-turn-on-iimage-in-org ()
;;   "display images in your org file"
;;   (interactive)
;;   (turn-on-iimage-mode)
;;   (set-face-underline-p 'org-link nil))
;; ;; function to toggle images in a org buffer
;; (defun org-toggle-iimage-in-org ()
;;   "display images in your org file"
;;   (interactive)
;;   (if (face-underline-p 'org-link)
;;       (set-face-underline-p 'org-link nil)
;;       (set-face-underline-p 'org-link t))
;;   (call-interactively 'iimage-mode))


;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(global-font-lock-mode t)
(set-variable 'font-lock-maximum-decoration t)
(set-default-font "-apple-Inconsolata-medium-normal-normal-*-18-*-*-*-m-0-*-1")
(setq default-frame-alist
      '((vertical-scroll-bars)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(background-color . "lightyellow1")
	(foreground-color . "black")
	(font . "-apple-Inconsolata-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
	))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))
(require 'color-theme-solarized)
(color-theme-solarized-light)

(eval-after-load "ring" '(progn (require 'ring+)))
; explicitly defining this to exclude `bbdb-complete-name'
(setq icicle-functions-to-redefine
      '(comint-dynamic-complete
		comint-dynamic-complete-filename
		comint-replace-by-expanded-filename
		customize-apropos customize-apropos-faces
		customize-apropos-groups
		customize-apropos-options
		customize-apropos-options-of-type
		customize-face
		customize-face-other-window
		dabbrev-completion
		dired-read-shell-command
		ess-complete-object-name
		gud-gdb-complete-command
		lisp-complete-symbol
		lisp-completion-at-point
		minibuffer-default-add-completions
		read-color
		read-from-minibuffer
		read-shell-command
		read-string
		recentf-make-menu-items
		repeat-complex-command))
(require 'icicles)
(icy-mode)

(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
(defun fg/anything-ac ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-emacs-commands
     anything-c-source-complex-command-history
     anything-c-source-extended-command-history)
   " *fg/anything-ac*"))
(anything-dired-bindings 1)

(require 'info)
(if running-on-mswindows-p
    (add-to-list 'Info-directory-list
		 "d:/programs/emacs/info"))
(if running-on-darwin-p
    (add-to-list 'Info-directory-list
		 "/Applications/Emacs.app/Contents/Resources/info"))

(defvar nc-minor-mode-map (make-keymap)
  "nc-minor-mode keymap.")
(let ((f (lambda (m)
           `(lambda () (interactive)
              (message (concat "No! use " ,m " instead."))))))
  (dolist (l '(("<left>" . "C-b") ("<right>" . "C-f") ("<up>" . "C-p")
               ("<down>" . "C-n")
               ("<C-left>" . "M-b") ("<C-right>" . "M-f") ("<C-up>" . "M-{")
               ("<C-down>" . "M-}")
               ("<M-left>" . "M-b") ("<M-right>" . "M-f") ("<M-up>" . "M-{")
               ("<M-down>" . "M-}")
               ("<delete>" . "C-d") ("<C-delete>" . "M-d")
               ("<M-delete>" . "M-d") ("<next>" . "C-v") ("<C-next>" . "M-x <")
               ("<prior>" . "M-v") ("<C-prior>" . "M-x >")
               ("<home>" . "C-a") ("<C-home>" . "M->")
               ("<C-home>" . "M-<") ("<end>" . "C-e") ("<C-end>" . "M->")))
    (define-key nc-minor-mode-map
      (read-kbd-macro (car l)) (funcall f (cdr l)))))
(define-minor-mode nc-minor-mode
  "A minor mode that disables the arrow-keys, pg-up/down, delete
  and backspace."  t " nc"
  'nc-minor-mode-map :global t)
(nc-minor-mode 0)

;; auto-save game
(set-variable 'auto-save-default nil)
(defvar autosave-dir
  (concat "~/.emacs.d/saved"))
(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name ()
  (concat autosave-dir
	  (if buffer-file-name
	      (concat "#" (file-name-nondirectory buffer-file-name) "#")
	    (expand-file-name (concat "#%" (buffer-name) "#")))))
(defvar backup-dir (concat "~/.emacs.d/saved"))
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq delete-old-versions t)

(defun pretty-lambda ()
    (font-lock-add-keywords
     nil `(("(\\(lambda\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(decode-char 'ucs #X03BB))
                      nil))))))
(add-hook 'emacs-lisp-mode-hook 'pretty-lambda)
(add-hook 'lisp-mode-hook 'pretty-lambda)

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert
the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
		((looking-at "\\s\)") (forward-char 1) (backward-list 1))
		(t                    (self-insert-command (or arg 1))) ))

(defvar ac-dictionary-directories '())
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(ac-config-default)
(setq ac-stop-flymake-on-completing t)
(setq-default ac-sources '(ac-source-abbrev
						   ac-source-words-in-same-mode-buffers
						   ac-source-yasnippet
						   ac-source-filename))
(global-auto-complete-mode t)
(setq ac-delay 0.05)
(setq ac-auto-start 0)
(setq ac-ignore-case 'smart) ; insensitive for lower case
(setq ac-fuzzy-enable t)
(set-face-background 'ac-candidate-face "#eee8d5")
(set-face-foreground 'ac-candidate-face "#657b83")
(set-face-underline 'ac-candidate-face nil)
(set-face-background 'ac-selection-face "#268bd2")
(set-face-foreground 'ac-selection-face "#fdf6e3")

(require 'autopair)
(require 'auto-pair+)
(autopair-global-mode)
(setq autopair-autowrap t)

;; csv
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; fill
(add-hook 'message-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq initial-major-mode 'org-mode) ;; set org-mode for *scratch*
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-c\C-xf" 'org-footnote-action)
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files `(,org-directory))
(setq org-default-notes-file (concat org-directory "/Notes.org"))
(setq org-capture-templates
      '(("t" "Todo"
	 entry (file+headline (concat org-directory "/Tasks.org") "Tasks")
	 "* TODO %?\n\n  %a\n  %i\n")
	("d" "Date"
	 entry (file+headline (concat org-directory "/Dates.org") "Calendar")
	 "* %?\n  %t\n  %a\n  %i\n")))
(setq org-agenda-custom-commands
      '(("d" . "Completed / archived items")
	("dt" "[t]oday"
	 tags "ARCHIVE_TIME>=\"<today>\""
	 ((org-agenda-archives-mode t)))
	("dy" "[y]esterday"
	 tags "ARCHIVE_TIME>=\"<-1d>\"&ARCHIVE_TIME<\"<today>\""
	 ((org-agenda-archives-mode t)))
	("dw" "[w]eek"
	 tags "ARCHIVE_TIME>=\"<-1w>\""
	 ((org-agenda-archives-mode t)))))

(defun fg/publish-state-entry-state-change-to-gcal ()
  (let ((new-title (org-get-heading))
	(gcal-id (org-entry-get nil "GCalId")))
    (when gcal-id
      (start-process
       "push2gcal" "*push2gcal*"
       "gcal2org.py" "fgeller@gmail.com" "update" gcal-id "title" new-title))))
(add-hook 'org-after-todo-state-change-hook
	  'fg/publish-state-entry-state-change-to-gcal)

;; emacs lisp
;; (add-hook 'emacs-lisp-mode-hook
;; 		  #'(lambda ()
;; 			  (push '(?` . ?')
;; 					(getf autopair-extra-pairs :comment))
;; 			  (push '(?` . ?')
;; 					(getf autopair-extra-pairs :string))))

;; erc
(setq erc-nick "felix^^")
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; epa
(require 'epa-file)
(epa-file-enable)

;; scheme
(require 'quack)

;; haskell
(load "~/.emacs.d/site-lisp/haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/addons/yasnippet/snippets/text-mode")
(setq yas/prompt-functions '(yas/dropdown-prompt))


;; flymake
(require 'flymake)
(custom-set-faces
 '(flymake-errline ((((class color)) (:foreground "#dc322f"))))
 '(flymake-warnline ((((class color)) (:foreground "#b58900")))))

(require 'flymake-cursor)


;; python
(require 'python)
;; Initialize Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; Initialize Rope
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
		  (lambda () (setq tab-width 4
					  indent-tabs-mode t)))
(add-hook 'python-mode-hook
          (lambda ()
			(setq ac-sources (add-to-list 'ac-sources 'ac-source-ropemacs))))
;; http://gunnarwrobel.de/wiki/Python.html
(setq pdb-path '/usr/lib/python2.7/pdb.py
      gud-pdb-command-name 'pdb)
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2003-10/msg00577.html
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
			    (file-name-nondirectory buffer-file-name)))))
(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
					 'flymake-create-temp-inplace))
		 (local-file (file-relative-name
					  temp-file
					  (file-name-directory buffer-file-name))))
	(list "pycheckers"  (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks
			 '("\\.py\\'" flymake-pyflakes-init))
;; Hooks
(add-hook 'python-mode-hook
		  (lambda () (unless (eq buffer-file-name nil) (flymake-mode 1))))
;; http://code.google.com/p/autopair/
;; (add-hook 'python-mode-hook
;; 		  #'(lambda ()
;; 			  (setq autopair-handle-action-fns
;; 					(list #'autopair-default-handle-action
;; 						  #'autopair-python-triple-quote-action))))
(add-hook 'python-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            (hs-minor-mode t)))


;; Javascript
(require 'flymake-jslint)
(setq lintnode-location "~/.emacs.d/addons/lintnode")
(setq lintnode-jslint-excludes '())
(add-hook 'js-mode-hook
          (lambda ()
            (lintnode-hook)))
(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            (hs-minor-mode t)))

(require 'js-comint)
;; Use node as our repl
(setq inferior-js-program-command "/usr/local/bin/node")

(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                     (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))

(if running-on-home-p (load "~/.emacs.d/home-config.el"))

;; nxml
(add-to-list
 'auto-mode-alist
 (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
	   'nxml-mode))
(unify-8859-on-decoding-mode)
(setq magic-mode-alist
	  (cons '("<＼＼?xml " . nxml-mode)
			magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)
