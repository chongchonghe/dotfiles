;; -*- mode: emacs-lisp -*-

;; (defun tangle-init ()
;;       "If the current buffer is 'init.org' the code-blocks are
;; tangled, and the tangled file is compiled."
;;       (when (equal (buffer-file-name)
;; 			       ;; (expand-file-name (concat user-emacs-directory "init.org")))
;; 			       "/Users/chongchonghe/dotfiles/emacs/init.org")
;; 	;; Avoid running hooks when tangling.
;; 	(let ((prog-mode-hook nil))
;; 	      (org-babel-tangle)
;; 	      (byte-compile-file (concat user-emacs-directory "init.el")))))
;; (add-hook 'after-save-hook 'tangle-init)

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold 100000000)

(package-initialize)
(add-to-list 'load-path "~/.emacs.d/pkgs")

;; (defvar myPackages
;;   '(better-defaults
;;     ;;ein
;;     elpy
;;     flycheck
;;     ;;spolky   ; theme?
;;     ;; py-autopep8
;;     ))

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
;; (unless (assoc-default "org" package-archives)
;;   (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

;; This is only needed once, near the top of the file
(require 'bind-key)
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa")
  (require 'use-package))

;; (add-to-list 'load-path "~/.emacs.d/elpa")
;; (require 'use-package)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; (use-package ein)
;; (use-package ein-notebook)
;; (use-package ein-subpackages)
;; (use-package better-defaults)

;; remove backup files (e.g. README.md~)
(setq make-backup-files nil)
;; enable line numbers globally
(global-linum-mode t) 
;; Shift-arrow to swith windows
(windmove-default-keybindings)
;; (global-unset-key (kbd "C-x C-c"))
;; (global-unset-key (kbd "M-`")) ; not working
(setq default-fill-column 80)
;; Auto revert mode
(global-auto-revert-mode 1)
;; Keep track of loading time
(defconst emacs-start-time (current-time))
;; start server at startup
(server-start)
;; Disable welcome screen
(setq inhibit-startup-screen t)
;; Search only visible 
(setq search-invisible nil)
(setq column-number-mode t)
(setq x-select-enable-clipboard t)
;; (desktop-save-mode 1)
;; end file with new line ("\n")
(setq mode-require-final-newline t)
;; always follow symlinks
(setq find-file-visit-truename t)
;; Save Place
(save-place-mode nil)
;; always confirm with y or n, not yes or no.
;; https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use cmd key for meta
;; https://superuser.com/questions/297259/set-emacs-meta-key-to-be-the-mac-key
;; (setq mac-option-key-is-meta nil
;;       mac-command-key-is-meta t
;;       mac-command-modifier 'meta
;;       mac-option-modifier 'super)
;; (setq mac-option-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; (setq-default
;;  ;; Column Marker at 80
;;  whitespace-line-column 80
;;  whitespace-style       '(face lines-tail))
;; (add-hook 'prog-mode-hook #'whitespace-mode)

;; set encoding
(set-language-environment "utf-8")
(set-default-coding-systems 'utf-8)
;; (set-buffer-file-coding-system 'utf-8-unix)
;; (set-clipboard-coding-system 'utf-8-unix)
;; (set-file-name-coding-system 'utf-8-unix)
;; (set-keyboard-coding-system 'utf-8-unix)
;; (set-next-selection-coding-system 'utf-8-unix)
;; (set-selection-coding-system 'utf-8-unix)
;; (set-terminal-coding-system 'utf-8-unix)
;; (setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; from enberg on #emacs
;; (add-hook 'compilation-finish-functions
;;   (lambda (buf str)
;;     (if (null (string-match ".*exited abnormally.*" str))
;;         ;;no errors, make the compilation window go away in a few seconds
;;         (progn
;;           (run-at-time
;;            "2 sec" nil 'delete-windows-on
;;            (get-buffer-create "*compilation*"))
;;           (message "No Compilation Errors!")))))

(defun no-auto-fill ()
  "Turn off auto-fill-mode."
  (auto-fill-mode -1)
  (setq word-wrap t)
  )

(use-package default-text-scale
  :defer 2)

(add-hook 'BSDmakefile-mode-hook #'(indent-according-to-mode 1))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq electric-indent-mode nil)

(add-to-list 'auto-mode-alist '("/\\.[^/]*rc" . conf-mode) t)

(global-set-key (kbd "C-c f w") 'isamert/toggle-side-work-org-buffer)
(global-set-key (kbd "C-c f t") 'isamert/toggle-side-work-org-buffer)
(global-set-key (kbd "C-c f j") 'isamert/toggle-side-journal-org-buffer)

(defun isamert/toggle-side-work-org-buffer ()
  "Toggle `bullet.org` in a side buffer for quick note taking.  The buffer is opened in side window so it can't be accidentaly removed."
  (interactive)
  (isamert/toggle-side-buffer-with-file "~/Dropbox/orgfiles/work.org"))

(defun isamert/toggle-side-journal-org-buffer ()
  "Toggle `bullet.org` in a side buffer for quick note taking.  The buffer is opened in side window so it can't be accidentaly removed."
  (interactive)
  (isamert/toggle-side-buffer-with-file "~/Dropbox/orgfiles/y-journals.org"))

(defun isamert/buffer-visible-p (buffer)
 "Check if given BUFFER is visible or not.  BUFFER is a string representing the buffer name."
  (or (eq buffer (window-buffer (selected-window))) (get-buffer-window buffer)))

(defun isamert/display-buffer-in-side-window (buffer)
  "Just like `display-buffer-in-side-window' but only takes a BUFFER and rest of the parameters are for my taste."
  (select-window
   (display-buffer-in-side-window
    buffer
    (list (cons 'side 'right)
          (cons 'slot 0)
          (cons 'window-width 84)
          (cons 'window-parameters (list (cons 'no-delete-other-windows t)
                                         (cons 'no-other-window nil)))))))

(defun isamert/remove-window-with-buffer (the-buffer-name)
  "Remove window containing given THE-BUFFER-NAME."
  (mapc (lambda (window)
          (when (string-equal (buffer-name (window-buffer window)) the-buffer-name)
            (delete-window window)))
        (window-list (selected-frame))))

(defun isamert/toggle-side-buffer-with-file (file-path)
  "Toggle FILE-PATH in a side buffer. The buffer is opened in side window so it can't be accidentaly removed."
  (interactive)
  (let ((fname (file-name-nondirectory file-path)))
  (if (isamert/buffer-visible-p fname)
      (isamert/remove-window-with-buffer fname)
    (isamert/display-buffer-in-side-window
     (save-window-excursion
       (find-file file-path)
       (current-buffer))))))

(defun my-space-as-tab ()
      "Insert spaces as tabs."
      (interactive)
      (setq-default indent-tabs-mode nil)
      (setq indent-line-function 'insert-tab)
      )

;; Work around a bug where esup tries to step into the byte-compiled
;; version of `cl-lib', and fails horribly.
(setq esup-depth 0)
(use-package esup)

(use-package crux
  :defer t)

;; (add-to-list 'load-path "~/.emacs.d/evil")
;; (require 'evil)
;; (evil-mode 1)

;; (use-package evil
;;   :ensure t
;;   :defer .1
;;   :init
;;   (setq evil-want-integration nil) ;; required by evil-collection
;;   (setq evil-want-keybinding nil)
;;   (setq evil-search-module 'evil-search)
;;   (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
;;   (setq evil-split-window-below t) ;; like vim's 'splitbelow'
;;   :config
;;   (evil-mode 1)
;;   (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;;   (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;;   ;; Make horizontal movement cross lines
;;   (setq-default evil-cross-lines t)
;;   (setq key-chord-two-keys-delay 0.4)
;;   (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
;;   )

;; DO NOT PUT EVIL INTO USE-PACKAGE because other part of this dotfile 
;; relies on it
(require 'evil)
;; (setq evil-want-integration nil) ;; required by evil-collection
;; (setq evil-want-keybinding nil)
(setq evil-search-module 'evil-search)
(setq evil-vsplit-window-right t) ;; like vim's 'splitright'
(setq evil-split-window-below t) ;; like vim's 'splitbelow'
(evil-mode 1)
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;; Make horizontal movement cross lines
(setq-default evil-cross-lines t)
(setq key-chord-two-keys-delay 0.4)
;; (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
;; (define-key evil-insert-state-map (kbd "M-v") 'yank)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

;; (key-chord-mode 1)
;; ;; Add key-chord-mode to minor-mode-alist
;; (if (not (assq 'key-chord-mode minor-mode-alist))
;;     (setq minor-mode-alist
;; 	  (cons '(key-chord-mode " KeyC ")
;; 		minor-mode-alist)))

;; For python
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; julia
(add-hook 'julia-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For Javascript
(add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; ;; For ruby
;; (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; (use-package org
;;   :pin gnu)
(require 'org)
(setq org-image-actual-width (list 400))
(setq org-hide-emphasis-markers t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; inline image size
;; (setq org-image-actual-width nil)
(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . nil)))
;; Initial visibility: do not set anything.
;; Default: showeverything
;; (setq org-startup-folded t)

(use-package htmlize
  :defer 10
  :load-path "/Users/chongchonghe/dotfiles/emacs/packages/emacs-htmlize")

(defun org-toggle-hide-emphasis-markers ()
  "Toggle org-hide-emphasis-markers"
  (interactive)
  (if org-hide-emphasis-markers
      (setq org-hide-emphasis-markers nil)
    (setq org-hide-emphasis-markers t)))

;; (setq org-descriptive-links nil)

(setq org-startup-indented t)

(setq outline-blank-line 2)

(defun org-show-two-levels ()
  (interactive)
  (org-content 2))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c 2") 'org-show-two-levels))
(defun org-show-three-levels ()
  (interactive)
  (org-content 3))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c 3") 'org-show-three-levels))
;; Evaluate it after startup
;; (add-hook 'org-mode-hook #'org-show-two-levels)
(add-hook 'org-view-mode-hook '(text-scale-adjust))

(use-package org-cliplink
  :defer 12
  ;; :bind ("C-c C-p" . 'org-cliplink)
  :config
  (global-set-key (kbd "C-x p i") 'org-cliplink)
)
;; (require 'org-cliplink)
;; (define-key org-mode-map (kbd "C-c C-p") 'org-cliplink)

;; (setq org-modules '(org-tempo))

;; (use-package org-autolist
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
;;   )

(setq org-return-follows-link t)

(defun my-export-and-refresh-safari ()
  (interactive)
  (org-html-export-to-html)
  (shell-command "osascript /Users/chongchonghe/local/bin/refresh_safari.scpt"))

(defun my-org-mode-config ()
  (local-set-key "\M-n" 'outline-next-visible-heading)
  (local-set-key "\M-p" 'outline-previous-visible-heading)
  ;; table
  (local-set-key "\C-\M-w" 'org-table-copy-region)
  (local-set-key "\C-\M-y" 'org-table-paste-rectangle)
  (local-set-key "\C-\M-l" 'org-table-sort-lines)
  ;; display images
  (local-set-key "\M-I" 'org-toggle-iimage-in-org)
  ;; TODOlist
  ;; fix tab
  ;; (local-set-key "\C-y" 'yank)
  (local-set-key "\M-h" 'windmove-left)
  (local-set-key "\C-cl" 'grg-store-link)
  (local-set-key "\C-cb" 'org-switchb)
  (local-set-key "\C-cp" 'org-display-inline-images)
  (local-set-key "\M-h" 'org-metaleft)
  (local-set-key "\M-l" 'org-metaright)
  ;; (local-set-key "\C-ce" 'org-html-export-to-html)
  (local-set-key "\C-ce" 'my-export-and-refresh-safari)
  ;; (local-set-key (kbd "s-p") (kbd "C-c C-e h h"))
  ;; (setq-local truncate-lines 'nil)
  ;; (org-indent-mode)  ;; not working?
  (local-set-key (kbd "S-<down>") 'org-priority-down)
  (local-set-key (kbd "S-<up>") 'org-priority-up)
  )
(add-hook 'org-mode-hook 'my-org-mode-config)

;; redefine because local-set-key is not working
(define-key org-mode-map (kbd "C-c e") 'my-export-and-refresh-safari)
(define-key org-mode-map (kbd "S-<down>") 'org-priority-down)

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-p") 'org-latex-preview)
  (define-key evil-normal-state-map (kbd "C-M-p") 'org-fragtog-mode))
;; (define-key org-mode-map (kbd "C-p") 'org-latex-preview)

;; (evil-define-key 'normal org-mode-map (kbd ", ,") 'org-insert-structure-template)

(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun my-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (setq org-super-agenda-header-map (make-sparse-keymap))
  )

;; 'evil-org-agenda is replaced by the following
;; (define-key org-agenda-mode-map "j" 'evil-next-line)
;; (define-key org-agenda-mode-map "k" 'evil-previous-line)

;; (use-package org-agenda
;;   :bind (:map org-agenda-mode-map
;; 	      ("j" . org-agenda-next-item)
;; 	      ("k" . org-agenda-previous-time)))

;; (use-package org-agenda
;;   :config
;;   (define-key org-agenda-mode-map (kbd "j") #'org-agenda-next-item)
;;   (define-key org-agenda-mode-map (kbd "k") #'org-agenda-previous-item))

;; (use-package org-evil
;;   :ensure t
;;   :after org
;;   )

;; (font-lock-add-keywords
;;  'org-mode
;;  '(("^ *\\([-]\\) "
;;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; (use-package org-bullets
;;       :init
;;       (add-hook 'org-mode-hook #'org-bullets-mode)
;;       (setq org-bullets-bullet-list '("⚫" "◆" "◉" "▶" "◇"))
;;       )
(setq-default org-list-indent-offset 4)
(use-package org-superstar              ; supersedes `org-bullets'
  :ensure
  :after org
  :config
  ;; Every non-TODO headline now have no bullet
  (setq org-superstar-headline-bullets-list '("\u200b"))
  (setq org-superstar-leading-bullet "\u200b")
  (setq org-superstar-item-bullet-alist
	'((?+ . ?+)
	  (?* . ?➤)
	  (?- . ?•)))
  ;; Enable custom bullets for TODO items
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-todo-bullet-alist
	'(("TODO" "☐")
	  ("NEXT" "✒")
	  ("HOLD" "✰")
	  ("WAIT" "☕")
	  ("CXLD" "✘")
	  ("DONE" "✔")))
  (org-superstar-restart))
;; (setq org-ellipsis "⤵")
;; (setq org-ellipsis "▼")

;; simple version, only change font size
;; (custom-set-faces
;;   '(org-level-1 ((t (:inherit outline-1 :height 1.75))))
;;   '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
;;   '(org-level-3 ((t (:inherit outline-3 :height 1.25))))
;;   '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
;;   '(org-level-5 ((t (:inherit outline-5))))
;;   '(org-level-6 ((t (:inherit outline-6))))
;;   '(org-level-7 ((t (:inherit outline-7))))
;;   '(org-document-title ((t (:height 2.0 :underline nil))))
;; )

;; complex version: change font as well
(add-hook 'org-mode-hook 'visual-line-mode)
(with-eval-after-load 'org
  (setq word-wrap t)
  )

;; check (custom-theme-set-faces) in the appearance section

;; (custom-set-faces
;;   ;; '(org-level-1 ((t (:inherit outline-1 :height 1.75))))
;;   ;; '(org-document-title ((t (:height 2.0 :underline nil))))
;;   '(mu4e-view-face ((t (:inherit default :height 1.2))))
;; )

;; (setq org-ellipsis "⤵")

(defvar mv-iframe-format
  ;; You may want to change your width and height.
  (concat "<video"
	  " height=\"400\""
      " autoplay"
	  " style=\"display:block; margin: 0 auto;\" controls>"
	  " <source"
	  " src=\"%s\""
	  " type=\"video/mp4\">"
	  "</video>"))

;; " <figcaption> \"%s\" </figcaption> "

(org-add-link-type
 "mv"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
	    handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format mv-iframe-format
		   path (or desc "")))
     (latex (format "\href{%s}{%s}"
		    path (or desc "video"))))))

(defvar audio-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe"
	  " width=\"600\""
	  " height=\"60\""
	  " style=\"display:block; margin: 0\""
	  " src=\"%s\">"
	  "</iframe>"))

(org-add-link-type
 "audio"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
	    handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format audio-iframe-format
		   path (or desc "")))
     (latex (format "\href{%s}{%s}"
		    path (or desc "audio"))))))

;; <tab> for 'indent-for-tab-command'
;; (evil-define-key 'insert org-mode-map (kbd "C-t") #'indent-for-tab-command)

;; load shared .el followed by Emacs specific config
;;(load-file "~/.my-elips/org.el")

;; (require 'org-mu4e)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)
   (C . t)
   (shell . t)
   (ipython . t)
   (perl . t)))

;; disable confirmation
(setq
 org-export-babel-evaluate nil
 org-confirm-python-evaluate nil
 org-confirm-babel-evaluate nil
 org-confirm-sh-evaluate nil
 org-confirm-C++-evaluate nil
 )
(setq org-babel-python-command "/Users/chongchonghe/anaconda3/bin/python")
(setq org-babel-sh-command "zsh")

;; (require 'ob-shell)

(setq org-src-preserve-indentation t)

(evil-define-key 'normal org-mode-map (kbd "t") 'org-todo)
(evil-define-key 'normal org-mode-map (kbd "C-t") 'org-todo-list)
(evil-define-key 'normal org-mode-map (kbd "C-t") 'org-todo-list)
(define-key evil-normal-state-map (kbd "C-a") 'org-agenda)
(setq org-directory "~/Dropbox/orgfiles")
(setq org-deadline-warning-days 7)

(with-eval-after-load 'org
  ;; (setq org-directory "/Users/chongchonghe/Dropbox/orgfiles")
  ;; (setq org-agenda-files '("~/Dropbox/orgfiles/work.org"
  ;; 			   ;; "~/Dropbox/orgfiles/proj.org"
  ;; 			   "~/Dropbox/orgfiles/life.org"
  ;; 			   "~/Dropbox/orgfiles/y-journals.org"
  ;; 			   "~/Dropbox/orgfiles/done.org"
  ;; 			   "~/Dropbox/orgfiles/journal/"
  ;;                          ))
  (setq org-agenda-files '("~/Dropbox/orgfiles"))
  ;; (setq org-agenda-files '("~/Dropbox/orgfiles/new.org"))
  ;; (setq org-default-notes-file "~/Dropbox/orgfiles/new.org")
  ;; (setq org-agenda-files "~/Dropbox/orgfiles/agenda.org")
  (setq org-default-notes-file "~/Dropbox/orgfiles/work.org")
  (setq org-agenda-confirm-kill t)
  ;;open agenda in current window
  (setq org-agenda-window-setup (quote current-window))
  ;; (define-key org-agenda-mode-map (kbd "M-j") nil)
  ;; (define-key org-agenda-mode-map (kbd "M-k") nil)
  )

(defun my/buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Roboto Slab"
                                :height 180
                                :width normal))
  (buffer-face-mode))

(defun my/style-org-agenda()
  ;; (my/buffer-face-mode-variable)
  (set-face-attribute 'org-agenda-date nil :height 1.1)
  (set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
  (set-face-attribute 'org-agenda-date-weekend nil :height 1.1)
)

(add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

(setq org-agenda-breadcrumbs-separator " ❱ "
      ;; org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
      org-agenda-time-grid '((weekly today require-timed)
                             (800 1000 1200 1400 1600 1800 2000)
                             "......" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
      org-agenda-prefix-format '(
				 ;; %c category (filename) %e effor, %t the HH:MM time-of-day, %s scheduling/deadline
				 (agenda . " %i %-10:c%?-16t%-4e% s")
				 ;; (agenda . " %i %12t  %4e % s")
				 (todo . " %i %-10:c")
				 (tags . " %i %-10:c")
				 (search . " %i %-10:c"))
)

(setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                    "\n"
                                                    (org-agenda-format-date-aligned date))))
(setq org-cycle-separator-lines 2)

(setq org-agenda-span 7
    org-agenda-start-on-weekday nil
    ;; org-agenda-start-day "-3d"
    )

;; ; ;; org-agenda
;; (use-package org-projectile
;;   :bind (("C-c n p" . org-projectile-project-todo-completing-read)
;; 	   ;; ("C-c c" . org-capture)
;; 	   ;; ("C-c a" . org-agenda)
;; 	   )
;;   :config
;;   (progn
;;     (setq org-projectile-projects-file "~/Dropbox/orgfiles/tasks.org")
;;     (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
;;     (push (org-projectile-project-todo-entry) org-capture-templates))
;;   :ensure t)

(use-package org-super-agenda
  :defer nil
  :after org-agenda
  :config
  ;; (setq org-super-agenda-groups
  ;; 	'((:auto-category t)
  ;; 	  (:discard (:anything t)))
  ;; 	)
  (org-super-agenda-mode)
  ;; (setq org-super-agenda-groups
  ;; 	'((:auto-group t)))
  )

(defun my-todo-by-projects ()
  (interactive)
  (let ((org-super-agenda-groups
	 '((:auto-group t))))
    (org-todo-list))
  )

(defun my-agenda-by-today ()
  (interactive)
  (let ((org-super-agenda-groups
	 '(
	   (:name "Schedule"
		  :time-grid t)
	   (:name "Today"
		  :scheduled today
		  :scheduled past)
	   (:name "Due soon"
		  :deadline future
		  :order 1)
	   (:name "Action"
		  :tag "Action")
	   )))
    (org-agenda-list))
  )

(defun my-agenda-by-today2 ()
  (interactive)
  (let ((org-super-agenda-groups
	 '((:log t)  ; Automatically named "Log"
           (:name "Schedule"
		  :time-grid t)
           (:name "Today"
                  :scheduled today)
           (:name "Overdue"
		  :face (:background "pink")
                  :deadline past)
           (:habit t)
           (:name "Due today"
                  :deadline today)
           (:name "Due soon"
                  :deadline future)
           (:name "Unimportant"
                  :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
                  :order 100)
           (:name "Waiting..."
                  :todo "WAITING"
                  :order 98)
           (:name "Scheduled earlier"
                  :scheduled past))))
    (org-agenda-list))
)

(defun my-ttt ()
  (interactive)
  (let ((org-super-agenda-groups
	 '((:log t)
	   (:name "Sche"
		  :time-grid t))))
    (org-agenda-list))
)

(defun my-todo-tag-low-energy ()
  (interactive)
  (let ((org-super-agenda-groups
	 '(
	   (:name "Lower Energy"
		  :tag "lowEnergy")
	   (:discard (:anything t))
	   )))
    (org-todo-list))
  )

(defun my-todo-tag-while-lunch ()
  (interactive)
  (let ((org-super-agenda-groups
	 '(
	   (:name "While having lunch"
		  :tag "lunch")
	   (:discard (:anything t))
	   )))
    (org-todo-list))
  )

(defun my-todo-tag-action ()
  (interactive)
  (let ((org-super-agenda-groups
	 '(
	   (:name "Action items"
		  :tag "action")
	   (:discard (:anything t))
	   )))
    (org-todo-list))
  )

;; (setq org-super-agenda-groups
  ;; 	'((:log t)  ; Automatically named "Log"
  ;;         (:name "Schedule"
  ;;                :time-grid t)
  ;;         (:name "Today"
  ;;                :scheduled today
  ;;                :deadline today
  ;;                :deadline past)
  ;;         (:name "Due soon"
  ;;                :deadline future)
  ;;         ;; (:name "Unimportant"
  ;;         ;;        :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
  ;;         ;;        :order 100)
  ;;         ;; (:name "Waiting..."
  ;;         ;;        :todo "WAITING"
  ;;         ;;        :order 98)
  ;;         ;; (:name "Scheduled earlier"
  ;;         ;;        :scheduled past)
  ;; 	  )
  ;; 	)

;; (setq org-super-agenda-groups
;;       '(
;; 	;; (:name "⏰ Calendar" :time-grid t)
;; 	(:name "Today" ; Optionally specify section name
;; 	       :time-grid t  ; Items that appear on the time grid
;; 	       :todo "TODAY") ; Items that have this TODO keyword
;; 	(:name "⚠ Overdue!" :deadline past)
;; 	(:name "⚠ Overdue!" :scheduled past)
;; 	(:name "⭐ Important" :priority "A")
;; 	(:name "Optional" :priority "D" :order 90)
;; 	(:auto-category t)))

;; (let ((org-super-agenda-groups
;;        '((:auto-category t))))
;;   (org-agenda-list))

(setq org-default-priority ?A)
(setq org-highest-priority ?A)
(setq org-lowest-priority ?D)
;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#FF0000" :weight bold))
			   (?B . (:foreground "#FF9815" :weight bold))
			   (?C . (:foreground "#68DF40"))
			   (?D . (:foreground "#11D3FF"))))
;;Different bullets
(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "DOIN(o!)" "WAIT(w!)" "FLUP(f!)" "REFI(r!)" "|" "(e!)" "SCHE(s!)" "CXLD(c!)" "DONE(d!)"))
      org-todo-keyword-faces
      '(("TODO" . (:foreground "magenta" :weight bold))
	("DOIN" . (:foreground "blue"))
	("FLUP" . (:foreground "orange"))
	("REFI" . (:foreground "#A52A2A"))
	;; ("CANCELLED" . (:foreground "white" :background "#4d4d4d" :weight bold))
	("CXLD" . (:foreground "gray"))
	("NEXT" . "#008080")
	("DONE" . "#333"))
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      )

(defun my-org-set-dark-todo-faces ()
  (setq org-todo-keyword-faces
	'(("TODO" . org-warning)
	  ("DOIN" . (:foreground "yellow"))
	  ("FLUP" . (:foreground "magenta"))
	  ("REFI" . (:foreground "#A52A2A"))
	  ;; ("CANCELLED" . (:foreground "white" :background "#4d4d4d" :weight bold))
	  ("CXLD" . (:foreground "gray"))
	  ("NEXT" . "#008080")
	  ("DONE" . "#333"))))

(defun my-org-set-light-todo-faces ()
  (setq org-todo-keyword-faces
	;; '(("TODO" . org-warning)
	;; '(("TODO" . (:foreground "purple"))
	'(("DOIN" . (:foreground "orange"))
	  ("FLUP" . (:foreground "magenta"))
	  ("REFI" . (:foreground "#A52A2A"))
	  ;; ("CANCELLED" . (:foreground "white" :background "#4d4d4d" :weight bold))
	  ("CXLD" . (:foreground "gray"))
	  ("NEXT" . "#008080")
	  ("DONE" . "#333"))))

(defun org-get-target-headline (&optional targets prompt)
  "Prompt for a location in an org file and jump to it.

This is for promping for refile targets when doing captures.
Targets are selected from `org-refile-targets'. If TARGETS is
given it temporarily overrides `org-refile-targets'. PROMPT will
replace the default prompt message.

If CAPTURE-LOC is is given, capture to that location instead of
prompting."
  (let ((org-refile-targets (or targets org-refile-targets))
        (prompt (or prompt "Capture Location")))
    (let ((org-refile-targets (or targets org-refile-targets))
          (prompt (or prompt "Capture Location")))
      (org-refile t nil nil prompt)))
    ;; (if org-capture-overriding-marker
    ;;     (org-goto-marker-or-bmk org-capture-overriding-marker)
    ;;   (org-refile t nil nil prompt)))
  )

(with-eval-after-load 'org
  (setq org-capture-templates
        '(("t" "Todo" entry (file+olp "" "Todos")
           "* TODO [#%^{Priority?||A|B|C|D}] %?\n %u\n\n\n" :empty-lines-after 1 :empty-lines-before 1)
          ("s" "Scheduled" entry (file+olp "" "Todos")
           "* [#%^{Priority?||A|B|C|D}] %^{Title}\nSCHEDULED: %^t\n%u\n%?\n\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ("i" "INBOX" entry (file+olp "" "INBOX")
           "* %^{Title}\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ("z" "project" entry (file+function "" org-get-target-headline)
           "* %^{Title}\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ("a" "Todo with link" entry (file+olp "" "Todos")
           "* TODO [#%^{Priority?||A|B|C|D}] %?\n %U\n %a\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ("m" "Meeting" entry (file+olp "" "Meetings")
           "* REFI Meeting with %? :MEETING:\n%U\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ("n" "Notes" entry (file+olp "" "General Notes")
           "* REFI %? :NOTE:\n%U\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ("l" "Learn" entry (file+olp "" "To Learn")
           "* %? \n\n" :empty-lines-before 1 :empty-lines-after 1)
          ;; ("l" "Life" entry (file+olp "~/Dropbox/orgfiles/life.org" "Todos")
          ;;  "* TODO [#%^{Priority?||A|B|C|D}] %?\nSCHEDULED: %^t %u\n\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ;; ("j" "Journal" entry (file+olp "~/Dropbox/orgfiles/y-journals.org" "2021")
          ;;  "** %?\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ;; ("o" "Doing" entry (file+olp "" "Todos")
          ;;  "* DOIN [#%^{Priority?||A|B|C|D}] %?\n %u\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ;; ("w" "Work task" entry (file+headline "" "Todos")
          ;;  "* TODO [#A] %?\n  %U\n  %a\n\n" :empty-lines-before 1)
          ;; ("s" "Scheduled" entry (file+olp "" "Todos")
          ;;  "* [#A] %^{Title}\nSCHEDULED: %^t\n%u\n%?\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ;; ("i" "Ideas" entry (file+olp "" "Ideas")
          ;;  "* %?\n  %u\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ;; ("e" "Event" entry (file+olp "" "Todos")
          ;;  "* %^{This is a?||TODO |NEXT |FLUP |DOIN |SCHE |REFI}%^{Title}\nSCHEDULED: %^t\n%t\n%?")
          ;; ("f" "Followup" entry (file+olp "" "Followups")
          ;;  "* FLUP [#B] %?\n  %U\n  %a\n\n" :empty-lines-before 1 :empty-lines-after 1)
          ;; ("w" "Work" entry (file+headline "" "Work")
          ;;  "* DOIN [#A] %? :WORK:\n%U\n\n" :empty-lines-before 1)
          ;; ("g" "General todo" entry (file+headline "/Users/chongchonghe/tasks.org" "Todos")
          ;;  "* TODO [#B] %?\n %a" :empty-lines 1)
          )))

(use-package org-journal
  :ensure t
  :defer t
  :bind
  ("s-j" . org-journal-new-entry)
  ("s-s" . org-journal-new-scheduled-entry)
  ("s-n" . org-journal-open-next-entry)
  ("s-p" . org-journal-open-previous-entry)
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq
   org-journal-prefix-key "M-g "
   org-journal-enable-agenda-integration t)
  :config
  (setq org-journal-dir (concat org-directory "/journal")
        org-journal-file-type 'weekly
        org-journal-file-format "%Y%m%d.org"
        org-journal-date-format "%A, %Y-%m-%d"
        ))

;; (setq org-refile-targets '((("~/Dropbox/orgfiles")
;; 			    :maxlevel . 2)))
(setq org-refile-targets '((("~/Dropbox/orgfiles/work.org"
			     "~/Dropbox/orgfiles/notes.org"
			     )
			    :maxlevel . 1)))
;; (setq org-refile-targets '((("~/Dropbox/orgfiles/work.org"
;; 			     ;; "~/Dropbox/orgfiles/proj.org"
;; 			     "~/Dropbox/orgfiles/life.org"
;; 			     "~/Dropbox/orgfiles/y-journals.org")
;; 			    :maxlevel . 2)))

(add-to-list 'load-path "~/path/to/org-present")
(autoload 'org-present "org-present" nil t)

(setq org-present-text-scale 2)

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)
                 ;;(toggle-frame-fullscreen)
                 ))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)
                 ;;(toggle-frame-fullscreen)
                 ))))

;; (with-eval-after-load 'org
;;   (use-package org-toggl
;;     :init
;;     (setq toggl-auth-token "ce3e8fc3922edda6986a6e729509338f")
;;     (setq org-toggl-inherit-toggl-properties t)
;;     :load-path "/Users/chongchonghe/dotfiles/emacs/packages"
;;     :config
;;     (toggl-get-projects)
;;     (org-toggl-integration-mode)
;;     ;; remove clock-out since it failed at stopping toggl timer
;;     (remove-hook 'org-clock-out-hook #'org-toggl-clock-out)
;;     ;; bind C-c i to clock-in then clock-out
;;     (define-key org-mode-map (kbd "C-c i")
;;       (lambda () (interactive) (org-clock-in) (sit-for 2) (org-clock-out)))
;;     )
;;   )

(use-package org-toggl
  :after org
  :defer 5
  :init
  (setq toggl-auth-token "ce3e8fc3922edda6986a6e729509338f")
  (setq org-toggl-inherit-toggl-properties t)
  (setq toggl-default-project "Research")
  :load-path "/Users/chongchonghe/emacs-dotfile/packages/org-toggl"
  :config
  (toggl-get-projects)
  (org-toggl-integration-mode)
  (define-key org-mode-map (kbd "C-c I") 'toggl-select-default-project)

  ;; remove clock-out since it failed at stopping toggl timer
  (remove-hook 'org-clock-out-hook #'org-toggl-clock-out)
  ;; bind C-c i to clock-in then clock-out
  (define-key org-mode-map (kbd "C-c i")
    (lambda () (interactive) (org-clock-in) (sit-for 5) (org-clock-out)))

  ;; This is the originial setup. Need to invoke clock-in and clock-out to
  ;; start and stop a projection
  ;; (add-hook 'org-clock-out-hook #'org-toggl-clock-out)
  ;; (define-key org-mode-map (kbd "C-c i") 'org-clock-in)
  ;; (define-key org-mode-map (kbd "C-c o") 'org-clock-out)

  )

(setq org-file-apps
      '(("\\.docx\\'" . default)
	("\\.mm\\'" . default)
	("\\.x?html?\\'" . default)
	("\\.pdf\\'" . default)
	("\\.md\\'" . default)
	("\\.png\\'" . default)
	("\\.gif\\'" . default)
	(auto-mode . emacs))
      )

(use-package exec-path-from-shell
  :defer 20
:ensure t
:config
(when (memq window-system '(mac ns x))
(exec-path-from-shell-initialize)))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

;; (use-package ox-md)
(setq org-export-backends '(ascii html md icalendar latex odt)
      org-use-sub-superscripts '{}
      org-export-with-sub-superscripts '{}
      org-export-with-broken-links "mark"
      ;; org-export-with-toc nil
      )

(defun my-org-html-postamble (plist)
  (concat "<p>Author: Chong-Chong He</p>"
	  (format "<p>Last updated: %s</p>" (format-time-string "%Y-%b-%d"))
	  "<a href='https://www.astro.umd.edu/~chongchong/'>www.astro.umd.edu/~chongchong/</a>"
	  ))
(setq org-html-postamble 'my-org-html-postamble)

;; (setq org-export-html-postamble-format 
;; 	      '(("en" "<p class=\"author\">Author: %a (%e)</p><p class=\"date\">Last Updated %d.</p>")))

;; (setq org-html-postamble-format
;; ;; (setq org-export-html-postamble-format 
;;       '(("en" "<p class=\"author\">Author: %a (%e)</p>
;; Last updated: <p class=\"date\">Date: %d</p>
;; <p class=\"creator\">Generated by %c</p>
;; <p class=\"xhtml-validation\">%v</p>
;; ")))

(setq org-html-validation-link nil)

(defun my-website-html-postamble (options)
(concat "<hr>"
		(if (and (plist-get options ':keywords) (not (string= (plist-get options ':keywords) "")))
			(format "<p>Keywords: %s</p>" (plist-get options ':keywords))
			"")
		(format "<p class=\"date\">Modified: %s</p>" (format-time-string "%Y-%m-%d %H:%M:%S"))
		(format "<p>Copyright (c) %s %s</p>"
				(car (split-string (car (plist-get options ':date)) "-")) ;; TODO: get from custom document option
				(car (plist-get options ':author)))
		(format "<p>%s</p>" (plist-get options ':creator))))

(require 'ox-publish)

;; inline image size
(setq org-image-actual-width 200)
(setq org-html-validation-link nil)
(defun my-org-html-postamble (plist)
  (concat "<hr>" "<p>Author: <a href='https://www.astro.umd.edu/~chongchong/'>Chong-Chong He</a></p>"
	  (format "<p>Last updated: %s</p>" (format-time-string "%Y-%m-%d"))
	  ))
(setq org-html-postamble 'my-org-html-postamble)

;; sitemap function
(defun my-org-publish-org-sitemap (title list)
  "Sitemap generation function."
  (concat (format "#+TITLE: %s\n" title)
	  "#+HTML_HEAD: <link rel='stylesheet' type='text/css' href='https://gongzhitaao.org/orgcss/org.css'/>\n"
	  "#+options: toc:nil\n"
          (org-list-to-subtree list)
          "\n\n"
          ;; (@-load-file-contents (expand-file-name "~/.emacs.d/aboutme.org"))
          )
  )

(setq org-publish-project-alist
      '(("body"
	 ;; generic
         :base-directory "."
         :base-extension "org"
	 :exclude "README.org"
         :publishing-directory "../public"
         :recursive nil
	 :language en
	 :with-latex t
         :publishing-function org-html-publish-to-html
	 :html-doctype "html5"
	 :html-style nil
         :html-html5-fancy t
	 :html-link-home "../index.html"
	 :html-link-up "./index.html"
	 ;; sitemap
	 :auto-sitemap t
	 :sitemap-filename "index.org"
	 :sitemap-title "Index"
	 :sitemap-function my-org-publish-org-sitemap
	 :html-home/up-format "<div> <a accesskey='h' href='index.html'> UP </a> | <a accesskey='H' href='index.html'> HOME </a> </div>"
	 )
	;; ("css"
        ;;  :base-directory "style/"
        ;;  :base-extension "css\\|js\\|setup"
        ;;  :publishing-directory "public/css"
        ;;  :publishing-function org-publish-attachment
        ;;  :recursive t)
	("attach"
         :base-directory "../attach/"
         :base-extension "png\\|jpg\\|ico\\|pdf"
         :publishing-directory "../public/attach"
         :publishing-function org-publish-attachment
         :recursive t)
	("all" :components ("body" "attach"))))

;; (use-package oc-csl
;;   :defer nil
;;   :config
;;   (setq org-cite-csl-styles-dir (list "/Users/chongchonghe/dotfiles/emacs/packages/csl-styles"))
;;   )
(use-package citeproc
  :defer 6
  :config
  (require 'oc-csl)
  )

(use-package plain-org-wiki
  :ensure t
  :load-path "/Users/chongchonghe/dotfiles/emacs/packages/plain-org-wiki/"
  :config
  (setq plain-org-wiki-directory "~/org/wiki/org")
  (setq plain-org-academic-directory "~/org/astronomy/org")
  (setq plain-org-wiki-extra-dirs '("~/org/astronomy/org"))
  (global-set-key (kbd "C-M-w") 'plain-org-wiki))

(org-add-link-type "message"
 (lambda (id)
  (shell-command
   ;; (concat "open -a mail.app message:" id))))
   (concat "open message:" id))))

(use-package org-fancy-priorities
  :diminish
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

(use-package ox-reveal)
(setq org-reveal-root "/Users/chongchonghe/local/reveal.js-master")

(use-package org-preview-html
  :defer 20
)

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

(use-package ox-gfm)
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-liveorg"))
;; (require 'liveorg)
(use-package liveorg
  :defer 5
  :load-path "~/.emacs.d/emacs-liveorg"
  :config
  (setq liveorg-browser "Safari.app")
  (define-key org-mode-map "\C-cE" 'liveorg-preview)
  )

(use-package org-inline-pdf
  :ensure t
  :defer 5
  :mode ("\\.org\\'" . org-mode)
  :config
  (add-hook 'org-mode-hook #'org-inline-pdf-mode))

;; This setup is tested on Emacs 24.3 & Emacs 24.4 on Linux/OSX
;; org v7 bundled with Emacs 24.3
(setq org-export-odt-preferred-output-format "doc")
;; org v8 bundled with Emacs 24.4
(setq org-odt-preferred-output-format "doc")
;; BTW, you can assign "pdf" in above variables if you prefer PDF format
;; "doc" for word document

;; Only OSX need below setup
(defun my-setup-odt-org-convert-process ()
  (interactive)
  (let ((cmd "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
    (when (and (eq system-type 'darwin) (file-exists-p cmd))
      ;; org v7
      (setq org-export-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))
      ;; org v8
      (setq org-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i"))))
    ))
(my-setup-odt-org-convert-process)

(setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
(setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(org-element-update-syntax)
;; 规定上下标必须加 {}，否则中文使用下划线时它会以为是两个连着的下标
(setq org-use-sub-superscripts "{}")

(use-package org-fragtog
  :defer 10
  )

(defun my-org-link-info (link)
  (interactive)
  (let ((path (org-element-property :path link))
        (type (org-element-property :type link))
        (desc (substring-no-properties (nth 2 link))))
    (list type path desc)))

(defun my-org-links ()
  (interactive)
(org-element-map (org-element-parse-buffer) 'link
  (lambda (link)
    (when (string= (org-element-property :type link) "file")
      (org-element-property :path link))))
)

(use-package yasnippet
  :defer 3
  :diminish yas-minor-mode
  :init
  (setq-default mode-require-final-newline nil)
  ;; :init (yas-global-mode)
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/dotfiles/emacs/snippets/yasnippet-snippets-20210105.1346/snippets" 
                           "~/dotfiles/emacs/snippets/personal"))
  (yas-global-mode 1)
  (yas-reload-all)
  ;; (progn
  ;;   (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  ;;   ;; (setq yas-key-syntaxes '("w_" "w_." "^ "))
  ;;   (setq yas-installed-snippets-dir "~/dotfiles/emacs/snippets")
  ;;   (setq yas-expand-only-for-last-commands nil))
  ;; snippet definitions, only KEY and TEMPLATE are actually mandatory.
  ;; (KEY TEMPLATE NAME CONDITION GROUP EXPAND-ENV LOAD-FILE KEYBINDING UUID SAVE-FILE) 
  (yas-define-snippets 'python-mode 
                       '(("np" "import numpy as np\n$0" "numpy")
                         ("plt" "import matplotlib.pyplot as plt\n$0" "plt")
                         ("rg" "np.arange(${1:start}, ${2:stop}, ${3:step})" "arange")
                         )) 
  (yas-define-snippets 'latex-mode 
                       '(("ct" "\\cite{$0}" "cite")
                         ("ctp" "\\citep{$0}" "citep")
                         )) 
  )

(use-package helm-flx
  :init (helm-flx-mode +1))

(use-package helm
  ;; :bind
  ;; (("C-M-z" . helm-resume)
  ;;  ("C-x C-f" . helm-find-files)
  ;;  ("C-h b" . helm-descbinds)
  ;;  ("C-x C-r" . helm-mini)
  ;;  ("C-x M-o" . helm-occur)
  ;;  ("M-y" . helm-show-kill-ring)
  ;;  ("C-h a" . helm-apropos)
  ;;  ("C-h m" . helm-man-woman)
  ;;  ("M-g >" . helm-ag-this-file)
  ;;  ("M-g ," . helm-ag-pop-stack)
  ;;  ("M-g ." . helm-do-grep)
  ;;  ("C-x C-i" . helm-semantic-or-imenu)
  ;;  ("M-x" . helm-M-x)
  ;;  ("C-x C-b" . helm-buffers-list)
  ;;  ("C-x C-r" . helm-mini)
  ;;  ("C-x b" . helm-mini)
  ;;  ("C-h t" . helm-world-time))
  :init
  (helm-mode 1)
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  ;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  ;; (global-set-key (kbd "M-C-b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "M-C-o") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB work in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  ;; unknonw
    ;; (setq helm-split-window-inside-p t
    ;;       helm-move-to-line-cycle-in-source t)
    ;; (setq helm-autoresize-max-height 0)
    ;; (setq helm-autoresize-min-height 20)
    ;; (helm-autoresize-mode 1)
  )

(defun my-turn-on-elpy-mode ()
  (interactive)
  (elpy-enable)
  (elpy-mode))

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  ;; :hook hs-minor-mode
  :bind (:map python-mode-map
	      ("C-c C-c" . compile)
	      ("s-e" . my-turn-on-elpy-mode)
	      )
  :config
  (setq python-indent-offset 4)
  (setq electric-indent-mode 1)
  (setq python-shell-interpreter "/Users/chongchonghe/anaconda3/bin/python3")
  (define-key python-mode-map (kbd "C-c C-z") 'run-python)
  (define-key python-mode-map (kbd "<backtab>") 'python-back-indent)
  (setq python-python-command "/Users/chongchonghe/anaconda3/bin/python")
  (defun my-insert-comments (string)
    "Insert \label{ARG} \index{\nameref{ARG}} at point"
    (interactive "sString for \\label and \\nameref: ")
    (insert "##### "  string  " #####"))
  (define-key python-mode-map (kbd "<f5>") 'my-insert-comments)
  (defun my-insert-comments-block (string)
    "Insert \label{ARG} \index{\nameref{ARG}} at point"
    (interactive "sString for \\label and \\nameref: ")
    (insert "# {{{ "  string  " 
  # }}}"))
  (define-key python-mode-map (kbd "<f6>") 'my-insert-comments-block)
  )
(add-hook 'python-mode-hook 'hs-minor-mode)

;; (use-package jedi
;; 	      :ensure t)
;; (setq elpy-rpc-backend "jedi")

(use-package flycheck)

(use-package elpy
  :bind
  (:map elpy-mode-map
	("C-M-n" . elpy-nav-forward-block)
	("C-M-p" . elpy-nav-backward-block))
  :hook ((elpy-mode . flycheck-mode))
  ;; :init
  ;; (elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
					; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-timeout 30)		; elpy autopep8 call timeout
  ) 
;; (use-package elpy
;;   :ensure t
;;   :commands elpy-enable
;;   :init (with-eval-after-load 'python (elpy-enable))
;;   )

(use-package anaconda-mode)

(defun set-selective-display-dlw (&optional level)
"Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

(use-package julia-mode
  :bind ("C-c C-c" . set-selective-display-dlw)
)

(defun my-CC++-config ()
  (setq-default c-basic-offset 4)
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (and (derived-mode-p 'c-mode 'c++-mode 'java-mode) (require 'ggtags nil 'noerror))
		(ggtags-mode 1))
	      (global-set-key (kbd "M-j") 'windmove-down)
	      ))
  ;; (define-skeleton 'c++-throwaway
  ;;   "Throwaway C skeleton"
  ;;   nil
  ;;   "#include <iostream>\n"
  ;;   "#include <string>\n"
  ;;   "#include <fstream>\n"
  ;;   "\n"
  ;;   "using namespace std;\n"
  ;;   "\n"
  ;;   "int main(void){\n"
  ;;   "  \n"
  ;;   "  return 0;\n"
  ;;   "}\n")
  ;; '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++-throwaway")
  (eval-after-load 'autoinsert
    '(define-auto-insert
       '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
       '("Short description: "
	 "/*" \n
	 (file-name-nondirectory (buffer-file-name))
	 " -- " str \n
	 " */" > \n \n
	 "#include <iostream>" \n \n
	 "using namespace std;" \n \n
	 "int main()" \n
	 -4 "{" \n
	 > _ \n
	 > _ "return 0;" \n
	 -4 "}" > \n)))
  ;; https://www.emacswiki.org/emacs/AutoInsertMode
  (eval-after-load 'autoinsert
    '(define-auto-insert '("\\.c\\'" . "C skeleton")
       '(
	 "Short description: "
	 "/**\n * "
	 (file-name-nondirectory (buffer-file-name))
	 " -- " str \n
	 "*" \n
	 "* Written on " (format-time-string "%a, %e %b %Y.") \n
	 "*/" > \n \n
	 "#include <stdio.h>" \n
	 \n
	 "int main()" \n
	 "{" > \n
	 > _ \n
	 "}" > \n)))
  )

(add-hook 'C++-mode-hook 'my-CC++-config)

(defun my-fortran-config ()
  (require 'f90-namelist-mode)
  (autoload 'f90-mode "f90" "Fortran 90 mode" t)
  (defun my-f90-mode-hook ()
    (setq f90-font-lock-keywords f90-font-lock-keywords-3)
    (abbrev-mode 1)                       ; turn on abbreviation mode
    (turn-on-font-lock)                   ; syntax highlighting
    (auto-fill-mode 0))                   ; turn off auto-filling
  (add-hook 'f90-mode-hook 'my-f90-mode-hook)
  (add-hook 'f90-mode-hook 'auto-fill-mode)
  (add-to-list 'auto-mode-alist '("\\.f9\\'" . f90-mode))
  ;; From https://gist.github.com/aradi/68a4ff8430a735de13f13393213f0ea8
  ;;
  ;; Add this settings to your ~/.emacs file
  ;;
  ;; Fortran settings
  (setq fortran-continuation-string "&")
  (setq fortran-do-indent 2)
  (setq fortran-if-indent 2)
  (setq fortran-structure-indent 2)
  ;; Fortran 90 settings
  (setq f90-do-indent 2)
  (setq f90-if-indent 2)
  (setq f90-type-indent 2)
  (setq f90-program-indent 2)
  (setq f90-continuation-indent 4)
  (setq f90-smart-end 'blink)
  ;; Set Fortran and Fortran 90 mode for appropriate extensions
  (setq auto-mode-alist
	(cons '("\\.F90$" . f90-mode) auto-mode-alist))
  (setq auto-mode-alist
	(cons '("\\.pf$" . f90-mode) auto-mode-alist))
  (setq auto-mode-alist
	(cons '("\\.fpp$" . f90-mode) auto-mode-alist))
  (setq auto-mode-alist
	(cons '("\\.F$" . fortran-mode) auto-mode-alist))
  ;; Swap Return and C-j in Fortran 90 mode
  (add-hook 'f90-mode-hook
	    '(lambda ()
	       (define-key f90-mode-map [return] 'f90-indent-new-line)
	       (define-key f90-mode-map "\C-j" 'newline)
	       (setq fill-column 100)
	       (abbrev-mode)
	       (setq-default indent-tabs-mode nil)
	       (setq whitespace-line-column 100)
	       (setq whitespace-style '(face tabs lines-tail empty))
	       (whitespace-mode)
	       ;; (add-to-list 'write-file-functions 'delete-trailing-whitespace)
	       )
	    )
  )

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown")
)
(eval-after-load 'markdown-mode
'(progn
  (define-key evil-normal-state-map (kbd "TAB") 'markdown-cycle)
(define-key markdown-mode-map (kbd "M-n") 'markdown-outline-next)
(define-key markdown-mode-map (kbd "M-p") 'markdown-outline-previous))
)



(add-hook 'text-mode-hook
        '(lambda ()
           (setq indent-tabs-mode t)
           (setq tab-width 4)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

;; Getting emacs to use the 'Marked' app
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked\\ 2.app %s"
       (shell-quote-argument (buffer-file-name))))
)
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "C-c m") 'markdown-preview-file))
(defalias 'marked 'markdown-preview-file)

(use-package latex
  :defer t
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :bind
  (:map LaTeX-mode-map
	("M-n" . outline-next-heading)
	("M-p" . outline-previous-heading)
	("C-c C-c" . TeX-command-run-all)
	("C-c l" . TeX-error-overview)
	;; ((kbd "C-tab") . TeX-complete-symbol) 
	("C-c w" . juanjo:textcount)
	("'" . nil)
	)
  :config
  (setq TeX-auto-save t)
  (setq TeX-auto-save t)
  (setq TeX-PDF-mode t)	      ;; Compile documents to PDF by default
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  ;; (setq-default TeX-master nil) ;; Make emacs aware of multi-file projects
  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'no-auto-fill)
  (add-hook 'LaTeX-mode-hook 'hs-minor-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (evil-define-key 'normal outline-minor-mode-map (kbd "SPC") 'evil-toggle-fold)
  ;; (setq reftex-plug-into-auctex t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (autoload 'helm-bibtex "helm-bibtex" "" t)
  (electric-pair-mode)
  ;; compile
  ;; (evil-define-key 'normal LaTeX-mode-map (kbd ", l") 'TeX-command-master)
  ;; do not query the user before saving each file with TeX-save-document
  (setq TeX-save-query nil) 
  (evil-define-key 'normal LaTeX-mode-map (kbd ", l") 'TeX-command-run-all)
  (evil-define-key 'normal LaTeX-mode-map (kbd ", v") 'TeX-view)
  (evil-define-key 'normal LaTeX-mode-map (kbd "M-w") 'LaTeX-fill-region)
  ;; sync
  ;; Enable the clicking feature of the sync
  (add-hook 'LaTeX-mode-hook
	    (lambda () (local-set-key (kbd "<S-s-mouse-1>") #'TeX-view))
	    )
  (setq TeX-PDF-mode t)	      ;; Compile documents to PDF by default
  ;; Use Skim as viewer, enable source <-> PDF sync
  ;; make latexmk available via C-c C-c
  ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (push
				'("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run latexmk on file")
				TeX-command-list)))
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
  ;; use Skim as default pdf viewer
  ;; Skim's displayline is used for forward search (from .tex to .pdf)
  ;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
	'(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  ;; keybindings
  ;; (define-key outline-mode-map [M-left] 'outline-hide-body)
  ;; (define-key outline-mode-map [M-right] 'outline-show-all)
  ;; (define-key outline-mode-map [M-up] 'outline-previous-heading)
  ;; (define-key outline-mode-map [M-down] 'outline-next-heading)
  ;; (define-key outline-mode-map [C-M-left] 'outline-hide-sublevels)
  ;; (define-key outline-mode-map [C-M-right] 'outline-show-children)
  ;; (define-key outline-mode-map [C-M-up] 'outline-previous-visible-heading)
  ;; (define-key outline-mode-map [C-M-down] 'outline-next-visible-heading)
  (defun turn-on-outline-minor-mode () (outline-minor-mode 1))
  (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
  (defun turn-on-flycheck-mode () (flycheck-mode 1))
  (add-hook 'LaTeX-mode-hook 'turn-on-flycheck-mode)
  )

(use-package reftex)
(setq reftex-default-bibliography
      '("/Users/chongchonghe/Academics/Bib/BIB_HE.bib"))
;; (setq reftex-default-bibliography
;; 	      '("/Users/chongchonghe/Academics/Bib/BIB_HE.bib",
;; 	"/Users/chongchonghe/Academics/Bib/Books.bib",
;; 	"/Users/chongchonghe/Academics/Bib/Bib_HE_PhD.bib"))

(setq reftex-external-file-finders
      '(("tex" . "/path/to/kpsewhich -format=.tex %f")
	("bib" . "/path/to/kpsewhich -format=.bib %f")))

;; CDLaTeX
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode

;; (defun my-turn-spell-checking-on ()
;;   "Turn flyspell-mode on."
;;   (flyspell-mode 1)
;;   )

;; (add-hook 'text-mode-hook 'my-turn-spell-checking-on)

;; (setq ispell-extra-args '("-W" "2" "--sug-mode=ultra"))
;; (setq ispell-extra-args '("--encoding=utf-8" "--sug-mode=ultra" "--lang=en_US"))
;; (setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-extra-args nil)

(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
;; use C-; instead
;; (global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  ;; (flyspell-auto-correct-word)
  (ispell-word)
  )
;; (global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
(global-set-key (kbd "C-:") 'flyspell-check-next-highlighted-word)
;; Enable flyspell mode for texts and flyspell-prog mode for C/Python...
;; https://stackoverflow.com/questions/15891808/how-to-enable-automatic-spell-check-by-default
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package math-preview)

(use-package windresize
	:defer t
	:bind
	("C-c w" . windresize)
	)

(defun my-html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (setq cmd "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org --wrap=none")
  (kill-new (shell-command-to-string cmd))
  (yank))

(define-key org-mode-map (kbd "s-V") #'my-html2org-clipboard)



(use-package transpose-frame
  :defer t)

(global-set-key (kbd "<f12>") 'next-buffer)
(global-set-key (kbd "<f11>") 'previous-buffer)

(fset 'my/shrink (kbd "C-u 39 C-x {"))

;; (use-package smooth-scroll
;;       :config
;;       (smooth-scroll-mode 1)
;;       (setq smooth-scroll/vscroll-step-size 5)
;;       )
;; (use-package smooth-scrolling
;; 	      :config
;; 	      (smooth-scrolling-mode 1))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
		(format "%s\\|%s"
					vc-ignore-dir-regexp
					tramp-file-name-regexp))
(setq tramp-verbose 1)
;; ;; tramp: speed up
;; (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

(setq server-use-tcp t
      server-port    9999)
(defun server-start-and-copy ()
  (server-start)
  (copy-file "~/.emacs.d/server/server" "/des:.emacs.d/server/server" t))
(add-hook 'emacs-startup-hook 'server-start-and-copy)

(add-hook 'hs-minor-mode-hook
	(lambda ()
		;;(local-set-key (kbd "C-c p") 'hs-toggle-hiding)
		;; (local-set-key (kbd "SPC") 'hs-toggle-hiding)
		(local-set-key (kbd "C-c h") 'hs-hide-all)
		(local-set-key (kbd "C-c s") 'hs-show-all)
		(local-set-key (kbd "C-c l") 'hs-hide-level)))
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(evil-define-key 'normal hs-minor-mode-map (kbd "SPC") 'hs-toggle-hiding)

;; (add-hook 'hs-minor-mode-hook 'my-hideshow-config)
;; (defun my-hideshow-config ()
;;       "For use in 'hs-minor-mode-hook'."
;;       ;;(local-set-key (kbd "C-c p") 'hs-toggle-hiding)
;;       ;; (local-set-key (kbd "SPC") 'hs-toggle-hiding)
;;       (local-set-key (kbd "C-c h") 'hs-hide-all)
;;       (local-set-key (kbd "C-c s") 'hs-show-all)
;;       (local-set-key (kbd "C-c l") 'hs-hide-level)
;;       )
;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
;; (evil-define-key 'normal hs-minor-mode-map (kbd "SPC") 'hs-toggle-hiding)

(defun taskinit ()
  (interactive)
  (split-window-right)
  ((kbd "C-u 10 C-x {"))
  (set-frame-height (selected-frame) 60)
  )

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Welcome! Work day.")
  ;; (setq dashboard-startup-banner "/path/to/image")
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 10)))
  (dashboard-setup-startup-hook)
  )

(use-package neotree
  :config
  (defun my-neotree-mode-config ()
    "For use in 'neotree-mode-hook'."
    ;; (local-set-key (kbd "j") 'neotree-next-line)
    ;; (local-set-key (kbd "k") 'neotree-previous-line)
    (local-set-key (kbd "C-j") 'neotree-change-root)
    (local-set-key (kbd "C-k") 'neotree-select-up-node)
    (local-set-key (kbd "<return>") 'neotree-enter)
    ;; (local-set-key (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    ;; (with-eval-after-load 'neotree
    ;;   (define-key neotree-mode-map (kbd "<return>") 'neotree-enter))
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
    (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
    (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
    )
  (add-hook 'neotree-mode-hook 'my-neotree-mode-config))

(add-hook 'neotree-mode-hook
	  (lambda () (define-key evil-motion-state-local-map (kbd "g") 'neotree-refresh)))

;; new
;; (global-set-key (kbd "C-x w") 'elfeed)
(use-package elfeed
  :ensure t
  :defer t
  :commands (elfeed)
  :bind ("C-x w" . elfeed)
  :config
  ;; (require 'elfeed)
  ;; (use-package elfeed
  ;;   :defer nil
  ;;   :ensure t
  ;;   :bind
  ;;   ("C-x w" . elfeed )
  ;;   :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
	elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-feeds
	'("http://export.arxiv.org/api/query?search_query=cat:astro-ph.GA&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending"
	  "http://export.arxiv.org/api/query?search_query=cat:astro-ph.SR&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending"
	  ))
  ;; (use-package elfeed-org
  ;;   :ensure t
  ;;   :config
  ;;   (setq rmh-elfeed-org-files (list "~/dotfiles/emacs/packages/elfeed/elfeed.org")))

  ;; https://www.reddit.com/r/emacs/comments/6z6xm6/comment/dmthpb7/?utm_source=share&utm_medium=web2x&context=3
  (add-to-list 'evil-motion-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-motion-state-modes 'elfeed-show-mode)
  ;; face for starred articles
  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")
  (push '(star elfeed-search-starred-title-face) elfeed-search-face-alist)
  ;; Default search filter
  (setq-default elfeed-search-filter "@1-week-ago -trash -read")

  (defun elfeed-tag-selection-as (mytag)
    "Returns a function that tags an elfeed entry or selection as
MYTAG"
    (lambda ()
      "Toggle a tag on an Elfeed search selection"
      (interactive)
      (elfeed-search-toggle-all mytag)))

  ;; https://emacs.stackexchange.com/a/2441/26582
  (defun elfeed-mark-all-as-read ()
    (interactive)
    (if (yes-or-no-p "Do you really want to mark everything as READ?")
	(progn
          (mark-whole-buffer)
          (elfeed-search-untag-all-unread))
      nil)
    )

  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))
  (defalias 'elfeed-toggle-starstar
    (elfeed-expose #'elfeed-search-toggle-all 'star2))
  (defalias 'elfeed-toggle-read
    (elfeed-expose #'elfeed-search-toggle-all 'unread))
  (defalias 'elfeed-mark-read
    (elfeed-expose #'elfeed-search-toggle-all 'read))
  (defalias 'elfeed-toggle-trash
    (elfeed-expose #'elfeed-search-toggle-all 'trash))
  (eval-after-load 'elfeed-search
    '(progn
       ;; (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)
       (define-key elfeed-search-mode-map (kbd "M") (elfeed-tag-selection-as 'starstar))
       (evil-define-key* 'motion elfeed-search-mode-map
         (kbd "RET") #'elfeed-search-show-entry
         "b" #'elfeed-search-browse-url
         "U" #'elfeed-update
         "m" #'elfeed-toggle-star
         "M" #'elfeed-toggle-starstar
         "r" #'elfeed-toggle-read
         "R" #'elfeed-mark-read
         "t" #'elfeed-toggle-trash
         "gr" #'elfeed-search-update--force
         "gR" #'elfeed-search-fetch)
       ))
  ;; (eval-after-load 'elfeed-search
  ;;   '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))

  ;; Managing ArXiv RSS Feeds in Emacs
  ;; Ref: https://cundy.me/post/elfeed/

  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
    (mapconcat
     (lambda (author) (plist-get author :name))
     authors-list ", "))

  (defun elfeed-search-format-date (date)
    (format-time-string "%m-%d" (seconds-to-time date)))

  (use-package elfeed-goodies)

  (defun my-search-print-fn (entry)
    "Print ENTRY to the buffer."

    (let* ((title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
	   (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
	   (date (elfeed-search-format-date (elfeed-entry-date entry)))
	   (feed (elfeed-entry-feed entry))
	   (feed-title
	    (when feed
	      (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
	   (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
	   (tags-str (concat "[" (mapconcat 'identity tags ",") "]"))
	   (title-width (- (window-width) elfeed-goodies/feed-source-column-width
			   elfeed-goodies/tag-column-width 4))
	   (title-column (elfeed-format-column
			  title (elfeed-clamp
				 elfeed-search-title-min-width
				 title-width
				 title-width)
			  :left))
	   (tag-column (elfeed-format-column
			tags-str 13
			:left))
	   (feed-column (elfeed-format-column
			 feed-title 41
			 :left))
	   (entry-authors (concatenate-authors
			   (elfeed-meta entry :authors)))
	   (authors-column (elfeed-format-column
			    entry-authors 24
			    :left))
	   )

      (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
	  (progn
	    (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
	    (insert (propertize date 'face 'elfeed-search-date-face) " ")
            (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
            (insert (propertize authors-column
				'face 'elfeed-search-date-face
				'kbd-help entry-authors) " ")
            (insert (propertize title 'face title-faces 'kbd-help title))
            )
	(insert (propertize title 'face title-faces 'kbd-help title)))))

  (setq elfeed-search-print-entry-function #'my-search-print-fn)

  )

;; (setq elfeed-show-mode-hook
;;       (lambda ()
;; 	(set-face-attribute 'variable-pitch (selected-frame)))
;; 	)

;; (defun my-show-elfeed (buffer)
;;   (with-current-buffer buffer
;;     (setq buffer-read-only nil)
;;     (goto-char (point-min))
;;     (re-search-forward "\n\n")
;;     (fill-individual-paragraphs (point) (point-max))
;;     (setq buffer-read-only t))
;;   (switch-to-buffer buffer))

;; (add-hook 'elfeed-show-mode-hook
;; 	  (lambda () (buffer-face-set 'variable-pitch)))

;; (add-hook 'elfeed-show-mode-hook 'variable-pitch-mode)

(defun ansi-term-post (&rest _)
  "configuration settings for ansi-term"
  (evil-local-mode -1))

;; (use-package matlab-mode)

(use-package yaml-mode
  :mode
  "\\.yml\\'"
)

;; (require 'epa-file)
;; (epa-file-enable)

(defun my-light-theme ()
  (interactive)
  (load-theme 'doom-one-light t)
  ;; (set-background-color "#e6e3df")
  ;; (set-background-color "#fffcf7")
  (set-background-color "#FFFDFB")
  (set-foreground-color "#000000")
  (my-org-set-light-todo-faces)
  )
(defun my-dark-theme ()
  (interactive)
  (load-theme 'doom-one t)
  (set-foreground-color "#eee")
  (my-org-set-dark-todo-faces)
  )
(defun my-white-background ()
  (interactive)
  (set-background-color "white")
  )
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-one-light t)
  (my-light-theme)
  )

;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;;;中文与英文字体设置
;; Setting English Font
;; (set-face-attribute
;;  'default nil :font "Monaco 14")
;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
		    charset (font-spec :family "STHeiti" :height 1.0)))
;; more options:
;; "PingFang SC": ugly
;; "STXihei": thicker

(set-face-attribute 'default nil :height 140)
(defun my-setfontsize (size)
  (interactive "sSet the fontsize (default 140): ")
  (set-face-attribute 'default (selected-frame) :height (string-to-number size))
  (message "Fontsize set to %s" size)
  )
(defun my-set-font-color-to-dark ()
  (interactive)
  (set-foreground-color "#000000")
  (message "Font color set to black")
  )

(when window-system (set-frame-size (selected-frame) 100 50))

;; (with-eval-after-load 'org
;;   (define-key org-agenda-mode-map (kbd "j") #'org-agenda-next-item)
;;   (define-key org-agenda-mode-map (kbd "k") #'org-agenda-previous-item))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c a") 'org-agenda-list)
;; (define-key yas-minor-mode-map (kbd "C-c t") nil)
;; (global-set-key (kbd "C-c t") 'org-todo-list)
;; ;; (bind-key* "C-c t" 'org-todo-list)
;; (global-set-key (kbd "<f9>") 'org-todo-list)
(defun my/kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(defun my/kill-all-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (buffer-list)))

(define-key org-mode-map (kbd "C-,") nil)
(define-key evil-normal-state-map (kbd "C-.") nil)
(define-key org-mode-map (kbd "C-.") nil)
(global-set-key (kbd "C-,") 'flyspell-goto-next-error)
(global-set-key (kbd "C-.") 'flyspell-auto-correct-word)
(global-set-key (kbd "C-=") 'ispell-word)

(defun my-org-open-current-window ()                                              
  "Opens file in current window."                                                  
  (interactive)                                                                    
  (let ((org-link-frame-setup (cons (cons 'file 'find-file) org-link-frame-setup)))
    (org-open-at-point)))                                                          
(define-key global-map (kbd "C-o") #'my-org-open-current-window)

;; (defun my-shrink ()
;;   (interactive)
;;   ;; (funcall (key-binding (kbd "C-u 39 C-x {")))
;;   ;; (call-interactively (key-binding (kbd "C-u 39 C-x {")))
;;   ;; (/ (loop repeat 39 collect (key-binding (kbd "C-u 39 C-x {"))))
;;   ;; (/ (loop repeat n sum (funcall f arg)) n)
;;   ;; (cl-loop repeat 39 (shrink-window-horizontally))
;;   ;; (r 39 'shrink-window-horizontally 'nil')
;;   'shrink-window-horizontally
;;   'shrink-window-horizontally
;;   'shrink-window-horizontally
;;   'shrink-window-horizontally
;;   )

;; Saving Emacs Sessions
;; (desktop-save-mode 1)

;; (fset 'my-shrink (kbd "C-u 43 C-x {"))
(defun my-shrink ()
  (interactive)
  ;; (shrink-window-horizontally 60)
  (evil-window-set-width 80)
  )
(defun my-todo ()
  (interactive)
  (split-window-right)
  (evil-window-right 1)
  ;; (find-file "~/Dropbox/orgfiles/work.org")
  (org-agenda-list)
  (my-shrink)
  )

;; open agenda list
;; (org-agenda-list)

(load-file ~/emacs-dotfile/init-share.el)
