;; (add-hook 'org-mode-hook 'turn-on-auto-fill)
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(setq compile-command "make ")

(setq org-hide-emphasis-markers t)

(set-default 'truncate-lines nil)
;; https://stackoverflow.com/a/950406
(add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))

;; close all buffers
(defun my-close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; evil mode, move visual line instead of real line
(setq evil-respect-visual-line-mode t)
;; Make horizontal movement cross lines                                    
(setq-default evil-cross-lines t)
;; ;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; (with-eval-after-load 'org
;;   ;; here goes your Org config
;;   )
(require 'org)
(setq-default org-startup-with-inline-images nil)

(eval-after-load 'ox
'(add-to-list 'org-export-options-alist '("toc" "toc" nil nil nil)))

(eval-after-load "org"
  '(require 'ox-gfm nil t))

(global-set-key (kbd "s-v") 'clipboard-yank)

;; set snippets directory
(yas-global-mode 1)
;; (setq auto-completion-private-snippets-directory "/startrek/chongchong/emacs-dotfile/shared/snippets/personal")
;; (setq yas-snippet-dirs (append yas-snippet-dirs '("/startrek/chongchong/emacs-dotfile/shared/snippets/personal")))
;; (setq auto-completion-private-snippets-directory "~/emacs-dotfile/shared/snippets/personal")
(setq yas-snippet-dirs (append yas-snippet-dirs '("~/dotfiles/spacemacs/snippets/personal")))
(yas-reload-all)

(defun python-args-to-google-docstring (text &optional make-fields)
"Return a reST docstring format for the python arguments in yas-text."
(interactive)
(let* ((indent (concat "\n" (make-string (current-column) 32)))
     (args (python-split-args text))
   (nr 0)
     (formatted-args
  (mapconcat
   (lambda (x)
     (concat "    " (nth 0 x)
       (if make-fields (format " ${%d:arg%d}" (cl-incf nr) nr))
       (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
   args
   indent)))
  (unless (string= formatted-args "")
  (concat
   (mapconcat 'identity
    (list "" "Args:" formatted-args)
    indent)
   "\n"))))

;; (require 'preview-dvisvgm)
(with-eval-after-load "latex"
  ;; here goes your Org config
  (define-key LaTeX-mode-map (kbd "C-c C-c") 'TeX-command-run-all)
  ;; not working
  (define-key LaTeX-mode-map (kbd "M-n") 'outline-next-heading)
  (define-key LaTeX-mode-map (kbd "M-p") 'outline-previous-heading)
  (setq TeX-save-query nil)
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-preview-latex-default-process 'dvisvgm)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  )
;; (setq org-preview-latex-default-process 'divpng)

;; (setq my:dvi-to-svg
;;     (my:dvi-to-svg :programs
;;         ("latex" "dvisvgm")
;;            :description "dvi > svg"
;;            :message "you need to install the programs: latex and dvisvgm."
;;            :use-xcolor t
;;            :image-input-type "dvi"
;;            :image-output-type "svg"
;;            :image-size-adjust (1.7 . 1.5)
;;            :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
;;            :image-converter ("dvisvgm %f -e -n -b min -c %S -o %O")))
;; (with-eval-after-load 'ox-latex
;;     (add-to-list 'org-preview-latex-process-alist my:dvi-to-svg)
;;     (setq org-preview-latex-default-process 'my:dvi-to-svg))

;; insert "\(" and "\)" on latex-mode when pressing an unmatched "$"
(setq-default TeX-electric-math (cons "\\( " " \\)"))
;; set electric-pair-mode to t in latex-mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq-local electric-pair-mode t)))

(defun my-remove-aux ()
  ;; list and remove all .aux files
  (interactive)
  (shell-command "ls *.aux")
  (shell-command "rm *.aux")
  )

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq indent-line-function 'insert-tab)))

;; accept completion from copilot and fallback to company

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(with-eval-after-load 'copilot
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(add-hook 'prog-mode-hook 'copilot-mode)

(define-key evil-insert-state-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
(define-key evil-insert-state-map (kbd "C-TAB") 'copilot-accept-completion-by-word)

(evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)

;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "M-n") #'org-next-visible-heading)
;;   (evil-define-key 'normal org-mode-map (kbd "M-n") #'org-next-visible-heading)
;;   )

;; (define-key org-agenda-mode-map (kbd "C-n") 'org-agenda-next-line)

(defun my-org-mode-config ()
  (local-set-key "\M-n" 'outline-next-visible-heading)
  (local-set-key "\M-p" 'outline-previous-visible-heading)
)
(add-hook 'org-mode-hook 'my-org-mode-config)

(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-K") 'kill-buffer-and-window)
(global-set-key (kbd "s-e") 'eval-region)
(global-set-key (kbd "s-b") 'eval-buffer)
(global-set-key (kbd "s-c") 'compile)
(global-set-key (kbd "s-r") 'recompile)
(global-set-key (kbd "s-,") 'previous-buffer)
(global-set-key (kbd "s-.") 'next-buffer)
;; (global-unset-key (kbd "s-j"))
;; (global-set-key (kbd "s-j") 'jump-to-register)
;; (global-set-key (kbd "M-v") 'evil-paste-after)

(require 'windmove)
(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)
(global-set-key (kbd "<S-s-down>") 'windmove-swap-states-down)
(global-set-key (kbd "<S-s-up>") 'windmove-swap-states-up)
(global-set-key (kbd "<S-s-left>") 'windmove-swap-states-left)
(global-set-key (kbd "<S-s-right>") 'windmove-swap-states-right)

(message "init-share.el sourced!!")
