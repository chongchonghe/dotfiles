;;; liveorg.el --- Realtime Markdown previews for Emacs.

;; Copyright (C) 2014-2016 Hrvoje Simic

;; Author: Hrvoje Simic <hrvoje@twobucks.co>
;; Version: 1.0.0
;; Keywords: markdown, preview, live
;; URL: https://github.com/shime/emacs-liveorg

;;; Commentary:

;; Realtime Markdown previews for Emacs.

;;; Code:

(defgroup liveorg nil
  "Realtime Markdown previews"
  :group 'liveorg
  :prefix "liveorg-")

(defcustom liveorg-port 1234
  "Port on which liveorg server will run."
  :type 'integer
  :group 'liveorg)

(defcustom liveorg-open t
  "Open browser automatically."
  :type 'boolean
  :group 'liveorg)

(defcustom liveorg-browser nil
  "Open alternative browser."
  :type 'string
  :group 'liveorg)

(defcustom liveorg-autostart nil
  "Auto-open previews when opening markdown files."
  :type 'boolean
  :group 'liveorg)

;;;###autoload
(defun liveorg-preview ()
    "Preview the current file in liveorg."
    (interactive)

   ;; (call-process-shell-command
   ;;           (format "livedown stop --port %s &"
   ;;                          liveorg-port))

        (start-process-shell-command
            (format "emacs-liveorg")
            (format "emacs-liveorg-buffer")
            (format "browser-sync start -s -f %s --no-notify --port %s %s --index %s "
		    (concat (file-name-base) ".html")
                    liveorg-port
                    (if liveorg-browser (concat "--browser " liveorg-browser) "")
		    (concat (file-name-base) ".html")
		    )
            ;; (format "http-server -o %s --port %s "
	    ;; 	    (concat (file-name-base) ".html")
            ;;         liveorg-port
	    ;; 	    )
	    )
        (print (format "%s rendered @ %s" buffer-file-name liveorg-port) (get-buffer "emacs-liveorg-buffer")))

;;;###autoload
(defun liveorg-kill (&optional async)
  "Stops the liveorg process."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer "emacs-liveorg-buffer"))
  )

(if liveorg-autostart
  (eval-after-load 'markdown-mode '(liveorg-preview)))

;; (add-hook 'kill-emacs-query-functions (lambda () (liveorg-kill t)))

(provide 'liveorg)
;;; livedown.el ends here
