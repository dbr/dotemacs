;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trash files instead of unlinking them
(setq delete-by-moving-to-trash t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable startup screen
(setq inhibit-splash-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI stuff
(tool-bar-mode 0)
(scroll-bar-mode 0)
(display-battery-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sensible tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load paths
(add-to-list 'load-path "~/.emacs.d/lib/")

(require 'ibuffer-git) ;; http://jrockway.github.com/ibuffer-git/
(require 'magit) ;; http://zagadka.vm.bytemark.co.uk/magit/
(require 'textmate) ;; http://github.com/defunkt/textmate.el
(require 'paredit)


(ido-mode 1) ;; C-x b and C-x C-f etc to use shiner minibuf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start textmate-mode for Python
(add-hook 'python-mode-hook
'(lambda ()
	(interactive)
	(textmate-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ignore .DS_Store *.pyc files in ido file select etc
(add-to-list 'ido-ignore-files "\\.DS_Store")
(add-to-list 'ido-ignore-files "\\.pyc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recent files
(require 'recentf)
(setq recentf-auto-cleanup 'never) ; http://stackoverflow.com/questions/880625
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bind M-space to completion
; (global-set-key "\M- " 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window resizing shortcuts
(global-set-key [(super control right)] 'shrink-window-horizontally)
(global-set-key [(super control left)] 'enlarge-window-horizontally)
(global-set-key [(super control up)] 'shrink-window)
(global-set-key [(super control down)] 'enlarge-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Super-return makes new line after current
(defun make-newline-after-current()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key [(super return)] 'make-newline-after-current)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F1/F2 for cycling buffers
(global-set-key [f1] 'previous-buffer)
(global-set-key [f2] 'next-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Super key with arrows for jumping between windows
(require 'windmove)
(windmove-default-keybindings 'super)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
(add-to-list 'load-path "~/.emacs.d/lib/color-theme")
(add-to-list 'load-path "~/.emacs.d/lib/")
(require 'color-theme)
(require 'gunmetal)
(require 'zenburn)
(require 'color-theme-tango)
(require 'folio)
(require 'highlight-parentheses)

;; (color-theme-tango)
;; (color-theme-gunmetal)
;; (color-theme-folio)
;; (color-theme-zenburn)

(color-theme-gunmetal)

(setq daytheme 0)
(defun toggle-daynight ()
  (interactive)
  (if (= daytheme 1)
      (progn
       (color-theme-gunmetal)
       (setq daytheme 0)
       (message "set theme gunmetal, daytheme to 0"))
    (progn
      (color-theme-folio)
      (setq daytheme 1)
      (message "set theme folio, daytheme to 1"))))

(global-set-key [f5] 'toggle-daynight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake mode for Python
(when (load "flymake" t) 
  (defun flymake-pyflakes-init () 
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 
		       'flymake-create-temp-inplace)) 
	   (local-file (file-relative-name 
			temp-file 
			(file-name-directory buffer-file-name)))) 
      (list "pyflakes" (list local-file)))) 

  (add-to-list 'flymake-allowed-file-name-masks 
	       '("\\.py\\'" flymake-pyflakes-init))) 

(add-hook 'find-file-hook 'flymake-find-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake customisation (from http://paste.lisp.org/display/60617 )
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
	  (let ((err (car (second elem))))
	    (message "%s" (fly-pyflake-determine-message err)))))))

(defun fly-pyflake-determine-message (err)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
	((null (flymake-ler-file err))
	 ;; normal message do your thing
	 (flymake-ler-text err))
	(t ;; could not compile err
	 (format "compile error, problem on line %s" (flymake-ler-line err)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook))) 

(defun my-flymake-show-next-error()
  (interactive)
  (flymake-goto-next-error)
;;  (flymake-display-err-menu-for-current-line))
  )
(global-set-key "\C-c\ v" 'my-flymake-show-next-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move autosave/backup files to ~/.emacs.d/autosaves and ~/.emacs.d/backups
(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/autosaves/" t)))

;; Clean up old backup files
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message file)
      (delete-file file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer customisation
;; http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-saved-filter-groups
      '(("emacs-config" (or (filename . ".emacs.d")
                            (filename . "emacs-config")))
        ("Org" (mode . org-mode))
        ("Python" (mode . python-mode))
        ("Magit" (name . "\*magit"))
        ("Jabber" (name . "jabber\*"))
        ("Help" (or (name . "\*Help\*")
                    (name . "\*Apropos\*")
                    (name . "\*info\*")))))

(setq ibuffer-expert t)

(setq ibuffer-show-empty-filter-groups nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs auto-settings
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(hl-paren-colors (quote ("firebrick" "lightgreen" "orange" "cyan")))
 '(ibuffer-formats (quote ((mark modified read-only git-status-mini " " (name 18 18 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " (git-status 8 8 :left) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(org-agenda-files (quote ("~/org/main.org"))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
