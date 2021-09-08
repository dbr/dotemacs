;; Loads the config.org babel file, originally based on
;; http://orgmode.org/worg/org-contrib/babel/intro.html

;; Helper function
(defun dotemacs-joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dotemacs-joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"

  (if (not dirs)
      root ; No more dirs to join, no more recursing
    (apply 'dotemacs-joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))


;; Determine the directory containing this file
(setq dotfiles-dir (file-name-directory (or load-file-name
                                            (buffer-file-name))))

;; Load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)


;; Load up the main file
(org-babel-load-file (dotemacs-joindirs dotfiles-dir "config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#454545" "#cd5542" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#bdbc61" "#bdbdb3"])
 '(custom-safe-themes
   '("3924f4d0cb873915a09cea3bdba3d378bb5fc575564b4c30093e18286ec4f395" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" default))
 '(package-selected-packages
   '(lsp-treemacs lsp-ui lsp-mode highlight-parentheses flycheck company stripe-buffer cmake-mode yaml-mode rust-mode magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
