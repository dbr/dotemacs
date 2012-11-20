;; Loads the config.org babel file, based on
;; http://orgmode.org/worg/org-contrib/babel/intro.html


;; Determine the directory containing this file
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; emacs 23 requires newer version of org-mode than default, e.g:
;; $ git clone git://orgmode.org/org-mode.git ext/org-mode/
;; A checkout of a tag like release_7.5 definitely works in emacs 23
(if (< emacs-major-version 24)
    (add-to-list 'load-path (expand-file-name
                             "lisp" (expand-file-name
                                     "org-mode" (expand-file-name
                                                 "ext" dotfiles-dir)))))

;; Load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)

;; Load up the main file
(org-babel-load-file (expand-file-name "config.org" dotfiles-dir))

