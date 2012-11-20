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


;; emacs 23 requires newer version of org-mode than default, e.g:
;; $ git clone git://orgmode.org/org-mode.git ext/org-mode/
;; A checkout of a tag like release_7.5 definitely works in emacs 23
(if (< emacs-major-version 24)
    (add-to-list 'load-path (dotemacs-joindirs dotfiles-dir "ext" "org-mode" "lisp")))


;; Load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)


;; Load up the main file
(org-babel-load-file (dotemacs-joindirs dotfiles-dir "config.org"))
