## Emacs version

This is used with emacs 27

## To use

- `git clone` the repo to ~/.emacs.d
- Launch emacs

## Making customisations

All the configuring is done in `config.org` using org-babel blocks, e.g:

    #+begin_src emacs-lisp
    (require 'somemodule)
    #+end_src

These blocks are automatically evaluated when emacs is launched, using
`org-babel` - they can be organised using regular org-mode stuff

To enable customiations live, it's easiest to use `eval-last-sexp`
(`C-x C-e`) or `eval-region` on the code block

## External modules

Most third-party packages are installed via `package-install` and
committed to this repo.

There are updated via `M-x list-packages` then press `U` to mark
available updates for install, then perform the install with `x`.

Things which cannot be installed this way live in the `ext/` folder.

Singular `.el` live in the root of `ext/`, e.g:

    config.org
    ext/
      tempbuf.el
      markdown-mode.el

These can be loaded with `(require 'tempbuf)` as `ext/` is added to
the `load-path`

Code with multiple files live in a subdirectory of `ext/`,

    config.org
    ext/
      tempbuf.el
      markdown-mode.el
      magit/
        magit-bisect.el
        magit-blame.el
        ...
      org-mode/
        ...

Under the "Load path" heading, there is a "Subdirs of ext/" section -
subdirectories are added to the list in the code block.

## How it works

Using [`org-babel`](http://orgmode.org/worg/org-contrib/babel/intro.html)

Basically, the code in `init.el` calls `org-babel-load-file`, which
evaluates any code-blocks in `config.org`

Customisations live tidily in `config.org`, removing the need to keep
`init.el` organised manually (as the init file just contains the
bootstrapping code for `org-babel`)
