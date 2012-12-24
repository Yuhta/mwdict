<!--- -*- eval: (auto-fill-mode) -*- -->

MWDict
======

MWDict is an HTML parser written in Python to convert web pages of
online dictionaries (e.g. [Merriam-Webster Learner's
Dictionary](http://www.learnersdictionary.com/)) into plain text
format.

Installation
------------

Just run `make install` once and the Python script will be copied to
your `$HOME/bin` or `/usr/local/bin`.

Usage
-----

`mwdict -h` gives you a brief help message on how to use the script.

### Running Interactively in Emacs

Insert the following codes into your init file.

    (defun mwdict (word)
      (interactive "MWord to search: ")
      (shell-command (concat "mwdict " word)))
    (global-set-key (kbd "<f5>") 'mwdict)

You can change the key binding (`F5` in the above example) to anything
you want.  After that, invoke the script by pressing the key bound or
the command `<M-x> mwdict`.

Todo
----

* Tab completion feature
* Parser for [Wiktionary](http://en.wiktionary.org/)
