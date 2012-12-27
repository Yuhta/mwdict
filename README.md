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
your `$HOME/bin` or `/usr/local/bin`.  The completion script will also
be copied to your home directory and will be sourced in your
`.bashrc`.

Usage
-----

`mwdict -h` gives you a brief help message on how to use this script.

### Bash Completion

Tab completion feature has been added to MWDict in Bash.  Words can be
completed by typing `<Tab>`, which is achieved by the completion
script `mwdict-comp`, who reads candidates from the output of
`mwdict-suggest`.

### Running Interactively in Emacs

Insert the following code into your init file.

    (defun mwdict (word)
      (interactive "MWord to search: ")
      (shell-command (concat "mwdict '" word "'")))
    (global-set-key (kbd "<f5>") 'mwdict)

You can change the key binding (`<F5>` in the above example) to
anything you want.  After that, invoke the script by pressing the key
bound or the command `<M-x> mwdict`.

Todo
----

* Emacs minibuffer completion feature
* Parser for [Wiktionary](http://en.wiktionary.org/)
