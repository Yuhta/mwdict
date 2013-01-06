<!--- -*- mode: text -*- -->

MWDict
======

MWDict  is an  HTML parser  written in  Python to  convert web  pages of
online      dictionaries      (e.g.      [Merriam-Webster      Learner's
Dictionary](http://www.learnersdictionary.com/)) into plain text format.

Installation
------------

Just run  `make install` once  and the Python  script will be  copied to
your `$HOME/bin` or `/usr/local/bin`.  A directory called `.mwdict` will
be  created in  your home  directory where  the program  will store  the
configuration   files  and   local  entries.    The  completion   script
(`mwdict-comp`) will  also be  copied to  the new-created  directory and
will be sourced in your `.bashrc`.

Usage
-----

`mwdict -h` gives you a brief help message on how to use this script.

### Bash Completion

Tab completion feature  has been added to MWDict in  Bash.  Words can be
completed by typing `<Tab>`, which  is achieved by the completion script
`mwdict-comp`, who reads candidates from the output of `mwdict-suggest`.

### Running Interactively in Emacs

To use  MWDict in Emacs,  copy `mwdict.el` to  where you put  Emacs Lisp
files  (say `~/.emacs.d/elisp`).   Then insert  the following  code into
your init file (don't forget to load the path first, if you didn't).

    ; MWDict
    (autoload 'mwdict "mwdict")
    (global-set-key (kbd "<f5>") 'mwdict)

   You  can change  the  key binding  (`<F5>` in  the  above script)  to
anything you  want.  After that, invoke  the script by pressing  the key
bound or the command `<M-x> mwdict`.

   Currently the  Emacs Lisp script  allows you  to use MWDict  in Emacs
with the  minibuffer completion feature,  with the default value  set to
the  word at  point.  You  can also  give the  command a  prefix numeric
argument to select different entries of a word.

Known Issue
-----------

* Words have different meanings when capitalized (e.g. catholic)

Todo
----

* Parser for [Wiktionary](http://en.wiktionary.org/)
