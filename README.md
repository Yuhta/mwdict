# MWDict #

MWDict is a command line interface program to look up words in
[Merriam-Webster Learner's
Dictionary](http://www.learnersdictionary.com/).

## Installation ##

Copy `mwdict` to anywhere you want, probably one of the directories in
`$PATH`.

## Usage ##

`mwdict -h` gives you a brief help message on how to use this program.

### Bash Completion ###

Tab completion feature  has been added to MWDict in  Bash.  Words can be
completed by typing `<Tab>`, which  is achieved by the completion script
`mwdict-comp`, who reads candidates from the output of `mwdict-suggest`.

### Running Interactively in Emacs ###

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

### Cache Directory ###

By default MWDict will cache the results in `~/.cache/mwdict`.  You can
clear it up if it gets too large.  The `--no-cache` option can be used
to prevent the program from using cache (read or write).
