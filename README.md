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

    ;;;;;;;;;;
    ; MWDict ;
    ;;;;;;;;;;
     
    (defun mwdict-completions (str pred mode)
      (setq suggestions
            (delete ""
                    (split-string
                     (shell-command-to-string (concat "mwdict-suggest '"
                                                      str
                                                      "'"))
                     "\n")))
      (cond
       ((null mode)
        (cond
         ((null suggestions) 'nil)
         ((eq (length suggestions) 1)
          (if (member str suggestions) 't
            (car suggestions)))
         ((and (> (length suggestions) 1) (< (length suggestions) 15))
          (reduce 'fill-common-string-prefix suggestions))
         ('t str)))
       ((eq mode 't) suggestions)
       ((eq mode 'lambda) (member str suggestions))))
     
    (defun mwdict (word &optional num)
      (interactive (list (let ((completion-ignore-case 't))
                           (completing-read "Word to search: "
                                            'mwdict-completions))
                         current-prefix-arg))
      (if (listp num) (shell-command (concat "mwdict '" word "'"))
        (shell-command (concat "mwdict '" word
                               "[" (number-to-string num) "]'"))))
    (global-set-key (kbd "<f5>") 'mwdict)

You can change the key binding (`<F5>` in the above script) to
anything you want.  After that, invoke the script by pressing the key
bound or the command `<M-x> mwdict`.

The above script allows you to use MWDict in Emacs with the minibuffer
completion feature.  See the corresponding part of [Emacs
manual](http://www.gnu.org/software/emacs/manual/html_node/emacs/Completion.html)
for more information.

Todo
----

* Parser for [Wiktionary](http://en.wiktionary.org/)
