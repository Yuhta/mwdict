# MWDict

[![Build Status](https://travis-ci.org/Yuhta/mwdict.svg)](https://travis-ci.org/Yuhta/mwdict)

Command line interface to
[Merriam-Webster dictionary](http://www.merriam-webster.com/).

## Installation

1. Make sure you have [Leiningen](http://leiningen.org/) installed
2. Clone this repository
3. Run `lein bin`
4. `install target/base+system+user+dev/mwdict /usr/local/bin/`

## Usage

    $ mwdict WORD-TO-SEARCH

## Environment Variables

- `MWDICT_API_KEY_COLLEGIATE`: API key for the dictionary (can be got
  from http://www.dictionaryapi.com/)
- `MWDICT_CACHE_HOME`: location for cache (default to `~/.cache/mwdict`)

## Emacs Interface

To use MWDict in Emacs, copy `mwdict.el` to your Emacs Lisp `load-path`.
Then insert the following code into your init file.

```elisp
(autoload 'mwdict "mwdict" nil 'interactive)
(global-set-key (kbd "C-c m") 'mwdict)
```

You can change the key binding (`C-c m` in the above script) to anything
you want.  After that, invoke the script by pressing the key bound or
the command `<M-x> mwdict`.

## License

Copyright © 2015 Jimmy Lu

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
