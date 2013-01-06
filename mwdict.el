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
                       (completing-read
                        (concat "Word to search (default "
                                (thing-at-point 'word)
                                "): ")
                        'mwdict-completions
                        nil nil nil
                        minibuffer-history
                        (thing-at-point 'word)))
                     current-prefix-arg))
  (if (listp num) (shell-command (concat "mwdict '" word "'"))
    (shell-command (concat "mwdict '" word
                           "[" (number-to-string num) "]'"))))
