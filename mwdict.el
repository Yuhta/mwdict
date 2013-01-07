(defun mwdict-completions (str pred mode)
  (let ((suggestions
         (delete ""
                 (split-string
                  (shell-command-to-string (concat "mwdict-suggest '"
                                                   str
                                                   "'"))
                  "\n"))))

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
     ((eq mode 'lambda) (member str suggestions)))))


; Initialize history list
(setq mwdict-history nil)

(defun mwdict (word &optional num)
  (interactive (list (let ((completion-ignore-case 't))
                       (completing-read
                        (concat "Word to search (default "
                                (thing-at-point 'word)
                                "): ")
                        'mwdict-completions
                        nil nil nil
                        'mwdict-history
                        (thing-at-point 'word)))
                     current-prefix-arg))

  (let ((command (concat "mwdict '" word)))
    (if (listp num) (shell-command (concat command "'")
                                   (get-buffer-create "*MWDict*"))
      (shell-command (concat command
                             "[" (number-to-string num) "]'")
                     (get-buffer-create "*MWDict*"))))

  (set-buffer "*MWDict*")
  (view-mode)
  (visual-line-mode))
