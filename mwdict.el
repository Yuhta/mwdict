(defvar mwdict-history nil)

(require 'cl)

(defun mwdict-completions (str pred mode)
  (let ((suggestions
         (delete ""
                 (split-string
                  (shell-command-to-string (concat "mwdict-suggest \""
                                                   str
                                                   "\""))
                  "\n"))))
    (cond
     ((null mode)
      (cond
       ((null suggestions)
        nil)
       ((eq (length suggestions) 1)
        (if (member str suggestions)
            t
          (car suggestions)))
       ((and (> (length suggestions) 1) (< (length suggestions) 15))
        (reduce 'fill-common-string-prefix suggestions))
       (t
        str)))
     ((eq mode t)
      suggestions)
     ((eq mode 'lambda)
      (member str suggestions)))))

(defun mwdict-process-filter (proc string)
  "A process filter used in `mwdict'.
It rolls the point back to the beginning of the buffer after each
insertion."
  (let ((old-buffer (current-buffer)))
    (unwind-protect
        (let ((inhibit-read-only t))
          (set-buffer (process-buffer proc))
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point))
          (goto-char (point-min)))
      (set-buffer old-buffer))))

(defun mwdict (word &optional num)
  (interactive (list (let ((completion-ignore-case 't))
                       (completing-read
                        (format "Word to search (default %s): "
                                (thing-at-point 'word))
                        'mwdict-completions
                        nil nil nil
                        'mwdict-history
                        (thing-at-point 'word)))
                     current-prefix-arg))
  (let ((prg "mwdict")
        (buf-name "*MWDict*"))
    (pop-to-buffer (get-buffer-create buf-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq buffer-read-only t)
    (set-process-filter (start-process prg buf-name prg
                                       "--plain-text" "--no-pager"
                                       (concat word
                                               (unless (listp num)
                                                 (format "[%d]" num))))
                        'mwdict-process-filter))
  (view-mode)
  (visual-line-mode))
