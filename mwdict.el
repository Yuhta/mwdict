(require 'ansi-color)

(defconst +mwdict-program+ "mwdict")
(defconst +mwdict-buffer+ "*MWDict*")
(defvar mwdict-history nil)

(defun mwdict-process-filter (proc string)
  "A process filter used in `mwdict'.
It rolls the point back to the beginning of the buffer after each
insertion."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (process-mark proc))
      (insert (ansi-color-apply string))
      (set-marker (process-mark proc) (point))
      (goto-char (point-min)))))

(defun mwdict (word)
  (interactive (list (let* ((default (thing-at-point 'word))
                            (default-prompt (if default
                                                (format " (default %s)"
                                                        default)
                                              "")))
                       (read-string (format "Word to search%s: "
                                            default-prompt)
                                    nil
                                    'mwdict-history
                                    default))))
  (pop-to-buffer (get-buffer-create +mwdict-buffer+))
  (font-lock-mode 1)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq buffer-read-only t)
  (set-process-filter (start-process +mwdict-program+
                                     +mwdict-buffer+
                                     +mwdict-program+
                                     word)
                      'mwdict-process-filter)
  (view-mode)
  (visual-line-mode))
