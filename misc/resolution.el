#!/usr/bin/emacs --script

;; This script exits with 1 if the resolution of the first connected
;; output is greater than 4k, and exits with 0 otherwise.

(with-temp-buffer
  (call-process "xrandr" nil t nil)
  (goto-char (point-min))
  (when (re-search-forward
         "\n[^ ]+ connected\\(?: .*\\)? \\([[:digit:]]+\\)x\\([[:digit:]]+\\)"
         nil t)
    (let ((resx (string-to-number (match-string 1)))
          (resy (string-to-number (match-string 2))))
      (when (and (<= 3840 resx)
                 (<= 2160 resy))
        (kill-emacs 1)))))
