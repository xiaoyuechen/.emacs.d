(put 'one-line-description 'safe-local-variable 'stringp)
(put 'lice:default-license 'safe-local-variable 'stringp)
(put 'lice:copyright-holder 'safe-local-variable 'stringp)

(defun insert-one-line-description (license)
  (when (boundp 'one-line-description)
    (insert (format "%s\n" one-line-description))))

(setq lice:header-spec '(insert-one-line-description
			 lice:insert-copyright
			 lice:insert-license))
