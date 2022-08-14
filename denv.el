;;; denv.el --- My desktop environment configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Xiaoyue Chen

;; Author: Xiaoyue Chen <xiaoyue.chen@it.uu.se>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

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
        (setenv "GDK_SCALE" "2")
        (setenv "QT_SCALE_FACTOR" "2")))))

(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-16"))

(call-process "xsetroot" nil nil nil "-cursor_name" "left_ptr")
(call-process "xset" nil nil nil "r" "rate" "200" "60")
(call-process "picom" nil nil nil "-b")
(start-process "xscreensaver" nil "xscreensaver" "--no-splash")
(start-process "nm-applet" nil "nm-applet")

(setenv "VISUAL" "emacsclient")
(setenv "EDITOR" (getenv "VISUAL"))

(use-package server
  :config
  (unless (server-running-p) (server-start)))

(use-package time
  :init
  (setq display-time-24hr-format t)
  (setq display-time-default-load-average nil)
  :config
  (display-time-mode))

(use-package battery
  :config
  (display-battery-mode))

(use-package exwm
  :init
  (setq exwm-workspace-number 4)
  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda () (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))
          ([?\s-b] . switch-to-buffer)
          ([?\s-o] . other-window)))
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-<] . [home])
          ([?\M->] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\M-d] . [C-delete])
          ([?\C-k] . [S-end delete])
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ([?\C-s] . [?\C-f])))

  :config
  (use-package exwm-config
    :config
    (exwm-config-ido))

  (use-package exwm-systemtray
    :config
    (exwm-systemtray-enable))

  (use-package exwm-randr
    :init
    (setq exwm-randr-workspace-monitor-plist
          '(0 "eDP-1-1"))
    :config
    (exwm-randr-enable))

  (use-package desktop-environment
    :init
    (setq desktop-environment-screenlock-command "xscreensaver-command -lock")
    :config
    (define-key desktop-environment-mode-map
      (kbd "<XF86KbdBrightnessUp>")
      'desktop-environment-keyboard-backlight-increment)
    (define-key desktop-environment-mode-map
      (kbd "<XF86KbdBrightnessDown>")
      'desktop-environment-keyboard-backlight-decrement)
    (desktop-environment-mode))

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (exwm-enable))

(defun ednc-buffer () (interactive)
       (switch-to-buffer ednc-log-name))

(use-package ednc
  :demand
  :config
  (defun list-notifications ()
    (mapconcat 'ednc-format-notification (ednc-notifications) ""))
  (nconc global-mode-string '((:eval (list-notifications))))
  (add-hook 'ednc-notification-presentation-functions
            (lambda (&rest _) (force-mode-line-update t)))
  (ednc-mode)
  :bind
  (("C-c n" . ednc-buffer)))

;;; desktop.el ends here
