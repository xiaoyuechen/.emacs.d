;;; denv.el --- My desktop environment configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023  Xiaoyue Chen

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

(defun shutdown (&optional reboot)
  (interactive "P")
  (save-some-buffers)
  (run-hooks 'kill-emacs-query-functions)
  (run-hooks 'kill-emacs-hook)
  (call-process"systemctl" nil nil nil (if reboot
                                           "reboot"
                                         "poweroff")))

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

(use-package desktop-environment
  :init
  (setq desktop-environment-screenlock-command "xscreensaver-command --lock")
  :bind
  (nil
   :map desktop-environment-mode-map
   ("<XF86KbdBrightnessUp>"
    . desktop-environment-keyboard-backlight-increment)
   ("<XF86KbdBrightnessDown>"
    . desktop-environment-keyboard-backlight-decrement)
   ("<XF86Launch4>"
    . (lambda ()
        (interactive)
        (message (desktop-environment--shell-command-to-string "fanboostctl --next"))))
   ("s-p"
    . (lambda ()
        (interactive)
        (message (desktop-environment--shell-command-to-string "autorandr --cycle"))))))

(use-package exwm-systemtray
  :ensure exwm
  :commands
  (exwm-systemtray-enable)
  :config
  (setq exwm-systemtray-icon-gap 4))

(use-package exwm-randr
  :ensure exwm
  :commands
  (exwm-randr-enable)
  :init
  (setq exwm-randr-workspace-monitor-plist
        '(0 "HDMI-0")))

(use-package exwm
  :demand
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
          ([?\s-&] . (lambda ()
                       (interactive)
                       (let ((default-directory "~")
                             (process-connection-type nil)
                             (command (read-shell-command "$ ")))
                         (start-process-shell-command command nil command))))
          ([?\s-b] . switch-to-buffer)
          ([?\s-j] . other-window)
          ([?\s-k] . (lambda ()
                       (interactive)
                       (other-window -1)))))
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
          ([?\C-s] . [?\C-f])
          ([?\C-g] . [escape])))
  :config
  (defun exwm-update-buffer-name ()
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ": "
             (truncate-string-to-width
              (or exwm-title "") 25 nil nil t))))
  (defun exwm-cd-home ()
    (setq-local default-directory "~/"))

  (desktop-environment-mode)
  (exwm-systemtray-enable)
  (exwm-randr-enable)
  (add-hook 'exwm-init-hook
            (lambda ()
              (run-at-time nil 1 (lambda ()
                                   (exwm-systemtray--refresh))))
            100)

  (exwm-enable)
  :bind
  (nil
   :map exwm-mode-map
   ("C-q" . exwm-input-send-next-key))
  :hook
  (exwm-update-class-hook . exwm-update-buffer-name)
  (exwm-update-title-hook . exwm-update-buffer-name)
  (exwm-manage-finish-hook . exwm-cd-home))

(use-package ednc
  :demand
  :config
  (defun ednc-buffer ()
    (interactive)
    (switch-to-buffer ednc-log-name))

  (defun list-notifications ()
    (mapconcat 'ednc-format-notification (ednc-notifications) ""))

  (defun stack-notifications (&optional hide)
    (mapconcat (lambda (notification)
                 (let ((app-name (ednc-notification-app-name notification)))
                   (unless (member app-name hide)
                     (push app-name hide)
                     (ednc-format-notification notification))))
               (ednc-notifications) ""))

  (nconc global-mode-string '((:eval (stack-notifications))))
  (add-hook 'ednc-notification-presentation-functions
            (lambda (&rest _) (force-mode-line-update t)))
  (ednc-mode)
  :bind
  (("C-c n" . ednc-buffer)))


;;; desktop.el ends here
