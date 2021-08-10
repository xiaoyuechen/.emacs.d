(load-theme 'solarized-dark t t)
(load-theme 'solarized-light t t)

(defun is-gtk-theme-dark ()
  (string-match-p "dark"
		  (shell-command-to-string
		   "gsettings get org.gnome.desktop.interface gtk-theme")))

(if (is-gtk-theme-dark)
    (enable-theme 'solarized-dark)
  (enable-theme 'solarized-light))

(require 'dbus)
(defun theme-switcher-dbus-method-handler (theme-name)
  (let ((theme (intern theme-name)))
    (enable-theme theme)))

(dbus-register-method
 :session dbus-service-emacs
 (concat dbus-path-emacs "/ThemeSwitcher")
 (concat dbus-interface-emacs ".ThemeSwitcher")
 "SetTheme"
 'theme-switcher-dbus-method-handler)
