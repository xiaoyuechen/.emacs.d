(require 'dbus)

(load-theme 'solarized-dark t t)
(load-theme 'solarized-light t t)

(defun smart-theme--is-gtk-theme-dark ()
  (string-match-p "dark"
		  (shell-command-to-string
		   "gsettings get org.gnome.desktop.interface gtk-theme")))

(if (smart-theme--is-gtk-theme-dark)
    (enable-theme 'solarized-dark)
  (enable-theme 'solarized-light))

(defun smart-theme--theme-switcher-dbus-method-handler (theme-name)
  (let ((theme (intern theme-name)))
    (enable-theme theme)))

(dbus-register-method
 :session dbus-service-emacs
 (concat dbus-path-emacs "/ThemeSwitcher")
 (concat dbus-interface-emacs ".ThemeSwitcher")
 "SetTheme"
 'smart-theme--theme-switcher-dbus-method-handler)
