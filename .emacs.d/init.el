(require 'cl)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar zmanji-packages
  '(evil solarized-theme)
  "A list of packages that must be installed at launch.")

(defun zmanji-packages-installed-p ()
  "Returns true if and only if all the packages in 'zmanji-packages'
are installed."
  (loop for p in zmanji-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(defun zmanji-install-packages ()
  (unless (zmanji-packages-installed-p)
    (message "Refreshing package database...")
    (package-refresh-contents)
    (message " done.")
    (dolist (p zmanji-packages)
      (unless (package-installed-p p)
	(package-install p)))))

(zmanji-install-packages)

(load-theme 'solarized-dark t)

(require 'evil)
(evil-mode 1)
