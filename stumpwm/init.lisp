;; Call package
(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; change the prefix key to something else
(set-prefix-key (kbd "C-z"))

;; DEFINE AUTOSTART
;; Load autostart file
;(run-shell-command "autostart")
(defcommand tt-autostart () ()
	    (run-shell-command "~/bin/autostart.sh"))

;; Initialize mode-line
(when *initializing*
  (tt-autostart))

;; turn mode-line on for current head
;(enable-mode-line (current-screen) (current-head) t 
;		  (list "%w |" '(:eval (run-shell-command "date" t))))

;; Focus linked to the mouse and moving float windows with super
(setf *mouse-focus-policy*    :click
      *float-window-modifier* :SUPER)

;; Extra modules load
(load-module "end-session")
(load-module "screenshot")

;; --- MODE-LINE improvements
;; NOT WORKING
(setq window-format "[%s] %t")

;; message box placement
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

;; Mode line all screens
(dolist (h (screen-heads (current-screen)))
  (enable-mode-line (current-screen) h t))

;; Keep particular commands in sepparated lisp files
(load "~/Code/dot_tt/stumpwm/keybindings.lisp")
;(load "~/.stumpwm.d/bluetooth.lisp")

;; INIT message
(setf *startup-message* "Running StumpWM in TEST")
