(in-package :stumpwm)

;; Top Meta enter for terminal
(define-key *top-map* (kbd "s-RET") "exec x-terminal-emulator")
;(define-key *top-map* (kbd "Print") "screenshot")

(defcommand backup-git () ()
	    (run-shell-command "~/bin/backup_git.sh"))

;; Execute my fav apps
(defvar *my-applications-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "w") "exec firefox")
    (define-key m (kbd "f") "exec thunar")
    (define-key m (kbd "i") "exec inkscape")
    (define-key m (kbd "g") "exec gimp")
    (define-key m (kbd "e") "exec emacsclient --create-frame")
    (define-key m (kbd "RET") "exec x-terminal-emulator")
    (define-key m (kbd "b") "backup-git")
    m))
;; Defines a subgroup C-z a key defined above
(define-key *root-map* (kbd "a") '*my-applications-keymap*)


;; end-session
(defvar *my-end-session-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") "end-session")
    (define-key m (kbd "l") "logout")
    (define-key m (kbd "s") "suspend-computer")
    (define-key m (kbd "S") "shutdown-computer")
    (define-key m (kbd "r") "loadrc")
    (define-key m (kbd "R") "restart-hard")
    (define-key m (kbd "C-r") "restart-computer")
    m))

(define-key *root-map* (kbd "q") '*my-end-session-keymap*)

;; Screenshots
(defvar *my-stumpwm-screenshots*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a") "screenshot")
    (define-key m (kbd "s") "screenshot-window")
    m))

(define-key *top-map* (kbd "Print") '*my-stumpwm-screenshots*)

