;; Function to beautify with all-the-icons package 
(defun custom-modeline-time ()
  (let* ((iweek (all-the-icons-octicon "calendar" 
				       :height 1.1 
				       :v-adjust -0.0 
				       :face 'all-the-icons-green))
	 (hour (string-to-number (format-time-string "%I"))))
    (concat
     (propertize iweek)
     (propertize (format-time-string "%W|%H:%M ") 'face `(:height 0.9)))))

;; Count (lines, words)
(defun custom-modeline-region-info ()
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
                   'face `(:family ,(all-the-icons-octicon-family))
                   'display '(raise -0.0))
       (propertize (format " (%s, %s)" words chars)
                   'face `(:height 0.9))))))

;; version control NOT SO GOOD
(defun -custom-modeline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-alltheicon "git")) 
		 'display '(raise -0.1))
     " Â· "
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                 'display '(raise -0.1))
     (propertize (format " %s" branch) 'face `(:height 0.9)))))

(defun -custom-modeline-svn-vc ()
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
     (propertize (format " Â· %s" revision) 'face `(:height 0.9)))))

(defun custom-modeline-icon-vc ()
  (when vc-mode
    (cond
      ((string-match "Git-" vc-mode) (-custom-modeline-github-vc))
      ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
      (t (format "%s" vc-mode)))))

;; --------------- From Prot --------------- ;;
;; Helpers
(defcustom prot-modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(defun prot-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (< (window-total-width) split-width-threshold)
       (> (length str) prot-modeline-string-truncate-length)
       (not (one-window-p :no-minibuffer))))

(defun prot-modeline-string-truncate (str)
  "Return truncated STR, if appropriate, else return STR.
Truncation is done up to `prot-modeline-string-truncate-length'."
  (if (prot-modeline--string-truncate-p str)
      (concat (substring str 0 prot-modeline-string-truncate-length) "...")
    str))

;;;; Buffer name and modified status

(defun prot-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `prot-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun prot-modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
See `prot-modeline-string-truncate'."
  (when-let ((name (buffer-name)))
    (prot-modeline-string-truncate name)))

(defun prot-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (prot-modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun prot-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `prot-modeline-buffer-identification'."
  (concat
   (or (buffer-file-name)
       (format "No underlying file.\nDirectory is: %s" default-directory))))

(defvar-local prot-modeline-buffer-identification
    '(:eval
      (propertize (prot-modeline-buffer-name)
                  'face (prot-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (prot-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")
