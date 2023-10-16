;;; tt-edit-mode.el --- Code for my custom edit mode

;; Copyright (C) 2023  Manuel Teodoro Tenango

;; Author: Manuel Teodoro <teotenn@proton.me>
;; URL: https://github.com/teotenn/dot_tt
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;;
;; Every piece of code in this repository is my own and it is intended for
;; personal purposes. I am not an expert and I do not recommend that you
;; copy any of this if you are not certain of what it does.

;;; Code:

;; -------------------------------------------------------------------
;; Xah funcs taken from:
;; http://xahlee.info/emacs/emacs/modernization_mark-word.html

(defvar xah-brackets '("“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄" "‹›" "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "〘〙" "｢｣" "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞" "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧" "⸨⸩" "｟｠")
 "A list of strings, each element is a string of 2 chars, the left bracket and a matching right bracket.
Used by `xah-select-text-in-quote' and others.
Version 2023-07-31")

(defconst xah-left-brackets
  (mapcar (lambda (x) (substring x 0 1)) xah-brackets)
  "List of left bracket chars. Each element is a string.")

(defconst xah-right-brackets
  (mapcar (lambda (x) (substring x 1 2)) xah-brackets)
  "List of right bracket chars. Each element is a string.")

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \" ` and anything in `xah-brackets'.
This command ignores nesting. For example, if text is
    (a(b)c▮)
the selected char is “c”, not “a(b)c”.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2020-11-24 2023-07-16 2023-07-23"
  (interactive)
  (let ((xskipChars (concat "^\"`" (mapconcat #'identity xah-brackets ""))))
    (skip-chars-backward xskipChars)
    (push-mark (point) t t)
    (skip-chars-forward xskipChars)))


(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2017-11-01 2021-03-19 2023-07-16"
  (interactive)
  (if (region-active-p)
      (if visual-line-mode
          (let ((xp1 (point)))
            (end-of-visual-line 1)
            (when (eq xp1 (point))
              (end-of-visual-line 2)))
        (progn
          (forward-line 1)
          (end-of-line)))
    (if visual-line-mode
        (progn (beginning-of-visual-line)
               (push-mark (point) t t)
               (end-of-visual-line))
      (progn
        (push-mark (line-beginning-position) t t)
        (end-of-line)))))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2016-11-22 2023-07-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((prog2 (backward-char) (looking-at (regexp-opt xah-right-brackets)) (forward-char))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))


(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

when there is no selection,
• If cursor is on any type of bracket (including parenthesis, quotation mark), select whole bracketed thing including bracket
• else, select current word.

when there is a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2020-02-04 2023-07-22 2023-07-23"
  (interactive)
  (if (region-active-p)
      (progn
        (let ((xrb (region-beginning)) (xre (region-end)))
          (goto-char xrb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  ;; (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (push-mark (line-beginning-position) t t))
              (progn
                ;; (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq xrb (line-beginning-position))
            (progn
              (goto-char xrb)
              (let ((xfirstLineEndPos (line-end-position)))
                (cond
                 ((eq xre xfirstLineEndPos)
                  (progn
                    ;; (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< xre xfirstLineEndPos)
                  (progn
                    ;; (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> xre xfirstLineEndPos)
                  (progn
                    ;; (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char xre)
                    (if (eq (point) (line-end-position))
                        (progn
                          ;; (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        ;; (message "multiple lines but end is not eol. make it so" )
                        (goto-char xre)
                        (end-of-line)))))
                 (t (error "%s: logic error 42946" real-this-command))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              ;; (message "less than 1 line" )
              (end-of-line) ; select current line
              (push-mark (line-beginning-position) t t)))
           (t
            ;; (message "last resort" )
            nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        ;; (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        ;; (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        ;; (message "string quote")
        (mark-sexp)) ; string quote
       ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
       ;;  (message "beginning of line and not empty")
       ;;  (end-of-line)
       ;;  (push-mark (line-beginning-position) t t))
       (
        ;; (prog2 (backward-char) (looking-at "[-_a-zA-Z0-9]") (forward-char))
        (looking-back "[-_a-zA-Z0-9]" (max (- (point) 1) (point-min)))
        ;; (message "left is word or symbol")
        (skip-chars-backward "-_a-zA-Z0-9")
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (push-mark)
        (skip-chars-forward "-_a-zA-Z0-9")
        (setq mark-active t)
        ;; (exchange-point-and-mark)
        )
       ((and (looking-at "[:blank:]")
             (prog2 (backward-char) (looking-at "[:blank:]") (forward-char)))
        ;; (message "left and right both space" )
        (skip-chars-backward "[:blank:]") (push-mark (point) t t)
        (skip-chars-forward "[:blank:]"))
       ((and (looking-at "\n")
             (eq (char-before) 10))
        ;; (message "left and right both newline")
        (skip-chars-forward "\n")
        (push-mark (point)  t t)
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next block
       (t
        ;; (message "just mark sexp" )
        (mark-sexp)
        (exchange-point-and-mark)
       ;;
       )))))


;; (defun xah-forward-quote-twice ()
;;   "Call `xah-forward-quote' twice.
;; Returns `t' if found, else `nil'.

;; URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
;; Version: 2016-07-23"
;;   (interactive)
;;   (when (xah-forward-quote)
;;     (xah-forward-quote)))

;; (defun xah-forward-quote-smart ()
;;   "Move cursor to the current or next string quote.
;; Place cursor at the position after the left quote.
;; Repeated call will find the next string.

;; URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
;; Version: 2016-11-22"
;;   (interactive)
;;   (let ((xpos (point)))
;;     (if (nth 3 (syntax-ppss))
;;         (progn
;;           (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
;;           (forward-sexp)
;;           (re-search-forward "\\\"" nil t))
;;       (progn (re-search-forward "\\\"" nil t)))
;;     (when (<= (point) xpos)
;;       (progn (re-search-forward "\\\"" nil t)))))


;; -------------------------------------------------------------------
;; Personal functions
;; select lines
(defun tt-select-n-lines (arg)
  "Select the current line and ARG lines IF no region is selected.
If a region is selected, continues the selection from the cursor."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (move-end-of-line arg))


(defun tt-copy-symbol-at-point ()
  "Copy symbol at point without killing it."
  (interactive)
  (kill-new (thing-at-point 'symbol)))


(defun tt-copy-word-at-point ()
  "Copy symbol at point without killing it."
  (interactive)
  (kill-new (thing-at-point 'word)))


(defun tt-delete-word-at-point ()
  (interactive)
  (progn
    (backward-word)
    (mark-word)
    (let ((wstart (region-beginning)) (wend (region-end)))
      (delete-region wstart wend))))

(defun tt-delete-symbol-at-point ()
  (interactive)
  (deactivate-mark)
  ;; (if (string= (thing-at-point) " ") "true")) 
  (xah-extend-selection)
  (let ((wstart (region-beginning)) (wend (region-end)))
    (delete-region wstart wend)))

;; Keybindings map
(defvar tt-edit-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'xah-select-text-in-quote)
    (define-key map "l" 'xah-select-line)
    (define-key map "(" 'xah-backward-left-bracket)
    (define-key map ")" 'xah-forward-right-bracket)
    (define-key map "m" 'xah-goto-matching-bracket)
    (define-key map "t" 'xah-extend-selection)
    (define-key map "j" 'tt-select-n-lines)
    (define-key map "s" 'tt-copy-symbol-at-point)
    (define-key map "w" 'tt-copy-word-at-point)
    (define-key map "k" 'tt-delete-symbol-at-point)
    (define-key map "d" 'tt-delete-word-at-point)
    (define-key map "p" 'mark-paragraph)
    (define-key map "f" 'mark-defun)
    map)
  "Key map for tt edit mode")

;; TODO
;; Add support to delete symbol and word when at the end (on empty line)

(provide 'tt-edit-mode)
