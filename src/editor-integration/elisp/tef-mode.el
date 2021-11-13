(defun edit-region-as-org ()
  (interactive)
  (switch-to-buffer (clone-indirect-buffer nil nil))
  (call-interactively 'narrow-to-region)
  (call-interactively 'org-mode)
  (message "You're in an indirect buffer; quit any time to return to the main document"))

(defun tef-edit-entry-as-org ()
  (interactive)
  (let ((original-point (point)))
    (search-backward-regexp "^=[^=]\\|\\`")
    (search-forward-regexp "^$") ; Find the blank line!
    (forward-char) ; And then go to the beginning of the next one
    (push-mark)
    (search-forward-regexp "^=[^=]\\|\\'")
    (search-backward-regexp "^")
    (sit-for 0.5)
    (edit-region-as-org)
    (pop-mark)
    (goto-char original-point)))

;;(defun tef-mode ()
;;  (interactive)
;;  (kill-all-local-variables)
;;  (setq major-mode 'tef-mode)
;;  (setq mode-name "TOGoS Entry File")
;;  )

;; Syntax highlighting: https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html
;; tef-mode-highlights is a 'font-lock-keywords'
;; font-lock-keywords: https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
;; faces https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Multiline-Font-Lock.html says:
;; 
;; "Normally, elements of font-lock-keywords should not match across
;; multiple lines; that doesn’t work reliably, because Font Lock
;; usually scans just part of the buffer, and it can miss a multi-line
;; construct that crosses the line boundary where the scan starts."

(setq tef-mode-regular-header-key-regex "\\(?:[^[:blank:]:\n]\\|:[^[:blank:]\n]\\)+")
(setq tef-mode-header-value-regex "\\(?:[^\n]\\|\n[[:blank:]]\\)*")

(setq tef-mode-header-regex
      (rx bol
	  (or
	   (group "tef:" (eval `(regexp ,tef-mode-regular-header-key-regex)))
	   (group        (eval `(regexp ,tef-mode-regular-header-key-regex))))
	  ":"
	  (or
	   (and (zero-or-more blank) eol)
	   (and (one-or-more blank)
		(group (eval `(regexp ,tef-mode-header-value-regex)))
		eol))))

;; Note: Since this uses subexpressions which are sometimes nil,
;; we MUST SAY "nil t" (the t means yeah, continue even if group not found)
;; otherwise nothing works and you'll scratch your head for hours.

(setq tef-mode-highlights
      (let (
	    (tef-mode-entry-line-regex "^\\(=\\)\\([^[:blank:]\n=][^[:blank:]\n]*\\)?\\(?:[[:blank:]]+\\([^\n]*\\)\\)?$")
	    (tef-mode-comment-line-regex "^\\(#\\)\\([^\n]*\\)$")
	   )
	`(
	  ("^\n\\(?:[^=\n]\\|\n[^=]\\|\n==\\)*$" 0 (list 'face 'tef-content-face 'font-lock-multiline t))
	  (,tef-mode-entry-line-regex 1 'font-lock-keyword-face nil t)
	  (,tef-mode-entry-line-regex 2 'font-lock-type-face nil t)
	  (,tef-mode-entry-line-regex 3 'font-lock-function-name-face  nil t)
	  ("^#[^ !\n][^\n]*$" 0 'font-lock-warning-face)
	  (,tef-mode-comment-line-regex 1 'font-lock-comment-delimiter-face nil t)
	  (,tef-mode-comment-line-regex 2 'font-lock-comment-face nil t)
	  (,tef-mode-header-regex 1 'font-lock-builtin-face nil t)
	  (,tef-mode-header-regex 2 'font-lock-variable-name-face nil t)
	  (,tef-mode-header-regex 3 (list 'face 'font-lock-string-face 'font-lock-multiline t) nil t)
	 )))

(define-derived-mode tef-mode fundamental-mode "tef"
  "major mode for editing TEF files"
  ;; I may be abusing this system.
  ;; Maybe I'm supposed to make a syntactic thing
  ;; instead of just relying on keywords ('t' is for keywords-only).
  ;; Or maybe I should do my own thing altogether like org-mode
  ;; so I can support the modes of embedded content.
  ;; Maybe another day.  This mostly works.
  (setq font-lock-defaults '(tef-mode-highlights t))
  (local-set-key (kbd "C-c C-e e o") 'tef-edit-entry-as-org))

;; this should be done outside of this script so that tef-mode can be autoloaded:
;;(add-to-list 'auto-mode-alist '("\\.tef\\'" . tef-mode))
