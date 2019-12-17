(defun edit-region-as-org ()
  (interactive)
  (switch-to-buffer (clone-indirect-buffer nil nil))
  (call-interactively 'narrow-to-region)
  (call-interactively 'org-mode)
  (message "You're in an indirect buffer; quit any time to return to the main document"))

(defun tef-edit-entry-as-org ()
  (interactive)
  (let ((original-point (point)))
    (search-backward-regexp "^=\\|\\`")
    (search-forward-regexp "^$") ; Find the blank line!
    (forward-char) ; And then go to the beginning of the next one
    (push-mark)
    (search-forward-regexp "^=\\|\\'")
    (search-backward-regexp "^")
    (sit-for 0.5)
    (edit-region-as-org)
    (pop-mark)
    (goto-char original-point)))

(defun tef-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'tef-mode)
  (setq mode-name "TOGoS Entry File")
  (local-set-key (kbd "C-c C-e e o") 'tef-edit-entry-as-org))

(add-to-list 'auto-mode-alist '("\\.tef\\'" . tef-mode))
