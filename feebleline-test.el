


;;; Code:

(defvar feebleline-msg-functions nil)

(defun my-own-linecol-string ()
  "Hey guy!"
  (format "%5s:%-2s" (format-mode-line "%l") (current-column)))

(setq
 feebleline-msg-functions
 '(
   (my-own-linecol-string)
   (buffer-file-name
    ((face . feebleline-dir-face) (post . "")))
   ((lambda () ":") ((post . "")))
   (magit-get-current-branch ((face . font-lock-comment-face)))
   ((lambda () "some string") ((pre . "@")))
   ))

;; and you can append stuff (todo: wrapper function)
(add-to-list
 'feebleline-msg-functions
 '((lambda () "end") ((pre . "/")
                      (face . font-lock-comment-face)))
 t
 (lambda (x y) nil))

(defun feebleline--insert ()
  "Insert stuff into the mini buffer."
  (let ((tmp-string ""))
    (dolist (idx feebleline-msg-functions)
      (let ((string-func (car idx) )
            (props (cadr  idx)))
        (let ((string-face (cdr (assoc 'face props)))
              (pre (cdr (assoc 'pre props)))
              (post (cdr (assoc 'post props)))
              )
          (unless string-face (setq string-face 'feebleline-default-face))
          (unless post (setq post " "))
          ;; todo: format string as a variable?
          (setq
           tmp-string
           (concat tmp-string (propertize
                               (concat pre
                                       (apply string-func nil)
                                       post)
                               'face string-face))))))
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert tmp-string))))

(defvar feebleline--new-timer)
(setq feebleline--new-timer (run-with-timer 0 1 'feebleline--insert))






                                        ;
