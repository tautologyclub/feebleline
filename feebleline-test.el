


;;; Code:

(defcustom feebleline-msg-functions nil
  "Fixme -- document me.")

(defcustom feebleline-timer-interval 0.1
  "Refresh interval of feebleline mode-line proxy."
  :group 'feebleline)

(defcustom feebleline-use-legacy-settings nil
  "Hacky settings only applicable to releases older than 25."
  :group 'feebleline
  )

(defvar feebleline--home-dir nil)
(defvar feebleline--msg-timer)
(defvar feebleline/mode-line-format-previous)

(defface feebleline-git-branch-face '((t :foreground "#444444" :italic t))
  "Example face for git branch."
  :group 'feebleline)

(defun feebleline-linecol-string ()
  "Hey guy!"
  (format "%4s:%-2s" (format-mode-line "%l") (current-column)))

(defun feebleline-previous-buffer-name ()
  "Get name of previous buffer."
  (buffer-name (other-buffer (current-buffer) 1)))

(defun feebleline-line-number ()
  "Line number as string."
  (if (buffer-file-name)
      (format "%s" (line-number-at-pos))))

(defun feebleline-column-number ()
  "Column number as string."
  (if (buffer-file-name)
      (format "%s" (current-column))))

(defun feebleline-file-directory ()
  "Current directory, if buffer is displaying a file."
  (if (buffer-file-name)
      (replace-regexp-in-string
       (concat "^" feebleline--home-dir) "~"
       default-directory)
    ""))

(defun feebleline-file-or-buffer-name ()
  "Current file, or just buffer name if not a file."
  (if (buffer-file-name)
      (file-name-nondirectory (buffer-file-name))
    (buffer-name)))

(defun feebleline-file-modified-star ()
  "Display star if buffer file was modified."
  (when (and (buffer-file-name) (buffer-modified-p)) "*"))

(defun feebleline-project-name ()
  "Return projectile project name if exists, otherwise nil."
  (unless (string-equal "-" (projectile-project-name))
    (projectile-project-name)))

;; -- TODO:
;; right-align property doesn't work with post/pre and it also messes up other
;; frames that don't have the same font size. Furthermore it has to be the last
;; element of the list and no more than one element can have the property.
;; Shortly, it's shite.
(setq
 feebleline-msg-functions
 '((feebleline-line-number         ((post . "") (fmt . "%5s")))
   (feebleline-column-number       ((pre . ":") (fmt . "%-2s")))
   (feebleline-file-directory      ((face . feebleline-dir-face)    (post . "")))
   (feebleline-file-or-buffer-name ((face . font-lock-keyword-face) (post . "")))
   (feebleline-file-modified-star  ((face . font-lock-warning-face) (post . "")))
   (magit-get-current-branch       ((face . feebleline-git-branch-face) (pre . " - ")))
   ;; (feebleline-project-name        ((right-align . t)))
   ))

(defmacro feebleline-append-msg-function (&rest b)
  "Macro for adding B to the feebleline mode-line, at the end."
  `(add-to-list 'feebleline-msg-functions ,@b t (lambda (x y) nil)))

(defmacro feebleline-prepend-msg-function (&rest b)
  "Macro for adding B to the feebleline mode-line, at the beginning."
  `(add-to-list 'feebleline-msg-functions ,@b nil (lambda (x y) nil)))

;; (feebleline-append-msg-function '((lambda () "end") ((pre . "/"))))
;; (feebleline-append-msg-function '(magit-get-current-branch ((pre . "/"))))
;; (feebleline-prepend-msg-function '((lambda () "-") ((post . "/"))))

(defun feebleline-default-settings-on ()
  "Some default settings that works well with feebleline."
  (setq window-divider-default-bottom-width 1
        window-divider-default-places (quote bottom-only))
  (window-divider-mode t)
  (setq-default mode-line-format nil)
  (setq mode-line-format nil))

(defun feebleline-legacy-settings-on ()
  "Some default settings for EMACS < 25."
  (set-face-attribute 'mode-line nil :height 0.1))

(defun feebleline--insert ()
  "Insert stuff into the mini buffer."
  (unless (current-message)
    (let ((tmp-string ""))
      (dolist (idx feebleline-msg-functions)
        (let ((string-func (car idx))
              (props (cadr  idx)))
          (let ((msg (apply string-func nil))
                (string-face (cdr (assoc 'face props)))
                (pre (cdr (assoc 'pre props)))
                (post (cdr (assoc 'post props)))
                (fmt (cdr (assoc 'fmt props)))
                (right-align (cdr (assoc 'right-align props)))
                )
            (when msg
              (unless string-face (setq string-face 'feebleline-default-face))
              (unless post (setq post " "))
              (unless fmt (setq fmt "%s"))
              (when right-align
                (setq fmt
                 (concat "%"
                         (format "%s" (- (window-width) (length tmp-string) 1))
                                  "s"))
                ;; (message "%s" fmt)
                )
              (setq tmp-string
                    (concat
                     tmp-string
                     (propertize
                      (concat pre (format fmt msg) post)
                          'face string-face)))))))
      (with-current-buffer " *Minibuf-0*"
        (erase-buffer)
        (insert tmp-string)))))

(defun feebleline--clear-echo-area ()
  "Erase echo area."
  (with-current-buffer " *Minibuf-0*"
    (erase-buffer))
  )

;;;###autoload
(define-minor-mode feebleline-mode
  "Replace modeline with a slimmer proxy."
  :require 'feebleline
  :global t
  (if feebleline-mode
      ;; Activation:
      (progn
        (setq feebleline--home-dir (expand-file-name "~"))
        (setq feebleline/mode-line-format-previous mode-line-format)
        (setq feebleline--msg-timer (run-with-timer 0 feebleline-timer-interval 'feebleline--insert))
        (if feebleline-use-legacy-settings (feebleline-legacy-settings-on)
          (feebleline-default-settings-on))
        (add-hook 'focus-in-hook 'feebleline-mode-line-proxy-fn)
        )

    ;; Deactivation:
    (set-face-attribute 'mode-line nil :height 1.0)
    (setq-default mode-line-format feebleline/mode-line-format-previous)
    (setq mode-line-format feebleline/mode-line-format-previous)
    (cancel-timer feebleline--msg-timer)
    (remove-hook 'focus-in-hook 'feebleline-mode-line-proxy-fn)
    (force-mode-line-update)
    (redraw-display)
    (feebleline--clear-echo-area)))




                                        ;
