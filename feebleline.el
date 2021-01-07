;;; feebleline.el --- Replace modeline with a slimmer proxy

;; Copyright 2018 Benjamin Lindqvist

;; Author: Benjamin Lindqvist <benjamin.lindqvist@gmail.com>
;; Maintainer: Benjamin Lindqvist <benjamin.lindqvist@gmail.com>
;; URL: https://github.com/tautologyclub/feebleline
;; Package-Version: 2.0
;; Version: 2.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For hardline Luddite editing!

;; Feebleline removes the modeline and replaces it with a slimmer proxy
;; version, which displays some basic information in the echo area
;; instead.  This information is only displayed if the echo area is not used
;; for anything else (but if you switch frame/window, it will replace whatever
;; message is currently displayed).

;; Feebleline now has a much improved customization interface. Simply set
;; feebleline-format-functions to whatever you want! Example:

;; (setq
;;  feebleline-format-functions
;;  '((feebleline-line-number)
;;    (feebleline-column-number)
;;    (feebleline-file-directory)
;;    (feebleline-file-or-buffer-name)
;;    (feebleline-file-modified-star)
;;    (magit-get-current-branch)
;;    (projectile-project-name)))

;; The elements should be functions, accepting no arguments, returning either
;; nil or a valid string. Even lambda functions work (but don't forget to quote
;; them). Optionally, you can include keywords  after each function, like so:

;; (feebleline-line-number :post "" :fmt "%5s")

;; Accepted keys are pre, post, face, fmt and align.
;; See source code for inspiration.

;;; Code:
(require 'seq)
(require 'subr-x)
(require 'vc-git)


(define-obsolete-variable-alias
  'feebleline-msg-functions
  'feebleline-format-functions)

(defcustom feebleline-format-functions
  '((feebleline-line-number         :post "" :fmt "%5s")
    (feebleline-column-number       :pre ":" :fmt "%-2s")
    (feebleline-file-directory      :face feebleline-dir-face :post "")
    (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
    (feebleline-file-modified-star  :face font-lock-warning-face :post "")
    (feebleline-git-branch          :face feebleline-git-face :pre " - ")
    (feebleline-project-name        :align right))
  "FIXME: Document me."
  :type 'list
  :group 'feebleline)

(defcustom feebleline-observed-hooks nil
  "Hooks observed by feebleline for updates."
  :type 'boolean
  :group 'feebleline)

(defface feebleline-git '((t :foreground "#444444"))
  "Example face for git branch."
  :group 'feebleline)

(defface feebleline-dir '((t :inherit 'font-lock-variable-name-face))
  "Example face for dir face."
  :group 'feebleline)

(defvar feebleline--home-dir (expand-file-name "~"))

(defvar feebleline--mode-line-format-previous)
(defvar feebleline--window-divider-previous)
(defvar feebleline-last-error-shown nil)

(defvar feebleline--debounce-interval 0.5)
(defvar feebleline--debounce-timer)

(defvar feebleline--minibuf " *Minibuf-0*")

(defun feebleline-git-branch ()
  "Return current git branch, unless file is remote."
  (let ((file-name (buffer-file-name)))
    (unless (and file-name (file-remote-p file-name))
      (car (vc-git-branches)))))

(defun feebleline-linecol-string ()
  "Hey guy!"
  (format "%4s:%-2s" (format-mode-line "%l") (current-column)))

(defun feebleline-previous-buffer-name ()
  "Get name of previous buffer."
  (buffer-name (other-buffer (current-buffer) 1)))

(defun feebleline-line-number ()
  "Line number as string."
  (format "%s" (line-number-at-pos)))

(defun feebleline-column-number ()
  "Column number as string."
  (format "%s" (current-column)))

(defun feebleline-file-directory ()
  "Current directory, if buffer is displaying a file."
  (when (buffer-file-name)
    (replace-regexp-in-string
     (concat "^" feebleline--home-dir) "~"
     default-directory)))

(defun feebleline-file-or-buffer-name ()
  "Current file, or just buffer name if not a file."
  (if (buffer-file-name)
      (file-name-nondirectory (buffer-file-name))
    (buffer-name)))

(defun feebleline-file-modified-star ()
  "Display star if buffer file was modified."
  (when (and (buffer-file-name) (buffer-modified-p)) "*"))

(defun feebleline-project-name ()
  "Return project name if exists, otherwise nil."
  (let ((proj (project-current)))
    (when proj
      (file-name-nondirectory (directory-file-name (cdr proj))))))

(defun feebleline--insert-ignore-errors ()
  "Insert stuff into the echo area, ignoring potential errors."
  (unless (current-message)
    (condition-case err (feebleline--insert)
      (error (unless (equal feebleline-last-error-shown err)
               (setq feebleline-last-error-shown err)
               (message (format "feebleline error: %s" err)))))))

(defun feebleline--debounced-insert ()
  "Debounce invocations of `feebleline--insert-ignore-errors'.
Invocations after the first invocation are debounced by
`feebleline--focus-interval' seconds.  A debounced invocation
also resets the timer.  Function is ran at the trailing end of
the debounce."
  (if (or (not (boundp 'feebleline--debounce-timer))
          (timer--triggered feebleline--debounce-timer))
      (progn
        (setq feebleline--debounce-timer
              (run-with-timer feebleline--debounce-interval
                              nil
                              'feebleline--insert-ignore-errors)))
    (timer-set-time feebleline--debounce-timer
                    (time-add (current-time)
                              (seconds-to-time feebleline--debounce-interval)))))

(defun feebleline--format (message-function &rest properties)
  "Format MESSAGE-FUNCTION based on its PROPERTIES.
Returns a cons with desired alignment and result of
MESSAGE-FUNCTION as a string with text properties added."
  (let* ((arguments (plist-get properties :args))
         (face (or (plist-get properties :face) 'default))
         (alignment (or (plist-get properties :align) 'left))
         (prefix (plist-get properties :pre))
         (format (or (plist-get properties :fmt) "%s"))
         (suffix (or (plist-get properties :post) " "))
         (result (apply message-function arguments)))
    (cons alignment
          (propertize (concat prefix
                              (when result
                                (format format result))
                              suffix)
                      'face face))))

(defun feebleline--insert ()
  "Insert stuff into the mini buffer."
  (unless (current-message)
    (let ((left ())
          (center ())
          (right ()))
      (dolist (format-function feebleline-format-functions)
        (let* ((fragment (apply 'feebleline--format format-function))
               (align (car fragment))
               (string (cdr fragment)))
          (push string (symbol-value align))))
      (with-current-buffer feebleline--minibuf
        (erase-buffer)
        (let* ((left-string (string-join (reverse left)))
               (message-truncate-lines t)
               (max-mini-window-height 1)
               (right-string (string-join (reverse right)))
               (center-string (string-join (reverse center)))
               (length-left (length left-string))
               (length-center (length center-string))
               (length-right (length right-string))
               (frame-width (frame-width))
               (left-space (- (/ frame-width 2) length-left (/ length-center 2)))
               (right-space (- frame-width length-left left-space length-center length-right))
               ;; When one side of padding is completely consumed start removing
               ;; padding from the opposite side
               (left-space (+ left-space (min 0 right-space)))
               (right-space (+ right-space (min 0 left-space)))
               (left-padding (make-string (max 0 left-space) ?\ ))
               (right-padding (make-string (max 0 right-space) ?\ )))
          (insert (concat left-string left-padding center-string right-padding right-string)))))))

(defun feebleline--clear-echo-area ()
  "Erase echo area."
  (with-current-buffer feebleline--minibuf
    (erase-buffer)))

(defun feebleline--observer (&rest _arguments)
  (feebleline--debounced-insert))

(defun feebleline--mode-turn-on ()
  "Some default settings that works well with feebleline."
  (setq feebleline--mode-line-format-previous mode-line-format)
  (setq window-divider-default-bottom-width 1
        window-divider-default-places (quote bottom-only))
  (setq feebleline--window-divider-previous window-divider-mode)
  (window-divider-mode 1)
  (setq-default mode-line-format nil)
  (dolist (buffer (seq-remove #'minibufferp (buffer-list)))
    (with-current-buffer buffer
      (setq mode-line-format nil)))
  (dolist (hook feebleline-observed-hooks)
    (add-hook hook #'feebleline--observer)))

(defun feebleline--mode-turn-off ()
  "Disable feebleline settings."
  (window-divider-mode feebleline--window-divider-previous)
  (set-face-attribute 'mode-line nil :height 1.0)
  (setq-default mode-line-format feebleline--mode-line-format-previous)
  (dolist (buffer (seq-remove #'minibufferp (buffer-list)))
    (with-current-buffer buffer
      (setq mode-line-format feebleline--mode-line-format-previous)))
  (dolist (hook feebleline-observed-hooks)
    (remove-hook hook #'feebleline--observer))
  (force-mode-line-update)
  (redraw-display)
  (feebleline--clear-echo-area))

;;;###autoload
(define-minor-mode feebleline-mode
  "Replace modeline with a slimmer proxy."
  :require 'feebleline
  :global t
  (if feebleline-mode
      (feebleline--mode-turn-on)
    (feebleline--mode-turn-off)))

(provide 'feebleline)
;;; feebleline.el ends here
