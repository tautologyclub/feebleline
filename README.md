# feebleline
For hardline Luddite editing!

Feebleline removes the modeline and replaces it with a slimmer proxy
version, which displays some basic information in the echo area
instead.  This information is only displayed if the echo area is not used
for anything else (but if you switch frame/window, it will replace whatever
message is currently displayed).

NOTE: feebleline.el will look considerably better with a small border between
the echo area and your other windows. This is best achieved with:

    (window-divider-mode t)
    (setq window-divider-default-bottom-width 1)
    (setq window-divider-default-places (quote bottom-only))

But in the new version, feebleline checks your major version and applies this
automatically when feebleline-mode gets activated. If you're on emacs 24 where
window-divider-mode is not available, then feebleline does a silly hack where
it keeps the modeline active, but shrinks it so that it looks like a
border.

The modeline gets restored when you toggle off feebleline-mode, of course.

## Customizations
To customize what is displayed, a smidget of elisp is required. The basic idea
is to override the following setq:

    (setq feebleline-mode-line-text
          '(("%6s"      ((format "%s,%s" (format-mode-line "%l") (current-column))))
            (" : %s"    ((if (buffer-file-name) (buffer-file-name)
                           (buffer-name))) (face feebleline-bufname-face))
            ("%s"       ((if (and (buffer-file-name) (buffer-modified-p)) "*" "" ))
             (face feebleline-asterisk-face))
            (" | %s"    ((feebleline-previous-buffer-name))
             (face feebleline-previous-buffer-face))))

Just replace the parts you don't like or add new ones as you please. If there's
any interest in feebleline.el, we can hopefully get some example customizations
(along with screenshots) posted on this page.

## Screenshots
These screenshots are a bit out of date. I promise to update them shortly.

![ScreenShot](scrot2.png)

How it looks when the file has been modified:
![ScreenShot](scrot3.png)
