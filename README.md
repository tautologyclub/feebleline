# feebleline
For hardline Luddite editing!

Feebleline removes the modeline and replaces it with a slimmer proxy
version, which displays some basic information in the echo area
instead.  This information is only displayed if the echo area is not used
for anything else (but if you switch frame/window, it will replace whatever
message is currently displayed).

The modeline gets restored when you toggle off feebleline-mode, of course.

## Note on new version
If you upgrade feebleline and it errors, something like "error running timer
feebleline-mode-line-proxy-fn" or whatever, then just toggle feebleline-mode on
and then off (i.e. M-x feebleline twice).

Hopefully you'll like this new version better. If you don't, please share your
reasoning in an issue :)


## Customizations
There are some customizations available, check out

    customize-group feebleline

to see what you can do. An example init snippet would look something like:

    (use-package    feebleline
      :ensure       t
      :config       (setq feebleline-msg-functions
                          '((feebleline-line-number         :post "" :fmt "%5s")
                            (feebleline-column-number       :pre ":" :fmt "%-2s")
                            (feebleline-file-directory      :face feebleline-dir-face :post "")
                            (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
                            (feebleline-file-modified-star  :face font-lock-warning-face :post "")
                            (magit-get-current-branch       :face feebleline-git-face :pre " - ")
                            (feebleline-project-name        :align right)))
                    (feebleline-mode 1))

The minibuffer should now show something similar to:

        1:0  ~/feebleline/feebleline.el - development                                                feebleline

## Screenshots
This is a screenshow from the latest version (yes that is my
actual ugly day-to-day theme):

![ScreenShot](scrot_v1_1.png)

### Outdated scrots

(but on less ugly theme :P)

![ScreenShot](scrot2.png)

How it looks when the file has been modified:
![ScreenShot](scrot3.png)
