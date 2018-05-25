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
      :custom       (feebleline-show-git-branch             t)
                    (feebleline-show-dir                    t)
                    (feebleline-show-time                   nil)
                    (feebleline-show-previous-buffer        nil)
      :config       (feebleline-mode 1))

## Screenshots
These screenshots are a bit out of date. I promise to update them shortly.

![ScreenShot](scrot2.png)

How it looks when the file has been modified:
![ScreenShot](scrot3.png)
