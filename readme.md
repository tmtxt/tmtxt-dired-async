# Asynchoronous execution library for Emacs Dired

A library for Emacs Dired mode to copy, compress, decompress files
asynchronously. It also provides the ability to mark files in multiple
directories and then copy all of them into a destination library. This library
is designed for Unix-based system like MacOS, Ubuntu,...

For more information, please refer to this project's homepage:
[tmtxt-dired-async homepage](http://truongtx.me/tmtxt-dired-async.html)

# Installation

Clone this repo and put it somewhere in your load-path  
Add this to your .emacs

	(require 'tmtxt-dired-async)

# Features

You don't have to follow the key bindings below, you can change them to whatever
you want. They are just examples.

## Asynchronously Copy files

This feature uses **rsync** for file copying. To use it, simply mark the files
that you want and then activate this function.

	(define-key dired-mode-map (kbd "C-c C-r") 'tmtxt/dired-async-rsync)

Show the progress when copy

	(setq-default tmtxt/dired-async-rsync-show-progress t)

Show verbosity when copy

	(setq-default tmtxt/dired-async-rsync-show-verbosity t)

Use archive mode when copy (to preserve time stamp)

	(setq-default tmtxt/dired-async-rsync-archive-mode t)

User compression mode when copy

	(setq-default tmtxt/dired-async-rsync-compress-mode t)

## Asynchronously Copy files (with delete option)

This feature is similar to the above feature. It also uses **rsync** to copy
file and the same config with the above. However, it includes the delete option
for rsync to ensure that you have the destination folder exactly the same as the
source directory

	(define-key dired-mode-map (kbd "C-c C-t") 'tmtxt/dired-async-rsync-delete)

Set the deletion method for rsync delete (--delete-after, --delete-during, --delete-before)

	(setq-default tmtxt/dired-async-rsync-delete-method "--delete-after")

## Asynchronously Compress files

Compress all marked files.

	(define-key dired-mode-map (kbd "C-c C-z") 'tmtxt/dired-async-zip)

Set the compression level, from 0-9

	(setq-default tmtxt/dired-async-zip-compression-level "9")

## Asynchronously Decompress files

Decompress the zip file at point.

	(define-key dired-mode-map (kbd "C-c C-u") 'tmtxt/dired-async-unzip)

## Copy from multiple directories

This feature allows you to select many files from multi directories and then
copy all of them to a destination folder.

	(define-key dired-mode-map (kbd "C-c C-a") 'tmtxt/dired-async-rsync-multiple-mark-file)
	(define-key dired-mode-map (kbd "C-c C-e") 'tmtxt/dired-async-rsync-multiple-empty-list)
	(define-key dired-mode-map (kbd "C-c C-d") 'tmtxt/dired-async-rsync-multiple-remove-item)
	(define-key dired-mode-map (kbd "C-c C-v") 'tmtxt/dired-async-rsync-multiple)

**C-c C-a** to add the file at point to the list for later copy. **C-c C-d** to
remove the current file at point from the waiting list. **C-c C-e** to empty the
waiting list. Finally, **C-c C-v** to copy all files in the list to the current
directory.

## Other config

Set the time to close the result window after finish, measured in second

	(setq-default tmtxt/dired-async-post-process-window-show-time "5")

Set the height for the result window, measured in the number of lines

	(setq-default tmtxt/dired-async-result-window-height 10)
