;;; an collection of functions that I developed to execute some commands
;;; asynchronously
;;; only run ob unix-based systems

;;; TODO: stick the output window with the result buffer
;;; using dedicated window
;;; TODO: shortcut keys for close the result window
;;; TODO: check process exit status, if not success, not close the result window
;;; TODO: undo function

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; argument for command
(defun tda/command-argument
  (cond-variable argument-string)
  "Check if the cond-variable is non-nil, return the argument string"
  (when (not (equal cond-variable nil))
	argument-string))

(defvar tda/get-files-size-command "du"
  "The name of the command for getting file size. By default, it's \"du\". If your \"du\" command is outside the load path, set the variable to the path of du executable. For example (setq tda/get-files-size-command \"/usr/bin/du\")")

;;; get file size
(defun tda/get-files-size ()
  "Calculate files size for all the marked files"
  (interactive)
  (let ((files (dired-get-marked-files)) command)
	;; the get files size command
	(setq command tda/get-files-size-command)
	(setq command (concat command " -hc "))
	;; add selected file names as arguments to the command
	(dolist (file files)
	  (setq command (concat command (shell-quote-argument file) " ")))
	(message command)
	;; execute the command
	(tat/execute-async command "file size"
					   'tda/get-files-size-handler)))

(defun tda/get-files-size-handler (process event)
  (when (equal (process-status process) 'exit)
	(let ((current-async-buffer (process-buffer process))
		  (current-window (selected-window)))
	(let ((current-async-window (get-buffer-window current-async-buffer)))
	  ;; set point to the end
	  (set-window-point current-async-window
						(buffer-size current-async-buffer))
	  ;; change to the result window
	  (select-window current-async-window)
	  ;; print the message
	  (message
	   (buffer-substring (line-beginning-position) (line-end-position)))
	  ;; switch back the the previous window
	  (select-window current-window)))
	;; close the window
	(tat/close-window process)))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Async Rsync
(defun tda/rsync (dest)
  "Asynchronously copy file using Rsync for dired.
	This function runs only on Unix-based system.
	Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  
  (let ((files (dired-get-marked-files nil current-prefix-arg))
		command)
	;; the rsync command
	(setq command "rsync -avz --progress ")
	;; add all selected file names as arguments to the rsync command
	(dolist (file files)
	  (setq command (concat command (shell-quote-argument file) " ")))
	;; append the destination to the rsync command
	(setq command (concat command (shell-quote-argument dest)))
	;; execute the command asynchronously
	(tat/execute-async command "rsync" 'tda/rsync-handler)))

(defun tda/rsync-sudo (dest)
  "Asynchronously copy file using Rsync for dired.
	This function runs only on Unix-based system.
	Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  
  (let ((files (dired-get-marked-files nil current-prefix-arg))
		command)
	;; the rsync command
	(setq command "sudo rsync -avz --progress ")
	;; add all selected file names as arguments to the rsync command
	(dolist (file files)
	  (setq command (concat command (shell-quote-argument file) " ")))
	;; append the destination to the rsync command
	(setq command (concat command (shell-quote-argument dest)))
	;; execute the command asynchronously
	(tat/execute-async command "rsync" nil)))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Rsync with delete option function
(defun tmtxt/dired-async-rsync-delete (dest)
  "Asynchronously copy file using Rsync for dired include the delete option
	This function runs only on Unix-based system.
	Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync delete to:" (dired-dwim-target-directory)))))
  ;; allow delete
  (setq tmtxt/dired-async-rsync-allow-delete t)
  ;; call the rsync function
  (tmtxt/dired-async-rsync dest)
  ;; reset the delete option
  (setq tmtxt/dired-async-rsync-allow-delete nil))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; async zip files
(defvar tmtxt/dired-async-zip-compression-level
  "9" "The compression level for dired async zip command, from 0-9. This variable is a string, so if you change this value, please set it as a string.")

(defun tmtxt/dired-async-zip (output)
  "Asynchronously compress marked files to the output file"
  (interactive
   (list (expand-file-name (read-file-name "Add to file: "))))

  (let (dired-async-zip-command
		(files (dired-get-marked-files nil current-prefix-arg)))
	;; the zip command
	(setq dired-async-zip-command
		  (concat "zip -ru" tmtxt/dired-async-zip-compression-level " "))
	;; append the output file
	(setq dired-async-zip-command
		  (concat dired-async-zip-command (shell-quote-argument output) " "))
	;; add all selected files as argument
	(dolist (file files)
	  (setq dired-async-zip-command
			(concat dired-async-zip-command
					(shell-quote-argument
					 (file-name-nondirectory file)) " ")))
	;; execute the command asynchronously
	(tmtxt/dired-async dired-async-zip-command "zip"
					   'tmtxt/dired-async-zip-process-handler)))

(defun tmtxt/dired-async-zip-process-handler (process event)
  "Handler for window that displays the zip process.

	Usage: Pass it as an argument when calling tmtxt/dired-async

	The function will print the message to notify user that the process is
	completed and automatically kill the buffer and window that runs the
	process."

  ;; check if the process status is exit, then kill the buffer and the window
  ;; that contain that process after 5 seconds (for the user to see the output)
  (when (equal (process-status process) 'exit)
	;; get the current async buffer and window
	(tmtxt/dired-async-close-window process)))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Uncompress function
(defun tmtxt/dired-async-unzip ()
  "Asynchronously decompress the zip file at point"
  (interactive)

  (let (dired-async-unzip-command
		dired-async-unzip-output-directory
		(file (dired-get-filename 'verbatim)))

	;; new directory name for the output files
	(setq dired-async-unzip-output-directory
		  (file-name-sans-extension
		   (dired-get-filename 'verbatim)))

	;; the unzip command
	(setq dired-async-unzip-command "unzip ")
	;; append the file name
	(setq dired-async-unzip-command
		  (concat dired-async-unzip-command
				  (shell-quote-argument file) " "))
	;; append the output directory name
	(setq dired-async-unzip-command
		  (concat dired-async-unzip-command "-d "
				  (shell-quote-argument dired-async-unzip-output-directory)))

	;; execute the command asynchronously
	(tmtxt/dired-async dired-async-unzip-command "unzip"
					   'tmtxt/dired-async-unzip-process-handler)))

(defun tmtxt/dired-async-unzip-process-handler (process event)
  "Handler for window that displays the zip process.

	Usage: Pass it as an argument when calling tmtxt/dired-async

	The function will print the message to notify user that the process is
	completed and automatically kill the buffer and window that runs the
	process."

  ;; check if the process status is exit, then kill the buffer and the window
  ;; that contain that process after 5 seconds (for the user to see the output)
  (when (equal (process-status process) 'exit)
	;; get the current async buffer and window
	(tmtxt/dired-async-close-window process)))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Rsync from multiple directories
(defvar tmtxt/dired-async-rsync-multiple-file-list
  () "The list of the files to be copied")

(defun tmtxt/dired-async-rsync-multiple-mark-file ()
  "Add file to waiting list for copying"
  (interactive)
  ;; Add file to the list
  (add-to-list 'tmtxt/dired-async-rsync-multiple-file-list
			   (dired-get-filename))
  ;; Message for user
  (message
   (concat "File " (dired-get-filename 'verbatim) " added to waiting list.")))

(defun tmtxt/dired-async-rsync-multiple-empty-list ()
  "Empty the waiting list"
  (interactive)
  ;; Empty the list
  (setq tmtxt/dired-async-rsync-multiple-file-list '())
  ;; message for the user
  (message "Waiting list empty."))

(defun tmtxt/dired-async-rsync-multiple-remove-item ()
  "Remove the file at point from the waiting list if it is in"
  (interactive)
  (let ((file-to-remove (dired-get-filename)))
	;; remove the item from the list
	(setq tmtxt/dired-async-rsync-multiple-file-list
		  (remove file-to-remove tmtxt/dired-async-rsync-multiple-file-list))
	;; message for the use
	(message
	 (concat "File " (dired-get-filename 'verbatim) " removed from the list."))))

;; Copy file from multiple directories
(defun tmtxt/dired-async-rsync-multiple ()
  "Mark file in multiple places and then paste in 1 directory"
  (interactive)

  (let (dired-async-rsync-multiple-command)
	(if (equal tmtxt/dired-async-rsync-multiple-file-list ())
		(progn
		  (message "Please add file to the waiting list."))
	  (progn
		;; the rsync command
		(setq dired-async-rsync-multiple-command "rsync -arvz --progress ")
		;; add all selected file names as arguments to the rsync command
		(dolist (file tmtxt/dired-async-rsync-multiple-file-list)
		  (setq dired-async-rsync-multiple-command
				(concat dired-async-rsync-multiple-command (shell-quote-argument file) " ")))
		;; append the destination to the rsync command
		(setq dired-async-rsync-multiple-command
			  (concat dired-async-rsync-multiple-command
					  (shell-quote-argument (expand-file-name default-directory))))
		;; execute the command asynchronously
		(tmtxt/dired-async dired-async-rsync-multiple-command "rsync"
						   'tmtxt/dired-async-rsync-process-handler)
		;; empty the waiting list
		(tmtxt/dired-async-rsync-multiple-empty-list)))))

(provide 'tmtxt-dired-async)
