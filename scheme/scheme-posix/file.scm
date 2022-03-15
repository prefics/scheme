;; file.scm -- file management functions

;;;
;; = File management procedure
;;;

;;;
;; Create a directory on the file system. `name` specifies the name of
;; the directory and `mode` defined the file mode to give to the directory.
(define (create-directory! name mode)
  (let ((errno (with-process-aligned
                (lambda ()
                  (posix-mkdir name mode)))))
    errno))

;;;
;; Create a FIFO on the file system. `name` specifies the name of the
;; FIFO file and `perm` specifies the permission of the created FIFO file.
(define (create-fifo! name perm)
  (with-process-aligned
   (lambda ()
     (posix-mknod name perm))))

;;;
;; Create a hard link on the file system. `old-name` defines the file
;; pointed to by the hard link. `new-name` defines the name of the
;; created hard link file.
(define (create-hard-link old-name new-name . override?) #f)

;;;
;; Create a symbolic link on the file system. `old-name` defines the
;; file pointed to by the symbolic link. `new-name` defines the name
;; of the created symbolic link file.
(define (create-symlink! old-name new-name . override?)
  (let ((error? (with-process-aligned
                 (lambda ()
                   (%posix-symlink old-name new-name)))))
    (if error?
        (error "Error ~a when creating symlink ~a to ~a"
               error? new-name old-name)
        error?)))

;;;
;; Deletes a the directory `name` from the file system. Note that
;; `name` should refer to a directory file.
(define (delete-directory! name)
  (let ((errno (with-process-aligned
                (lambda ()
                  (posix-rmdir name)))))
    errno))

;;;
;; Deletes the file pointed to by `name` from the file system. Note
;; that `name` should refer to a regular file.
(define (delete-file! name)
  (let ((errno (with-process-aligned 
                (lambda ()
                  (posix-unlink name)))))
    errno))

;;;
;; Deletes the file pointed to by `name` without regards for its type
;; (directory or otherwise).
(define (delete-filesys-object! name) #f)

;;;
;; Returns the file pointed to by the symbolic link file named `name`.
(define (read-symlink name)
  (let ((errno? (with-process-aligned
                 (lambda ()
                   (%posix-readlink name)))))
    (if (number? error?)
        (error "Error ~a reading symlink value for ~a" errno? name)
        errno?)))

;;;
;; Rename a file on the file system. `old-name` refers to the actual
;; name and `new-name` defines the new name of the file.
(define (rename-file! old-name new-name)
  (let ((errno (with-process-aligned 
                (lambda () 
                  (posix-rename old-name new-name)))))
    errno))

;;;
;; Sets the file mode, given as `mode` on the file named `name`.
(define (set-file-mode! name mode)
  (let ((errno (with-process-aligned
                (lambda ()
                  (posix-chmod name mode)))))
    errno))

;;;
;; Sets the owner, given by its user id `uid` of the file pointed to
;; by `name`.
(define (set-file-owner! name uid)
  (let ((errno (with-process-aligned 
                (lambda ()
                  (posix-chown name uid (- 2 3))))))
    errno))

;;;
;; Sets the group, given by its group id `gid`, of the file pointed to
;; by `name`.
(define (set-file-group! name gid)
  (let ((errno (with-process-aligned
                (lambda ()
                  (posix-chown name (- 2 3) gid)))))
    errno))

;;;
;; Sets the file times associated with the file `name`. `access-time`
;; is the time to set for the last access time associated with the
;; file. `mod-time` is the time to set for the last modification time
;; associated with the file.
(define (set-file-time! name &opt access-time mod-time)
  (with-process-aligned
   (lambda ()
     (posix-utime name access-time mod-time))))

;;;
;; Sync the file associated with the specified `port`.
(define (sync-file! port)
  (if (channel-port? port)
      (let ((errno (posix-sync (channel-port-channel port))))
	errno)
      (error "PORT in not a channel port")))

;;;
;; Sync the complete file system on the machine.
(define (sync-file-system!)
  #f)

;;;
;; Truncate the length of the file refered to by `name` to the
;; specified length `len`
(define (truncate-file! name len)
  (let ((errno (with-process-aligned
                (lambda ()
                  (posix-truncate name len)))))
    errno))

;;;
;; == File information

;;;
;; A record containing all information associated with a file.
(define-record-type <file-info>
  (make-file-info type device inode mode nlinks uid gid size 
		  atime mtime ctime)
  file-info?
  (type   file-info-type set-file-info-type!)
  (device file-info-device)
  (inode  file-info-inode)
  (mode   file-info-mode)
  (nlinks file-info-nlinks)
  (uid    file-info-uid)
  (gid    file-info-gid)
  (size   file-info-size)
  (atime  file-info-atime)
  (mtime  file-info-mtime)
  (ctime  file-info-ctime))

;;;
;; Returns a `file-info` record of the file refered to by `name`.  If
;; `name` does not exist, an error is raised.
(define (file-info name . chase?)
  (let ((fileinfo (with-process-aligned 
                   (lambda ()
                     (posix-stat name 
                                 (make-file-info #f #f #f #f #f 
                                                 #f #f #f #f #f #f))))))
    (if (eq? fileinfo 2)
	(error "File does not exist" name)
	fileinfo)))

;; @todo should check for existence and accessibility of FILE-NAME
;;;
;; Returns the type of the file pointed to by `file-name`.
(define (file-type file-name) (file-info-type (file-info file-name)))
;;;
;; Returns the device associated with the file pointed to by
;; `file-name`.
(define (file-device file-name) (file-info-device (file-info file-name)))
;;;
;; Returns the device associated with the file pointed to by
;; `file-name`.
(define (file-inode file-name) (file-info-inode (file-info file-name)))
;;;
;; Returns the mode associated with the file pointed to by
;; `file-name`.
(define (file-mode file-name) (file-info-mode (file-info file-name)))
;;;
;; Returns the number of hard link associated with the file pointed to
;; by `file-name`.
(define (file-nlinks file-name) (file-info-nlinks (file-info file-name)))
;;;
;; Returns the user id associated with the file pointed to by
;; `file-name`.
(define (file-uid file-name) (file-info-uid (file-info file-name)))
;;;
;; Returns the group id associated with the file pointed to by
;; `file-name`.
(define (file-gid file-name) (file-info-gid (file-info file-name)))
;;;
;; Returns the size of the file pointed to by `file-name`.
(define (file-size file-name) (file-info-size (file-info file-name)))
;;;
;; Returns the last file access time of the file pointed to by
;; `file-name`.
(define (file-atime file-name) (file-info-atime (file-info file-name)))
;;;
;; Returns the last modification time of the file pointed to by
;; `file-name`.
(define (file-mtime file-name) (file-info-mtime (file-info file-name)))
;;;
;; Returns the creation time of the file pointed to by `file-name`.
(define (file-ctime file-name) (file-info-ctime (file-info file-name)))

;; @todo should check what file-mtime returns and check that < apply
;;;
;; returns true is file `f1` is newer than file `f2`. A file is newer
;; than another one if its last modification time is before the last
;; modification time of the other one.
(define (file-newer? f1 f2) (< (file-mtime f1) (file-mtime f2)))

;;;
;; Returns `#t` if the file pointed to by `name` is a directory.
(define (file-directory? name) (= $mode/directory (file-info-type (file-info name))))
;;;
;; Returns `#t` if the file pointed to by `name` is a FIFO.
(define (file-fifo? name)      (= $mode/fifo (file-info-type (file-info name))))
;;;
;; Returns `#t` if the file pointed to by `name` is a regular file.
(define (file-regular? name)   (= $mode/file (file-info-type (file-info name))));;;
;; Returns `#t` if the file pointed to by `name` is a socket
(define (file-socket? name)    (= $mode/socket (file-info-type (file-info name))))
;;;
;; Returns `#t` if the file pointed to by `name` is a special file.
(define (file-special? name)   (= $mode/block (file-info-type (file-info name))))
;;;
;; Returns `#t` if the file pointed to by `name` is a symbolic link.
(define (file-symlink? name)   (= $mode/link (file-info-type (file-info name))))
;;;
;; Returns `#t` if file `name` is not readable.
(define (file-not-readable? name) (not (file-readable? name)))
;;;
;; Returns `#t` if file `name` is not writable.
(define (file-not-writable? name) (not (file-writable? name)))
;;;
;; Returns `#t` if file `name` is not executable.
(define (file-not-executable? name) (not (file-executable? name)))

;;;
;; Returns `#t` if file `name` is readable
(define (file-readable? name)
  (let ((ok? (with-process-aligned 
              (lambda ()
                (posix-access name $access/read)))))
    (if ok? #f #t)))

;;;
;; Returns `#t` if file `name` is writeable
(define (file-writable? name)
  (let ((ok? (with-process-aligned 
              (lambda ()
                (posix-access name $access/write)))))
    (if ok? #f #t)))

;;;
;; Returns `#t` if file `name` is executable.
(define (file-executable? name)
  (let ((ok? (with-process-aligned
              (lambda ()
                (posix-access name $access/execute)))))
    (if ok? #f #t)))

;;;
;; Returns `#t` if file pointed to by `name` does not exist on the
;; file system.
(define (file-not-exists? name . chase?)
  (not (file-exists? name)))

;;;
;; Returns `#t` if file pointed to by `name` already exists on the
;; file system.
(define (file-exists? name . chase?)
  (let ((ok? (with-process-aligned
              (lambda ()
                (posix-access name $access/exist)))))
    (if ok? #f #t)))

;; not particularly efficient
;;;
;; Returns the file contents of file named `file-name`. `file-name`
;; should ultimately be a file having a contens (not be a directory).
(define (file-contents file-name)
  (if (file-readable? file-name)
      (let* ((len (file-info-size (file-info file-name)))
	     (str (make-string len #\space)))
	(with-input-from-file file-name
	  (lambda ()
	    (let fill ((i 0))
	      (if (< i len)
		  (let ((ch (read-char)))
		    (string-set! str i ch)
		    (fill (+ i 1)))
		  str)))))
      (error "file ~a not readable" file-name)))

;;;
;; == Directory information

;;; 
;; Returns the list of file names contained in the directory named
;; `dir`.
(define (directory-files dir)
  (let ((files (with-process-aligned
                (lambda ()
                  (posix-dirfiles (if (string=? dir "")
                                      "."
                                      dir))))))
    (if (number? files)
	(error "error ~a in DIRECTORY-FILES" files)
	files)))

;;;
;; Returns a list of file name matching the different patterns
;; specified as `patterns`.
(define (glob . patterns) #f)

(define (globe-quote str) #f)
(define (file-match root dot-files? . patterns) #f)

;;;
;; Creates a temporary file. `prefix` specifies the prefix of the name
;; of the file. Note the temporary file is created under `/tmp`.
(define (create-temp-file! prefix)
  (posix-tempnam "/tmp" prefix))

(define (temp-file-iterate maker &opt template) #f)

(define *temp-file-template* #f)

(define (temp-file-channel) #f)

;;;
;; Executes the procedure specified by `thunk` with a new temporary
;; file.  `thunk` should expect one argument containing the name of
;; the temporary file. `name` is the prefix for the name of the
;; temporary file.
(define (with-temporary-file name thunk)
  (let ((file-name (create-temp-file! name)))
    (dynamic-wind
 	(lambda () #f)
 	(thunk file-name)
 	(lambda () (delete-file! file-name)))))

