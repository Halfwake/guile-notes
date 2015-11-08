#! /usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 optargs)
	     (ice-9 getopt-long)
	     (ice-9 rdelim))

(define note-file-path "notes.txt")

(define (save-task task)
  (with-file note-file-path
    (lambda (file)
      (display task file)
      (newline file))
    #:mode "a"))

(define* (with-file filename proc #:key (mode #f) (encoding #f))
  "Open FILENAME with MODE and pass it to PROC. The filename is
guranteed to close."
  (unless mode
    (error "Mode must be provided."))  
  (let ((file #f))
    (dynamic-wind
      (lambda ()
	(set! file (open-file filename mode #:encoding encoding)))
      (lambda ()
	(proc file))
      (lambda ()      
	(close-port file)))))

(define (view-tasks)
  "Display all tasks to stdout."
  (when (file-exists? note-file-path)
    (with-input-from-file note-file-path
      (lambda ()
	(let iter ()
	  (define line (read-line))
	  (unless (eof-object? line)
	    (display line)
	    (newline)
	    (iter)))))))

(define (clear-tasks)
  "Delete all tasks."
  (delete-file note-file-path))

(define version "Note 0.0.1")
(define help "\
note [options]
 -v --version  Display version
 -h --help     Display help
 -n --new      New task
 -s --show     Show tasks")

(define (main args)
  (let* ((option-spec '((version (single-char #\v) (value #f))
			(help (single-char #\h) (value #f))
			(new (single-char #\n) (value #t))
			(show (single-char #\s) (value #f))))
	 (options (getopt-long args option-spec))
	 (version-wanted (option-ref options 'version #f))
	 (help-wanted (option-ref options 'help #f))
	 (new-task (option-ref options 'new #f))
	 (show-wanted (option-ref options 'show #f)))
    (when version-wanted
      (display version)
      (newline))
    (when help-wanted
      (display help)
      (newline))
    (when new-task
      (save-task new-task))
    (when show-wanted
      (view-tasks))))
