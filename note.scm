#! /usr/bin/guile \
-e main -s
!#

;; Guile Notes
;; Copyright (C) 2015  Andrew "Drew" Dudash

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

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
