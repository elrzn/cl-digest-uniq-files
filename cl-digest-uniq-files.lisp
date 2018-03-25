;;;; cl-digest-uniq-files.lisp

(in-package #:cl-digest-uniq-files)

(defun string-in-list-p (e l)
  "Checks whether a string is contained in a list."
  (declare (type string e)
           (type (proper-list string) l))
  (dolist (x l)
    (when (string-equal e x)
      (return T))))

(defun pathname-has-extension-p (pathname extensions)
  "Checks whether the file's extension is contained within the given ones."
  (let ((type (pathname-type pathname)))
    (string-in-list-p type extensions)))

(defun process-file (pathname)
  (format t "working on ~a~%" pathname)
  ;; nyi
  )

(defun process-directory (from to &rest ext)
  (declare (ignore to))
  (declare (type string from))
  ;; Make sure directory exists.
  (unless (fad:directory-exists-p from)
    (error (format nil "directory ~a does not exist" from)))
  (let ((cnt-total 0)
        (cnt-exists 0))
    (fad:walk-directory
     from
     (lambda (pathname)
       (incf cnt-exists)
       (process-file pathname))
     ;; Only get those files whose extension matches the given ones.
     :test (lambda (pathname)
             (incf cnt-total)
             (pathname-has-extension-p pathname ext)))))
