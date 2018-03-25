;;;; cl-digest-uniq-files.lisp

(in-package #:cl-digest-uniq-files)

(defparameter *digests* (make-hash-table)
  "The hash table where the file digests will be stored.")

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

(defun process-file (pathname &key target-dir)
  (let ((file-digests (digest-file-string pathname)))
    (unless (gethash file-digests *digests*)
      (let ((final-path (fad:merge-pathnames-as-file
                         target-dir
                         (make-pathname :name file-digests
                                        :type (pathname-type pathname)))))
        (copy-file pathname final-path))
      (setf (gethash file-digests *digests*) T))))

(defun digest-file-string (pathname)
  "The string representation of the md5 digest of the given file."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :md5 pathname)))

(defun process-directory (from to &rest ext)
  (declare (type string from to))
  ;; Make sure directory exists.
  (unless (fad:directory-exists-p from)
    (error (format nil "directory ~a does not exist" from)))
  (ensure-directories-exist to)
  (let ((cnt-total 0)
        (cnt-exists 0))
    (fad:walk-directory
     from
     (lambda (pathname)
       (incf cnt-exists)
       (process-file pathname :target-dir to))
     ;; Only get those files whose extension matches the given ones.
     :test (lambda (pathname)
             (incf cnt-total)
             (pathname-has-extension-p pathname ext)))))
