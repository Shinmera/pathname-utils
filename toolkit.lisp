#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.pathname-utils)

(defvar *wild-component* #+cormanlisp "*" #-cormanlisp :wild)
(defvar *wild-file* (make-pathname :directory NIL
                                   :name *wild-component*
                                   :type *wild-component*
                                   :version (and #-(or allegro abcl xcl) *wild-component*)))
(defvar *wild-directory* (make-pathname :directory `(:relative :wild)
                                        :name NIL :type NIL :version NIL))
(defvar *wild-inferiors* (make-pathname :directory '(:relative :wild-inferiors)
                                        :name NIL :type NIL :version NIL))
(defvar *wild-path* (merge-pathnames *wild-file* *wild-directory*))

(defun clean-directory-spec (dir)
  (when dir
    (let ((parts ()))
      (loop with back = 0
            for el in (reverse dir)
            until (find el '(:absolute :relative :home))
            do (cond 
                 ((unspecific-p el))
                 ((equal el "."))
                 ((eql el :back) (incf back))
                 ((< 0 back) (decf back))
                 (T (push el parts)))
            finally (case el
                      (:home (loop repeat back do (push :up parts)) (push :home parts))
                      (:relative (loop repeat back do (push :up parts)))))
      (list* (car dir) parts))))

(defun normalize-directory-spec (dir)
  (clean-directory-spec
   (etypecase dir
     (null NIL)
     (string `(:absolute ,dir))
     (cons
      (if (member (first dir) '(:absolute :relative))
          dir
          #+gcl `(:relative ,dir)
          #-gcl (error "Invalid directory component ~s" dir))))))

(defun normalize-pathname (pathname)
  (let ((pathname (pathname pathname)))
    (flet ((maybe-component (component)
             (let ((value (funcall component pathname)))
               (if (unspecific-p value) NIL value))))
      (make-pathname
       :host (maybe-component #'pathname-host)
       :device (maybe-component #'pathname-device)
       :name (maybe-component #'pathname-name)
       :type (maybe-component #'pathname-type)
       :version (maybe-component #'pathname-version)
       :directory (normalize-directory-spec (pathname-directory pathname))
       :defaults pathname))))

(defun pathname* (pathname)
  (typecase pathname
    (pathname pathname)
    (T (normalize-pathname pathname))))

(defun unspecific-p (component)
  (member component '(NIL :unspecific "")))

(defun relative-p (pathname)
  (let ((pathname (pathname* pathname)))
    (and (or (eql :relative (car (pathname-directory pathname)))
             (unspecific-p (pathname-directory pathname)))
         pathname)))

(defun absolute-p (pathname)
  (let ((pathname (pathname* pathname)))
    (and (eql :absolute (car (pathname-directory pathname)))
         pathname)))

(defun logical-p (pathname)
  (let ((pathname (pathname* pathname)))
    (and (typep (pathname* pathname) 'logical-pathname)
         pathname)))

(defun physical-p (pathname)
  (let ((pathname (pathname* pathname)))
    (and (typep (pathname* pathname) '(and pathname (not logical-pathname)))
         pathname)))

(defun root-p (pathname)
  (let ((pathname (pathname* pathname)))
    (and (directory-p pathname)
         (equal (pathname-directory pathname) '(:absolute))
         pathname)))

(defun directory-p (pathname)
  (let ((pathname (pathname* pathname)))
    (and (unspecific-p (pathname-name pathname))
         (unspecific-p (pathname-type pathname))
         pathname)))

(defun file-p (pathname)
  (let ((pathname (pathname* pathname)))
    (and (not (directory-p pathname))
         pathname)))

(defun subpath-p (subpath base)
  (let ((pathname (enough-pathname subpath base)))
    (and (relative-p pathname)
         pathname)))

(defun pathname= (a b)
  (let ((a (normalize-pathname a))
        (b (normalize-pathname b)))
    (labels ((normalize (part)
               (if (unspecific-p part) NIL part))
             (part= (part)
               (equal (normalize (funcall part a))
                      (normalize (funcall part b)))))
      (and (part= #'pathname-name)
           (part= #'pathname-type)
           (part= #'pathname-host)
           (part= #'pathname-device)
           (part= #'pathname-version)
           (part= #'pathname-directory)
           T))))

(defun pathname-equal (a b)
  (pathname= (truename a) (truename b)))

(defun to-root (pathname)
  (make-pathname :name NIL :type NIL :version NIL :directory '(:absolute) :defaults (pathname pathname)))

(defun to-physical (pathname)
  (let ((pathname (pathname* pathname)))
    (if (logical-p pathname)
        (translate-logical-pathname pathname)
        pathname)))

(defun to-directory (pathname)
  (case pathname
    ((:up :back) (make-pathname :name NIL :type NIL :version NIL :directory `(:relative ,pathname)))
    ((:home) (make-pathname :name NIL :type NIL :version NIL :directory '(:absolute :home)))
    (T (make-pathname :name NIL :type NIL :version NIL :defaults (pathname* pathname)))))

(defun subdirectory (pathname &rest subdirs)
  (loop for sub in subdirs
        for subpath = (etypecase sub
                        ((or pathname keyword stream) (to-directory sub))
                        (string (to-directory (concatenate 'string sub "/"))))
        for dir = (merge-pathnames subpath (to-directory pathname))
        then (merge-pathnames subpath dir)
        finally (return dir)))

(defun pop-directory (pathname)
  (let ((pathname (pathname* pathname)))
    (make-pathname :directory (list* (car (pathname-directory pathname))
                                     (butlast (cdr (pathname-directory pathname))))
                   :defaults pathname)))

(defun parent (pathname)
  (let ((pathname (pathname* pathname)))
    (cond ((directory-p pathname)
           (let ((dir (pathname-directory pathname)))
             (if (root-p pathname)
                 pathname
                 (make-pathname
                  :directory (typecase (car (last (cdr dir)))
                               (null (list :relative :up))
                               (string (list* (car dir) (butlast (cdr dir))))
                               (T (append dir '(:up))))
                  :defaults pathname))))
          (T
           (to-directory pathname)))))

(defun upwards (pathname)
  (cond ((directory-p pathname)
         (subdirectory (parent (parent pathname))
                       (directory-name pathname)))
        (T
         (make-pathname :directory (pathname-directory (parent pathname))
                        :defaults pathname))))

(defun downwards (pathname subdir)
  (cond ((directory-p pathname)
         (subdirectory (parent pathname)
                       subdir
                       (directory-name pathname)))
        (T
         (make-pathname :directory (subdirectory pathname subdir)
                        :defaults pathname))))

(defun enough-pathname (subpath base)
  (pathname* (enough-namestring subpath base)))

(defun file-type (pathname)
  (let ((pathname (pathname pathname)))
    (let ((type (pathname-type pathname))
          (name (pathname-name pathname)))
      (cond ((unspecific-p type)
             (let ((pos (position #\. name :from-end T)))
               (if pos (subseq name (1+ pos)) NIL)))
            (T
             (let ((pos (position #\. type :from-end T)))
               (if pos (subseq type (1+ pos)) type)))))))

(defun file-name (pathname)
  (let ((pathname (pathname pathname)))
    (format NIL "~a~@[.~a~]" (pathname-name pathname) (pathname-type pathname))))

(defun directory-name (pathname)
  (let ((pathname (to-directory pathname)))
    (car (last (rest (pathname-directory pathname))))))

(defun directory-separator (&optional (pathname *default-pathname-defaults*))
  (let ((name (namestring (make-pathname :directory '(:absolute "nowhere") :defaults pathname))))
    (char name (1- (length name)))))
