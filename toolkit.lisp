#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.pathname-utils)

(defun root-p (pathname)
  (and (directory-p pathname)
       (equal (pathname-directory pathname) '(:absolute))))

(defun relative-p (pathname)
  (eql :relative (car (pathname-directory pathname))))

(defun absolute-p (pathname)
  (eql :absolute (car (pathname-directory pathname))))

(defun directory-name (pathname)
  (let ((pathname (to-directory pathname)))
    (car (last (rest (pathname-directory pathname))))))

(defun directory-p (pathname)
  (let ((pathname (pathname pathname)))
    (and (member (pathname-name pathname) '(NIL :unspecific ""))
         (member (pathname-type pathname) '(NIL :unspecific "")))))

(defun to-directory (pathname)
  (make-pathname :name NIL :type NIL :version NIL :defaults (pathname pathname)))

(defun subdirectory (pathname &rest subdirs)
  (loop for sub in subdirs
        for dir = (merge-pathnames (to-directory sub) (to-directory pathname))
        then (merge-pathnames (to-directory sub) dir)
        finally (return dir)))

(defun pop-directory (pathname)
  (make-pathname :directory (list* (car (pathname-directory pathname))
                                   (butlast (cdr (pathname-directory pathname))))
                 :defaults pathname))

(defun parent (pathname)
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
         (to-directory pathname))))

(defun upwards (pathname)
  (cond ((directory-p pathname)
         (subdirectory (parent (parent pathname))
                       (directory-name pathname)))
        (T
         (let ((rootpath (butlast (pathname-directory pathname))))
           (make-pathname :directory rootpath :defaults pathname)))))

(defun downwards (pathname subdir)
  (cond ((directory-p pathname)
         (subdirectory (parent pathname)
                       subdir
                       (directory-name pathname)))
        (T
         (make-pathname :directory (subdirectory pathname subdir) :defaults pathname))))

(defun file-type (pathname)
  (let* ((type (pathname-type pathname))
         (pos (position #\. type :from-end T)))
    (if pos (subseq type (1+ pos)) type)))

(defun file-name (pathname)
  (format NIL "~a~@[.~a~]" (pathname-name pathname) (pathname-type pathname)))
