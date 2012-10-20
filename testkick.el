;;; testkick.el --- an interface to `compile'

;; Copyright (C) 2012 hadashiA

;; Author: hadashiA <dev@hadashikick.jp>
;; $Id: smart-compile.el 764 2012-07-10 15:58:08Z zenitani $
;; Keywords: tools, unix, rspec, mocha
;; Created: 2012-10-14
;; Compatibility: Emacs 23 or later
;; URL: http://github.com/f-kubotar/testkick

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

;; custom vars

(defgroup testkick nil
  "run test command helper.")

(defvar testkick-alist
  '(("rspec"
     (command . "rspec --color --format documentation")
     (test-file-pattern   . "_spec\\.rb$")
     (test-syntax-pattern . "^\\s-*describe\\s-+\\S-+\\s-+do")
     )
    
    ("vows"
     (command . "vows --spec")
     (test-file-pattern   . "\\.js$")
     (test-syntax-pattern . "vows\\.describe(.+[^\\S-]*.*addBatch(")
     )

    ("mocha"
     (command . "mocha --reporter spec")
     (test-file-pattern   . "\\.js$")
     (test-syntax-pattern . "^\\s-*describe\\s-*(\\s-*['\"]\\S-+['\"]\\s-*,\\s-*function\\s-*(")
     )
    ))

(defcustom testkick-directory-search-parent-limit 10
  "Number of times up to look for a parent directory"
  :group 'testkick)

;;
;; macros
;;

(defmacro testkick-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put #'testkick-aif 'lisp-indent-function 2)

(defmacro testkick-awhen (test-form &rest body)
  "Anaphoric when."
  (declare (indent 1))
  `(testkick-aif ,test-form
	(progn ,@body)))

(defmacro testkick-aand (&rest args)
  "Anaphoric and"
  (declare (indent 0))
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(testkick-aif ,(car args) (testkick-aand ,@(cdr args))))))

(defmacro testkick-alist-loop (&rest body)
  "loop for testkick-alist"
  (declare (indent 1))
  `(loop named testkick-alist-loop
         for alist in testkick-alist
         do (let ,(loop for arg in (car body)
                        collect (list arg `(cond
                                            ((equal (quote ,arg) 'name) (car alist))
                                            (t (cdr (assoc (quote ,arg) (cdr alist)))))
                                      ))
              ,@(cdr body))))

;; 
;; structs
;; 

(defstruct (testkick-test
            (:constructor
             new-testkick-test
             (name &key command test-file test-root-directory source-file
                   &aux
                   (command (or command
                                (error "argument :command must not be nil")))
                   (test-file (or test-file
                                  (error "argument :test-file must not be nil")))
                   )))
  name
  command
  test-file
  test-root-directory
  source-file
  )

;; 
;; buffer local
;;

(defvar testkick-test nil)
(make-variable-buffer-local 'testkick-test)

;; 
;; commands
;; 

;;;###autoload
(defun testkick (&optional specify)
  (interactive "P")

  (if (null buffer-file-name)
      (testkick-root sepcify)
    (if specify
        (testkick-awhen (testkick-test-from-read-test-file-name)
          (with-current-buffer (find-file-noselect (testkick-test-test-file it))
            (setq testkick-test it))
          (setq testkick-test it)
          (testkick-test-run it))

      (testkick-aif (or (testkick-find-test-for-file buffer-file-name)
                        (testkick-test-from-read-test-file-name
                         "test not found. please enter test file path: "))
          (testkick-test-run it)))))

;;;###autoload
(defun testkick-root (&optional specify)
  (interactive "P")
  (let ((test-root-directory (testkick-find-test-root-directory (testkick-current-directory))))
    (testkick-aand test-root-directory
                   (testkick-find-file-recursive test-root-directory
                                                 #'testkick-test-from-file 5)
                   (testkick-test-run it test-root-directory))))

;;;###autoload
(defun testkick-toggle (&optional specify)
  (interactive "P")
  (testkick-awhen (and buffer-file-name
                       (testkick-find-test-for-file buffer-file-name))
    (find-file (if (file-equal-p (testkick-test-test-file it)
                                 buffer-file-name)
                   (testkick-test-test-file it)
                 (testkick-test-find-source-file it)))))

;; 
;; settings helper
;;

(defun testkick-alist-update (name update-alist)
  (let ((alist (loop for alist in testkick-alist
                     when (string= (car alist) name)
                     return alist)))
    (if alist
        (loop for cons in update-alist
              for key = (car cons)
              for val = (cdr cons)
              do (setf (cdr (assoc key alist)) val)
              )
      (add-to-list 'testkick-alist (cons name update-alist) t))))

;; 
;; find test target
;;

(defun* testkick-find-test-root-directory (cur-dir)
  (unless (file-directory-p cur-dir)
    (setq cur-dir (file-name-directory cur-dir)))
  
  (testkick-aif (testkick-find-file-in-same-project
                 cur-dir #'(lambda (path)
                             (when (and (file-directory-p path)
                                        (testkick-test-root-directory-name-p path)
                                        (testkick-find-file-recursive path
                                                                      #'testkick-test-from-file 3))
                               path)) 1)
      it
    (read-directory-name "Enter a test root directory: ")))
    

;; find test associated soruce-file or cache
(defun* testkick-find-test-for-file (source-file)
  (when (file-directory-p source-file)
    (return-from testkick-find-test-for-file))

  (let ((buf (get-file-buffer source-file)))
    (when buf
      (with-current-buffer buf
        (when testkick-test
          (return-from testkick-find-test-for-file testkick-test))))

    (or (testkick-test-from-file source-file)
        (testkick-aand (testkick-find-test-root-directory (file-name-directory source-file))
                       (testkick-find-file-recursive
                        it #'(lambda (test-file)
                               (when (and (null (file-directory-p test-file))
                                          (testkick-match-to-test-file source-file test-file))
                                 test-file))
                        5)
                       (testkick-test-from-file it)
                       (progn
                         (unless (file-equal-p source-file
                                               (testkick-test-test-file it))
                           (setf (testkick-test-source-file it) source-file))
                         
                         (with-current-buffer (find-file-noselect source-file)
                           (setq testkick-test it))
                         (with-current-buffer (find-file-noselect (testkick-test-test-file it))
                           (setq testkick-test it))
                         it)))))

(defun* testkick-match-to-test-file (source-file test-file)
  (when (file-directory-p test-file)
    (return-from testkick-match-to-test-file))

  (let* ((source-basename (testkick-file-basename source-file))
         (test-basename (testkick-file-basename test-file))
         (match-pos (string-match (replace-regexp-in-string "\\..+?$" "" source-basename)
                                  test-basename)))
    (and match-pos (= 0 match-pos))))

(defun testkick-test-root-directory-name-p (path)
  (let ((basename (testkick-file-basename path)))
    (loop for pattern in '("test" "spec")
          thereis (string= pattern basename)
          )))

;; 
;; create testkick-test obj
;; 

(defun* testkick-test-from-file (file)
  (when (file-directory-p file)
    (return-from testkick-test-from-file))

  (testkick-alist-loop (name command test-syntax-pattern)
    (with-current-buffer (testkick-search-buffer file)
      (goto-char (point-min))
      (when (re-search-forward test-syntax-pattern nil t)
        (return-from testkick-test-from-file
          (new-testkick-test name :command command :test-file file))))))
  
(defun testkick-test-from-read-test-file-name (&optional prompt)
  (let ((file (read-file-name (or (or prompt "Test file: ")))))
    (or (testkick-test-from-file file)
        (and (message "Don't know what one test.")
             nil))))

;; 
;; testkick-test methods
;; 

(defun testkick-test-run (test &optional target)
  (compile (concat (testkick-test-command test) " "
                   (case (or target :test-file)
                     (:test-file (testkick-test-test-file test))
                     (:test-root-directory (testkick-test-test-root-directory test))
                     (t target)))))

(defun* testkick-test-source-file-or-find (test)
  (let ((source-file (or (testkick-test-source-file test)
                         (read-file-name "Enter source file path: "))))
    (unless (file-exists-p source-file)
      (return-from testkick-test-source-file-or-find))
    
    (with-current-buffer (get-file-buffer it)
      (setq testkick-test test))
    (setf (testkick-test-source-file test) source-file)))

(defun* testkick-test-find-source-file (test)
  (testkick-awhen (testkick-test-source-file test)
    (return-from testkick-test-find-source-file it)))

;;
;; temp buffer
;;

(defun testkick-search-buffer (file)
  (or (get-file-buffer file)
      (with-current-buffer (get-buffer-create "*testkick temp*")
        (erase-buffer)
        (insert-file-contents file)
        (current-buffer))))

;; 
;; generic utils
;; 

(defun testkick-current-directory ()
  (file-name-directory
   (expand-file-name
    (or (buffer-file-name)
        default-directory))))

(defun testkick-file-basename (path)
  (file-name-nondirectory (expand-file-name path)))

(defun testkick-directory-files-without-dot (directory)
  (directory-files directory t "^[^.]" t))

(defun* testkick-find-file-in-same-project (start-directory callback max-depth)
  (loop for count from 0 to testkick-directory-search-parent-limit
        for cur-dir = (expand-file-name
                       (concat start-directory (apply #'concat (make-list count "/.."))))
        until (string= cur-dir "/")
        do (let ((result (testkick-find-file-recursive cur-dir callback max-depth)))
             (when result
               (return-from testkick-find-file-in-same-project result)))))

(defun* testkick-find-file-recursive (cur-dir callback &optional (max-depth nil) (depth 0))
  (when (and max-depth
             (>= depth max-depth))
    (return-from testkick-find-file-recursive))

  (let ((files (testkick-directory-files-without-dot cur-dir)))
    (when files
      (loop for file in files
            do (let ((result (or (funcall callback file)  
                                 (when (file-directory-p file)
                                   (testkick-find-file-recursive file callback max-depth
                                                                 (+ 1 depth))))))
                 (when result
                   (return result)))))))

(provide 'testkick)
