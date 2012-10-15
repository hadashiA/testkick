;;; testkick.el --- an interface to `compile'

;; Copyright (C) 2012 hadashiA

;; Author: hadashiA <dev@hadashikick.jp>
;; $Id: smart-compile.el 764 2012-07-10 15:58:08Z zenitani $
;; Keywords: tools, unix, rspec, mocha
;; Created: 2012-10-14
;; Compatibility: Emacs 23 or later
;; URL(en): http://github.com/f-kubotar/testkick

;; Contributors: hadashiA

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

;; custom vars

(defgroup testkick nil
  "run test command helper.")

(defcustom testkick-alist
  '(("rspec"
     :command "rspec --color --format documentation"
     :test-file-pattern "_spec\\.rb$"
     :test-syntax-patterns ("^\\s-*describe\\s-+\\S-+\\s-+do")
     :test-root-basename "spec")
    
    ("mocha"
     :command "mocha --reporter spec"
     :test-file-pattern "\\.js$"
     :test-syntax-patterns ("^\\s-*describe\\s-*(\\s-*['\"]\\S-+['\"]\\s-*,\\s-*function\\s-*(")
     :test-root-basename "test"))
  ""
  :group 'testkick)

(defcustom testkick-directory-search-depth-limit 10
  "Number of times up to look for a test directory"
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

(defmacro testkick-alist-loop (&rest body)
  (declare (indent 1))
  `(loop named testkick-alist-loop
         for alist in testkick-alist
         do (let ,(loop for arg in (car body)
                        for key = (intern (concat ":" (symbol-name arg)))
                        collect (list arg `(cond
                                            ((equal (quote ,arg) 'name) (car alist))
                                            (t (cadr (memq ,key (cdr alist)))))
                                      ))
              ,@(cdr body))))

;; 
;; structs
;; 

(defstruct testkick-test
  name
  command
  test-syntax-pattern
  test-root-basename
  test-file
  test-root-directory
  source-file
  )

;; 
;; commands
;; 

(defun testkick ()
  (interactive)
  (let ((test (if (file-directory-p buffer-file-name)
                  nil
                (testkick-awhen (testkick-find-test-for-file buffer-file-name)
                  (testkick-test-run it)))))
    (when test
      (testkick-test-run test))))
  
;; 
;; find test target
;;

(defun* testkick-find-test-for-file (source-file)
  (unless(file-directory-p source-file)
    (return-from testkick-find-test))

  (or (testkick-test-from-file source-file)
      (let* ((test (testkick-find-test-for-root (file-name-directory source-file)))
             (test-root (testkick-test-test-root-directory test))
             (source-basename (replace-regexp-in-string "\\..+?$" ""
                                                        (testkick-file-basename source-file))))
        (testkick-find-file-recursive test-root
                                      #'(lambda (test-file)
                                          (unless (file-directory-p test-file)
                                            (testkick-match-to-test-file source-basename test-file))))
        )))

(defun* testkick-find-test-for-root (cur-dir)
  (setq cur-dir (or cur-dir (testkick-current-directory)))
  (testkick-alist-loop (test-root-basename)
    (let* ((test-root (and test-root-basename
                           (testkick-find-file-in-same-project
                            cur-dir
                            #'(lambda (path)
                                (when (and (file-directory-p path)
                                           (string= test-root-basename (testkick-file-basename path)))
                                  path)))))
           (test (and test-root
                      (testkick-find-file-recursive test-root
                                                    #'testkick-test-from-file))))
      (when test
        (setf (testkick-test-test-root-directory test) test-root)
        (return-from testkick-find-test-for-root test)))))

(defun* testkick-match-to-test-file (source-basename test-file)
  (when (file-directory-p test-file)
    (return-from testkick-match-to-test-file))

  (let ((test-basename (testkick-file-basename test-file))
        (match-pos (string-match source-basename test-basename)))
    (and match-pos (= 0 match-pos))))

;; 
;; testkick-test methods
;; 

(defun testkick-test-run (testkick-test &optional target)
  (testkick-aif (or target
                    (testkick-test-test-file testkick-test)
                    (testkick-test-test-directory testkick-test))
      (compile (concat (testkick-test-command testkick-test) " " it))
    (error "invalid test target %s" testkick-test)))

(defun* testkick-test-from-file (file)
  (when (file-directory-p file)
    (return-from testkick-test-from-file))

  (with-current-buffer (testkick-temp-buffer file)
    (testkick-alist-loop (name command test-root-basename test-syntax-patterns)
      (loop for pattern in test-syntax-patterns
            when (and (goto-char (point-min))
                      (re-search-forward pattern nil t))
            do (return-from testkick-test-from-file
                 (make-testkick-test :name name
                                     :command command
                                     :test-root-basename test-root-basename 
                                     :test-syntax-pattern pattern
                                     :test-file file
                                     ))))))

;;
;; temp buffer
;;

(defun testkick-temp-buffer (file)
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

(defun* testkick-find-file-in-same-project (start-directory callback)
  (loop for count from 0 to testkick-directory-search-depth-limit
        for cur-dir = (expand-file-name
                       (concat start-directory (apply #'concat (make-list count "/.."))))
        until (string= cur-dir "/")
        do (let ((files (testkick-directory-files-without-dot cur-dir)))
             (when files
               (loop for subdir in files
                     do (let ((result (funcall callback subdir)))
                          (when result
                            (return-from testkick-find-file-in-same-project result))))))
        ))

(defun* testkick-find-file-recursive (cur-dir callback)
  (let ((files (testkick-directory-files-without-dot cur-dir)))
    (when files
      (loop for file in files
            do (let ((result (funcall callback file)))
                 (when result (return result)))
            when (file-directory-p file)
            return (testkick-find-file-recursive file callback)))))

(provide 'testkick)
