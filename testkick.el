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

(defcustom testkick-alist
  '(("rspec"
     :command "rspec --color --format documentation"
     :test-file-pattern "_spec\\.rb$"
     :test-syntax-pattern "^\\s-*describe\\s-+\\S-+\\s-+do"
     :test-root-basename "spec")
    
    ("vows"
     :command "vows --spec"
     :test-file-pattern "\\.js$"
     :test-syntax-pattern "vows\\.describe(.+[^\\S-]*.*addBatch("
     :test-root-basename "test"
     )

    ("mocha"
     :command "mocha --reporter spec"
     :test-file-pattern "\\.js$"
     :test-syntax-pattern "^\\s-*describe\\s-*(\\s-*['\"]\\S-+['\"]\\s-*,\\s-*function\\s-*("
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
  test-file
  test-root-directory
  source-file
  )

;; 
;; caching
;;

(defvar testkick-test-root nil)
(make-variable-buffer-local 'testkick-test-root)

(defvar testkick-test nil)
(make-variable-frame-local 'testkick-test)

;; 
;; commands
;; 

;;;###autoload
(defun testkick ()
  (interactive)
  (if buffer-file-name
      (testkick-aif (testkick-find-test-for-file buffer-file-name)
          (testkick-test-run it (testkick-test-test-file it))
        (message "test not found."))
    nil))
  
;;;###autoload
(defun testkick-root ()
  (interactive)
  (testkick-awhen (testkick-find-test-root (testkick-current-directory))
    (testkick-test-run it (testkick-test-test-root-directory it))))

;;;###autoload
(defun testkick-at (file-or-directory)
  (interactive "ftest file or directory: ")
  (testkick-aif (if (file-directory-p file-or-directory)
                    (testkick-find-file-recursive file-or-directory
                                                  #'testkick-test-from-file)
                  (testkick-test-from-file file-or-directory))
      (testkick-test-run it file-or-directory)
    (message "no sush test code.")))

;; 
;; find test target
;;

(defun* testkick-find-test-root (cur-dir)
  (unless (file-directory-p cur-dir)
    (setq cur-dir (file-name-directory cur-dir)))

  (testkick-alist-loop (test-root-basename)
    (let* ((like-it-dir (and test-root-basename
                             (testkick-find-file-in-same-project
                              cur-dir
                              #'(lambda (path)
                                  (when (and (file-directory-p path)
                                             (string= test-root-basename
                                                      (testkick-file-basename path)))
                                    path)))))
           (test (when like-it-dir
                   (testkick-find-file-recursive like-it-dir #'testkick-test-from-file))))
      (when test
        (setf (testkick-test-test-root-directory test) like-it-dir)
        (return-from testkick-find-test-root test)))))

(defun* testkick-find-test-for-file (source-file)
  (when (file-directory-p source-file)
    (return-from testkick-find-test-for-file))

  (or (testkick-test-from-file source-file)
      (testkick-aand (testkick-find-test-root (file-name-directory source-file))
                     (testkick-test-test-root-directory it)
                     (testkick-find-file-recursive
                      it #'(lambda (test-file)
                             (when (and (null (file-directory-p test-file))
                                        (testkick-equal-to-test-file source-file test-file))
                               test-file)))
                     (testkick-test-from-file it))))

(defun* testkick-equal-to-test-file (source-file test-file)
  (when (file-directory-p test-file)
    (return-from testkick-match-to-test-file))

  (let* ((source-basename (testkick-file-basename source-file))
         (test-basename (testkick-file-basename test-file))
         (match-pos (string-match (replace-regexp-in-string "\\..+?$" "" source-basename)
                                  test-basename)))
    (and match-pos (= 0 match-pos))))

;; 
;; testkick-test methods
;; 

(defun testkick-test-run (test target)
  (compile (concat (testkick-test-command test) " " target)))

(defun* testkick-test-from-file (file)
  (when (file-directory-p file)
    (return-from testkick-test-from-file))

  (with-current-buffer (testkick-temp-buffer file)
    (testkick-alist-loop (name command test-syntax-pattern)
      (goto-char (point-min))
      (when (re-search-forward test-syntax-pattern nil t)
        (return-from testkick-test-from-file
          (make-testkick-test :name name
                              :command command
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
            do (let ((result (or (funcall callback file)  
                                 (when (file-directory-p file)
                                   (testkick-find-file-recursive file callback)))))
                 (when result
                   (return result)))))))

(provide 'testkick)
