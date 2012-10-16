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
  (let ((test (if (file-directory-p buffer-file-name)
                  nil
                (testkick-awhen (testkick-find-test-for-file buffer-file-name)
                  (testkick-test-run it)))))
    (when test
      (testkick-test-run test))))
  
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
                                    path))))))
      (when (and like-it-dir
                 (testkick-find-file-recursive like-it-dir
                                               #'testkick-test-from-file))
        (return-from testkick-find-test-root like-it-dir)))))

(defun* testkick-find-test-for-file (source-file)
  (when (file-directory-p source-file)
    (return-from testkick-find-test-for-file))

  (or (testkick-test-from-file source-file)
      (let* ((test-root (testkick-find-test-root (file-name-directory source-file)))
             (test-file (and test-root
                             (testkick-find-file-recursive
                              test-root
                              #'(lambda (test-file)
                                  (unless (file-directory-p test-file)
                                    (testkick-equal-to-test-file source-file test-file)
                                    test-file))))))
        (when test-file
          (testkick-test-from-file test-file)))))

(defun* testkick-equal-to-test-file (source-file test-file)
  (when (file-directory-p test-file)
    (return-from testkick-match-to-test-file))

  (let* ((source-basename (testkick-file-basename source-file))
         (test-basename (testkick-file-basename test-file))
         (match-pos (string-match (replace-regexp-in-string "\\..+?$" ""source-basename)
                                  test-basename)))
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
            do (let ((result (funcall callback file)))
                 (when result (return result)))
            when (file-directory-p file)
            return (testkick-find-file-recursive file callback)))))

(provide 'testkick)
