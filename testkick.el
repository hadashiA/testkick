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
     (command . (lambda (test-target)
                  (let ((rspec-command (concat "rspec --color --format documentation "
                                               test-target))
                        (gemfile (testkick-find-gemfile test-target)))
                    (if gemfile
                        (format "BUNDLE_GEMFILE=%s bundle exec %s" gemfile rspec-command)
                      rspec-command))))
     (test-file-pattern   . "_spec\\.rb$")
     (test-syntax-pattern . "^\\s-*describe\\s-+\\S-+\\s-+do")
     )
    
    ("vows"
     (command . (lambda (test-target)
                  (let ((vows-command (concat "vows --spec " test-target))
                        (node-modules (testkick-find-node-modules-dir test-target)))
                    (princ node-modules)
                    (if node-modules
                        (concat node-modules "/.bin/" vows-command)
                      vows-command))))
     (test-file-pattern   . "\\.js$")
     (test-syntax-pattern . "vows\\.describe(.+[^\\S-]*.*addBatch(")
     )

    ("mocha"
     (command . (lambda (test-target)
                  (let ((mocha-command (concat "mocha --reporter spec " test-target))
                        (node-modules (testkick-find-node-modules-dir test-target)))
                    (if node-modules
                        (concat node-modules "/.bin/" mocha-command)
                      mocha-command))))
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
      (testkick-root specify)
    (testkick-aif (testkick-find-test-for-file buffer-file-name)
        (testkick-test-run it)
      (message "Sorry. No such test code: %s" buffer-file-name))))

;;;###autoload
(defun testkick-root (&optional specify)
  (interactive "P")
  (let* ((test-root-directory (if specify
                                 (read-directory-name "test root directory: ")
                               (testkick-find-test-root-directory (testkick-current-directory))))
         (test (and test-root-directory
                    (testkick-find-file-recursive test-root-directory
                                                  #'testkick-test-from-file 5))))
    (if test
        (testkick-test-run test test-root-directory)
      (message "not such test file in %s" test-root-directory))))

;;;###autoload
(defun testkick-toggle (&optional specify)
  (interactive "P")
  (testkick-awhen (and buffer-file-name
                       (testkick-find-test-for-file buffer-file-name))
    (find-file (if (file-equal-p (testkick-test-test-file it)
                                 buffer-file-name)
                   (testkick-test-source-file-or-find it)
                 (testkick-test-test-file it)
                 ))
    (setq testkick-test it)))

(defun testkick-cache-clear ()
  (loop for buf in (buffer-list)
        do (with-current-buffer buf
             (setq testkick-test nil))))

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
(defun* testkick-find-test-for-file (source-file &optional specify prompt)
  (when (file-directory-p source-file)
    (error "Not a file %s" soruce-file))

  (let ((test (if specify
                  (testkick-test-from-read-test-file-name prompt)
                (or (let ((buf (get-file-buffer source-file)))
                      (when buf
                        (with-current-buffer buf
                          (if (testkick-test-p testkick-test)
                              testkick-test
                            (setq testkick-test nil)))))

                    (testkick-test-from-file source-file)
                    
                    (testkick-aand (testkick-find-test-root-directory (file-name-directory source-file))
                                   (testkick-find-file-recursive
                                    it #'(lambda (test-file)
                                           (when (and (null (file-directory-p test-file))
                                                      (testkick-match-to-test-file source-file test-file))
                                             test-file))
                                    5)
                                   (testkick-test-from-file it))

                    (testkick-test-from-read-test-file-name
                     "Test not found. Please enter test file path: ")))))
    (unless (file-equal-p source-file
                          (testkick-test-test-file test))
      (setf (testkick-test-source-file test) source-file))

    (testkick-awhen (testkick-test-test-file test)
      (with-current-buffer (find-file-noselect it)
        (setq testkick-test test)))
    (testkick-awhen (testkick-test-source-file test)
      (with-current-buffer (find-file-noselect it)
        (setq testkick-test test)))
    test))

(defun* testkick-match-to-test-file (source-file test-file)
  (when (file-directory-p test-file)
    (return-from testkick-match-to-test-file))

  (let* ((source-basename (testkick-file-basename source-file))
         (test-basename (testkick-file-basename test-file))
         (match-pos (string-match (replace-regexp-in-string "\\..+?$" "" source-basename)
                                  test-basename)))
    (and match-pos (= 0 match-pos))))

(defun testkick-test-root-directory-name-p (path)
  (string-match "/\\(tests?\\|spec\\)$" path))

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
  (let ((file (read-file-name (or prompt "Test file: "))))
    (or (testkick-test-from-file file)
        (and (message "Don't know what one test.")
             nil))))

;; 
;; testkick-test methods
;; 

(defun testkick-test-run (test &optional target)
  (setq target (case (or target :test-file)
                 (:test-file (testkick-test-test-file test))
                 (:test-root-directory (testkick-test-test-root-directory test))
                 (t target)))
  (let ((command-or-func  (testkick-test-command test)))
    (compile (if (functionp command-or-func)
                 (funcall command-or-func target)
               (concat command-or-func " " target)))))

(defun* testkick-test-source-file-or-find (test)
  (let ((source-file (or (testkick-test-source-file test)
                         (read-file-name "Enter a source file: "))))
    (setf (testkick-test-source-file test) source-file)))

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
;; find context file
;; 

(defun testkick-find-gemfile (cur-dir)
  (testkick-find-file-in-same-project
   cur-dir
   #'(lambda (file)
       (unless (file-directory-p file)
         (when (string=
                (file-name-nondirectory file)
                "Gemfile")
           file)))
   1))

(defun testkick-find-node-modules-dir (cur-dir)
  (testkick-find-file-in-same-project
   cur-dir
   #'(lambda (file)
       (when (and (file-directory-p file)
                  (string= (file-name-nondirectory file)
                           "node_modules"))
         file))
   1))

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
  (unless (file-directory-p start-directory)
    (setq start-directory (file-name-directory
                           (expand-file-name start-directory))))

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

  (unless (file-directory-p cur-dir)
    (setq cur-dir (file-name-directory (expand-file-name cur-dir))))

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
