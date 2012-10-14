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

(defmacro testkick-alist-loop (args &rest body)
  `(loop for alist in testkick-alist
         do (let (loop for arg in args
                       for key = (intern (concat ":" (symbol-name arg)))
                       for value = (cond
                                    ((equal arg 'name) (car alist))
                                    (t (cadr (memq key (cdr alist)))))
                       collect (list arg value))
              ,@body)))

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
  (unless (file-directory-p buffer-file-name)
    (testkick-awhen (testkick-test-from-file buffer-file-name)
      (testkick-test-run it))))

(defun testkick-with-more-options (options)
  (interactive)
  )

;; 
;; find test target
;;

(defun* testkick-find-test (&optional cur-file)
  (setq cur-file (or cur-file buffer-file-name))
  (when (or (null cur-file)
            (file-directory-p cur-file))
    (return-from testkick-find-test))

  (or (testkick-test-from-file cur-file)
      (let* ((basename (replace-regexp-in-string "\\.\\S-+?$" "" 
                                                 (file-name-nondirectory cur-file)))
             (test (testkick-find-test-for-root (file-name-directory cur-file)))
             (test-root (testkick-test-test-root-directory test)))
        
             ))
      )
  )



(defun* testkick-find-test-for-root (&optional cur-dir)
  (setq cur-dir (or cur-dir (testkick-current-directory)))
  (loop for alist in testkick-alist
        do (destructuring-bind (name &key
                                     command
                                     test-file-pattern
                                     test-syntax-patterns
                                     (test-root-basename nil)) alist
             (let* ((test-root (and test-root-basename
                                    (testkick-find-directory-in-same-project
                                     cur-dir test-root-basename)))
                    (test (and test-root
                               (testkick-find-test-in-directory test-root))))
               (when test
                 (setf (testkick-test-test-root-directory test) test-root)
                 test)))))

(defun* testkick-find-test-in-directory (&optional cur-dir)
  (setq cur-dir (or cur-dir (testkick-current-directory)))
  (let ((files (directory-files cur-dir t nil t)))
    (loop for alist in testkick-alist
          do (destructuring-bind (name &key
                                       command
                                       test-file-pattern
                                       test-syntax-patterns
                                       (test-root-basename nil)) alist
               
               (loop for file in files
                     unless (string-match "^\\.\\.?$" (file-name-nondirectory file))
                     if (not (file-directory-p file))
                     do (let ((test (testkick-test-from-file file)))
                          (when test
                            (return-from testkick-find-test-in-directory test)))
                     else
                     do (let ((test (testkick-find-test-in-directory file)))
                          (when test
                            (return-from testkick-find-test-in-directory test)
                            ))
                     )))))

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

  (let* ((file-not-opened (null (get-file-buffer file)))
         (buffer (or (get-file-buffer file)
                     (with-current-buffer (generate-new-buffer (concat "*testkick " file " *"))
                       (insert-file-contents file)
                       (current-buffer))))
         (result (with-current-buffer buffer
                   (loop named match-patterns
                         for alist in testkick-alist
                         do (destructuring-bind (name &key
                                                      command
                                                      test-file-pattern
                                                      test-syntax-patterns
                                                      (test-root-basename nil)) alist
                              (loop for pattern in test-syntax-patterns
                                    do (when (and (goto-char (point-min))
                                                  (re-search-forward pattern nil t))
                                         (return-from match-patterns
                                           (make-testkick-test :name name
                                                               :command command
                                                               :test-root-basename test-root-basename 
                                                               :test-syntax-pattern pattern
                                                               :test-file file
                                                               )))))
                         ))))
    (when file-not-opened (kill-buffer buffer))
    result))

;; 
;; generic utils
;; 

(defun testkick-current-directory ()
  (file-name-directory
   (expand-file-name
    (or (buffer-file-name)
        default-directory))))

(defun testkick-directory-files-without-dot (directory)
  (directory-files directory t "^[^.]" t))

(defun* testkick-find-directory-in-same-project (start-directory find-directory-name)
  (loop for count from 0 to testkick-directory-search-depth-limit
        for cur-dir = (expand-file-name
                       (concat start-directory (apply #'concat (make-list count "/.."))))
        until (string= cur-dir "/")
        when (file-directory-p cur-dir)
        do (let ((files (testkick-directory-files-without-dot cur-dir)))
             (when files
               (loop for subdir in files
                     when (and 
                           (file-directory-p subdir)
                           (string= (file-name-nondirectory (expand-file-name subdir))
                                    find-directory-name))
                     do (return-from testkick-find-directory-in-same-project (expand-file-name subdir)))))
        ))

(defun testkick-directory-files-recursive (directory &optional full-name match-regexp nosort)
  (append (directory-files directory full-name match-regexp nosort)
          (loop for file in (directory-files directory full-name nil match-regexp)
                when (and (not (string-match "^\\.\\.?$" file))
                          (file-directory-p file))
                return (testkick-directory-files-recursive directory full-name match-regexp nosort))
          ))

(provide 'testkick)





