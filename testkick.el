;;; testkick.el --- an interface to `compile'

;; Copyright (C) 2012 hadashiA

;; Author: hadashiA <dev@hadashikick.jp>
;; $Id: smart-compile.el 764 2012-07-10 15:58:08Z zenitani $
;; Keywords: tools, unix, rspec, mocha
;; Created: 2012-10-14
;; Compatibility: Emacs 23 or later
;; URL(en): http://github.com/f-kubotar/testkick

;; Contributors: hadashiA

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))


;; custom

(defgroup testkick nil
  "run test command helper.")

(defcustom testkick-alist
  '(("rspec"
     (:command  . "rspec --color --format documentation")
     (:patterns . ("^\\s-*describe\\s-+\\S-+\\s-+do"))
     (:test-dir . "spec"))
    
    ("mocha"
     ((:command  . "mocha --reporter spec")
      (:patterns . ("^\\s-*describe\\s-*(\\s-*['\"]\\S-+['\"]\\s-*,\\s-*function\\s-*("))
      (:test-dir . "test")))
    )
  ""
  :group 'testkick)


(defcustom testkick-test-file-p-function-alist
  ("\\.rb" . (lambda (cur-file)
               
               ))
  ("\\.js") . (lambda (cur-file)
                
                ))
  )

;; interactive 

(defun testkick (&optional file-or-directory)
  (interactive "ftest file or directory: ")
  (let ((file (if (file-directory-p file-or-directory)
                  nil
                file-or-directory)
              )))
  
  )

(defun testkick-with-more-options (options)
  (interactive)
  )

;; utils


(defun testkick-any-test (path)
  (let* ((buffer (get-file-buffer path))
         (file-not-opened (null buffer)))
    (when file-not-opened
      (setq buffer (find-file-noselect path)))

    (with-current-buffer buffer
      (loop named find-test
            for test in testkick-alist
            do (destructuring-bind (name . alist) test
                 (loop for pattern in (cdr (assoc :patterns alist))
                       do (when (and (goto-char (point-min))
                                     (re-search-forward pattern nil t))
                            (return-from find-test test)))))
      )))

(defun testkick-find-test-directory ()
  
  )

(provide 'testkick)
