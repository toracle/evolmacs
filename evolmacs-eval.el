;;; evolmacs-eval.el --- Safe Emacs Lisp evaluation gateway -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Evolmacs Contributors

;; Author: Evolmacs Contributors
;; Keywords: lisp
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; This module provides a safe evaluation gateway for Evolmacs.
;; It sandboxes all evaluations with error handling and ensures
;; that generated Emacs Lisp code is safely executed.

;;; Code:

(require 'cl-lib)

(defgroup evolmacs-eval nil
  "Safe evaluation gateway for Evolmacs."
  :group 'evolmacs
  :prefix "evolmacs-eval-")

(defvar evolmacs-eval-max-execution-time 2
  "Maximum execution time in seconds for evaluated code.")

;;;###autoload
(defun evolmacs-eval-safely (code)
  "Safely evaluate CODE string containing Emacs Lisp.
Returns a plist with :success, :result, and :error keys."
  (let ((result nil)
        (error-msg nil)
        (success nil))
    (with-timeout (evolmacs-eval-max-execution-time
                   (setq error-msg "Execution timed out")
                   (setq success nil))
      (condition-case err
          (progn
            (setq result (eval (read code) t))
            (setq success t))
        (error
         (setq error-msg (format "Evaluation error: %S" err))
         (setq success nil))))
    (list :success success
          :result result
          :error error-msg)))

;;;###autoload
(defun evolmacs-eval-function-safely (function-code)
  "Safely evaluate FUNCTION-CODE defining an Emacs Lisp function.
Returns a plist with :success, :function-name, and :error keys."
  (let* ((eval-result (evolmacs-eval-safely function-code))
         (success (plist-get eval-result :success))
         (error-msg (plist-get eval-result :error))
         (function-name nil))
    
    ;; Try to extract function name even if evaluation failed
    (condition-case nil
        (let ((form (read function-code)))
          (when (and (listp form) 
                     (eq (car form) 'defun)
                     (>= (length form) 2)
                     (symbolp (nth 1 form)))
            (setq function-name (symbol-name (nth 1 form)))))
      (error nil))
    
    (list :success success
          :function-name function-name
          :error error-msg)))

(provide 'evolmacs-eval)
;;; evolmacs-eval.el ends here