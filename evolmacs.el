;;; evolmacs.el --- Self-evolving Emacs agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Evolmacs Contributors

;; Author: Evolmacs Contributors
;; Keywords: lisp, tools
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (llm "0.1"))
;; URL: https://github.com/toracle/evolmacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; Evolmacs is a self-evolving Emacs agent that dynamically generates,
;; tests, repairs, and integrates new Emacs Lisp functions based on
;; user natural language descriptions.

;;; Code:

(require 'evolmacs-core)
(require 'evolmacs-eval)
(require 'evolmacs-llm)

;;;###autoload
(defun evolmacs-create-function (spec function-name &optional library)
  "Create a new function based on natural language SPEC.
FUNCTION-NAME is the name to use for the function.
Optional LIBRARY specifies where to save the function."
  (interactive "sFunction specification: \nsFunction name: \nsTarget library (optional): ")
  (let* ((library (or library "user-functions"))
         (result (evolmacs-generate-and-test spec function-name library)))
    (if (plist-get result :success)
        (message "Successfully created and tested function %s" function-name)
      (message "Failed to create function: %s" (plist-get result :error)))))

(provide 'evolmacs)
;;; evolmacs.el ends here