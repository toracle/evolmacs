;;; evolmacs-tests.el --- Tests for Evolmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Evolmacs Contributors

;; Author: Evolmacs Contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; Test suite for Evolmacs.

;;; Code:

(require 'ert)
(require 'evolmacs-eval)

;; We're only testing evolmacs-eval.el here, so we don't need the actual llm dependency
;; Let's mock what we need from evolmacs-llm.el instead of requiring it directly
(declare-function evolmacs-llm-ensure-deps "evolmacs-llm")
(declare-function evolmacs-llm-generate-package "evolmacs-llm")
(declare-function evolmacs-llm-repair-function "evolmacs-llm")

;; Tests for the eval gateway
(ert-deftest evolmacs-test-eval-safely-success ()
  "Test that valid code is evaluated successfully."
  (let ((result (evolmacs-eval-safely "(+ 2 2)")))
    (should (plist-get result :success))
    (should (equal (plist-get result :result) 4))
    (should (null (plist-get result :error)))))

(ert-deftest evolmacs-test-eval-safely-failure ()
  "Test that invalid code returns an error."
  (let ((result (evolmacs-eval-safely "(/ 1 0)")))
    (should-not (plist-get result :success))
    (should-not (null (plist-get result :error)))))

(ert-deftest evolmacs-test-function-extraction ()
  "Test that function name is correctly extracted."
  (let ((result (evolmacs-eval-function-safely "(defun test-function () \"Test function.\" 42)")))
    (should (plist-get result :success))
    (should (equal (plist-get result :function-name) "test-function"))))

;; Add more tests as functions are implemented

(provide 'evolmacs-tests)

;;; evolmacs-tests.el ends here