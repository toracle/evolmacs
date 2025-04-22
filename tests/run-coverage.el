;;; run-coverage.el --- Run tests with coverage reporting -*- lexical-binding: t; -*-

;; First load undercover with debugging
(message "Loading undercover for coverage reporting...")
(require 'undercover)
(message "Undercover loaded successfully")

;; Force coverage to be enabled
(setq undercover-force-coverage t)

;; Setup undercover with explicit path for report file
(message "Setting up undercover...")
(let ((report-file (expand-file-name "coverage-final.json")))
  (message "Coverage report will be saved to: %s" report-file)
  (undercover "evolmacs.el" "evolmacs-core.el" "evolmacs-eval.el" "evolmacs-llm.el"
             (:report-file report-file)
             (:report-format 'simplecov)
             (:send-report nil)))
(message "Undercover setup complete")

;; Check if any custom package path patterns are needed based on environment
(message "Setting up load paths...")
(let ((package-paths '())
      (root-dir default-directory))
  
  ;; Look for .eldev structure which may contain packages
  (let ((eldev-dir (expand-file-name ".eldev" root-dir)))
    (when (file-directory-p eldev-dir)
      (dolist (version (directory-files eldev-dir t "^[0-9]+\\.[0-9]+$"))
        (when (file-directory-p version)
          (let ((pkg-dir (expand-file-name "packages" version)))
            (when (file-directory-p pkg-dir)
              (dolist (pkg (directory-files pkg-dir t "^[^\\.]"))
                (when (file-directory-p pkg)
                  (push pkg package-paths)))))))))
  
  ;; Add paths to load-path if found
  (dolist (path package-paths)
    (add-to-list 'load-path path)))

;; Load all the source files explicitly to ensure they're instrumented
(message "Loading source files for instrumentation...")

;; First load the files directly to ensure they're instrumented
(let ((files '("evolmacs-eval.el" "evolmacs-llm.el" "evolmacs-core.el" "evolmacs.el")))
  (dolist (file files)
    (let ((file-path (expand-file-name file)))
      (if (file-exists-p file-path)
          (progn
            (message "Loading file directly: %s" file-path)
            (load file-path))
        (message "File not found: %s" file-path)))))

;; Now try to load the features
(condition-case err
    (progn
      (require 'evolmacs-eval nil t)
      (require 'evolmacs-llm nil t)
      (require 'evolmacs-core nil t)
      (require 'evolmacs nil t))
  (error (message "Error requiring features: %S" err)))

(message "Source files loaded")

;; Load test files with error handling
(message "Loading test files...")
(let ((test-files '("tests/evolmacs-tests.el" "tests/test-evolmacs-integration.el")))
  (dolist (file test-files)
    (let ((file-path (expand-file-name file)))
      (if (file-exists-p file-path)
          (condition-case err
              (progn
                (message "Loading test file: %s" file-path)
                (load file-path))
            (error (message "Error loading test file %s: %S" file file err)))
        (message "Test file not found: %s" file-path)))))
(message "Test files loaded")

;; Run the tests
(message "Running tests...")
(ert-run-tests-batch)
(message "Tests completed")

;; Save coverage report using the proper API
(message "Saving coverage report...")
(undercover-safe-report)
(message "Coverage report should be saved now")
(kill-emacs 0)