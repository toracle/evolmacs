;;; coverage-summary-ci.el --- Display coverage report for CI -*- lexical-binding: t; -*-

(require 'json)

(defun coverage-summary-format-file-ci (file-path stats)
  "Format coverage statistics for FILE-PATH with STATS for CI."
  (let* ((short-path (file-name-nondirectory file-path))
         (covered (plist-get stats :covered))
         (total (plist-get stats :total))
         (percent (if (> total 0)
                      (round (* 100 (/ (float covered) total)))
                    0))
         (bar-length 30)
         (filled-length (round (* bar-length (/ (float percent) 100))))
         (empty-length (- bar-length filled-length))
         (bar (concat 
               (make-string filled-length ?█)
               (make-string empty-length ?░))))
    (format "| %-20s | %3d%% | %s | %3d/%3d |" 
            short-path percent bar covered total)))

(defun coverage-summary-calculate-stats (coverage-data)
  "Calculate coverage statistics from COVERAGE-DATA."
  (let ((file-stats nil)
        (total-covered 0)
        (total-lines 0))
    (maphash
     (lambda (file-path line-coverage)
       (let ((covered 0)
             (total 0))
         (cl-loop for count across line-coverage
                  when (not (null count))
                  do (progn
                       (cl-incf total)
                       (when (and (numberp count) (> count 0))
                         (cl-incf covered))))
         (setq total-covered (+ total-covered covered))
         (setq total-lines (+ total-lines total))
         (push (list file-path 
                     :covered covered 
                     :total total
                     :percent (if (> total 0)
                                  (round (* 100 (/ (float covered) total)))
                                0))
               file-stats)))
     coverage-data)
    (list :files (nreverse file-stats)
          :total-covered total-covered
          :total-lines total-lines
          :total-percent (if (> total-lines 0)
                            (round (* 100 (/ (float total-covered) total-lines)))
                          0))))

(defun coverage-summary-ci ()
  "Display a CI-friendly summary of the coverage report."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'vector)
         (json-key-type 'string)
         (coverage-file (expand-file-name "coverage-final.json"))
         (coverage-data nil)
         (stats nil))
    
    (if (not (file-exists-p coverage-file))
        (princ "No coverage file found.\n")
      ;; Load coverage data
      (with-temp-buffer
        (insert-file-contents coverage-file)
        (setq coverage-data (json-read))
        (let ((undercover-data (gethash "undercover.el" coverage-data))
              (coverage-map (make-hash-table :test 'equal)))
          
          (when undercover-data
            (setq coverage-map (gethash "coverage" undercover-data))
            (setq stats (coverage-summary-calculate-stats coverage-map))
            
            ;; Print header
            (princ "| File                 | Coverage | Bar                              | Lines    |\n")
            (princ "|----------------------|---------|----------------------------------|----------|\n")
            
            ;; Print file details
            (dolist (file-stats (plist-get stats :files))
              (princ (concat (coverage-summary-format-file-ci 
                              (car file-stats) 
                              (cdr file-stats))
                             "\n")))
            
            ;; Print total
            (let* ((total-percent (plist-get stats :total-percent))
                   (bar-length 30)
                   (filled-length (round (* bar-length (/ (float total-percent) 100))))
                   (empty-length (- bar-length filled-length))
                   (bar (concat 
                         (make-string filled-length ?█)
                         (make-string empty-length ?░))))
              (princ "|----------------------|---------|----------------------------------|----------|\n")
              (princ (format "| %-20s | %3d%% | %s | %3d/%3d |\n" 
                             "TOTAL" 
                             total-percent 
                             bar
                             (plist-get stats :total-covered)
                             (plist-get stats :total-lines))))))))))

;; Run the summary and print to stdout
(coverage-summary-ci)