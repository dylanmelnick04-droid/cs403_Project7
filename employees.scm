;; employees.scm
(define (read-all-lines filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

;; function to calculate hourly pay
(define (calcHourly pay hours)
  (* pay hours))

;; function to calculate commissioned pay
(define (calcCommission min gross commissionRate)
  (if (> (* gross commissionRate) min)
    min
  (* gross commissionRate)
))

(define (perform . args)
  (call-with-current-continuation
    (lambda (return)
      (let ((n (length args)))
        (cond
          ((or (= n 2) (= n 4))
           (display "Second argument: ")
           (display (cadr args))
           (newline)

           ;; if 4 args, display 3rd and 4th
           (when (= n 4)
             (display "Third argument: ")
             (display (caddr args))
             (newline)
             (display "Fourth argument: ")
             (display (cadddr args))
             (newline))

           ;; let* at the bottom, properly nested
           (let* ((filename (car args))
                  (action (cadr args))
                  (employee-lines (read-all-lines filename)))
            
             ;; use employee-lines here
             (display "Lines read from file:") (newline)
             (for-each (lambda (line)
                         (display line)
                         (newline))
                       employee-lines)))

          ;; invalid argument count
          (else
           (newline)
           (display "Usage: (perform employee_file action)") (newline)
           (display "or") (newline)
           (display "Usage: (perform employee_file action operator threshold)") (newline)
           (display "Valid actions: count print min max total avg") (newline)
           (display "Valid operators: eq ne gt ge lt le") (newline)
           (return -1)))))))