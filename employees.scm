;; employees.scm

(define (caddddr x) (car (cdr (cdr (cdr (cdr x))))))
(define (cadddddr x) (car (cdr (cdr (cdr (cdr (cdr x)))))))

(define (split-string str)
  (let ((len (string-length str)))
    (let loop ((i 0) (current "") (parts '()))
      (if (= i len)
          (reverse (if (string=? current "") parts (cons current parts)))
          (let ((c (string-ref str i)))
            (if (char=? c #\space)
                (loop (+ i 1) "" (if (string=? current "") parts (cons current parts)))
                (loop (+ i 1) (string-append current (string c)) parts)))))))

(define (read-all-lines filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (let ((line-trim (string-trim line)))
                (if (string=? line-trim "")
                    (loop lines)
                    (let* ((parts (split-string line-trim))
                           (parsed (map (lambda (s)
                                          (let ((n (string->number s)))
                                            (if n n (string->symbol s))))
                                        parts)))
                      (loop (cons parsed lines)))))))))))

;; function to calculate hourly pay
(define (calcHourly pay hours)
  (* pay hours))

;; function to calculate commissioned pay
(define (calcCommission min gross commissionRate)
  (if (> (* gross commissionRate) min)
    min
  (* gross commissionRate)
))

(define (alterLinesInFatCollection lines modifier scalar)
  (filter
   (lambda (line)
     (let ((type (car line)))
       (cond
         ((eq? modifier 'lt)
          (cond
            ((eq? type 'salaried)
             (< (cadddr line) scalar))
            ((eq? type 'commission)
             (< (calcCommission (cadddr line)
                                (caddddr line)
                                (cadddddr line))
                scalar))
            ((eq? type 'hourly)
             (< (calcHourly (cadddr line)
                            (caddddr line))
                scalar))
            (else #f)))

         ((eq? modifier 'gt)
          (cond
            ((eq? type 'salaried)
             (> (cadddr line) scalar))
            ((eq? type 'commission)
             (> (calcCommission (cadddr line)
                                (caddddr line)
                                (cadddddr line))
                scalar))
            ((eq? type 'hourly)
             (> (calcHourly (cadddr line)
                            (caddddr line))
                scalar))
            (else #f)))

         ((eq? modifier 'le)
          (cond
            ((eq? type 'salaried)
             (<= (cadddr line) scalar))
            ((eq? type 'commission)
             (<= (calcCommission (cadddr line)
                                (caddddr line)
                                (cadddddr line))
                scalar))
            ((eq? type 'hourly)
             (<= (calcHourly (cadddr line)
                            (caddddr line))
                scalar))
            (else #f)))

         ((eq? modifier 'ge)
          (cond
            ((eq? type 'salaried)
             (>= (cadddr line) scalar))
            ((eq? type 'commission)
             (>= (calcCommission (cadddr line)
                                (caddddr line)
                                (cadddddr line))
                scalar))
            ((eq? type 'hourly)
             (>= (calcHourly (cadddr line)
                            (caddddr line))
                scalar))
            (else #f)))

         ((eq? modifier 'eq)
          (cond
            ((eq? type 'salaried)
             (= (cadddr line) scalar))
            ((eq? type 'commission)
             (= (calcCommission (cadddr line)
                                (caddddr line)
                                (cadddddr line))
                scalar))
            ((eq? type 'hourly)
             (= (calcHourly (cadddr line)
                            (caddddr line))
                scalar))
            (else #f)))

         ((eq? modifier 'ne)
          (cond
            ((eq? type 'salaried)
             (not (= (cadddr line) scalar)))
            ((eq? type 'commission)
             (not (= (calcCommission (cadddr line)
                                (caddddr line)
                                (cadddddr line))
                scalar)))
            ((eq? type 'hourly)
             (not (= (calcHourly (cadddr line)
                            (caddddr line))
                scalar)))
            (else #f)))

         (else #f))))
   lines))

(define (perform . args)
  (call-with-current-continuation
   (lambda (return)
     (let ((n (length args)))
       (cond
         ((or (= n 2) (= n 4))
          ;; first let* binds filename, action, employee-lines
          (let* ((filename (car args))
                 (action (cadr args))
                 (employee-lines (read-all-lines filename)))
            
            ;; nested let binds symbols and altered lines
            (let ((action-sym (if (string? action) (string->symbol action) action))
                  (operator-sym (if (= n 4) (string->symbol (caddr args)) 'none))
                  (altered-lines (if (= n 4)
                                     (alterLinesInFatCollection employee-lines
                                                               (if (string? (caddr args)) (string->symbol (caddr args)) (caddr args))
                                                               (cadddr args))
                                     employee-lines)))
              
              (display "Lines read from file:") (newline)
              (for-each (lambda (line)
                          (display line)
                          (newline))
                        employee-lines)

              (display "Altered lines after applying filter:") (newline)
              (for-each (lambda (line)
                          (display line)
                          (newline))
                        altered-lines)

              (cond
                ((eq? action-sym 'print)
                 (display "Printing altered lines done.") (newline))
                ((eq? action-sym 'count)
                 (display "Count of altered lines: ")
                 (display (length altered-lines))
                 (newline))
                ((eq? action-sym 'total) (display "TODO total") (newline))
                ((eq? action-sym 'min)   (display "TODO min") (newline))
                ((eq? action-sym 'max)   (display "TODO max") (newline))
                ((eq? action-sym 'avg)   (display "TODO avg") (newline))
                (else (display "Unknown action") (newline))))))

         ;; invalid argument count
         (else
          (newline)
          (display "Usage: (perform employee_file action)") (newline)
          (display "or") (newline)
          (display "Usage: (perform employee_file action operator threshold)") (newline)
          (display "Valid actions: count print min max total avg") (newline)
          (display "Valid operators: eq ne gt ge lt le") (newline)
          (return -1)))))))