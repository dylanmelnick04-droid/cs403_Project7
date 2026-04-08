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
  (let ((comm (* gross commissionRate)))
    (if (> comm min)
        comm
        min)))

(define (wrap-lines lines) lines)

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

(define (compute-total lines)
  (if (null? lines)
      0
      (+ (get-first-number (car lines))
         (compute-total (cdr lines)))))

(define (compute-max lines)
  (if (null? lines)
      -inf.0  ;; start with negative infinity
      (let ((income (get-first-number (car lines))))
        (max income (compute-max (cdr lines))))))

(define (compute-min lines)
  (if (null? lines)
      +inf.0  ;; start with positive infinity
      (let ((income (get-first-number (car lines))))
        (min income (compute-min (cdr lines))))))

(define (compute-avg lines)
  (if (null? lines)
      0
      (/ (compute-total lines)
         (length lines))))


(define (get-first-number line)
  (let ((type (car line))) ; line is a list: (hourly Viola Jennings 65 17.5)
    (cond
      ((eq? type 'salaried)
       (cadddr line)) ; 4th element = salary
      ((eq? type 'hourly)
       (calcHourly (cadddr line) (cadddr (cdr line)))) ; 65 * 17.5
      ((eq? type 'commission)
       (calcCommission (cadddr line) (cadddr (cdr line)) (cadddr (cddr line))))
      (else 0))))

(define (earned-pay line)
  (let ((type (car line)))
    (cond
      ((eq? type 'salaried) (cadddr line))                               ; 4th element = salary
      ((eq? type 'hourly) (* (cadddr line) (caddddr line)))             ; rate * hours
      ((eq? type 'commission) (calcCommission (cadddr line)             ; min
                                             (caddddr line)            ; gross
                                             (cadddddr line)))         ; rate
      (else 0))))

(define (print-lines lines)
  (for-each
   (lambda (line)
     (let ((type (car line)))
       (cond
         ;; salaried employee
         ((eq? type 'salaried)
          (display "Salaried employee: ")
          (display (symbol->string (cadr line))) ; first name
          (display " ")
          (display (symbol->string (caddr line))) ; last name
          (newline)
          (display "weekly salary: ")
          (display-2decimal (cadddr line)) ; salary
          (newline)
          (display "earned: $")
          (display-2decimal (earned-pay line))
          (newline)(newline))

         ;; hourly employee
         ((eq? type 'hourly)
          (display "Hourly employee: ")
          (display (symbol->string (cadr line)))
          (display " ")
          (display (symbol->string (caddr line)))
          (newline)
          (display "hours worked: ")
          (display-2decimal (caddddr line))     ; hours
          (display ", hourly rate: ")
          (display-2decimal (cadddr line))      ; rate
          (newline)
          (display "earned: $")
          (display-2decimal (earned-pay line))
          (newline)(newline))

         ;; commissioned employee
         ((eq? type 'commission)
          (display "Commission employee: ")
          (display (symbol->string (cadr line)))
          (display " ")
          (display (symbol->string (caddr line)))
          (newline)
          (display "minimum salary: ")
          (display-2decimal (cadddr line))
          (display ", sales amount: ")
          (display-2decimal (caddddr line))
          (display ", commission rate: ")
          (display-2decimal (* 100 (cadddddr line)))
          (display "%")
          (newline)
          (display "Earned: $")
          (display-2decimal (earned-pay line))
          (newline)(newline))

         (else
          (display "Unknown employee type") (newline)))))
   lines))
  
(define (display-2decimal x)
  (let* ((r (round (* x 100)))
         (int-part (inexact->exact (quotient r 100)))
         (frac-part (inexact->exact (modulo r 100))))
    (display
     (string-append (number->string int-part)
                    "."
                    (if (< frac-part 10)
                        (string-append "0" (number->string frac-part))
                        (number->string frac-part))))))

(define (perform . args)
  (call-with-current-continuation
   (lambda (return)
     (let ((n (length args)))
       (cond
         ;; handle 2 or 4 arguments
         ((or (= n 2) (= n 4))
          (let* ((filename (car args))
                 (action (cadr args))
                 (employee-lines (read-all-lines filename))
                 ;; wrap lines if only 2 arguments, else filter them
                 (altered-lines (if (= n 2)
                                    (wrap-lines employee-lines)
                                    (alterLinesInFatCollection employee-lines
                                                              (if (string? (caddr args))
                                                                  (string->symbol (caddr args))
                                                                  (caddr args))
                                                              (cadddr args)))))
            (let ((action-sym (if (string? action) (string->symbol action) action)))
              
              ;; display original lines
              ;;(display "Lines read from file:") (newline)
              ;;(for-each (lambda (line)
              ;;            (display line)
              ;;            (newline))
              ;;          employee-lines)

              ;; display altered/wrapped lines
              ;;(display "Altered lines after applying filter/wrap:") (newline)
              ;;(for-each (lambda (line)
              ;;            (display line)
              ;;            (newline))
              ;;          altered-lines)

              ;; handle actions
              (cond
                ((eq? action-sym 'print)
                  ;; call the pretty-print function
                  (print-lines altered-lines))
                ((eq? action-sym 'count)
                 (display "There are ")
                 (display (length altered-lines))
                 (display " employees")
                 (newline)(newline))
                ((eq? action-sym 'total)
                 (let ((total (compute-total altered-lines)))
                   (display "Total payments is: $")
                   (display-2decimal total)
                   (newline)(newline)))
                ((eq? action-sym 'min)
                 (let ((min (compute-min altered-lines)))
                   (display "Minimum payment is: $")
                   (display-2decimal min)
                   (newline)(newline)))
                ((eq? action-sym 'max)
                 (let ((max (compute-max altered-lines)))
                   (display "Maximum payment is: $")
                   (display-2decimal max)
                   (newline)(newline)))
                ((eq? action-sym 'avg)
                 (let ((total (compute-total altered-lines)))
                   (display "Average payment per employee is $")
                   (display-2decimal (/ total (length altered-lines)))
                   (newline)(newline)))
                (else (display "Unknown action") (newline))))))
         
         ;; invalid argument count
         (else
          (newline)
          (display "Usage: (perform employee_file action)") (newline)
          (display "or") (newline)
          (display "Usage: (perform employee_file action operator threshold)") (newline) (newline)
          (display "Valid actions: count print min max total avg") (newline)
          (display "Valid operators: eq ne gt ge lt le") (newline) (newline)
          (return -1)))))))