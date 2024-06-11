#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                                    ;; -----------------------
                                    ;; INSERT YOUR CODE HERE 
                                    ;; -----------------------
                              ((= op 3) (/ num1 num2))
                              (else
                              (- num1 num2))

                                    ;; -----------------------
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1 num2bot) num2top))
                              (else
                               (cons (- (* num1 num2bot) num2top) num2bot))

                              ;; -----------------------

                              
                              ))))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                               ((= op 3) (cons num1top (* num1bot num2))) ;; 5/2 4
                              (else
                               (cons (- num1top (* num2 num1bot)) num1bot)) ;; 5/2 4

                              ;; -----------------------
                              ))))

                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1top num2bot) (* num1bot num2top)))
                              (else
                               (cons (- (* num1top num2bot) (* num2top num1bot))(* num1bot num2bot))) ;; 3/2 - 4/5

                              ;; ----------------------- 
                            ))))))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                        (if (zero? num1)
                          (bool-val #t)
                          (bool-val #f))
                          ;; -----------------------
                          ;; INSERT YOUR CODE HERE 
                          ;; -----------------------
                          (if (and (pair? num1) (= 0 (cdr num1)))
                              #t
                              #f
                              )

                          ;; ----------------------- 
                        ))))

      

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------

      (rational-exp (num1 num2)
                    (if (= num2 0)
                        (eopl:error "divide by zero error")
                        (rational-val (cons num1 num2))))
                    
      (if-elif-exp (ifexp exp1 elifexp exp2 elsexp)
                   (let ((ifval (value-of ifexp env)))
                     (let ((bool1 (expval->bool ifval)))
                       (if bool1
                           (value-of exp1 env)
                           (let ((elifval (value-of elifexp env)))
                             (let ((bool2 (expval->bool elifval)))
                               (if bool2
                                   (value-of exp2 env)
                                   (value-of elsexp env))))))))
                   
      (simpl-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                         num1
                         (if (pair? num1)
                             (rational-val (cons (/ (car num1) (gcd (car num1) (cdr num1))) (/ (cdr num1) (gcd (car num1)(cdr num1)))))
                             (eopl:error "cannot be simplified")
                             )))))
      
      (list-exp () (list-val '()))

      (cons-exp (exp1 exp2)
                (let ((num (expval->num (value-of exp1 env)))
                      (lst (expval->list (value-of exp2 env))))
                  (list-val (cons num lst))))

      (mul-exp (exp1)
               (let ((lst (expval->list (value-of exp1 env))))
                 (if (null? lst)
                     (num-val 0)
                     (num-val (multiply-lst lst)))))

      (min-exp (exp1)
               (let ((lst (expval->list (value-of exp1 env))))
                 (if (null? lst)
                     (num-val -1)
                     (num-val (min-of-lst lst)))))
                      

      ;; -----------------------

      )))

      ;; helper functions we added

     (define (gcd a b)
         (if (= b 0)
             a
             (gcd b (remainder a b))))

     (define multiply-lst
       (lambda (lst)
         (if (null? lst)
            1
            (* (car lst) (multiply-lst (cdr lst))))))

     (define min-of-lst
       (lambda (lst)
         (min-iter-helper lst +inf.0)))

     (define min-iter-helper
       (lambda (lst min)
         (if (null? lst)
             min
             (if (< (car lst) min)
                 (min-iter-helper (cdr lst) (car lst))
                 (min-iter-helper (cdr lst) min)))))
 