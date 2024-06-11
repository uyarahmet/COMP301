#lang eopl

;; interpreter for the PROC language, using the procedural
;; representation of procedures.



(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      
      (const-exp (num) (num-val num))
      
      
      (var-exp (var) (apply-env env var))
      
     
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
     
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (value-of rand env)))
                  (apply-procedure proc arg)))
      
      ;;----------------------------------------------------
      ; INSERT YOUR CODE HERE
      ; Write the required expressions starting from here

      (stack-exp () (stack-val '()))

      (stack-push-exp (exp1 exp2)
                      (let ((stack (expval->stack (value-of exp1 env)))
                            (num (expval->num (value-of exp2 env))))
                        (stack-val (stack-push stack num))))

      (stack-pop-exp (exp)
                     (let ((stack (expval->stack (value-of exp env))))
                       (if (null? stack)
                           (begin
                             (display "Warning: Stack is empty for stack pop!\n")
                             (stack-val '()))
                        (stack-val (stack-pop stack)))))

      (stack-peek-exp (exp)
                      (let ((stack (expval->stack (value-of exp env))))
                        (num-val (if (null? stack)
                                     (begin
                                       (display "Warning: Stack is empty for stack peek!\n")
                                       2813)
                                       (car stack)))))

      (stack-push-multi-exp (exp exps)
                            (let ((stack (expval->stack (value-of exp env)))
                                  (nums (map (lambda (exp) (expval->num (value-of exp env))) exps)))
                              (stack-val (stack-push-multi stack nums))))

      (stack-pop-multi-exp (exp1 exp2)
                            (let ((stack (expval->stack (value-of exp1 env)))
                                  (num (expval->num (value-of exp2 env))))
                              (stack-val (stack-pop-multi stack num))))

      (stack-merge-exp (exp1 exp2)
                        (let ((q1 (expval->stack (value-of exp1 env)))
                              (q2 (expval->stack (value-of exp2 env))))
                          (stack-val (stack-merge q1 q2))))

      ;;-------------------------------------------------
      
      )))

;;-----------------------------------------
; INSERT YOUR CODE HERE
; you may use this area to define helper functions
;;-----------------------------------------

(define stack-push
  (lambda (stack num)
    (cons num stack)))

(define stack-pop
  (lambda (stack)
    (if (null? stack)
        '()
        (cdr stack))))

(define stack-push-multi
  (lambda (stack nums)
    (if (null? nums) stack
        (stack-push-multi (stack-push stack (car nums))(cdr nums)))))

(define stack-pop-multi
  (lambda (stack num)
  (if (or (zero? num) (if (null? stack) (begin (display "Warning: cannot pop more than size!\n") #t) #f)) ; putting zero? pred left of or shortcircuits the second pred if num is zero. 
      stack
      (stack-pop-multi (stack-pop stack) (- num 1)))))

(define stack-merge
  (lambda (stack1 stack2)
    (if (null? stack2)
        stack1
        (stack-merge (stack-push stack1 (car stack2)) (stack-pop stack2)))))


;;-----------------------------------------

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))
