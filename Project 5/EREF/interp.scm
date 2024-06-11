(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require racket/trace)
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
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

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################

        ;; Part A

        (newvector-exp (exp1 exp2)
                       (let ((size (expval->num (value-of exp1 env)))
                             (value (value-of exp2 env)))
                         (vec-val (new-vector size value))))

        (update-vector-exp (exp1 exp2 exp3)
                           (let ((my-vector (expval->vec (value-of exp1 env)))
                                 (index (expval->num (value-of exp2 env)))
                                 (value (value-of exp3 env)))
                             (update-vector my-vector index value)))

        (read-vector-exp (exp1 exp2)
                         (let ((my-vector (expval->vec (value-of exp1 env)))
                               (index (expval->num (value-of exp2 env))))
                           (read-vector my-vector index)))

        (length-vector-exp (exp1)
                         (let ((my-vector (expval->vec (value-of exp1 env))))
                           (num-val (length-vector my-vector))))

        (swap-vector-exp (exp1 exp2 exp3)
                         (let ((my-vector (expval->vec (value-of exp1 env)))
                               (index1 (expval->num (value-of exp2 env)))
                               (index2 (expval->num (value-of exp3 env))))
                           (swap-vector my-vector index1 index2)))
        
        (copy-vector-exp (exp)
                         (let ((my-vector (expval->vec (value-of exp env))))
                           (vec-val (copy-vector my-vector))))

        ;; Part B

        (newqueue-exp (exp1)
                    (let ((max-size (expval->num (value-of exp1 env))))
                      (queue-val (new-queue max-size))))

        (enqueue-exp (exp1 exp2)
                   (let ((queue (expval->queue (value-of exp1 env)))
                         (val (value-of exp2 env)))
                     (enqueue-queue queue val)))

        (dequeue-exp (exp1)
                     (let ((queue (expval->queue (value-of exp1 env))))
                       (dequeue-queue queue)))

        (queue-size-exp (exp1)
                        (let ((queue (expval->queue (value-of exp1 env))))
                          (num-val (size-queue queue))))

        (peek-queue-exp (exp1)
                        (let ((queue (expval->queue (value-of exp1 env))))
                          (peek-queue queue)))

        (queue-empty-exp (exp1)
                         (let ((queue (expval->queue (value-of exp1 env))))
                           (bool-val (empty-queue? queue))))

        (print-queue-exp (exp1)
                         (let ((queue (expval->queue (value-of exp1 env))))
                           (print-queue queue))) 

        ;; Part C

        (vec-mult-exp (exp1 exp2)
                      (let ((vec1 (expval->vec (value-of exp1 env)))
                            (vec2 (expval->vec (value-of exp2 env))))
                        (vec-val (vec-mult vec1 vec2))))

        )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE

  ;; Part A

  (define (new-vector size value) ;; new vector 
    (if (> size 0)
        (new-vector-loop 0 -1 value size)
        (eopl:error 'new-vector "length of vector should be positive")))

  (define (new-vector-loop i ref value size) ;; helper function
    (if (= i size)
      (my-vector (- ref (- size 1)) size)
      (new-vector-loop (+ i 1) (newref value) value size)))


  (define (update-vector vector index value) ;; update vector
    (cases vec vector
      (my-vector (first size)
                 (if (and (> index -1) (> size index))
                     (setref! (+ index first) value)
                     (eopl:error 'update-vector "index out of bounds!")))))

  (define (read-vector vector index) ;; read vector 
    (cases vec vector
      (my-vector (first size)
                 (if (and (> index -1) (> size index))
                     (deref (+ index first))
                     (eopl:error 'update-vector "index out of bounds!")))))

  
  (define (length-vector vector) ;; length of vector 
    (cases vec vector
      (my-vector (first size)
                 size))) ;; num-val here

  (define (swap-vector vector index1 index2) ;; swap vector
    (cases vec vector
      (my-vector (first size)
                 (if (and (and (> index1 -1) (> size index1)) (and (> index2 -1) (> size index2)))
                     (let ((tmp (deref (+ index1 first))))
                       (setref! (+ index1 first) (deref (+ index2 first)))
                       (setref! (+ index2 first) tmp))
                     (eopl:error 'swap-vector "one of the indices are out of bounds!")))))

  (define (copy-vector vector) ;; copy vector 
    (cases vec vector
      (my-vector (first size)
                (copy-vector-loop 0 (new-vector size (num-val 0)) first size))))

  (define (copy-vector-loop i copy first size) ;; helper function
    (if (= i size)
        copy
        (begin
          (update-vector copy i (deref (+ first i)))
          (copy-vector-loop (+ i 1) copy first size))))

  ;; Part B

  (define (new-queue l)
    (my-queue (new-vector l 0)
              (newref 0)
              (newref -1)
              (newref 0)))
  
  (define (enqueue-queue que value)
  (cases queue que
    (my-queue (data front back size)
              (if (full-queue? que)
                  (eopl:error 'enqueue-queue "queue is full, cannot insert new element")
                  (begin
                    (setref! back (modulo (+ 1 (deref back)) (length-vector data)))
                    (update-vector data (deref back) value)
                    (setref! size (+ 1 (deref size)))
                    que)))))

  (define (dequeue-queue que)
  (cases queue que
    (my-queue (data front back size)
              (if (empty-queue? que)
                  -1 ;; this is wanted in the pdf
                  (let ((value (read-vector data (deref front))))
                    (setref! front (modulo (+ 1 (deref front)) (length-vector data)))
                    (setref! size (- (deref size) 1))
                    value)))))
  
  (define (size-queue que)
    (cases queue que
      (my-queue (data front back size)
                 (deref size))))

  (define (peek-queue que)
  (cases queue que
    (my-queue (data front back size)
      (if (empty-queue? que)
          (eopl:error 'peek-queue "queue is empty, no element to peek.")
          (read-vector data (deref front))))))

  (define (empty-queue? que) 
    (cases queue que
      (my-queue (data front back size)
                (= (deref size) 0))))

  (define (full-queue? que) ;; helper function for full check. 
    (cases queue que
      (my-queue (data front back size)
                 (= (deref size) (length-vector data)))))

  (define (print-queue que)
    (cases queue que
      (my-queue (data front back size)
                (print-queue-helper data (deref front) (deref size) (length-vector data)))))
  
  (define (print-queue-helper data index count length) ;; helper for print-que 
    (if (= count 0)
        (begin
          (newline))
        (begin
          (display (read-vector data index))
          (display " ")
          (print-queue-helper data (modulo (+ index 1) length) (- count 1) length))))

  ;; Part C
  
  (define (vec-mult vector1 vector2)
    (cases vec vector1
      (my-vector (first1 size1)
                 (cases vec vector2
                   (my-vector (first2 size2)
                              (if (= size1 size2)
                                  (let ((result (new-vector size1 0))) ;; create empty vector of equal size to fill it.
                                    (vec-mult-helper first1 first2 result 0 size1) ;; fill the vector 
                                    result)
                                  (eopl:error 'vec-mult "Vectors dimension mismatch.")))))))

  (define (vec-mult-helper first1 first2 result index size) ;; helper function for filling the vector 
    (if (< index size)
        (begin
          (let ((val1 (expval->num (deref (+ first1 index))))
                (val2 (expval->num (deref (+ first2 index)))))
            (update-vector result index (num-val (* val1 val2))) ;; update the result's index
            (vec-mult-helper first1 first2 result (+ index 1) size))) ;; recursion
        result))



  
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
