#lang racket

; Basic Hash Table Examples
(define ht (let ((my-hash-table (make-immutable-hash '((a . 10) (b . 5) (c . 2)))))
  (hash-set my-hash-table 'd -2)))

;ht
;(hash-ref ht 'c)

;(hash-remove ht 'a)

; make-empty-env creates an environment given an ID of the enclosing environment.
; Returns a pair containing the following:
; 1st element: empty hash table
; 2nd environment: ID of enclosing environment
(define (make-empty-env enclosing)
  (cons (make-immutable-hash) enclosing))


; built-in functions in our interpreter
(define built-in-names '(define lambda set! + - / *))


(define (get-global-env)
  (let ((global-frame (make-immutable-hash (map (lambda (x) (cons x 'BUILT-IN-FUNCTION)) built-in-names))))
    (cons global-frame 0)))


; An environment map is a mapping from environment names (IDs) to environments
(define (get-global-env-map)
  (make-immutable-hash (list (cons 1 (get-global-env)))))


(define (env-lookup env-map env-id key)
  (if (zero? env-id)
      (error (string-append "Wasn't able to find key "
                            (symbol->string key)
                            " in any environment."))
      (let* ((env (hash-ref env-map env-id))
             (value (hash-ref (car env) key 'NOT-FOUND)))
        (if (equal? value 'NOT-FOUND)
            (env-lookup env-map (cdr env) key)
            value))))


(define (env-add-key-value env-map env-id key value)
  (let* ((env (hash-ref env-map env-id))
         (frame (car env))
         (new-frame (hash-set frame key value))
         (new-env (cons new-frame (cdr env))))
    (hash-set env-map env-id new-env)))


; Delete the environment with the associated env-id
(define (env-remove env-map env-id)
  (hash-remove env-map env-id))


; Example of adding a new environment to the env-map
; (hash-set (get-global-env-map) 2 (make-empty-env 1))


(define (eval-define pre post env-id env-map)
  (if (number? post)
      (cons '() (env-add-key-value env-map env-id pre post))
      (cons '() (env-add-key-value env-map env-id (first pre) (append '(lambda) (list (rest pre)) (list post))))))


(define (eval-add-helper pre post env-id env-map)
  (if (empty? post)
       pre
      (eval-add-helper (+ (car (my-eval pre env-id env-map)) (car (my-eval (first post) env-id env-map))) (rest post) env-id env-map)))


(define (eval-add pre post env-id env-map)
  (cons (eval-add-helper pre post env-id env-map) env-map))


(define (eval-sub-helper pre post env-id env-map)
  (if (empty? post)
       pre
      (eval-sub-helper (- (car (my-eval pre env-id env-map)) (car (my-eval (first post) env-id env-map))) (rest post) env-id env-map)))


(define (eval-sub pre post env-id env-map)
  (cons (eval-sub-helper pre post env-id env-map) env-map))


(define (eval-mul-helper pre post env-id env-map)
  (if (empty? post)
       pre
      (eval-mul-helper (* (car (my-eval pre env-id env-map)) (car (my-eval (first post) env-id env-map))) (rest post) env-id env-map)))


(define (eval-mul pre post env-id env-map)
  (cons (eval-mul-helper pre post env-id env-map) env-map))


(define (eval-div-helper pre post env-id env-map)
  (if (empty? post)
       pre
      (eval-div-helper (/ (car (my-eval pre env-id env-map)) (car (my-eval (first post) env-id env-map))) (rest post) env-id env-map)))


(define (eval-div pre post env-id env-map)
  (cons (eval-div-helper pre post env-id env-map) env-map))


(define (eval-remainder pre post env-id env-map)
  (cons (remainder (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map))) env-map))


(define (eval-mod pre post env-id env-map)
  (cons (modulo (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map))) env-map))


(define (eval-and pre post env-id env-map)
  (cons (and (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map))) env-map))


(define (eval-or pre post env-id env-map)
  (cons (or (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map))) env-map))


(define (eval-set pre post env-id env-map)
  (let* ((new-env (hash-ref env-map env-id))
        (new-frame (hash-remove (car new-env) pre))
        (new-frame (hash-set new-frame pre (car (my-eval post env-id env-map))))
        (new-env (cons new-frame env-id))
        (new-map (hash-remove env-map env-id)))
    (cons '() (hash-set new-map env-id new-env))))


(define (eval-cons pre post env-id env-map)
  (cons (cons (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map))) env-map))


(define (eval-car post env-id env-map)
  (let ((post-result (car (my-eval post env-id env-map))))
    (cons (car post-result) env-map)))


(define (eval-cdr post env-id env-map)
  (let ((post-result (car (my-eval post env-id env-map))))
    (cons (cdr post-result) env-map)))


(define (eval-eq pre post env-id env-map)
  (let ((result (eq? (second (car (my-eval pre env-id env-map))) (second (car (my-eval post env-id env-map))))))
    (cons result env-map)))


(define (eval-equal pre post env-id env-map)
  (let ((result (equal? (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map)))))
    (cons result env-map)))


(define (eval-equal-sign pre post env-id env-map)
  (let ((result (= (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map)))))
    (cons result (get-global-env-map))))


(define (eval-gt pre post env-id env-map)
  (let ((result (> (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map)))))
    (cons result env-map)))


(define (eval-lt pre post env-id env-map)
  (let ((result (< (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map)))))
    (cons result env-map)))


(define (eval-nlt pre post env-id env-map)
  (let ((result (>= (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map)))))
    (cons result env-map)))


(define (eval-ngt pre post env-id env-map)
  (let ((result (<= (car (my-eval pre env-id env-map)) (car (my-eval post env-id env-map)))))
    (cons result env-map)))
        

(define (eval-if premise if-st else-st env-id env-map)
  (let ((result (if (car (my-eval premise env-id env-map))
                    (car (my-eval if-st env-id env-map))
                    (car (my-eval else-st env-id env-map)))))
    (cons result env-map)))


(define (length elems)
  (if (empty? elems)
      0
      (+ 1 (length (cdr elems)))))


(define (insert-mul-key env-map env-id params argus)
  (if (not (= (length params) (length argus)))
      (error "# of parameters does not equal to # of arguments")
      (if (= (length params) 0)
          env-map
          (let* ((new-map (env-add-key-value env-map env-id (first params) (car (my-eval (first argus) env-id env-map)))))
            (insert-mul-key new-map env-id (rest params) (rest argus))))))


(define (eval-empty sexpr env-id env-map)
  (cons (empty? (second (car (my-eval sexpr env-id env-map)))) env-map))


(define (eval-number sexpr env-id env-map)
  (cons (number? (car (my-eval sexpr env-id env-map))) env-map))


(define (eval-pair sexpr env-id env-map)
  (cons (pair? (car (my-eval sexpr env-id env-map))) env-map))


(define (eval-symbol sexpr env-id env-map)
  (let ((result (car (my-eval sexpr env-id env-map))))
    (if (list? result)
        (cons (symbol? (second (car (my-eval sexpr env-id env-map)))) env-map)
        (cons #f env-map))))
  

(define (eval-lambda expr argus env-id env-map)
  (let* ((new-env (make-empty-env env-id))
         (new-map (hash-set env-map (+ 1 env-id) new-env))
         (new-map (insert-mul-key new-map (+ 1 env-id) (second expr) argus)))
    (my-eval (third expr) (+ 1 env-id) new-map)))
                        

(define (my-eval-helper sexpr env-id env-map)
  (cond ((eq? (first sexpr) 'define) (eval-define (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) '+) (eval-add (second sexpr) (rest (rest sexpr)) env-id env-map))
        ((eq? (first sexpr) '-) (eval-sub (second sexpr) (rest (rest sexpr)) env-id env-map))
        ((eq? (first sexpr) '*) (eval-mul (second sexpr) (rest (rest sexpr)) env-id env-map))
        ((eq? (first sexpr) '/) (eval-div (second sexpr) (rest (rest sexpr)) env-id env-map))
        ((eq? (first sexpr) 'remainder) (eval-remainder (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) 'modulo) (eval-mod (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) 'and) (eval-and (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) 'or) (eval-or (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) 'set!) (eval-set (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) 'quote) (cons sexpr (get-global-env-map)))
        ((eq? (first sexpr) 'cons) (eval-cons (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) 'car) (eval-car (second sexpr) env-id env-map))
        ((eq? (first sexpr) 'cdr) (eval-cdr (second sexpr) env-id env-map))
        ((eq? (first sexpr) 'eq?) (eval-eq (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) 'equal?) (eval-equal (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) '=) (eval-equal-sign (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) '>) (eval-gt (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) '<) (eval-lt (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) '>=) (eval-nlt (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) '<=) (eval-ngt (second sexpr) (third sexpr) env-id env-map))
        ((eq? (first sexpr) 'if) (eval-if (second sexpr) (third sexpr) (fourth sexpr) env-id env-map))
        ((eq? (first sexpr) 'empty?) (eval-empty (second sexpr) env-id env-map))
        ((eq? (first sexpr) 'number?) (eval-number (second sexpr) env-id env-map))
        ((eq? (first sexpr) 'pair?) (eval-pair (second sexpr) env-id env-map))
        ((eq? (first sexpr) 'symbol?) (eval-symbol (second sexpr) env-id env-map))
        ((list? (first sexpr))
             (if (eq? (first (first sexpr)) 'lambda)
                 (eval-lambda (first sexpr) (rest sexpr) env-id env-map)
                   "invalid expression"))
        ((symbol? (first sexpr))
         (let* ((lamb (env-lookup env-map env-id (first sexpr)))
                (sexpr (append (list lamb) (list (second sexpr)))))
           (eval-lambda (first sexpr) (rest sexpr) env-id env-map)))
        (else "invalid expression")))


(define (last l)
  (cond ((null? (cdr l)) (car l))
        (else (last (cdr l)))))


(define (my-eval sexpr env-id env-map)
  (cond ((number? sexpr) (cons sexpr '()))
        ((boolean? sexpr) (cons sexpr '()))
        ((symbol? sexpr) (cons (env-lookup env-map env-id sexpr) env-map))
        (else
         (my-eval-helper sexpr env-id env-map))))


(define (eval-prog-helper sexprs answer end-id env-map)
  (if (empty? sexprs)
      answer
      (let ((result (my-eval (first sexprs) end-id env-map)))
        (eval-prog-helper (rest sexprs) (car result) (first (hash-keys (cdr result))) (cdr result)))))


(define (eval-prog sexprs)
  (eval-prog-helper sexprs '() 1 (get-global-env-map)))


; test Part 1
(display '(my-eval '#t 1 (get-global-env-map)))
(my-eval '#t 1 (get-global-env-map))

(display '(my-eval '5 1 (get-global-env-map)))
(my-eval '5 1 (get-global-env-map))

(display '(my-eval '+ 1 (get-global-env-map)))
(my-eval '+ 1 (get-global-env-map))
;(my-eval '% 1 (get-global-env-map))

(display '(my-eval '(+ 2 3) 1 (get-global-env-map)))
(my-eval '(+ 2 3) 1 (get-global-env-map))

; test Part 2
(display '(define x 5) )
(my-eval '(define x 5) 1 (get-global-env-map))

(display '(define (test x) x))
(my-eval '(define (test x) x) 1 (get-global-env-map))

(display '((lambda (x) (* x x)) 5))
(my-eval '((lambda (x) (* x x)) 5) 1 (get-global-env-map))

(display '(+ 1 2 3))
(my-eval '(+ 1 2 3) 1 (get-global-env-map))

(display '(- 3 2 1))
(my-eval '(- 3 2 1) 1 (get-global-env-map))

(display '(* 1 2 3))
(my-eval '(* 1 2 3) 1 (get-global-env-map))

(display '(/ 4 2 2))
(my-eval '(/ 4 2 2) 1 (get-global-env-map))

(display '(/ (+ 1 3) (- 5 3)))
(my-eval '(/ (+ 1 3) (- 5 3)) 1 (get-global-env-map))

(display '(remainder 5 2))
(my-eval '(remainder 5 2) 1 (get-global-env-map))

(display '(modulo 5 2))
(my-eval '(modulo 5 2) 1 (get-global-env-map))

(display '(set! x 10))
(define test-env-map (cdr (my-eval '(define x 5) 1 (get-global-env-map))))
test-env-map
(my-eval '(set! x 10) 1 test-env-map)

(display '(quote (1 2 3)))
(my-eval '(quote (1 2 3)) 1 test-env-map)

(display '(cons 5 7))
(my-eval '(cons 5 7) 1 test-env-map)

(display '(car (cons 5 7)))
(my-eval '(car (cons 5 7)) 1 test-env-map)

(display '(cdr (cons 5 7)))
(my-eval '(cdr (cons 5 7)) 1 test-env-map)

(display '(eq? 'a 'b))
(my-eval '(eq? 'a 'b) 1 test-env-map)

(display '(eq? 'a 'a))
(my-eval '(eq? 'a 'a) 1 test-env-map)

(display '(equal? 1 2))
(my-eval '(equal? 1 2) 1 test-env-map)

(display '(equal? 1 1))
(my-eval '(equal? 1 1) 1 test-env-map)

(display '(= 1 2))
(my-eval '(= 1 2) 1 test-env-map)

(display '(= 1 1))
(my-eval '(= 1 1) 1 test-env-map)

(display '(> 2 1))
(my-eval '(> 2 1) 1 test-env-map)

(display '(< 2 1))
(my-eval '(< 2 1) 1 test-env-map)

(display '(>= 2 1))
(my-eval '(>= 2 1) 1 test-env-map)

(display '(<= 2 1))
(my-eval '(<= 2 1) 1 test-env-map)

(display '(if (equal? 1 1) (* 1 1) (* 1 2)))
(my-eval '(if (equal? 1 1) (* 1 1) (* 1 2)) 1 test-env-map)

(display '(if (equal? 1 2) (* 1 1) (* 1 2)))
(my-eval '(if (equal? 1 2) (* 1 1) (* 1 2)) 1 test-env-map)

(display '(empty? '(1)))
(my-eval '(empty? '(1)) 1 test-env-map)

(display '(empty? '()))
(my-eval '(empty? '()) 1 test-env-map)

(display '(number? 5))
(my-eval '(number? 5) 1 test-env-map)

(display '(number? 'a))
(my-eval '(number? 'a) 1 test-env-map)

(display '(pair? (cons 1 2)))
(my-eval '(pair? (cons 1 2)) 1 test-env-map)

(display '(pair? 1))
(my-eval '(pair? 1) 1 test-env-map)

(display '(symbol? 'a))
(my-eval '(symbol? 'a) 1 test-env-map)

(display '(symbol? 1))
(my-eval '(symbol? 1) 1 test-env-map)

(display '(eval-prog '((define x 10) (set! x (+ 2 3)) (* x x))))
(eval-prog '((define x 10) (set! x (+ 2 3)) (* x x)))

;'(length (cons 1 (cons 3 (cons 5 (quote ())))))
;(define new-test-env-map (cdr (my-eval '(define (length elems)  (if (empty? elems) 0 (+ 1 (length (cdr elems))))) 1 (get-global-env-map))))
;(my-eval '(length (cons 1 (cons 3 (cons 5 (quote ()))))) 1 new-test-env-map)

; ((lambda (elems) (if (empty? elems) 0 (+ 1 (length (cdr elems))))) (cons 1 (cons 3 (cons 5 '()))))
(display '(eval-prog '((define (length elems)  (if (empty? elems) 0 (+ 1 (length (cdr elems)))))(length (cons 1 (cons 3 (cons 5 (quote ()))))))))
(eval-prog '((define (length elems)  (if (empty? elems) 0 (+ 1 (length (cdr elems)))))(length (cons 1 (cons 3 (cons 5 (quote ())))))))

