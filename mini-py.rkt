#lang eopl
(require racket/hash)
;; John Jaider Ramos Gaviria - 2370742
;; Kevin Ariel Ramirez Amaya - 2324793
;; Isabella Ruiz Celis - 2372234

;; Repositorio: https://github.com/KevinRamirezAmaya/Project-FLP.git

;;
;; <program>           ::= {<class-decl>} <expression>
;;                         ; a-program
;;
;; <expression>        ::= <identifier>
;;                         ; id-exp
;;                      | <number>
;;                         ; number-exp
;;                      | <string>
;;                         ; string-exp
;;                      | <boolean>
;;                         ; bool-exp
;;                      | x16 "(" {<number>} ")"
;;                         ; hex-exp
;;                      | var {<identifier> = <expression>} (",") in <expression>
;;                         ; var-exp
;;                      | const {<identifier> = <expression>} (",") in <expression>
;;                         ; const-exp
;;                      | rec {<identifier> "(" {<identifier>} (",") ")" = <expression>} in <expression>
;;                         ; letrec-exp
;;                      | set <identifier> = <expression>
;;                         ; set-exp
;;                      | begin <expression> {";" <expression>} end
;;                         ; begin-exp
;;                      | for "(" <identifier> = <expression> to <expression> ")"
;;                        "{" <expression> {"," <expression>} "}"
;;                         ; for-exp
;;                      | while <expression> do "(" <expression> {"," <expression>} ")" done
;;                         ; while-exp
;;                      | if <expression> then <expression> else <expression>
;;                         ; if-exp
;;                      | <primitive> "(" <expression> {"," <expression>} ")"
;;                         ; primapp-exp
;;                      | proc "(" {<identifier>} ")" <expression>
;;                         ; proc-exp
;;                      | "(" <expression> {<expression>} ")"
;;                         ; app-exp
;;                      | "[" <expression> {";" <expression>} "]"
;;                         ; list-exp
;;                      | vacio
;;                         ; emptyL-exp
;;                      | vacio? "(" <expression> ")"
;;                         ; emptyL?-exp
;;                      | crear-lista "(" <expression> {"," <expression>} ")"
;;                         ; create-list-exp
;;                      | lista? "(" <expression> ")"
;;                         ; list?-exp
;;                      | cabeza "(" <expression> ")"
;;                         ; head-exp
;;                      | cola "(" <expression> ")"
;;                         ; cola-exp
;;                      | append "(" <expression> "," <expression> ")"
;;                         ; append-exp
;;                      | ref-list "(" <expression> "," <expression> ")"
;;                         ; ref-list-exp
;;                      | set-list "(" <expression> "," <expression> "," <expression> ")"
;;                         ; set-list-exp
;;                      | tupla "[" <expression> {";" <expression>} "]"
;;                         ; tuple-exp
;;                      | crear-tupla "(" <expression> {"," <expression>} ")"
;;                         ; create-tuple-exp
;;                      | tupla? "(" <expression> ")"
;;                         ; tuple?-exp
;;                      | ref-tupla "(" <expression> "," <expression> ")"
;;                         ; ref-tuple-exp
;;                      | cabeza-tupla "(" <expression> ")"
;;                         ; head-tuple-exp
;;                      | cola-tupla "(" <expression> ")"
;;                         ; cola-tuple-exp
;;                      | "{" <identifier> = <expression> {"," <identifier> = <expression>} "}"
;;                         ; record-exp
;;                      | crear-registro "(" <identifier> = <expression> {"," <identifier> = <expression>} ")"
;;                         ; create-record-exp
;;                      | registro? "(" <expression> ")"
;;                         ; record?-exp
;;                      | ref-registro "(" <expression> "," <expression> ")"
;;                         ; ref-record-exp
;;                      | set-registro "(" <expression> "," <expression> "," <expression> ")"
;;                         ; set-record-exp
;;                      | print "(" <expression> ")"
;;                         ; print-exp
;;                      | ">" "(" <expression> <expression> ")"
;;                         ; greater-than-exp
;;                      | ">=" "(" <expression> <expression> ")"
;;                         ; greater-than-equal-exp
;;                      | "<" "(" <expression> <expression> ")"
;;                         ; less-than-exp
;;                      | "<=" "(" <expression> <expression> ")"
;;                         ; less-than-equal-exp
;;                      | "==" "(" <expression> <expression> ")"
;;                         ; equal-exp
;;                      | "!=" "(" <expression> <expression> ")"
;;                         ; not-equal-exp
;;                      | circuit <gate-list>
;;                         ; circuit-exp
;;                      | and-type
;;                         ; and-type-exp
;;                      | or-type
;;                         ; or-type-exp
;;                      | not-type
;;                         ; not-type-exp
;;                      | xor-type
;;                         ; xor-type-exp
;;                      | "'" <identifier>
;;                         ; quote-exp
;;
;; <primitive>         ::= + | - | * | / | % | add1 | sub1 | longitud | concatenar
;;                      | eval-circuit | connect-circuits | merge-circuits
;;
;; <gate-list>         ::= empty
;;                         ; empty-gate-list
;;                      | <gate> <gate-list>
;;                         ; cons-gate-list
;;
;; <gate>              ::= gate <identifier> <type> <input-list>
;;                         ; gate-def
;;
;; <type>              ::= and | or | not | xor
;;
;; <input-list>        ::= empty
;;                         ; empty-input-list
;;                      | <boolean> <input-list>
;;                         ; cons-bool-input-list
;;                      | <identifier> <input-list>
;;                         ; cons-id-input-list
;;
;; <boolean>           ::= True | False
;;                         ; true-boolean | false-boolean

(define scanner-spec-mini-py
  '((white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (identifier(letter (arbno (or letter digit "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (number (digit (arbno digit) "." digit (arbno digit)) number)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (string("\"" (arbno (not #\")) "\"") string)))


(define grammar-mini-py
  '((program ((arbno class-decl) expression) a-program)

    ;;identificador
    (expression (identifier) id-exp)

    ;;Datos
    (expression (number) number-exp)
    (expression (string) string-exp)
    (expression (boolean) bool-exp)
    (expression ("x8" "(" (arbno number)")") oct-exp)
    (expression ("x16" "("(arbno number) ")" ) hex-exp)
    (expression ("x32" "(" (arbno number)")" ) bignum-exp)



    ;;Variables
    (expression (primitive "(" (separated-list expression ",")")")
                primapp-exp)
    (expression ("var" (separated-list identifier "=" expression ",") "in" expression) var-exp)
    (expression ("const" (separated-list identifier "=" expression ",") "in" expression) const-exp)
    (expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression) letrec-exp)
    (expression ("set" identifier "=" expression) set-exp)



    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
    (expression ("for" identifier "in" expression "do" expression "done") for-exp)
    (expression ("while" expression "do" expression "done") while-exp)

    (expression ("if" expression "then" expression "else" expression)
                if-exp)

    (expression ("proc" "(" (arbno identifier) ")" expression)
                proc-exp)





    ;;listas
    (expression ("[" (separated-list expression ";") "]") list-exp)
    (expression ("vacio?" "(" expression ")") emptyL?-exp)
    (expression ("vacio") emptyL-exp)
    (expression ("crear-lista" "(" expression (arbno "," expression) ")" ) create-list-exp)
    (expression ("lista?" "(" expression ")") list?-exp)
    (expression ("cabeza" "(" expression ")") head-exp)
    (expression ("cola" "(" expression ")") cola-exp)
    (expression ("append" "(" expression "," expression ")") append-exp)
    (expression ("ref-list" "(" expression "," expression ")") ref-list-exp)
    (expression ("set-list" "(" expression "," expression "," expression ")") set-list-exp)

    ;;tuplas
    (expression ("tupla" "[" (separated-list expression ";") "]") tuple-exp)
    (expression ("crear-tupla" "(" expression (arbno "," expression) ")" ) create-tuple-exp)
    (expression ("tupla?" "(" expression ")") tuple?-exp)
    (expression ("ref-tupla" "(" expression "," expression ")" ) ref-tuple-exp)
    (expression ("cabeza-tupla""("expression")") head-tuple-exp)
    (expression ("cola-tupla""(" expression")")cola-tuple-exp)

    ;;registros
    (expression ("{" identifier "=" expression (arbno "," identifier "=" expression)"}" ) record-exp)
    (expression ("registro?" "(" expression ")") record?-exp)
    (expression ("crear-registro" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" ) create-record-exp)
    (expression ("ref-registro" "(" expression "," expression")") ref-record-exp)
    (expression ("set-registro" "(" expression "," expression "," expression")") set-record-exp)



    ;; Primitivas
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") connect-circuits-prim)
    (primitive ("merge-circuits") merge-circuits-prim)


    ;; Circuitos
    (expression ("circuit" gate-list) circuit-exp)
    (circuit (gate-list) circuit-def)
    (gate-list ("empty") empty-gate-list)
    (gate-list (gate gate-list) cons-gate-list)
    (gate ("gate" identifier type input-list) gate-def)
    (type ("and") and-type)
    (type ("or") or-type)
    (type ("not") not-type)
    (type ("xor") xor-type)
    (input-list ("empty") empty-input-list)
    (input-list (boolean input-list) cons-bool-input-list)
    (input-list (identifier input-list) cons-id-input-list)
    (boolean ("True") true-boolean)
    (boolean ("False") false-boolean)


    ;^;;;;;;;;;;;;;;; new productions for oop ;;;;;;;;;;;;;;;;

    (class-decl
     ("class" identifier
              "extends" identifier
              (arbno "field" identifier)
              (arbno method-decl)
              )
     a-class-decl)

    (method-decl
     ("method" identifier
               "("  (separated-list identifier ",") ")" ; method ids
               expression
               )
     a-method-decl)
    (expression ("mostrar") mostrar-exp)

    (expression
     ("new" identifier "(" (separated-list expression ",") ")")
     new-object-exp)

    (expression
     ("send" expression identifier
             "("  (separated-list expression ",") ")")
     method-app-exp)

    (expression
     ("super" identifier    "("  (separated-list expression ",") ")")
     super-call-exp)


    ))



;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-mini-py grammar-mini-py)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-mini-py grammar-mini-py)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-mini-py grammar-mini-py))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-mini-py grammar-mini-py))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
                         (lambda (pgm) (eval-program  pgm))
                         (sllgen:make-stream-parser
                          scanner-spec-mini-py
                          grammar-mini-py)))



(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
                 (elaborate-class-decls! c-decls) ;\new1
                 (eval-expression exp (empty-env))))))



(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (mostrar-exp () the-class-env)

      (number-exp (number) number)
      (oct-exp (number) number)
      (hex-exp (number) number)
      (bignum-exp (number) number)
      (string-exp (datum) (substring datum 1 (- (string-length datum) 1)))
      (id-exp (id) (apply-env env id))
      (var-exp (vars rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extended-env-record vars (list->vector args) env))))

      (const-exp (ids rands body)
                 (let ((args (map (lambda (x) (eval-expression x env)) rands)))
                   (eval-expression body
                                    (const-env-record ids args env))))



      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args env)))

      (set-exp (id new-exp)
               (let ((val (eval-expression new-exp env))
                     (ref (apply-env-ref env id)))
                 (if (reference? ref)
                     (begin
                       (setref! ref val)
                       val)
                     (eopl:error 'set-exp "No se puede modificar la constante ~a" id))))




      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (proc-exp (ids body)
                (closure ids body env))

      (bool-exp (val)
                (cases boolean val
                  (true-boolean () #t)
                  (false-boolean () #f)))

      (circuit-exp (gate-list) (circuit-def gate-list))


      ;Listas
      (list-exp (list) (eval-list list env))
      (create-list-exp (ca co)
                       (cons (eval-expression ca env) (eval-list co env))
                       )
      (emptyL-exp () '())
      (emptyL?-exp (list) (eqv? (eval-expression list env) '()))
      (list?-exp (list) (list? (eval-expression list env)))
      (head-exp (list) (car (eval-expression list env)) )
      (cola-exp (list) (cdr (eval-expression list env)))
      (append-exp (list1 list2)
                  (append (eval-expression list1 env) (eval-expression list2 env)))
      (ref-list-exp (l p) (list-ref (eval-expression l env) (eval-expression p env)))
      (set-list-exp (l p exp)
                    (let
                        (
                         (le (eval-expression l env))
                         (pe (eval-expression p env))
                         (expe (cons(eval-expression exp env) '()))
                         )
                      (append (append (head-to-position '() le pe 0) expe) (list-tail le (+ pe 1)))
                      ))
      ;;Tuplas
      (tuple-exp (list) (list->vector (map (lambda (arg) (eval-expression arg env)  ) list )))

      (create-tuple-exp (head tail)
                        (list->vector (map (lambda (arg) (eval-expression arg env)  ) (cons head tail)))
                        )
      (tuple?-exp (body) (vector? (eval-expression body env)))
      (ref-tuple-exp (tuple index)
                     (vector-ref (eval-expression tuple env) (eval-expression index env)))
      (head-tuple-exp (tuple)(car (vector->list (eval-expression tuple env))))
      (cola-tuple-exp (tuple) (list->vector (cdr (vector->list (eval-expression tuple env)))))
      ;;Registros
      (record-exp (id exp ids exps)
                  (list (cons id ids) (eval-list (cons exp exps) env))
                  )

      (record?-exp (reg) ((list-of list?) (eval-expression reg env)))
      (create-record-exp (id exp ids exps) (list (cons id ids) (eval-list (cons exp exps) env)))
      (ref-record-exp (lis reg)
                      (cases expression reg
                        (id-exp (x)
                                (let (
                                      (list (eval-expression lis env))
                                      )
                                  (list-ref (car(cdr list)) (pos-record (car list) x 0))
                                  )
                                )
                        (else (eopl:error 'invalid-register "No es un indice de registro valido"))
                        )
                      )
      (set-record-exp (lis reg exp)
                      (cases expression reg
                        (id-exp (x)
                                (let* (
                                       (le (eval-expression lis env))
                                       (expe (cons(eval-expression exp env) '()))
                                       (pe (pos-record (car le) x 0))
                                       (listval (car(cdr le)))
                                       )
                                  (cons (car le) (cons (append (append (head-to-position '() listval pe 0) expe) (list-tail listval (+ pe 1)))'()))
                                  ))
                        (else (eopl:error 'invalid-register "No es un indice de registro valido"))
                        ))

      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))

      (for-exp (var-exp collection-exp body-exp)
               (let* ((collection-val (eval-expression collection-exp env)))
                 (if (list? collection-val)
                     (let loop ((lst collection-val)
                                (env env)
                                (result '()))
                       (if (null? lst)
                           (if (null? result)
                               '()
                               (car (reverse result))) ; devolver el último resultado evaluado del cuerpo
                           (let ((current-val (car lst)))
                             (let ((new-env (extend-env (list var-exp)
                                                        (list current-val)
                                                        env)))
                               (loop (cdr lst)
                                     env
                                     (cons (eval-expression body-exp new-env) result))))))
                     (eopl:error 'for-exp "Se esperaba una lista para iterar, pero se obtuvo: ~a" collection-val))))


      (begin-exp (exp exps)
                 (let loop ((acc (eval-expression exp env))
                            (exps exps))
                   (if (null? exps)
                       acc
                       (loop (eval-expression (car exps) env)
                             (cdr exps)))))

      (while-exp (test body)
                 (let loop ((last-val 1))
                   (if (true-value? (eval-expression test env))
                       (loop (eval-expression body env))
                       last-val)))




      (new-object-exp (class-name rands)
                      (let ((args (eval-rands rands env))
                            (obj (new-object class-name)))
                        (find-method-and-apply
                         'initialize class-name obj args)
                        obj))
      (method-app-exp (obj-exp method-name rands)
                      (let ((args (eval-rands rands env))
                            (obj (eval-expression obj-exp env)))
                        (find-method-and-apply
                         method-name (object->class-name obj) obj args)))
      (super-call-exp (method-name rands)
                      (let ((args (eval-rands rands env))
                            (obj (apply-env env 'self)))
                        (find-method-and-apply
                         method-name (apply-env env '%super) obj args)))

      )))

; funciones auxiliares para aplicar eval-expression a cada elemento de una
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args env)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (eval-circuit-prim () (eval-circuit (car args) env))
      (connect-circuits-prim () (connect-circuits (car args) (cadr args) (caddr args)))
      (merge-circuits-prim () (merge-circuits (car args) (cadr args) (caddr args)))

      )))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))
;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;


(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
                    class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
                    super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
                    field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
                    m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))


;;; we'll just use the list of class-decls.

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
                     "Unknown class ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))
;;;;;;;;;;;;;;;; selectors of all sorts ;;;;;;;;;;;;;;;;

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
              class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
              fields))))

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))

;^;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record     ; para var
   (syms (list-of symbol?))
   (vals vector?)
   (env environment?))
  (const-env-record        ; para const
   (syms (list-of symbol?))
   (vals (list-of expval?))
   (env environment?)))


(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "Variable no encontrada: ~a" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-index1 sym syms)))
                             (if pos
                                 (a-ref pos vals) ; <- Esto es una referencia
                                 (apply-env-ref env sym))))
      (const-env-record (syms vals env)
                        (let ((pos (list-index1 sym syms)))
                          (if pos
                              ;; ERROR porque no tiene referencia
                              (eopl:error 'set-exp "No se puede modificar la constante ~a" sym)
                              (apply-env-ref env sym)))))))



(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "Variable no encontrada: ~a" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-index1 sym syms)))
                             (if pos
                                 (vector-ref vals pos)
                                 (apply-env env sym))))
      (const-env-record (syms vals env)
                        (let ((pos (list-index1 sym syms)))
                          (if pos
                              (list-ref vals pos)
                              (apply-env env sym)))))))


(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
           (lambda (pos ids body)
             (vector-set! vec pos (closure ids body env)))
           (iota len) idss bodies)
          env)))))

(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (x lst)
    (let loop ((lst lst) (pos 0))
      (cond ((null? lst) #f)
            ((equal? x (car lst)) pos)
            (else (loop (cdr lst) (+ pos 1)))))))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))
(define list-index1
  (lambda (x lst)
    (let loop ((lst lst) (i 0))
      (cond ((null? lst) #f)
            ((equal? x (car lst)) i)
            (else (loop (cdr lst) (+ i 1)))))))


(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
          (cons next (loop (+ 1 next)))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))


;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))

;^; waiting for 5-4-2.  Brute force code.
(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))

;; evaluar
(define aux
  (lambda (x)
    x))

(define-datatype part part?
  (a-part
   (class-name symbol?)
   (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'object)
        '()
        (let ((c-decl (lookup-class class-name)))
          (cons
           (make-first-part c-decl)
           (new-object (class-decl->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
     (class-decl->class-name c-decl)
     (make-vector (length (class-decl->field-ids c-decl))))))
;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

;;; methods are represented by their declarations.  They are closed
;;; over their fields at application time, by apply-method.

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'object)
        (eopl:error 'find-method-and-apply
                    "No method for name ~s" m-name)
        (let ((m-decl (lookup-method-decl m-name
                                          (class-name->method-decls host-name))))
          (if (method-decl? m-decl)
              (apply-method m-decl host-name self args)
              (find-method-and-apply m-name
                                     (class-name->super-name host-name)
                                     self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
        parts
        (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-expression body
                       (extend-env
                        (cons '%super (cons 'self ids))
                        (cons super-name (cons self args))
                        (build-field-env
                         (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
        (empty-env)
        (extend-env-refs
         (part->field-ids (car parts))
         (part->fields    (car parts))
         (build-field-env (cdr parts))))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

;; find a method in a list of method-decls, else return #f

(define lookup-method-decl
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))



;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Circuitos
(define eval-circuit
  (lambda (circ env)
    (cases circuit circ
      (circuit-def (gate-list)
                   (eval-gate-list gate-list env)))))


(define eval-gate-list
  (lambda (glist env)
    (cases gate-list glist
      (empty-gate-list () (eopl:error 'eval-gate-list "Circuito vacío no puede evaluarse"))
      (cons-gate-list (gate rest)
                      (let* ((updated-env (eval-gate gate env)))
                        (if (empty-gate-list? rest)
                            (apply-env updated-env (gate-output gate))
                            (eval-gate-list rest updated-env)))))))

(define eval-gate
  (lambda (g env)
    (cases gate g
      (gate-def (id type inputs)
                (let* ((input-values (eval-input-list inputs env))
                       (result (apply-gate type input-values)))
                  (extend-env (list id) (list result) env))))))


(define eval-input-list
  (lambda (input-list-val env)
    (cases input-list input-list-val
      (empty-input-list () '())
      (cons-bool-input-list (b rest)
                            (cons (cases boolean b
                                    (true-boolean () #t)
                                    (false-boolean () #f))
                                  (eval-input-list rest env)))
      (cons-id-input-list (id rest)
                          (cons (apply-env env id)
                                (eval-input-list rest env))))))


(define apply-gate
  (lambda (type-val inputs)
    (cases type type-val
      (and-type ()
                (if (= (length inputs) 2)
                    (and (car inputs) (cadr inputs))
                    (eopl:error 'apply-gate "AND gate requires 2 inputs")))
      (or-type ()
               (if (= (length inputs) 2)
                   (or (car inputs) (cadr inputs))
                   (eopl:error 'apply-gate "OR gate requires 2 inputs")))
      (not-type ()
                (if (= (length inputs) 1)
                    (not (car inputs))
                    (eopl:error 'apply-gate "NOT gate requires 1 input")))
      (xor-type ()
                (if (= (length inputs) 2)
                    (xor (car inputs) (cadr inputs))
                    (eopl:error 'apply-gate "XOR gate requires 2 inputs"))))))


(define xor
  (lambda (a b)
    (or (and a (not b)) (and (not a) b))))

; Funciones para conectar y combinar circuitos
(define connect-circuits
  (lambda (circuit1 circuit2 input-to-replace)
    (cases circuit circuit1
      (circuit-def (gate-list1)
                   (cases circuit circuit2
                     (circuit-def (gate-list2)
                                  (let ((output-gate (last-gate-output gate-list1)))
                                    (circuit-def
                                     (append-gate-lists
                                      gate-list1
                                      (replace-input gate-list2 input-to-replace output-gate))))))))))

; Función auxiliar para reemplazar entradas en una lista de gates

(define replace-input
  (lambda (glist input-to-replace replacement)
    (cases gate-list glist
      (empty-gate-list () (empty-gate-list))
      (cons-gate-list (gate rest)
                      (cons-gate-list
                       (replace-gate-input gate input-to-replace replacement)
                       (replace-input rest input-to-replace replacement))))))


; Función auxiliar para reemplazar entradas en un gate específico
(define replace-gate-input
  (lambda (g input-to-replace replacement)
    (cases gate g
      (gate-def (id type inputs)
                (gate-def id type
                          (replace-in-input-list inputs input-to-replace replacement))))))

; Función auxiliar para reemplazar entradas en una lista de entradas

(define replace-in-input-list
  (lambda (input-list-val input-to-replace replacement)
    (cases input-list input-list-val
      (empty-input-list () (empty-input-list))
      (cons-bool-input-list (b rest)
                            (cons-bool-input-list b
                                                  (replace-in-input-list rest input-to-replace replacement)))
      (cons-id-input-list (id rest)

                          (if (equal? id input-to-replace)
                              (cons-id-input-list replacement
                                                  (replace-in-input-list rest input-to-replace replacement))
                              (cons-id-input-list id
                                                  (replace-in-input-list rest input-to-replace replacement)))))))


;;
(define append-gate-lists
  (lambda (gl1 gl2)
    (cases gate-list gl1
      (empty-gate-list () gl2)
      (cons-gate-list (gate rest)
                      (cons-gate-list gate (append-gate-lists rest gl2))))))

(define last-gate-output
  (lambda (glist)
    (cases gate-list glist
      (empty-gate-list () (eopl:error 'last-gate-output "Empty circuit has no output"))
      (cons-gate-list (gate rest)
                      (if (empty-gate-list? rest)
                          (gate-output gate)
                          (last-gate-output rest))))))


(define merge-circuits
  (lambda (circuit1 circuit2 op-type)
    (cases circuit circuit1
      (circuit-def (glist1)
                   (cases circuit circuit2
                     (circuit-def (glist2)
                                  (let* (
                                         ;; Unimos las listas de compuertas
                                         (combined-gates (append-gate-lists glist1 glist2))
                                         ;; Obtenemos la salida final de cada circuito
                                         (out1 (last-gate-output glist1))
                                         (out2 (last-gate-output glist2))
                                         ;; Creamos la nueva compuerta de combinación
                                         (merge-gate (gate-def 'G-merged op-type
                                                               (cons-id-input-list out1
                                                                                   (cons-id-input-list out2
                                                                                                       (empty-input-list)))))
                                         ;; Agregamos la nueva compuerta al final
                                         (final-gates (append-gate-lists combined-gates
                                                                         (cons-gate-list merge-gate (empty-gate-list)))))
                                    ;; Retornamos el nuevo circuito
                                    (circuit-def final-gates))))))))





;*******************************************************************************************
;Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))


(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))



;****************************************************************************************
;; Funciones auxiliares


;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (const-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))
(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (string? x) (list? x) (vector? x))))
(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (const-target (v) #t)
                    (indirect-target (v) #f)))))))


; Comprobación de tipos para gate-list
(define empty-gate-list?
  (lambda (glist)
    (cases gate-list glist
      (empty-gate-list () #t)
      (cons-gate-list (gate rest) #f))))

; Obtiene el identificador de salida de un gate
(define gate-output
  (lambda (g)
    (cases gate g
      (gate-def (id type inputs) id))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LISTAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define eval-list
  (lambda (list env)
    (if (null? list)
        '()
        (cons(eval-expression (car list) env) (eval-list (cdr list) env))
        )
    ))
(define head-to-position
  (lambda (list2 list position counter)
    (if (eqv? position counter)
        (reverse list2)
        (head-to-position (cons (car list) list2) (cdr list) position (+ counter 1))))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;REGISTROS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pos-record
  (lambda (lis reg counter)
    (if (= counter (+ (length lis) 1))
        (eopl:error 'out-of-register "No existe el registro ~s" reg)
        (if (eqv? reg (car lis))
            counter
            (pos-record (cdr lis) reg (+ counter 1))
            )
        )
    )
  )


(interpretador)