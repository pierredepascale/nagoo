;;; parsing.scm -- parser for the Melon language

;;; TRANSLATION ===================================================================

;; form        ::= exp
;; body        ::= exp ; body
;;                 exp ;
;; exp         ::= LET ID = exp ; body
;;                 IF (exp) body ELSE body END
;;                 DEF ID (ID ...) body END
;;                 id = exp
;;                 bool-exp
;; bool-exp    ::= rel-exp AND bool-exp
;;                 rel-exp OR bool-exp
;;                 rel-exp
;; rel-exp     ::= term < term
;;                 term <= term
;;                 term = term
;;                 term == term
;;                 term ~= term
;;                 term ~== term
;;                 term > term
;;                 term >= term
;;                 term
;; term        ::= factor + term
;;                 factor - term
;; factor      ::= path * factor
;;                 path / factor
;;                 path MOD factor
;;                 path DIV factor
;; path        ::= path [ exp , exp ...]
;;                 path ( exp , exp ...)
;;                 path . NAME
;;                 basic
;; basic       ::= IMMEDIATE
;;                 FN (ID , ID ...) body END
;;                 NAME
;;                 ( exp )
;;                 BTICK any BTICK
;;                 ~ basic

(define (opt p1 value)
  (lambda (fs k)
    (or (p1 fs k) (k value fs))))

(define (either . ps)
  (lambda (fs k)
    (let try ((ps ps))
      (if (null? ps)
          #f
          (let ((value ((car ps) fs k)))
            (or value (try (cdr ps))))))))

(define (seq . ps)
  (lambda (fs k)
    (let try ((ps ps)
              (fs fs)
              (vals '()))
      (if (null? ps)
          (k (reverse vals) fs)
          ((car ps) fs (lambda (v rest)
                         (try (cdr ps) rest (cons v vals))))))))

(define (many ps)
  (lambda (fs k)
    (let try ((vals '())
              (fs fs))
      (let ((value (ps fs (lambda (v rest)
                            (try (cons v vals) rest)))))
        (or value
            (k (reverse vals) fs))))))

(define (sep p s)
  (lambda (fs k)
    (let try ((vals '())
              (fs fs))
      (let ((value (p fs (lambda (v rest)
                           (or (s rest (lambda (v* rest*)
                                         (try (cons v vals) rest*)))
                               (k (reverse (cons v vals)) rest))))))
        value))))

(define (nonassoc p op)
  (lambda (fs k)
    (p fs (lambda (v rest)
            (let ((value (op rest (lambda (v* rest*)
                                    (p rest* (lambda (v** rest**)
                                               (k (list v* v v**)
                                                  rest**)))))))
              (or value
                  (k v rest)))))))
(define (left-op p op)
  (lambda (fs k)
    (p fs (lambda (left rest)
            (let try ((left left)
                      (fs rest))
              (or (op fs (lambda (o rest*)
                           (p rest* (lambda (r rest**)
                                      (try (list o left r) rest**)))))
                  (k left fs)))))))

(define-syntax defparser
  (syntax-rules ()
    ((defparser ?name ?rule)
     (define (?name fs k)
       (?rule fs k)))
    ((defparser ?name ?rules ...)
     (define (?name fs k)
       ((either (defandrule ?rules) ...) fs k)))))

(define-syntax defandrule
  (syntax-rules ()
    ((defandrule (?ps ...)) (seq ?ps ...))
    ((defandrule ?ps) ?ps)))

(define ($num fs k)
  (if (and (pair? fs) (number? (car fs)))
      (k (car fs) (cdr fs))
      #f))

(define ($str fs k)
  (if (and (pair? fs) (string? (car fs)))
      (k (car fs) (cdr fs))
      #f))

(define ($name fs k)
  (if (and (pair? fs) (symbol? (car fs)))
      (k (car fs) (cdr fs))
      #f))

(define (name sym)
  (lambda (fs k)
    (if (and (pair? fs) (eq? (car fs) sym))
        (k sym (cdr fs))
        #f)))

(define ($semi fs k)
  (if (and (pair? fs) (equal? (car fs) (list 'semi)))
      (k (car fs) (cdr fs))
      #f))

(define ($opar fs k)
  (if (and (pair? fs) (equal? (car fs) (list 'opar)))
      (k (car fs) (cdr fs))
      #f))

(define ($cpar fs k)
  (if (and (pair? fs) (equal? (car fs) (list 'cpar)))
      (k (car fs) (cdr fs))
      #f))

(define ($comma fs k)
  (if (and (pair? fs) (equal? (car fs) (list 'comma)))
      (k (car fs) (cdr fs))
      #f))

(define ($comma fs k)
  (if (and (pair? fs) (equal? (car fs) (list 'newline)))
      (k (car fs) (cdr fs))
      #f))

(define $end (name 'end))
(define $if (name 'if))
(define $def (name 'def))

(define $separator (either $semi $newline))

(defparser $simple
  $num
  $str
  $name
  (seq $opar $exp $cpar))

(define $mulop (either (name '*) (name '%)))
(define $factor (left-op $simple $mulop))

(define $addop (either (name '+) (name '-)))
(define $term (left-op $factor $addop))

(define $rel-op (nonassoc $term (either (name '<) (name '<=) (name '==)
                                        (name '>) (name '>=) (name '!=))))
  
(define $bool-exp (left-op $rel-op (either (name 'and) (name 'or))))

(define $formals (seq $opar (sep $name $comma) $cpar))

(defparser $exp
  ((name 'let) $name (name '=) $exp)
  ((name 'if) $exp $semi $exp $end)
  ((name 'def) $name $formals $semi $body $end)
  ($name (name '=) $exp)
  $bool-exp)

(define $body
  (sep $exp $separator))

;;;

(define (test-parsing)
  (let ((fs '((name (keyword "hello")) (name (keyword "world")))))
    ((seq (pkey "hello")
           pname) fs list)))

(define (test1) ($body '(1 (semi) 2 (semi) 3) list))

(define (parse-file filename)
  (let ((tokens (lex-file filename)))
    ($body tokens list)))
