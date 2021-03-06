;;; lexer.scm

(define *lex* #f)
(define *level* 0)
(define (level+) (set! *level* (+ *level* 1)))
(define (level-) (set! *level* (- *level* 1)))

(define (skip-spaces port)
  (let loop ((ch (peek-char port)))
    (cond ((eof-object? ch) ch)
          ((or (char=? ch #\space) (char=? ch #\tab)
               (and (> *level* 0) (char=? ch #\newline))
               (and (> *level* 0) (char=? ch #\return)))
           (read-char port)
           (loop (peek-char port)))
          (else ch))))

(define (read-lex port) 
  (if *lex* 
      (let ((lex *lex*))
	(set! *lex* #f)
	lex)
      (really-read-lex port)))

(define (peek-lex port)
  (or *lex*
      (let ((lex (really-read-lex port)))
	(set! *lex* lex)
	lex)))

(define (really-read-lex port)
  (skip-spaces port)
  (let* ((ch (peek-char port)))
    (cond ((eof-object? ch) ch)
          ((char-numeric? ch) (read-num port))
          ((char-name? ch) (read-name port))
          ((char=? ch #\") (read-str port))
          ((char=? ch #\') (read-ch port))
          ((char=? ch #\:) (read-name port))
          ((char=? ch #\;) (read-char port) '(semicolon))
          ((char=? ch #\`) (read-char port) '(btick))
          ((char=? ch #\.) (read-char port) '(dot))
          ((char=? ch #\,) (read-char port) '(comma))
          ((char=? ch #\() (level+) (read-char port) '(opar))
          ((char=? ch #\)) (level-) (read-char port) '(cpar))
          ((char=? ch #\[) (level+) (read-char port) '(obra))
          ((char=? ch #\]) (level-) (read-char port) '(cbra))
          ((char=? ch #\{) (level+) (read-char port) '(ocurly))
          ((char=? ch #\}) (level-) (read-char port) '(ccurly))
          ((char=? ch #\return) (read-char port) '(newline))
          ((char=? ch #\newline) (read-char port) '(newline))
          ((char=? ch #\~) (read-char port) (list (read-name port)))
          ((char=? ch #\/) (read-comment-maybe port) (really-read-lex port))
          (else (error "unknown token beginning with ~a" ch)))))

(define (read-comment-maybe port)
  (read-char port)
  (let ((ch (read-char port)))
    (cond ((eof-object? ch) '/)
          ((char=? ch #\/) (read-single-line-comment port))
          ((char=? ch #\*) (read-multi-line-comment port))
          (else (error "unknown lex starting with /")))))

(define (read-single-line-comment port)
  (let ((ch (read-char port)))
    (or (eof-object? ch) (char=? ch #\newline)
        (read-single-line-comment port))))

(define (read-multi-line-comment port)
  (let ((ch (read-char port)))
    (cond ((eof-object? ch) ch)
          ((char=? ch #\*) (read-multi-line-comment-2 port))
          (else (read-multi-line-comment port)))))

(define (read-multi-line-comment-2 port)
  (let ((ch (read-char port)))
    (cond ((eof-object? ch) (error "end of line inside multi line comment"))
          ((char=? ch #\/) ch)
          (else (read-multi-line-comment port)))))

(define (operator-char? ch)
  (memq ch (#\- #\% #\& #\: #\+ #\- #\| #\= #\* #\/ #\< #\> #\? #\!)))

(define (read-operator port)
  (string->symbol
   (list->string
    (let lp ((ch (peek-char port)))
      (cond ((eof-object? ch)
(define (read-num port)
  (let loop ((ch (peek-char port))
             (n 0))
    (if (and (char? ch) (char-numeric? ch))
        (let ((ch* (read-char port)))
          (loop (peek-char port) 
                (+ (* 10 n) (- (char->integer ch) (char->integer #\0)))))
        n)))

(define (read-str port)
  (read-char port)
  (list->string 
   (let loop ((ch (peek-char port)))
     (if (eof-object? ch)
         (error "End of file encountered while reading a string literal")
         (if (char=? ch #\")
             (begin (read-char port) '())
             (let ((ch* (read-char port)))
               (cond ((char=? ch #\\)
                      (let ((next (read-char port)))
                        (if (eof-object? next)
                            (error "End of file while reading string escapes")
                            (cond ((char=? next #\n)
                                   (cons #\newline (loop (peek-char port))))
                                  ((char=? next #\\)
                                   (cons #\\ (loop (peek-char port))))
                                  ((char=? next #\")
                                   (cons #\" (loop (peek-char port))))
                                  (else 
                                   (error "Unknown string escape ~a" next))))))
                     (else
                      (cons ch* (loop (peek-char port)))))))))))

(define (read-ch port)
  (let* ((open (read-char port))
         (char (read-char port))
         (close (read-char port)))
    (if (char=? close #\')
        (make-lex-char char)
        (error "A character syntax needs a closing '" close))))

(define (char-name? ch)
  (or (char-alphabetic? ch) (char=? ch #\_)))

(define (read-name port)
  (string->symbol
   (list->string
    (let lp ((ch (peek-char port))
             (chars '()))
      (cond ((eof-object? ch) (reverse chars))
            ((or (char-name? ch)
                 (member ch '(#\? #\! #\=)))
             (read-char port)
             (lp (peek-char port) (cons ch chars)))
            (else (reverse chars)))))))

;'(#\_ #\~ #\- #\% #\& #\: #\+ #\= #\* #\/ #\< #\> #\? #\!))))
      
(define (lex-file file-name)
  (with-input-from-file file-name
    (lambda ()
      (let lp ((lex (read-lex (current-input-port))))
        (if (eof-object? lex)
            '()
            (cons lex (lp (read-lex (current-input-port)))))))))
