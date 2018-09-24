#lang racket
(provide add-active-token def-active-token process-string)

(require racket/port)

(define triggers (make-hash))

;String processor
(define (process-string str)
    (begin

        (define str_copy (string-copy str))
        (define match_pairs null)
        (define same_str null)
        (define mod_str null)
        (define atp null)
        
        ;Iterate over all active tokens
        (if (hash-empty? triggers)
            (display "empty hash")
            (begin
                ;For each tokens check the string for triggering expressions
                (hash-for-each triggers (lambda (key value) (begin
                    
                    (for ([i (in-range (length (regexp-match-positions* key str_copy)))] #:break (not (regexp-match-positions key str_copy))) (begin    
                        (set! atp (car (regexp-match-positions key str_copy)))
                        (set! same_str (substring str_copy 0 (car atp)))
                        (set! mod_str (substring str_copy (cdr atp)))

                        (set! mod_str (value mod_str));string passada sob a forma de input-port
                        (set! str_copy (string-append same_str mod_str)) ;str_copy is final pre processed string
                    ))

                )))
                (display str_copy);DEBUG
            )
        )
    )
)

;Add a active token procedure to the hash
(define (add-active-token str func)
    (if (procedure? func)
        (hash-set! triggers str func)
        (error "Its not a function")
        ;Depois por o nome da função
    )
)
;Syntax rule to define and add a active token to the hash
(define-syntax-rule (def-active-token name (str ...) body)
   (begin
        (define proc (lambda (str ...) body))
        (add-active-token name proc )
    )
)

;Active Tokens Implementation
(define (local-type-inference str) (begin

    (define aux_str null)

    (define valid (regexp-match-positions #px"\\b\\s*=\\s*new\\s+" str))

    (if (not valid)
        (string-append "var" str)
        (begin
            (set! aux_str (substring str (cdr (car valid))))
            (set! valid (regexp-match-positions #px"[:alnum:]*\\(" aux_str))
            (set! aux_str (substring aux_str 0 (- (cdr (car valid)) 1)))
            (string-append aux_str str)
        )
    )
))

;String interpolation procedure
(define (string-interpolation str) (begin

    (define str_copy null)
    (set! str_copy (string-copy str))

    (define pos null)
    (define left_str null)
    (define right_str null)
    (define code_str null)
    (define code_pos null)
    (define code_str_stripped null)

    ;valid is not false for any pattern that has a #, followed by (spaces in the way or not) a String
    ;The latter delimited by " "
    (define valid (regexp-match-positions #px"\\s*\"[^\"]*\"" str_copy))

    (if (not valid)
        (string-append "#" str_copy)
        (begin
            ;iterate for all patterns
            (for ([i (in-range (length (regexp-match-positions* #px"#\\s*\\{[^\\}]*\\}" str_copy)))])
                (begin    

                (set! pos (regexp-match-positions #px"#\\s*\\{[^\\}]*\\}" str_copy))

                ;save the strings after and before the token
                (set! left_str (substring str_copy 0 (car (car pos))))
                (set! right_str (substring str_copy (cdr (car pos))))

                (set! code_str (substring str_copy (car (car pos)) (cdr(car pos))))

                ;Strip Hashtags and brackets
                (set! code_pos (car (regexp-match-positions #px"(?<=\\{)[^\\}]*(?=\\})" code_str)))
                (set! code_str_stripped (substring code_str (car code_pos) (cdr code_pos)))

                ;Construct result string
                (set! left_str (string-append left_str "\" + ("))
                (set! right_str (string-append ") + \"" right_str))
                (set! str_copy (string-append left_str (string-append code_str_stripped right_str)))
                )
            )
            (string-append "" str_copy)
        )
    )
))

;Type aliases procedure
(define (type-aliases str) (begin
                                  
    (define alias_str null)
    (define real_name_str null)
    (define aux_str null)

    (define valid (regexp-match-positions #px"\\s+\\w+\\s*=\\s*[^;]*" str));

    (if (not valid)
        (string-append "alias" str)
        (begin

          (set! aux_str (substring str (car (car valid)) (cdr (car valid))))
          (set! str (substring str ( + (cdr (car valid)) 1)))
          (set! aux_str (string-split aux_str "="))

          (set! alias_str (string-trim (car aux_str)))
          (set! real_name_str (string-trim (cadr aux_str)))

          (set! aux_str "\\b")
          (set! alias_str (string-append (string-append aux_str alias_str) "\\b"))

          (string-replace str (pregexp alias_str) real_name_str)  
        )
    )
))

;Define Active Tokens
(add-active-token #px"\\bvar\\b" local-type-inference)
(add-active-token #px"\\B#\\B" string-interpolation)
(add-active-token #px"\\balias\\b" type-aliases)
