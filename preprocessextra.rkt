#lang racket
(require "master.rkt")
(require net/url)

(define srcLang "pt-PT")
(define destLang "en")

;Translates comments using Google translate API
(define (translateComment str) (begin
        (display str)

        (define endLine (car (car (regexp-match-positions #px"(?m:$)" str))))

        ;fetch targeted string
        (define sendStr (substring str 0 endLine))
        ;fetch the rest of the string
        (define rightStr (substring str endLine))

        ;Compose request with method = GET
        (define baseUrl "https://translate.googleapis.com/translate_a/single?client=gtx&sl=")
        (define url1 (string-append baseUrl srcLang))
        (define url2 (string-append url1 "&tl="))
        (define url3 (string-append url2 destLang))
        (define url4 (string-append url3 "&dt=t&q="))
        (define url5 (string-append url4 sendStr))

        (define input (get-pure-port (string->url url5)))
        
        ;Get response
        (define response (port->string input))

        (close-input-port input)

        ;Filter response
        (define translatedStrPos (car (car (regexp-match-positions #px",\"" response))))
        (define translatedStr (substring response 4 (- translatedStrPos 1)))

        (string-append translatedStr rightStr)
    )
)


(define (macroDefinition str) (begin

    ;Get the end of macro position newline
    (define endofMacro (car (regexp-match-positions #px"(?m:$)" str)))
    (define macroStr (substring str 0 (car endofMacro)))
    (define restOfStr (substring str (cdr endofMacro)))

    ;Get macro name - Filtering until a '(' is found
    (define nameEndPos (car (regexp-match-positions #px"^[^(]*" macroStr) ) )
    (define macroName (string-trim (substring macroStr 0 (cdr nameEndPos))))

    ;String starting with first arg
    (define macroWithoutNameStr (substring macroStr (+(cdr nameEndPos) 1)))

    ;Macro Arguments
    (define argsEndPos (car (regexp-match-positions #px"^[^)]*\\)" macroWithoutNameStr)))
    (define macroArgsStr (substring macroWithoutNameStr 0 (- (cdr argsEndPos) 1)))
    (define argsList (string-split macroArgsStr ","))

    ;Macro Body
    (define macroBody (substring macroWithoutNameStr (cdr argsEndPos) ))

    (define findCond1 (string-append "\\b" macroName) )
    (define findCond2 (string-append findCond1 "\\([^)]*\\)"))
    (define findCond (pregexp findCond2))

    (for ([i (in-range (length (regexp-match-positions* findCond restOfStr)))] #:break (not (regexp-match-positions findCond restOfStr))) (begin    
        
        (define atp1 (car (regexp-match-positions findCond restOfStr)))
        (define rest (substring restOfStr (cdr atp1)))
        (define left (substring restOfStr 0 (car atp1)))

        (define calling (substring restOfStr (car atp1) (cdr atp1)))

        ;Fetch the called arguments
        (define argsPos (car (regexp-match-positions #px"\\([^)]*\\)" calling)))
        (define argsStr (string-trim (substring calling (+ (car argsPos) 1 ) (- (cdr argsPos) 1 ))))

        (define argsCalled (string-split argsStr ","))
        (define newBody macroBody)

        (display argsCalled)
        
        ;Check if number of called arguments match
        (if (equal? (length argsCalled) (length argsList))
            (begin
                ;Replace argument prototypes with actual arguments
                (map (lambda (reqArg calledArg)(begin
                    (define regExStr (string-append "\\b" reqArg))
                    (set! regExStr (string-append regExStr "\\b"))

                    ;Iterate over all occurences
                    (for ([j (in-range (length (regexp-match-positions* (pregexp regExStr) newBody)))] #:break (not (regexp-match-positions (pregexp regExStr) newBody))) (begin   
                        (define occurPos (car (regexp-match-positions (pregexp regExStr) newBody)))
                        (define leftTemp (substring newBody 0 (car occurPos)))
                        (define rightTemp (substring newBody (cdr occurPos)))
                        (set! newBody (string-append leftTemp calledArg))
                        (set! newBody (string-append newBody rightTemp))
                    ))
                
                )) argsList argsCalled)

                (set! restOfStr (string-append newBody rest) )
                (set! restOfStr (string-append left restOfStr))
            )
            (error "Arguments dont match")
        )
    ))
    (string-append restOfStr )
))

(add-active-token "//{trans}" translateComment)
(add-active-token #px"(?m:)\\s*#define\\s*" macroDefinition)