#lang racket/gui

(define (escrever-arquivo caminho a b c delta texto)
  (call-with-output-file
   caminho
   (lambda (output-port)
     (displayln (string-append "Coeficiente a: " (number->string a)) output-port)
     (displayln (string-append "Coeficiente b: " (number->string b)) output-port)
     (displayln (string-append "Coeficiente c: " (number->string c)) output-port)
     (displayln (string-append "Delta: " (number->string delta)) output-port)
     (displayln texto output-port)
   )
  )
)

(define (gerar-nome-arquivo prefixo)
  (let* ([timestamp (current-seconds)]
         [nome-arquivo (string-append prefixo "-" (number->string timestamp) ".txt")])
    nome-arquivo
  )
)

(define (criar-pasta caminho)
  (unless (directory-exists? caminho)
    (make-directory caminho)
  )
)

(define frame
  (new frame%
       [label "Calculadora de Raízes de uma Parábola - Thiago e Vinícius"]
       [width 500]
       [height 300]
  )
)

(define panel (new vertical-panel% [parent frame]))

(define label-a (new message% [parent panel] [label "Coeficiente a:"]))

(define a-field (new text-field% [parent panel] [label "a"] [init-value ""]))

(define label-b (new message% [parent panel] [label "Coeficiente b:"]))

(define b-field (new text-field% [parent panel] [label "b"] [init-value ""]))

(define label-c (new message% [parent panel] [label "Coeficiente c:"]))

(define c-field (new text-field% [parent panel] [label "c"] [init-value ""]))

(define button
  (new button%
       [parent panel]
       [label "Calcular"]
       [callback
        (lambda (button event)
          (let* ([a (string->number (send a-field get-value))]
                 [b (string->number (send b-field get-value))]
                 [c (string->number (send c-field get-value))]
                 [delta (- (* b b) (* 4 a c))]
                 [h (/ (- b) (* 2 a))]
                 [k (+ (* a (sqr h)) (* b h) c)]
                 [nome-pasta "respostas"]
                 [nome-arquivo (gerar-nome-arquivo "resposta")]
                 [caminho-arquivo (string-append nome-pasta "/" nome-arquivo)])
            (criar-pasta nome-pasta)
            (if (>= delta 0)
                (let* ([x1 (/ (- (- b) (sqrt delta)) (* 2 a))]
                       [x2 (/ (+ (- b) (sqrt delta)) (* 2 a))]
                       [vertex-string (format "Vx = ~a\nVy = ~a" h k)]
                       [result-string (string-append (format "x1 = ~a\nx2 = ~a\n" x1 x2)
                                                     vertex-string)])
                  (escrever-arquivo caminho-arquivo a b c delta result-string)
                  (message-box "Raízes"
                               (string-append (format "x1 = ~a\nx2 = ~a\n" x1 x2)
                                              vertex-string
                                              "\nDelta: "
                                              (number->string delta))
                               frame))
                (let* ([result-string
                        (format "Esta parábola não possui raízes reais\nVx = ~a\nVy = ~a" h k)])
                  (escrever-arquivo caminho-arquivo a b c delta result-string)
                  (message-box "Raízes Complexas"
                               (string-append result-string "\nDelta: " (number->string delta))
                               frame
                  )
                )
            )
          )
        )
       ]
  )
)

(define (main)
  (send frame show #t))

(main)
