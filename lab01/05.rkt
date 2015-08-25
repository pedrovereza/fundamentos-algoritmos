#lang racket
(require "04.rkt")

;; Função verifica-temperatura-caldeira: Número -> String
;; Objetivo: Emitir uma mensagem de acordo com a temperatura especificada em Fahrenheit.
;; Exemplos:
;; (verifica-temperatura-caldeira 400) -> "Temperatura ok"
;; (verifica-temperatura-caldeira 0) -> "Temperatura Baixa"
;; (verifica-temperatura-caldeira 600) -> "Temperatura Alta"
(define (verifica-temperatura-caldeira temperaturaFahrenheit)
  (cond
	[(< (fahrenheit-celsius temperaturaFahrenheit) 150) "Temperatura Baixa"] 
	[(> (fahrenheit-celsius temperaturaFahrenheit) 250) "Temperatura Alta"]
	[else "Temperatura ok"]
	)
  )

(verifica-temperatura-caldeira 400)
(verifica-temperatura-caldeira 0)
(verifica-temperatura-caldeira 600)


