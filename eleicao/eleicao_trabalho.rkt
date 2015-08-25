#lang racket

(define-struct candidato (nome partido numero votos))
;;Um elemento candidato do conjunto Candidatos é uma estrutura
;;	(make-candidato nome partido numero votos), onde
;; nome: String, nome do candidato
;; partido: Símbolo, partido do candidato
;; numero: Número, número eleitoral do candidato
;; votos: Número, total de votos recebidos pelo candidato


;;Uma urna é 
;; - empty, ou
;; - (cons voto urna), onde
;; voto: Número, número do candidato
;; urna: Urna
(define urna  
  (cons 1
		(cons 2
			  (cons 2
					(cons 3
						  (cons 3
								(cons 3 empty))))))
) 

(define jose (make-candidato "Jose" 'PT 1 0))
(define joao (make-candidato "Joao" 'PSDB 2 0))
(define julio (make-candidato "Julio" 'PT 3 0))

;;Candidatos é
;; - empty, ou
;; - (cons candidato lista), onde
;; candidato: Candidato, um candidato
;; lista: Candidatos
(define candidatos 
 (cons jose
	   (cons joao
			 (cons julio empty)))
  
)

;;Função contabiliza-votos: Urna, Número -> Número
;;Objetivo: Dado uma Urna e o numero de um candidato, retorna o total de votos recebidos pelo candidato
;;Exemplos:
;;(= 1 (contabiliza-votos urna 1))
;;(= 2 (contabiliza-votos urna 2))
;;(= 3 (contabiliza-votos urna 3))
;;(= 0 (contabiliza-votos urna 5))
(define (contabiliza-votos urna numeroCandidato)
  (cond
	[(empty? urna) 0]
	[(= numeroCandidato (first urna)) (+ 1 (contabiliza-votos (rest urna) numeroCandidato))]
	[else (contabiliza-votos (rest urna) numeroCandidato)]
	)
)

;;Função une-urnas: Urna, Urna -> Urna
;;Objetivo: Dado duas urnas, retorna uma nova urna contendo os votos das duas primeiras
;;Exemplo:
;;(une-urnas urna  (cons 4 (cons 5 (cons 6 empty)))) -> (1 2 2 3 3 3 4 5 6)
(define (une-urnas urna1 urna2)
	(cond 
	  [(empty? urna1) (cond
						[(empty? urna2) empty]
						[else (cons (first urna2) (une-urnas urna1 (rest urna2)))]
						)
	  ]
	  [else (cons (first urna1) (une-urnas (rest urna1) urna2))]
	  )
)

;;Função contabiliza Candidatos, Urna, Urna, Urna, Urna -> Candidatos
;;Objetivo: Dado Candidatos e 4 urnas, retorna Candidatos contendo cada Candidato com o número de votos atualizado
;;Exemplo
;;(= 4 (candidato-votos (first (contabiliza candidatos urna urna urna urna)))) 
;;(= 8 (candidato-votos (first (rest (contabiliza candidatos urna urna urna urna))))) 
;;(= 12 (candidato-votos (first (rest (rest (contabiliza candidatos urna urna urna urna))))))
(define (contabiliza listaCandidatos urna1 urna2 urna3 urna4)
  (cond 
	[(empty? listaCandidatos) empty]
	[else (cons (make-candidato
			 (candidato-nome (first listaCandidatos))
			 (candidato-partido (first listaCandidatos))
			 (candidato-numero (first listaCandidatos))
			 (contabiliza-votos (une-urnas urna4 (une-urnas urna3 (une-urnas urna1 urna2))) (candidato-numero (first listaCandidatos)))
			 ) (contabiliza (rest listaCandidatos) urna1 urna2 urna3 urna4))
	])
)

;;Testes

(= 1 (contabiliza-votos urna 1))
(= 2 (contabiliza-votos urna 2))
(= 3 (contabiliza-votos urna 3))
(= 0 (contabiliza-votos urna 5))

(une-urnas urna  (cons 4 (cons 5 (cons 6 empty)))) 

(= 4 (candidato-votos (first (contabiliza candidatos urna urna urna urna)))) ;;Jose
(= 8 (candidato-votos (first (rest (contabiliza candidatos urna urna urna urna))))) ;;Joao
(= 12 (candidato-votos (first (rest (rest (contabiliza candidatos urna urna urna urna)))))) ;;Julio
