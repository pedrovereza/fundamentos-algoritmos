#lang racket

;; DEFINIÇÕES

(define-struct quarto (nro capacidade tipo estado esq dir))
;; Um Hotel:
;; - ou empty
;; - ou (make-quarto n c t es e d), onde
;;      n  : Número (número do quarto)
;;      c  : Número (capacidade do quarto)
;;      t  : String (tipo do quarto: "standard", "luxo" ou "suíte")
;;      es : Símbolo (estado atual do quarto: 'v para vago, 'o para ocupado)
;;      e  : Hotel (subárvore esquerda)
;;      d  : Hotel (subárvore direita)

(define-struct funcionário (nome cargo salário equipe))
;; Um elemento funcionário de Funcionário é uma estrutura
;; (make-funcionário n f sal eq), onde
;; n : String (nome do funcionário do hotel)
;; c : Símbolo (cargo no hotel)
;; sal: Número (salário em reais)
;; eq : Lista-de-funcionários (lista de membros da equipe do funcionário)

;; Uma Lista-de-funcionários é
;; 1. empty
;; 2. (cons f ldf) onde
;; f : Funcionário
;; ldf : Lista-de-funcionários

;; Uma Lista-String é
;; 1. empty
;; 2. (cons s lstr) onde
;; s: String
;; lstr : Lista-String

;; Uma Lista-Números é
;; 1. empty
;; 2. (cons n lnro) onde
;; n: Número
;; lnro : Lista-Números

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXEMPLOS

;; Quartos
(define Q1 (make-quarto 301 2 "suíte" 'v empty empty))
(define Q2 (make-quarto 214 3 "standard" 'o empty empty))
(define Q3 (make-quarto 222 2 "standard" 'v Q2 Q1))
(define Q4 (make-quarto 105 2 "luxo" 'v empty empty))
(define Q5 (make-quarto 206 3 "luxo" 'o empty empty))
(define Q6 (make-quarto 113 4 "standard" 'o Q4 Q5))
(define Q7 (make-quarto 211 1 "suíte" 'v Q6 Q3))
(define Quartos-Hotel Q7)

;; Funcionários
(define Silvia (make-funcionário "Sílvia" 'empregado 400 empty))
(define Carlos (make-funcionário "Carlos" 'empregado 500 empty))
(define Fernando (make-funcionário "Fernando" 'empregado 500 empty))
(define Eq4 (list Silvia))
(define Jorge (make-funcionário "Jorge" 'chefe-de-área 8500 Eq4))
(define Eq3 (list Fernando Carlos))
(define Julio (make-funcionário "Júlio" 'chefe-de-área 10000 Eq3)) 
(define Gustavo (make-funcionário "Gustavo" 'subgerente 20000 empty))
(define Eq2 (list Julio Jorge))
(define Tatiana (make-funcionário "Tatiana" 'subgerente 22000 Eq2))
(define Eq1 (list Gustavo Tatiana))
(define Sérgio (make-funcionário "Sérgio" 'gerente 30000 Eq1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Questão 1)

;;Função: altera-tipo-quarto: Hotel -> Hotel
;;Obj: Dado um hotel, altera todos os quartos do tipo "standard" para "luxo"
;;Exemplo: (todos-do-tipo "luxo" (altera-tipo-quarto Quartos-Hotel)) -> (list 113 105 206 222 214)
(define (altera-tipo-quarto hotel)
	(cond 
	  [(empty? hotel) empty]
	  [(string=? "standard" (quarto-tipo hotel)) (make-quarto
												   (quarto-nro hotel)
												   (quarto-capacidade hotel)
												   "luxo"
												   (quarto-estado hotel)
												   (altera-tipo-quarto (quarto-esq hotel))
												   (altera-tipo-quarto (quarto-dir hotel))
												   )]
	  [else (make-quarto
			(quarto-nro hotel)
			(quarto-capacidade hotel)
			(quarto-tipo hotel)
			(quarto-estado hotel)
			(altera-tipo-quarto (quarto-esq hotel))
			(altera-tipo-quarto (quarto-dir hotel)))]
	  )
  )

;;;; TESTE (função abaixo criada para auxiliar os testes);;;;
;;Função: String, Hotel -> Lista-Números
;;Obj: Dado um tipo de um quarto e um hotel, retorna os números de todos os quartos que possuem este tipo
;;Obs: Função criada para facilitar testes da função altera-tipo-quarto
(define (todos-do-tipo umTipo hotel)
	(cond
	  [(empty? hotel) empty]

	  [(string=? umTipo (quarto-tipo hotel)) 
	   (cons (quarto-nro hotel) (append (todos-do-tipo umTipo (quarto-esq hotel))
										(todos-do-tipo umTipo (quarto-dir hotel))))]

	  [else (append (todos-do-tipo umTipo (quarto-esq hotel))
					(todos-do-tipo umTipo (quarto-dir hotel)))]
	  )
)

(equal? (list 113 105 206 222 214) (todos-do-tipo "luxo" (altera-tipo-quarto Quartos-Hotel)))
(equal? (list 211 301) (todos-do-tipo "suíte" (altera-tipo-quarto Quartos-Hotel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Questão 2)

;;Função lista-cargo: Simbolo, Funcionário -> Lista-String
;;Obj: Dado um cargo e um funcionário, retorna a lista de nome de todos os funcionários que possuem o cargo, seguindo a hierarquia a partir do funcionário dado.
;;Exmeplo:(lista-cargo 'gerente Sérgio) -> (list "Sérgio")
;;Exmeplo:(lista-cargo 'subgerente Sérgio) -> (list "Gustavo" "Tatiana")
(define (lista-cargo cargo funcionario-n)
	(cond 
	  [(symbol=? cargo (funcionário-cargo funcionario-n)) 
	   (cons (funcionário-nome funcionario-n) (lista-cargos-subordinados cargo (funcionário-equipe funcionario-n)))]

	  [else 
		(lista-cargos-subordinados cargo (funcionário-equipe funcionario-n))]
	  )
)

;;Função: lista-cargos-subordinados: Simbolo, Lista-de-Funcionários -> Lista-String
;;Obj: Dado um cargo em nível da árvore de hierarquia, retorna o nome dos funcionários que possuem o cargo informado, a partir do nível de hierarquia dado.
(define (lista-cargos-subordinados cargo lstEquipe)
  (cond 
	[(empty? lstEquipe) empty]
	[else (append (lista-cargo cargo (first lstEquipe))
				  (lista-cargos-subordinados cargo (rest lstEquipe)))]
	)
)

;;;;;; TESTES ;;;;;;;

(equal? (list "Sérgio") (lista-cargo 'gerente Sérgio))
(equal? (list "Gustavo" "Tatiana") (lista-cargo 'subgerente Sérgio))
(equal? (list "Júlio" "Jorge") (lista-cargo 'chefe-de-área Sérgio))
(equal? (list "Fernando" "Carlos" "Sílvia") (lista-cargo 'empregado Sérgio))

(equal? (list "Tatiana") (lista-cargo 'subgerente Tatiana))
(equal? empty (lista-cargo 'subgerente Carlos))
