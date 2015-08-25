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
;
;
;; Uma Lista-de-funcionários é
;; 1. empty
;; 2. (cons f ldf) onde
;; f : Funcionário
;; ldf : Lista-de-funcionários

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

(define-struct tipo-capacidade (tipo capacidade))
;; Um elemento tipo-capacidade de Tipo-Capacidade é uma estrutura
;; (make-tipo-capacidade tipo capacidade), onde
;;      capacidade  : Número (capacidade do quarto)
;;      tipo  : String (tipo do quarto: "standard", "luxo" ou "suíte")


;;Função busca-quarto: Numero, Hotel -> Tipo-Capacidade
;;Obj: Dado um numero e um hotel, retorna o tipo e capacidade do quarto referente ao Numero
;;Exemplo: (= 1 (tipo-capacidade-capacidade (busca-quarto 211 Quartos-Hotel))) -> true
(define (busca-quarto nro hotel)
  (cond
	[(empty? hotel) empty]
	[(= nro (quarto-nro hotel)) (make-tipo-capacidade (quarto-tipo hotel) (quarto-capacidade hotel))]
	[(< nro (quarto-nro hotel)) (busca-quarto nro (quarto-esq hotel))]
	[else  (busca-quarto nro (quarto-dir hotel))]
	)
  )

(= 1 (tipo-capacidade-capacidade (busca-quarto 211 Quartos-Hotel)))
(= 4 (tipo-capacidade-capacidade (busca-quarto 113 Quartos-Hotel)))
(= 2 (tipo-capacidade-capacidade (busca-quarto 105 Quartos-Hotel)))
(= 3 (tipo-capacidade-capacidade (busca-quarto 206  Quartos-Hotel)))
(= 2 (tipo-capacidade-capacidade (busca-quarto 222  Quartos-Hotel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Questão 2)

;;Função: Simbolo, Hotel -> Hotel
;;Obj: Dado um estado de um quarto e um hotel, retorna todos os quartos que possuem este estado
;;Exemplo: (equal? (list 113 206 214) (todos-no-estado 'o Quartos-Hotel)) -> true
(define (todos-no-estado umEstado hotel)
	(cond
	  [(empty? hotel) empty]
	  [(symbol=? umEstado (quarto-estado hotel)) (cons (quarto-nro hotel) (append (todos-no-estado umEstado (quarto-esq hotel)) (todos-no-estado umEstado (quarto-dir hotel))))]
	  [else (append (todos-no-estado umEstado (quarto-esq hotel)) (todos-no-estado umEstado (quarto-dir hotel)))]
	  )
)

(equal? (list 113 206 214) (todos-no-estado 'o Quartos-Hotel))
(equal? (list 211 105 222 301) (todos-no-estado 'v Quartos-Hotel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Questão 3)

;;Função conta-cargos Simbolo, Funcionário -> Número
;;Objetivo: Dado um funcionário na arvore de hierarquia, retorna quantos funcionários a partir deste possuem o cargo indicado
;;Exemplo:(= 3 (conta-cargos 'empregado Sérgio)) -> true
;;(= 2 (conta-cargos 'chefe-de-área Sérgio)) -> true
;;(= 2 (conta-cargos 'subgerente Sérgio))-> true
(define (conta-cargos cargo funcionario-n)
	(cond 
	  [(symbol=? cargo (funcionário-cargo funcionario-n)) (+ 1 (conta-cargos-subordinados cargo (funcionário-equipe funcionario-n)))]
	  [else (conta-cargos-subordinados cargo (funcionário-equipe funcionario-n))]
	  )
)

;;Função conta-cargos-subordinados Simbolo, Lista-de-Funcionarios -> Número
;;Objetivo: Dado uma lista-de-funcionarios retorna quantos funcionários a partir deste nivel da árvore de hierarquia  possuem o cargo indicado
(define (conta-cargos-subordinados cargo lstEquipe)
  (cond 
	[(empty? lstEquipe) 0]
	[else (+ (conta-cargos cargo (first lstEquipe)) (conta-cargos-subordinados cargo (rest lstEquipe)))]
	)
)

(= 3 (conta-cargos 'empregado Sérgio))
(= 2 (conta-cargos 'chefe-de-área Sérgio))
(= 2 (conta-cargos 'subgerente Sérgio))
(= 1 (conta-cargos 'gerente Sérgio))
(= 0 (conta-cargos 'empregado Gustavo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
