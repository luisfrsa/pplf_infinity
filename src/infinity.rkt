#lang racket

;; Este programa resolve um jogo infinity rotacionando seus blocos
;; até que todos se encaixem entre si. O método de resolução utilizado será
;; por meio de busca com retrocesso (backtracking).
;;
;; ** Conceitos **
;; Bloco
;;  Um bloco é uma peça do jogo Infinity. O bloco é representado como um
;;  caractere quando lido ou escrito de um arquivo, e representado como um
;;  número entre 0 e 15 quando processado dentro do programa.
;; Jogo
;;  Um jogo Infinity é um tabuleiro de M linhas e N colunas (tamanho MxN)
;;  preenchidas por blocos. O jogo infinity é dito resolvido quando todos seus
;;  blocos estão conectados. O jogo é representado por uma lista de listas.

;; exporta as funções que podem ser utilizadas em outros arquivos
(provide tamanho
	rotacionar
	encaixa-h?
	encaixa-v?
	seguro?
	resolver
	main)

;; lista com as representações dos blocos como caracteres
(define blocos-reps (string->list " ╹╺┗╻┃┏┣╸┛━┻┓┫┳╋"))

;;(define blocos-repr (list " " "╹" "╺" "┗" "╻" "┃" "┏" "┣" "╸" "┛" "━" "┻" "┓" "┫" "┳" "╋"))

;; Tamanho representa o tamanho de um jogo em altura e largura
;;     altura : Número - quantidade de linhas do jogo.
;;    largura : Número - quantidade de colunas do jogo.
(struct tamanho (altura largura) #:transparent)

;;iter professor para o run:
;;
;;(define (iter solucao possibilidades)
;;	(cond
;;		[(empty? possibilidades) solucao]
;;		[(empty? candidatos) #f]
;;		[(seguro? candidato solucao tam)
;;		(or (iter (adiciona candidato solucao) (remove candidatos possibilidades))
;;			(iter solucao (exclui candidato possibilidades)))]
;;		[else (iter solucao (exclui candidato possibilidades))]
;;		))
;;
;; Lista -> Lista
;; --------------
;; Retorna lista de possibilidades de rotações
;; Exemplo: (get-lista-rotacoes (list 0 1 1 0))
;;          > (list 0 1 1 0) (list (list 1 1 0 0) (list  1 0 0 1) )
(define (get-lista-rotacoes lista) resolver) 


;; Decimal -> Lista binária
;; --------------
;; Converte um decimal em uma lista de binário
;; Exemplo: (dec-bin 10)
;;          > '(list 1 0 1 0)
(define (dec-bin numero)
	(define (fill lista)
		(if(< (length lista) 4) (fill (cons 0 lista)) lista)      
		)
	(define (core numero lista)
		(cond
			((zero? numero) (fill lista))
			(else
				(core (quotient numero 2) 
					(cons (if (= 0 (remainder numero 2)) 0 1) lista)))))
	(core numero empty)
	)


;; Lista binária -> Decimal
;; --------------
;; Converte uma lista de binário em um decimal
;; Exemplo: (bin-dec (list 1 0 0 1))
;;          > 10
(define (bin-dec lista)
	(define (core lista num multi)
		(cond
			((empty? lista) num)
			(else
				(core (rest lista) 
					(+ num (* (first lista) multi)) (/ multi 2))
				)
			)
		)
    ;+ (8, 4, 2, 1 ) * o binario
    (core lista 0 8)
    )


;; Lista -> Lista
;; --------------
;; Rotaciona um bloco 90 graus em sentido horário
;; Exemplo: (rotacionar (list 0 1 1 0))
;;          > (list 1 1 0 0)
(define (rotacionar lista) 
	(append (rest lista) (list (first lista))))


;; Lista binária -> Lista de listas binárias de roações
;; --------------
;; Converte uma lista binária em uma lista de listas binárias, 
;;sendo essas, suas possíveis rotações
;; Exemplo: (gera-rotacoes (list 1 1 0 0))
;;          > '(list (list 1 1 0 0) (list 1 0 0 1) (list 0 0 1 1) (list 0 1 1 0))

(define (gera-rotacoes lista)
	(define (core lista primeiro return)
		(cond
			[(empty? lista) return]
			[else 
				(if (equal? primeiro lista) 
					  return 
					  (core (rotacionar lista) 
			    	  (if (empty? primeiro) lista primeiro) 
			    	  (cons (rotacionar lista) return)
			    	  )
			     )
			]
		)
	)
	(core lista empty empty)
)
;; Bloco Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco-e se encaixa horizontalmente à esquerda do bloco-d
;; 3  = '(0 0 X 1)
;; 14 = '(X 1 1 0)
;; o terceiro elemento do bloco-e deve ser o mesmo que o first do bloco d
;; Exemplo: (encaixa-h? '(0 0 1 1) '(1 1 1 0))
;;          > #t
(define (encaixa-h? bloco-e bloco-d) 
	(if (or  (empty? bloco-e) (empty? bloco-d)) 
		#f 
		(equal? (first (rest (rest bloco-e))) (first bloco-d))
	)
)


;; Bloco Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco-t se encaixa verticalmente acima do bloco-b
;; 3  = '(0 0 1 X)
;; 14 = '(1 X 1 0)
;; o último do bloco-t deve ser o mesmo que o segundo do bloco b
;; Exemplo: (encaixa-v? '(0 0 1 1) '(1 1 1 0))
;;          > #t
(define (encaixa-v? bloco-t bloco-b)
	(if (or  (empty? bloco-t) (empty? bloco-b)) 
		#f 
		(equal? (last bloco-t) (first (rest bloco-b)))
	)
)

;; Bloco List Tamanho -> Lógico
;; -----------------------------
;; Verifica se um bloco é seguro de ser adicionado a uma solução. Ser 
;; seguro significa que, ao ser adicionado à solução, o bloco se 
;; encaixa a todos os blocos adjacentes à posição em que ele seria 
;; inserido. Uma solução é uma lista de blocos que representa a solução 
;; do jogo até o presente momento. Para facilitar a implementação, 
;; considere que a solução será construída em ordem invertida. Assim, a 
;; solução '(9 7 12 14 6), referente ao um jogo de tamanho 4x3, 
;; representa a seguinte situação:
;; 
;; [6][14][12]    [┏][┳][┓]
;; [7][ 9][  ] => [┣][┛][ ]
;; [ ][  ][  ]    [ ][ ][ ]
;; [ ][  ][  ]    [ ][ ][ ]
;; 
;; A chamada (seguro? 5 '(9 7 12 14 6) (tamanho 4 3)) deve verificar se 
;; o bloco 5 [┃] é seguro de ser adicionado à solução, isto é, inserido 
;; na posição lin=2, col=3 da situação descrita acima. Observe que para 
;; este exemplo o bloco 5 é seguro, pois ele se encaixa a todos os 
;; blocos adjacentes: ao bloco 9 à esquerda, ao bloco 12 acima e à 
;; borda direita (branco) do tabuleiro. Veja que não houve necessidade 
;; de se verificar o encaixe com o bloco abaixo, já que o mesmo ainda 
;; não existe na solução.
(define (seguro? bloco solucao tam) #f)


(define (caracter-numero char)
	(define (core lista char count)
		(cond
			((empty? lista) #f)
			(else (if (equal? (first lista) char) 
				   count
				   (core (rest lista) char (add1 count))
				   )
			)
				
		)
	)
	(core blocos-reps char 0)
)
(define (listachar-listanum lista)
	(define (core listachar listanum)
		(cond
			((empty? listachar) listanum)
			(else (core (rest listachar) (cons listanum (caracter-numero (first listachar)))))
		)

	)

	(core lista empty)
)
(define (numero-caracter num)
		(cond
			((empty? num) #f)
			(else (list-ref blocos-reps num))
)
		)

;; Lista de caracteres -> lista de números
;; Converte a lista de caracteres para lista de numeros
;; Exemplo: (converter-arquivo-numero  (list "┗┃┳┓" "┫┻┣┃" "┃┫┣┣" "┏┫┃┗") )
;;          > (list (0 6 6 1) (12 15 15 6) (1 10 10 0) (0 2 1 0))
(define (converter-arquivo-numero lista-arquivo)
	(define (core lista-arquivo lista-retorno)
		(cond
			((empty? lista-arquivo) lista-retorno)
			(else (core (rest lista-arquivo) (cons lista-retorno (listachar-listanum  (string->list(first lista-arquivo))))))
			)
		)
	(core lista-arquivo empty)
	)

;;string->list 

;; String(direotrio) -> ????
;; Faz leitura do diretorio do arquivo passado por parametro
;; Exemplo: (ler-jogo "testes/5.txt")
;;          > ????
(define (abrir-arquivo diretorio)
	(cond 
		((empty? diretorio) #f)
		(else  (converter-arquivo-numero (port->lines (open-input-file diretorio)))
		)
	))

(abrir-arquivo "../testes/casos/05.txt")



;; String -> Jogo
;; Faz a leitura e processa um jogo armazenado em arquivo.
;; Exemplo: (ler-jogo "testes/5.txt")
;;          > '((0 6 6 1) (12 15 15 6) (1 10 10 0) (0 2 1 0))
(define (ler-jogo arquivo) '())
;; Dica: procure pelas funções pré-definidas open-input-file e port->lines


;; Jogo -> void
;; Escreve o jogo na tela codificado em caracteres.
;; Exemplo: (escrever-jogo '((6 10 14 12) (7 14 13 5) (5 7 11 13) (3 11 10 9)))
;;          > ┏━┳┓
;;            ┣┳┫┃
;;            ┃┣┻┫
;;            ┗┻━┛
(define (escrever-jogo jogo) void)
;; Dica: procure pelas funções pré-definidas list->string e string-join


;; Jogo -> Jogo || #f
;; Resolve o jogo Infinity e o retorna resolvido. Caso não seja possível
;; resolvê-lo, retorna o valor falso. Por exemplo, se passado o seguinte jogo:
;;
;; '(( 0  6  6 1)         [ ][┏][┏][╹]
;;   (12 15 15 6)    =>   [┓][╋][╋][┏]
;;   ( 1 10 10 0)         [╹][━][━][ ]
;;   ( 0  2  1 0))        [ ][╺][╹][ ]
;;
;; a função deve retornar:
;;
;; '((0  6 12 4)          [ ][┏][┓][╻]
;;   (6 15 15 9)     =>   [┏][╋][╋][┛]
;;   (1  5  5 0)          [╹][┃][┃][ ]
;;   (0  1  1 0))         [ ][╹][╹][ ]
(define (resolver jogo) #f)

;; List String -> void
;; Esta é a função principal. Esta função é chamada a partir do arquivo
;; infinity-main.rkt
;;
;; args é a lista de parâmetros para o programa.
;;
;; O primeiro e único parâmetro deve ser o nome (caminho) do arquivo que contém 
;; o jogo a ser resolvido. O jogo é representado na forma de caracteres, o qual
;; deverá ser primeiramente convertido para a representação numérica antes de
;; ser resolvido. Veja exemplos de arquivos no diretório de testes.
;;
;; A saída desta função é a escrita na tela do jogo resolvido, representado na
;; forma de caracteres. Caso o jogo não possua solução, nada deve ser escrito na
;; tela.

;;eof-object?

(define (main args)
	(display args))
