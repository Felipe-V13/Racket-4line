#lang racket

(provide generarMatriz)
(provide jugada)
(provide objetivo)


#|========================FUNCIONES DE TRATAMIENTO DE LISTAS=================================|#


;;Función que revisa si es posible colocar una ficha
(define (posible? lst)
  (cond
    ((null? lst) #f) ;;Falso si no hay posibilidad de jugar
    ((equal? (car lst) 0) #t) ;;True si aún hay campo para jugar
    (else (posible? (cdr lst)))))

;;Función que verifica si un valor existe en una fila
(define (exist? value list)
  (cond ((null? list) ;;Falso si no existe
         #f)
        ((equal? (car list) value) ;;Verdadero si existe
         #t)
        (else
         (exist? value (cdr list)))))

;;Función que genera la matriz del juego llena de ceros 
(define (generarMatriz m n)
  (cond
    ((tamano? m n) (generarMatriz_aux m n))
    (else #f)))

;;Función que cuenta el largo de una lista
(define (largo lst)
  (cond
    ((null? lst) 0)
    (else (+ 1 (largo (cdr lst))))))

;;Función que realiza una función en cada elemento de una lista
(define (aplicar-f func lista)
  (cond
    ((null? lista) null)
    (else (cons (func (car lista)) (aplicar-f func (cdr lista))))))

;;Función que retorna la transpuesta de una matriz
(define (transpuesta matrx)
  (cond
    ((null? (car matrx)) '()) 
    (else (cons (aplicar-f car matrx) (transpuesta (aplicar-f cdr matrx))))))

;;Función que coloca la ficha del jugador en la columna
(define (colocarFicha column player)
  (cond
    ((null? (car column)) '())
    ((null? (cdr column)) (list player)) ;;Coloca la ficha en la posicion mas baja de la columna       
    ((not (equal? (cadr column) 0)) (cons player (cdr column)))
    (else (cons (car column) (colocarFicha (cdr column) player)))))

#|======================FUNCIONES DE VERIFICACIONES GENERALES==============================|#

(define (llena_aux matrx)
  (cond
    ((null? matrx) #t) ;;True si esta llena
    ((posible? (car matrx)) #f) ;;Falso si no esta llena
    (else (llena_aux (cdr matrx)))))

;;Función que verifica si la matriz del juego esta llena
(define (llena? matrx)
  (cond
    ((and (list? matrx) (list? (car matrx))) (llena_aux matrx))))

;;Funcion que valida si el numero de filas y columnas esta en el rango
(define (tamano? rows col)
  (cond
    ((and (>= rows 8) (<= rows 16) (>= col 8) (<= col 16)) #t)
    (else #f)))

;;Función que crea las filas de la matriz con el numero de columnas dado
(define (crearFilas col_number)
  (cond
    ((zero? col_number) null)
    (else (cons 0 (crearFilas (- col_number 1))))))

(define (generarMatriz_aux m n)
  (cond
    ((zero? m) null)
    (else (cons (crearFilas n) (generarMatriz_aux (- m 1) n)))))

(define (buscarEle_aux index lst cont)
  (cond
    ((null? lst) #f) ;;Falso si no encuentra el índice
    ((equal? index cont) (car lst)) ;;La posicion donde esta el elemento cuando llega al índice
    (else (buscarEle_aux index (cdr lst) (+ cont 1)))))

;;Función que obtiene el elemento en ese índice
(define (buscarEle index lst)
  (buscarEle_aux index lst 0))

(define (columnaLlena_aux colNum matrx cont)
  (cond
    ((null? matrx) #f) ;;Falso si la columna no está llena
    ((equal? cont colNum) (not (posible? (car matrx)))) ;;True si está llena
    (else (columnaLlena_aux colNum (cdr matrx) (+ cont 1)))))

;;Función que verifica si una columna está llena
(define (columnaLlena? colNum matrx)
  (columnaLlena_aux colNum (transpuesta matrx) 0))

#|=================FUNCIONES DE VERIFICACION DE MATRIZ================|#

;;Función que verifica si hay 4 en linea del mismo jugadir en una fila 
(define (fin? player line cont)
  (cond 
    ((= cont 4) #t) ;;True si hay 4 seguidos
    ((null? line) #f) ;;Falso si no hay 4 seguidos
    ((equal? (car line) player) (fin? player (cdr line) (+ cont 1)))
    (else (fin? player (cdr line) 0))))


;;Verificar en todas las filas y columnas de la matriz si hay 4 en línea 
(define (verificarFilCol player matrx)
  (cond
    ((null? matrx) #f) ;;Falso si no hay 4 en linea
    ((fin? player (car matrx) 0) #t) ;;Verdadero si no hay 4 en linea
    (else (verificarFilCol player (cdr matrx)))))

;;Función que recorre las diagonales de la matrix de izquierda a derecha
(define (diagonalIDSup_aux matrx i j vec result)
  (cond
    ((>= i (largo matrx)) result)
    ((<= j i)
      (diagonalIDSup_aux matrx i (+ j 1) (append vec (list (buscarEle j (buscarEle (- i j) matrx)))) result))
    ((> j i)
      (diagonalIDSup_aux matrx (+ i 1) 0 '() (append result (list vec))))))

;;Función que recorre las diagonales de la matrix de izquierda a derecha
(define (diagonalIDInf_aux matrx i j vec result)
  (cond
    ((>= i (largo matrx)) result)
    ((< j (- (largo matrx) i 1))
      (diagonalIDInf_aux matrx i (+ j 1)
         (append vec (list (buscarEle (+ j i 1) (buscarEle (- (largo matrx) j 1) matrx)))) result)
    )
    ((>= j (- (largo matrx) i 1))
      (diagonalIDInf_aux matrx (+ i 1) 0 '() (append result (list vec))))))

(define (inversa_aux lista resultList)
  (cond
    ((null? lista) resultList)
    (else (inversa_aux (cdr lista) (cons (car lista) resultList)))))

;;Función que retorna la inversa de una lista
(define (inversa lista)
  (cond
    ((and (not(null? lista)) (list? lista)) (inversa_aux lista '()))))

;;Función que invierte los elementos de las filas o columnas
(define (filaInversa matrx)
  (cond
    ((null? matrx) null)
    (else (cons (inversa (car matrx)) (filaInversa (cdr matrx))))))

;;Función que une las funciones de diagonal superior e inferior
(define (diagonalIzqDer matrx)
  (append
    (diagonalIDSup_aux matrx 0 0 '() '())
    (diagonalIDInf_aux matrx 0 0 '() '())))

;;Función que une las funciones de diagonal superior e inferior de derecha a izquierda
(define (diagonalDerIzq matrx)
  (append
    (diagonalIDSup_aux (filaInversa matrx) 0 0 '() '())
    (diagonalIDInf_aux (filaInversa matrx) 0 0 '() '())))

(define (verificarDiag_aux player diagList)
  (cond
    ((null? diagList) #f)
    ((fin? player (car diagList) 0) #t)
    (else (verificarDiag_aux player (cdr diagList)))))

;;Verifica si hay 4 en línea en las diagonales de la matriz
(define (verificarDiag player matrx)
  (cond
    ((> (largo (car matrx)) (largo matrx))
     (or (verificarDiag_aux player (diagonalIzqDer (transpuesta matrx)))
         (verificarDiag_aux player (diagonalDerIzq (transpuesta matrx)))))
    (else (or (verificarDiag_aux player (diagonalIzqDer matrx))
         (verificarDiag_aux player (diagonalDerIzq matrx))))))

;;Verifica quién ganó o si no nadie ha ganado
(define (resultado? matrx)
  (cond
    ((or (verificarFilCol 1 matrx) (verificarFilCol 1 (transpuesta matrx)) (verificarDiag 1 matrx)) "victoria");;Verifica si el jugador ganó
    ((or (verificarFilCol 2 matrx) (verificarFilCol 2 (transpuesta matrx)) (verificarDiag 2 matrx)) "derrota");;Verifica si el PC ganó
    (else "Nadie ha ganado aún")))

(define (jugada_aux player colNum matrx cont)
  (cond
    ((null? matrx) null)
    ((equal? cont colNum) (cons (colocarFicha (car matrx) player) (cdr matrx)))
    (else (cons (car matrx) (jugada_aux player colNum (cdr matrx) (+ cont 1))))))

;;Función para realizar una jugada
(define (jugada player colNum matrx)
  (cond
    ((or (> colNum (largo (car matrx))) (columnaLlena? colNum matrx)) matrx)
    ((equal? (resultado? (transpuesta (jugada_aux player colNum (transpuesta matrx) 0))) "victoria") 1)
    ((equal? (resultado? (transpuesta (jugada_aux player colNum (transpuesta matrx) 0))) "derrota") 2)
    ((llena? (transpuesta (jugada_aux player colNum (transpuesta matrx) 0))) 0)
    (else (transpuesta (jugada_aux player colNum (transpuesta matrx) 0)))))

#|=========================ALGORITMO VORAZ=============================|#

;;Función que da una solución parcial
(define (objetivo matriz columna columnaaux)
  (cond
    [(equal?  columnaaux columna) (viabilidad (reverse matriz))]
    ((and (equal? (jugada 2 columnaaux (reverse matriz)) 2) (not (columnaLlena? columnaaux (reverse matriz)))) columnaaux)
    ((and (equal? (jugada 1 columnaaux (reverse matriz)) 1) (not (columnaLlena? columnaaux (reverse matriz)))) columnaaux)
    ((and (equal? (jugada 2 columnaaux (reverse matriz)) 0) (not (columnaLlena? columnaaux (reverse matriz)))) columnaaux)
    [else (objetivo matriz columna (+ columnaaux 1))]))

;;Función que revisa si un candidato se puede utilizar para la solución
(define (viabilidad matrix)
  (seleccion (transpuesta matrix) 0))

;;Función que elige el mejor candidato entre las diferentes soluciones
(define (seleccion matrix val)
  (seleccion_aux matrix val 0 matrix))

(define (seleccion_aux matrix val counter tot)
  (cond ((null? matrix)
          (seleccion_aux tot 1 0 tot))
        ((and (posible? (car matrix)) (or (exist? 2 (car matrix))(exist? 1 (car matrix))) (= val 1)) counter)
        ((and (posible? (car matrix)) (not (or (exist? 2 (car matrix))(exist? 1 (car matrix)))) (= val 0)) counter)
        (else
         (seleccion_aux (cdr matrix) val (+ counter 1) tot))))