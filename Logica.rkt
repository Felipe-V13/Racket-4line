#lang racket

(provide generarMatriz)
(provide jugada)
(provide objetivo)


;#Manejador de listas#


;Esta función toma una lista como argumento y devuelve verdadero si aún hay espacio para jugar
;(es decir, si aún hay un cero en la lista) y falso en caso contrario. Si la lista está vacía,
;la función devuelve falso.
(define (posible? lst)
  (cond
    ((null? lst) #f) ;;Falso si no hay posibilidad de jugar
    ((equal? (car lst) 0) #t) ;;True si aún hay campo para jugar
    (else (posible? (cdr lst)))))


;;Esta función toma un valor y una lista como argumentos y devuelve verdadero si el valor se
;encuentra en la lista y falso en caso contrario.
(define (exist? value list)
  (cond ((null? list) ;;Falso si no existe
         #f)
        ((equal? (car list) value) ;;Verdadero si existe
         #t)
        (else
         (exist? value (cdr list)))))


;;Esta función toma dos argumentos, m y n, y devuelve una matriz (una lista de listas) de tamaño m x n,
; llena de ceros. Si los argumentos no corresponden a un tamaño válido, la función devuelve falso.
(define (generarMatriz m n)
  (cond
    ((tamano? m n) (generarMatriz_aux m n))
    (else #f)))


;;Esta función toma una lista como argumento y devuelve la longitud de la lista.
(define (largo lst)
  (cond
    ((null? lst) 0)
    (else (+ 1 (largo (cdr lst))))))

;;Esta función toma una función y una lista como argumentos y devuelve una lista que resulta de aplicar
; la función a cada elemento de la lista original.
(define (aplicar-f func lista)
  (cond
    ((null? lista) null)
    (else (cons (func (car lista)) (aplicar-f func (cdr lista))))))

;;Esta función toma una matriz (una lista de listas) como argumento y devuelve la transpuesta de la matriz
;(es decir, una matriz en la que las filas y columnas están intercambiadas).
(define (transpuesta matrx)
  (cond
    ((null? (car matrx)) '()) 
    (else (cons (aplicar-f car matrx) (transpuesta (aplicar-f cdr matrx))))))

;;Esta función toma una columna (una lista) y un jugador (un número) como argumentos, y devuelve la columna
; con la ficha del jugador colocada en la posición más baja posible. Si la columna ya está llena, la función
; no coloca ninguna ficha.
(define (colocarFicha column player)
  (cond
    ((null? (car column)) '())
    ((null? (cdr column)) (list player)) ;;Coloca la ficha en la posicion mas baja de la columna       
    ((not (equal? (cadr column) 0)) (cons player (cdr column)))
    (else (cons (car column) (colocarFicha (cdr column) player)))))


;#VERIFICACION DEL JUEGO#
;#VERIFICACION DEL JUEGO#
;#VERIFICACION DEL JUEGO#
;#VERIFICACION DEL JUEGO#

;función auxiliar que verifica si una matriz está llena. Recibe una matriz y verifica si todas las columnas
;están llenas, retorna verdadero si están llenas y falso si no lo están.
(define (llena_aux matrx)
  (cond
    ((null? matrx) #t) ;;True si esta llena
    ((posible? (car matrx)) #f) ;;Falso si no esta llena
    (else (llena_aux (cdr matrx)))))



;; función que verifica si la matriz del juego está llena. Recibe la matriz del juego y utiliza la función
;auxiliar llena_aux para determinar si la matriz está llena.
(define (llena? matrx)
  (cond
    ((and (list? matrx) (list? (car matrx))) (llena_aux matrx))))



;;función que valida si el número de filas y columnas está en el rango permitido para el juego.
; Recibe dos números enteros que representan el número de filas y columnas, respectivamente, y
;devuelve verdadero si están en el rango permitido y falso si no lo están.
(define (tamano? rows col)
  (cond
    ((and (>= rows 8) (<= rows 16) (>= col 8) (<= col 16)) #t)
    (else #f)))


;;función que crea las filas de una matriz con un número de columnas dado. Recibe el número de
;columnas que tendrá la fila y crea una lista con ceros con esa cantidad de elementos.
(define (crearFilas col_number)
  (cond
    ((zero? col_number) null)
    (else (cons 0 (crearFilas (- col_number 1))))))


;función auxiliar que crea una matriz con un número de filas y columnas dados, utilizando la función crearFilas.
; Recibe el número de filas y columnas que tendrá la matriz y devuelve la matriz creada.
(define (generarMatriz_aux m n)
  (cond
    ((zero? m) null)
    (else (cons (crearFilas n) (generarMatriz_aux (- m 1) n)))))



;;función auxiliar que busca un elemento en una lista por su índice. Recibe el índice del elemento que se desea buscar,
;la lista en la que se realizará la búsqueda y un contador que lleva la posición actual en la lista, y devuelve el
;elemento correspondiente al índice buscado.
(define (buscarEle_aux index lst cont)
  (cond
    ((null? lst) #f) ;;Falso si no encuentra el índice
    ((equal? index cont) (car lst)) ;;La posicion donde esta el elemento cuando llega al índice
    (else (buscarEle_aux index (cdr lst) (+ cont 1)))))


;;función que busca un elemento en una lista por su índice. Recibe el índice del elemento que se desea buscar
;y la lista en la que se realizará la búsqueda, y utiliza la función buscarEle_aux para realizar la búsqueda.
(define (buscarEle index lst)
  (buscarEle_aux index lst 0))

; función auxiliar que verifica si una columna de una matriz está llena. Recibe el número de la columna que se
;desea verificar, la matriz en la que se realizará la verificación y un contador que lleva la posición actual
;en la lista, y devuelve verdadero si la columna está llena y falso si no lo está.
(define (columnaLlena_aux colNum matrx cont)
  (cond
    ((null? matrx) #f) ;;Falso si la columna no está llena
    ((equal? cont colNum) (not (posible? (car matrx)))) ;;True si está llena
    (else (columnaLlena_aux colNum (cdr matrx) (+ cont 1)))))


;;función que verifica si una columna de una matriz está llena. Recibe el número de la columna que se desea
;verificar y la matriz en la que se realizará la verificación, y utiliza la función columnaLlena_aux para
;realizar la verificación. También utiliza la función transpuesta para convertir la matriz en su transpuesta
;y verificar la columna correspondiente.
(define (columnaLlena? colNum matrx)
  (columnaLlena_aux colNum (transpuesta matrx) 0))



;#VERIFICACION DEL JUEGO (GANE)(PIERDE)(EMPATE)#
;#VERIFICACION DEL JUEGO (GANE)(PIERDE)(EMPATE)#
;#VERIFICACION DEL JUEGO (GANE)(PIERDE)(EMPATE)#


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

;invierte una lista dada. Recibe como argumentos una lista lista y una lista vacía resultList.
; Utiliza una serie de condicionales para recorrer la lista lista y agregar sus elementos en
;orden inverso a la lista resultList. Cuando se completa la lista lista, la función devuelve
;la lista resultList.
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
    ((>= (largo (car matrx)) (largo matrx))
     (or (verificarDiag_aux player (diagonalIzqDer matrx))
         (verificarDiag_aux player (diagonalDerIzq matrx)))))) 


;;Verifica quién ganó o si no nadie ha ganado
(define (resultado? matrx)
  (cond
    ((or (verificarFilCol 1 matrx) (verificarFilCol 1 (transpuesta matrx)) (verificarDiag 1 matrx)) "victoria");;Verifica si el jugador ganó
    ((or (verificarFilCol 2 matrx) (verificarFilCol 2 (transpuesta matrx)) (verificarDiag 2 matrx)) "derrota");;Verifica si el PC ganó
    (else "Nadie ha ganado aún")))

;devuelve la nueva matriz con la ficha dl jugador en la columan elegida y mantiene los emas espacios igual
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


;#ALGORITMO VORAZ#

; Esta funcion recibe como parametro una matriz, una columna y el contador columnaaux,;
;llama a la funcion viabilidad para la traspuesta d ela matriz
;caso contrario varifica si es posible colocar una ficaen la columna actual
;este caso lo evalua en la juagada 1 jugada 2 y jugada 0, si no se cumple ninguna de estas se llama al contador;
;se le suma 1 a la columna
;Se evaluan las condiciones en donde el algoritmo cree que es mejor jugar
(define (objetivo matriz columna columnaaux)
  (cond
    [(equal? columnaaux columna) (viabilidad (reverse matriz))]
    [(and (not (columnaLlena? columnaaux (reverse matriz))) (equal? (jugada 2 columnaaux (reverse matriz)) 2)) columnaaux]
    [(and (not (columnaLlena? columnaaux (reverse matriz))) (equal? (jugada 1 columnaaux (reverse matriz)) 1)) columnaaux]
    [(and (not (columnaLlena? columnaaux (reverse matriz))) (equal? (jugada 2 columnaaux (reverse matriz)) 0)) columnaaux]
    [else (objetivo matriz columna (add1 columnaaux))]))


;recibe unna matriz que representa al tablero actual del juego y llama a la funcion seleccion con la traspuesta;
;de la matriz y un valor 0 para la seleccion del movimiento
(define (viabilidad matrix)
  (seleccion (transpuesta matrix) 0))

;Esta funcion recibe una matrix y un valor val que correpsonde a 0 en el moemnto de la entrada;
; a la salida tenemos la misma funcion solo que llamando a su auxiliar, un valor val y num "0" y la matrix
(define (seleccion matrix val)
  (seleccion-aux matrix val 0 matrix))


;La función seleccion-aux es una función auxiliar que se utiliza para seleccionar el mejor movimiento posible.
; Recibe una matriz que representa el tablero actual del juego, un valor val, un contador counter y la matriz
;original tot. Evalúa varias condiciones y devuelve el mejor movimiento posible. Si ninguna
;de estas condiciones se cumple, llama a la función seleccion-aux recursivamente con la cola de la matriz,
;el valor val, aumentando el contador counter y la matriz original tot. 
(define (seleccion-aux matrix val counter tot)
  (cond
    [(null? matrix) (seleccion tot 1 0 tot)]
    [(and (posible? (car matrix)) (= val (if (or (exist? 2 (car matrix)) (exist? 1 (car matrix))) 1 0))) counter]
    [else (seleccion-aux (cdr matrix) val (add1 counter) tot)]))
