#lang racket

#|==================================================================|#

(provide generateMatrx)
(provide play)
(provide candi)
(provide selec)
(provide objetivo)
(provide viabilidad)
(provide buscar)
(provide traspuesta)
(provide maximo)
(provide solucion)
(provide fullColumn?)
(provide greedybegin)


#|==================================================================|#


;;Verifica si hay ceros en una fila
(define (hayCeros? lst)
  (cond
    ((null? lst) #f)
    ((equal? (car lst) 0) #t)
    (else (hayCeros? (cdr lst)))
  )
)

(define (full_aux matrx)
  (cond
    ((null? matrx) #t)
    ((hayCeros? (car matrx)) #f)
    (else (full_aux (cdr matrx)))
  )
)

;;Verifica si la matrix está llena => empate
(define (full? matrx)
  (cond
    ((and (list? matrx) (list? (car matrx))) (full_aux matrx))
  )
)

#|==================================================================|#

;;Verifica si las dimensiones están en el rango
(define (validDim? rows col)
  (cond
    ((and (>= rows 8) (<= rows 16) (>= col 8) (<= col 16)) #t)
    (else #f)
  )
)

;;Hace una lista de tantos ceros como número de columnas se den
(define (makeRow col_number)
  (cond
    ((zero? col_number) null)
    (else (cons 0 (makeRow (- col_number 1))))
  )
)

(define (generateMatrx_aux m n)
  (cond
    ((zero? m) null)
    (else (cons (makeRow n) (generateMatrx_aux (- m 1) n)))
  )
)

;;Generar una matriz de dimensiones m x n vacía (solo de ceros) 
(define (generateMatrx m n)
  (cond
    ((validDim? m n) (generateMatrx_aux m n))
    (else #f)
  )
)

#|==================================================================|#

(define (getPos_aux elem lst index out)
  (cond
    ((null? lst) (reverseList out))
    ((equal? elem (car lst))
     
     (getPos_aux elem (cdr lst) (+ index 1) (cons (list index) out)))
    
    (else (getPos_aux elem (cdr lst) (+ index 1) out))
  )
)

;;Obtener el índice de ese elemeto
(define (getPos elem lst)
  (getPos_aux elem lst 0 '())
)


#|==================================================================|#

(define (at_aux index lst cont)
  (cond
    ((null? lst) #f)
    ((equal? index cont) (car lst))
    (else (at_aux index (cdr lst) (+ cont 1)))
  )
)

;;Obtener el elemento en ese índice
(define (at index lst)
  (at_aux index lst 0)
)

;; matrx[0][3]  =>
;; > (at 3 (at 0 '((1 2 3 4 5) (6 7 8 9 10))))
;; > 4

#|==================================================================|#

;;Cuenta los elementos de una lista
(define (len lst)
  (cond
    ((null? lst) 0)
    (else (+ 1 (len (cdr lst))))
  )
)

;;doFunc: Realiza una función en cada elemento de una lista
;;E: función por realizar, lista donde se va a aplicar la función
;;S: lista con la función aplicada
;;R: null si la lista es nula o recibe un #f
(define (doFunc func lista)
  (cond
    ((null? lista) null)
    (else (cons (func (car lista)) (doFunc func (cdr lista)))) 
  )
)

;;Devuelve la traspuesta de una matriz
(define (traspuesta matrx)
  (cond
    ((null? (car matrx)) '()) 
    (else (cons (doFunc car matrx) (traspuesta (doFunc cdr matrx))))
  )
)

(define (fullCol_aux colNum matrx cont)
  (cond
    ((null? matrx) #f)
    ((equal? cont colNum) (not (hayCeros? (car matrx))))
    (else (fullCol_aux colNum (cdr matrx) (+ cont 1)))
  )
)

;;Verifica si una columna está llena
(define (fullColumn? colNum matrx)
  (fullCol_aux colNum (traspuesta matrx) 0)
)


#|==================================================================|#


;;Efecto que cae hasta abajo
(define (gravityCheck column player)
  (cond
    ;;Si ya no hay más
    ((null? (car column)) '())
    ;;Si es el último espacio de la columna
    ((null? (cdr column)) (list player))
    ;;Si el siguiente no es cero  ->  hay que ponerla ahí         
    ((not (equal? (cadr column) 0)) (cons player (cdr column)))
    (else (cons (car column) (gravityCheck (cdr column) player)))
  )
)


#|=================  Verificar Horizontales y Verticales   ================|#


;;Verificar si hay 4 seguidos del mismo número en una línea 
(define (4Line? player line cont)
  (cond 
    ((= cont 4) #t)
    ((null? line) #f)
    ((equal? (car line) player) (4Line? player (cdr line) (+ cont 1)))
    (else (4Line? player (cdr line) 0))
  )
)


;;Verificar en todas las líneas de la matriz si hay 4 en línea 
(define (checkAllLines player matrx)
  (cond
    ((null? matrx) #f)
    ((4Line? player (car matrx) 0) #t)
    (else (checkAllLines player (cdr matrx)))
  )
)


#|======================  Verificar Diagonales  ======================|#


;;Recorrer la matriz superior por sus diagonales de izquierda(L) a derecha (R)

(define (recorrerDiag_Sup_L_R_aux matrx i j vec result)
  (cond
    ((>= i (len matrx)) result)
    ((<= j i)
      (recorrerDiag_Sup_L_R_aux matrx i (+ j 1)
                            (append vec (list (at j (at (- i j) matrx)))) result)
    )
    ((> j i)
      (recorrerDiag_Sup_L_R_aux matrx (+ i 1) 0 '() (append result (list vec)))
    )
  )
)

;;> (recorrerDiag_Sup_L_R_aux '((1 2 3)
;;                              (4 5 6)
;;                              (7 8 9)) 0 0 '() '())
;;'((1) (4 2) (7 5 3))



;;Recorrer la matriz inferior por sus diagonales de izquierda(L) a derecha (R)

(define (recorrerDiag_Inf_L_R_aux matrx i j vec result)
  (cond
    ((>= i (len matrx)) result)
    ((< j (- (len matrx) i 1))
      (recorrerDiag_Inf_L_R_aux matrx i (+ j 1)
         (append vec (list (at (+ j i 1) (at (- (len matrx) j 1) matrx)))) result)
    )
    ((>= j (- (len matrx) i 1))
      (recorrerDiag_Inf_L_R_aux matrx (+ i 1) 0 '() (append result (list vec)))
    )
  )
)

;> (recorrerDiag_Inf_L_R_aux '((1 2 3)
;;                             (4 5 6)
;                              (7 8 9)) 0 0 '() '())
;'((8 6) (9) ())


(define (reverseList_aux lista resultList)
  (cond
    ((null? lista) resultList)
    (else (reverseList_aux (cdr lista) (cons (car lista) resultList)))
  )
)

;;Invertir los elementos de una lista
(define (reverseList lista)
  (cond
    ((and (not(null? lista)) (list? lista)) (reverseList_aux lista '()))
   )
)

;;Invertir los elementos de las filas de la matriz (Invertir columnas)
(define (reverseRows matrx)
  (cond
    ((null? matrx) null)
    (else (cons (reverseList (car matrx)) (reverseRows (cdr matrx))))
  )
)

;;Recorrer la matriz por sus diagonales de izquierda a derecha
;;Une las funciones de diagonal superior e inferior
(define (recorrerDiag_L_R matrx)
  (append
    (recorrerDiag_Sup_L_R_aux matrx 0 0 '() '())
    (recorrerDiag_Inf_L_R_aux matrx 0 0 '() '())
  )
)

;;Recorrer la matriz por sus diagonales de derecha a izquierda
;;Une las funciones de diagonal superior e inferior con las columnas invertidas
(define (recorrerDiag_R_L matrx)
  (append
    (recorrerDiag_Sup_L_R_aux (reverseRows matrx) 0 0 '() '())
    (recorrerDiag_Inf_L_R_aux (reverseRows matrx) 0 0 '() '())
  )
)

;;Verifica si hay 4 en línea en la lista de diagonales
(define (checkAllDiag_aux player diagList)
  (cond
    ((null? diagList) #f)
    ((4Line? player (car diagList) 0) #t)
    (else (checkAllDiag_aux player (cdr diagList)))
  )
)

;;Verifica si hay 4 en línea en las diagonales de izquierda a derecha o viceversa
(define (checkAllDiag player matrx)
  (cond
    ((> (len (car matrx)) (len matrx))
     (or (checkAllDiag_aux player (recorrerDiag_L_R (traspuesta matrx)))
         (checkAllDiag_aux player (recorrerDiag_R_L (traspuesta matrx))))
    )
    (else (or (checkAllDiag_aux player (recorrerDiag_L_R matrx))
         (checkAllDiag_aux player (recorrerDiag_R_L matrx)))
    )
  )
)

#|==================================================================|#

;;Verifica quién ganó o si no nadie ha ganado
(define (win? matrx)
  (cond
    ;;Verifica para el jugador si hay 4 en línea en filas y columnas
    ((or (checkAllLines 1 matrx) (checkAllLines 1 (traspuesta matrx))) "You won")
    ;;Verifica para la máquina si hay 4 en línea en filas y columnas
    ((or (checkAllLines 2 matrx) (checkAllLines 2 (traspuesta matrx))) "You Lost")
    ;;Verifica para el jugador si hay 4 en línea en las diagonales
    ((checkAllDiag 1 matrx) "You won")
    ;;Verifica para la máquina si hay 4 en línea en las diagonales
    ((checkAllDiag 2 matrx) "You Lost")
    (else "Nadie ha ganado aún")
  )
)


(define (play_aux player colNum matrx cont)
  (cond
    ((null? matrx) null)
    ((equal? cont colNum) (cons (gravityCheck (car matrx) player) (cdr matrx)))
    (else (cons (car matrx) (play_aux player colNum (cdr matrx) (+ cont 1))))
  )
)


;;Hacer una jugada
;;player: ficha de quien va a jugar: 1 (jugador), 2 (máquina)
;;colNum: número de columna donde se quiere tirar
(define (play player colNum matrx)
  (cond
    ((equal? (win? (traspuesta (play_aux player colNum (traspuesta matrx) 0))) "You won") 1)
    ((equal? (win? (traspuesta (play_aux player colNum (traspuesta matrx) 0))) "You Lost") 2)
    ((full? (traspuesta (play_aux player colNum (traspuesta matrx) 0))) 0)
    ((or (> colNum (len (car matrx))) (fullColumn? colNum matrx)) matrx)
    (else (traspuesta (play_aux player colNum (traspuesta matrx) 0)))
  )
)


#|==================================================================|#
;;GREEDY ALGORITHM

#| FUNCION CANDIDATOS |#
;Define la cantidad de columnas disponibles para jugar
(define (candi matrx)
  (cond
    ((full? matrx) null)
    (else
     (len (car matrx)))))
#| FUNCION CANDIDATOS |#

#| FUNCION SELECCION |#
;Selecciona las columnas con mas fichas del PC 
(define(selec matrx index out exp)
  
  (cond
       ((null? matrx) out)
       (( and  (not(null? exp)) (equal? index (car exp)) )
        (selec (cdr matrx) (+ index 1) out (cdr exp)))
       (else
        (cons(compu? (car matrx) index 0) (selec (cdr matrx) (+ index 1) out exp)))  
       ))

;Funcion para saber si las fichas son del PC
(define(compu? lista  index cant)
  (cond
    ((null? lista) (list index cant))
    ((equal? (car lista) 2)
     (compu? (cdr lista)  index (+ cant 1)))
      
    (else
     (compu? (cdr lista)  index cant))))

;Saca el maximo de fichas del PC que hay en una columna    
(define (maximo mat)
  (cond
    ((null? mat) 0)
    (else 
     (max (cadar mat) (maximo (cdr mat) ) ) 
     )))
;Busca la columna con el numero de fichas retornado en maximo
(define (buscar mat elemento out)
  (cond
    ((null? mat) out)
    ((equal? elemento (cadar mat))
     (cons(car mat) (buscar (cdr mat) elemento out)))
    (else
     (buscar (cdr mat) elemento out)))) 
#| FUNCION SELECCION |#     
      

#| FUNCION VIABILIDAD |#

;Verifica que la columna a jugar no este llena
(define (viabilidad columnas matrx exp)
  (cond
    ((null? columnas) null)
    ((and (equal? (fullColumn? (caar columnas) matrx) #t) (=(len columnas) 1))
     (buscar  (selec (traspuesta matrx) 0 '()  (cons (caar columnas) exp) )(maximo (selec (traspuesta matrx) 0 '()  (cons (caar columnas) exp) ) ) '())
     )
    ((equal? (fullColumn? (caar columnas) matrx) #t)
     (viabilidad (cdr columnas)  matrx  exp))
    (else
     (cons (car columnas) (viabilidad (cdr columnas)  matrx exp) )))  ) 

(define( eliminarCol elemento index lista)
  (cond((null? lista)
        '() )
       ((equal? index elemento)
       (eliminarCol elemento (+ index 1) (cdr lista)) ) 
       (else
        (cons (car lista) (eliminarCol elemento (+ index 1) (cdr lista))))))

  
#| FUNCION VIABILIDAD |#


#| FUNCION OBJETIVO |#
;asigna valores a las soluciones 
(define (objetivo soluciones matrx)
  (cond
    ((null? soluciones)#f)
    ((> (len soluciones) 1)
     (car soluciones)
     )
    (else
     (car soluciones))))  
     
#| FUNCION OBJETIVO |#


#| FUNCION SOLUCION |#
;selecciona el mejor valor de las soluciones
(define (solucion respuesta)

   (not(null? respuesta))
  (car respuesta) 
  )
#| FUNCION SOLUCION |#


(define (greedybegin matriz fila columna filaaux columnaaux)
  (cond
   ;; [(equal? filaaux fila) (greedy-algorithm-coords (reverse matriz))]
    [(equal?  columnaaux columna) (greedy-algorithm-coords (reverse matriz))]
    ((equal? (play 1 columnaaux (reverse matriz)) 1) (list filaaux columnaaux))
    ((equal? (play 2 columnaaux (reverse matriz)) 2) (list filaaux columnaaux))
    ((equal? (play 2 columnaaux (reverse matriz)) 0) (list filaaux columnaaux))
;;    [(or (equal? (list-ref (list-ref matriz filaaux) columnaaux) 0) (equal? (list-ref (list-ref matriz filaaux) columnaaux) 1) (equal? (list-ref (list-ref matriz filaaux) columnaaux) 2))
;;     (cond
  ;;     ((equal? (play 1 columnaaux matriz) 1) (list filaaux columnaaux))
    ;;   ((equal? (play 2 columnaaux matriz) 2) (list filaaux columnaaux))
      ;; ((equal? (play 2 columnaaux matriz) 0) (list filaaux columnaaux))
   ;;    (else
     ;;   (greedybegin matriz fila columna filaaux (+ columnaaux 1))))]
;    (( or (equal? (list-ref (list-ref matriz filaaux) columnaaux) 1) (equal? (list-ref (list-ref matriz filaaux) columnaaux) 2))
 ;;    (cond
   ;;    ((= (+ columnaaux 3) columna)
     ;;   (cond
       ;;   ((equal? (play 1 (+ columnaaux 3) matriz) 1) (list filaaux (+ columnaaux 3)))
         ;; ((equal? (play 2 (+ columnaaux 3) matriz) 2) (list filaaux (+ columnaaux 3)))
;;          ((equal? (play 2 (+ columnaaux 3) matriz) 0) (list filaaux (+ columnaaux 3)))
  ;;        (else
    ;;       (greedybegin matriz fila columna filaaux (+ columnaaux 1)))))
;;       (else
  ;;      (greedybegin matriz fila columna filaaux (+ columnaaux 1)))))
    [else (greedybegin matriz fila columna filaaux (+ columnaaux 1))]
  )
)

(define (greedy-algorithm-coords matrix)
  (list 0 (greedy-row (traspuesta matrix) 0)))

(define (greedy-row matrix val)
  (greedy-row-comp matrix val 0 matrix))

(define (greedy-row-comp matrix val counter tot)
  (cond ((null? matrix)
          (greedy-row-comp tot 1 0 tot))
        ((and (hayCeros? (car matrix)) (or (exist? 2 (car matrix))(exist? 1 (car matrix))) (= val 1)) counter)
        ((and (hayCeros? (car matrix)) (not (or (exist? 2 (car matrix))(exist? 1 (car matrix)))) (= val 0)) counter)
         
     ;;    (cond ((= val 0)
       ;;         (car matrix))
         ;;      ((= val 1)
           ;;     counter)))
        (else
         (greedy-row-comp (cdr matrix) val (+ counter 1) tot))))

(define (greedy-column matrix)
  (greedy-column-comp (greedy-row matrix 0) 0))

(define (greedy-column-comp row counter)
  (cond ((null? row)
         0)
        ((or (equal? (car row) 2) (equal? (car row) 1))
         (+ counter 2))
        ((or (equal? (cadr row) 2) (equal? (cadr row) 1))
         (+ counter 1))
        ((or (equal? (caddr row) 2) (equal? (caddr row) 1))
         counter)
        (else
         (greedy-column-comp (cdr row) (+ counter 1)))))

(define (exist? value list)
  (cond ((null? list)
         #f)
        ((equal? (car list) value)
         #t)
        (else
         (exist? value (cdr list)))))
