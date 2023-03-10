#lang slideshow
(provide GenerarMatriz)
(provide transpuesta)

;verificar los espacios libres para jugar, asignados como 0
(define (libre? lista)
  (cond ((null? lista) #f)
        ((equal? (car lista) 0) #t)
        (else (libre? (cdr lista)))))

;verificar si la matriz (tablero) esta llena
(define (TableroL? matriz)
  (cond ((and (list? matriz) (list? (car matriz)))
         (TableroL_aux matriz))))

;funcion auxiliar
(define (TableroL_aux matriz)
  (cond ((null? matriz) #t)
        ((libre? (car matriz)) #f)
        (else (TableroL_aux (cdr matriz)))))


; longitud de una lista
(define (Longitud lista)
  (cond ((null? lista) 0)
        (else (+ 1 (Longitud (cdr lista))))))

;#Funciones de matrices ###########################

;Funcion para generar matriz
(define (GenerarMatriz m n)
  (cond ((validarFC m n) (Generar_aux m n))
        (else null)))

(define (Generar_aux m n)
  (cond ((zero? m) null)
        (else (cons (hacer_filas n) (Generar_aux (- m 1) n)))))
;Verificar si el numero de filas y colummnas dados estan entre 8 y 16
(define (validarFC filas columnas)
  (cond ((and (>= filas 8) (<= filas 16) (>= columnas 8) (<= columnas 16)) #t)
        (else #f)))

;Hacer la columnas de 0's apartir de la cantidad de columnas dado
(define (hacer_filas columnas)
  (cond ((zero? columnas) null)
        (else (cons 0( hacer_filas (- columnas 1))))))

;############Funciones para recorrer matriz#################################3
;funcion para sacar el primer elemento de cada fila (funcion opcional)
(define (sacar-1f matriz)
  (cond ((null? matriz) '())
        (else (cons (car (car matriz)) (sacar-1f (cdr matriz))))))

;borrar primer elemento de cada fila de la matriz(funcion opcional)

(define (borrar-1f matriz)
  (cond ((null? matriz) '())
        (else (cons (cdr (car matriz)) (borrar-1f (cdr matriz)))))) 



;Funcion para obtener la transpuesta de una matirz
(define (transpuesta matriz)
  (cond(( null? (car matriz)) '())
       (else( cons( aplicar-f car matriz) (transpuesta (aplicar-f cdr matriz))))))


; pedir al usuario las dimensiones de la matriz
(display "Ingrese el número de filas: ")
(define filas (read))
(display "Ingrese el número de columnas: ")
(define columnas (read))

; generar la matriz y mostrarla en pantalla
(define matriz (GenerarMatriz filas columnas))
(cond ((not matriz) (display "Las dimensiones de la matriz son incorrectas"))
      (else (displayln "La matriz generada es:")
            (for-each displayln matriz)))



(define (agregar-1-en-fila-y-columna fila columna matriz)
  (cond ((null? matriz) '())
        ((< fila 0) (error "Fila no válida"))
        ((< columna 0) (error "Columna no válida"))
        ((= fila 0) (cons (agregar-1-en-columna columna (car matriz)) (cdr matriz)))
        (else (cons (car matriz) (agregar-1-en-fila-y-columna (- fila 1) columna (cdr matriz))))))

(define (agregar-1-en-columna columna lista)
  (cond ((null? lista) (error "La lista no tiene elementos"))
        ((= columna 0) (cons 1 (cdr lista)))
        (else (cons (car lista) (agregar-1-en-columna (- columna 1) (cdr lista))))))

(display "Ingrese la fila en la que quiere agregar el 1: ")
(define fila (read))
(display "Ingrese la columna en la que quiere agregar el 1: ")
(define columna (read))

(define nueva-matriz (agregar-1-en-fila-y-columna fila columna matriz))
(displayln "La matriz resultante es:")
(for-each displayln nueva-matriz)




;Funcion para aplicar una funcion a todos los elementos de una lista (reemplazo para map)
(define (aplicar-f fun lista)
  (cond (( null? lista) '())
        (else (cons(fun (car lista)) (aplicar-f fun (cdr lista))))))

;Funcion para aplicar una funcion a todos los elementos de una lista (reemplazo para map)
(define (aplicar-f2 fun lista)
  (cond (( null? lista) '())
        (else (cons(fun (car lista)) (aplicar-f2 fun (cdr lista))))))

;funcion para voltear lista (sustitudo para la funcion de reverse)

(define (voltear lista)
  (cond ((null? lista) '( ))
         (append (voltear (cdr lista)) (list (car lista)))))

(define (append lista1 lista2)
  (cond ((null? lista1) lista2)
        (else (cons (car lista1) (append (lista1) lista2)))))
  


;verificar si la columna esta llena

(define (ColumnaF? colNum matriz)
  (ColumnaF_aux colNum (transpuesta matriz) 0))


(define (ColumnaF_aux colNum matriz contador)
  (cond ((null? matriz) #f)
        ((equal? colNum) (not (libre? (car matriz))))
        (else (ColumnaF_aux (cdr matriz) (+ contador 1)))))

(define (matriz_prueba)
  '((0 0 0 0 0 0 0 0 0 1)
(0 0 0 0 0 0 1 1 1 1)
(0 0 0 0 0 0 0 0 0 1)
(0 0 0 0 0 0 0 0 0 1)
(0 0 0 0 0 0 0 0 0 0)
(0 0 0 0 0 0 0 0 0 0)
(0 0 0 0 0 0 0 0 0 0)
(0 0 0 0 0 0 0 0 0 0)
(0 0 0 0 0 0 0 0 0 0)
(0 0 0 0 0 0 0 0 0 0)))


;buscar y verificar si hay 4 elementos iguales en una linea
;jugador variara entre 1 y 2, 1 siendo jugador y 2 la pc;
;ganar: contador que esta en 0 que va aumentando hasta llegar al 4 para saber si hay 4 en linea en una fila
(define (4enfila jugador linea ganar)
  (cond((= ganar 4) #t)
    ((null? linea) #f)
    ((equal? (car linea) jugador) (4enfila jugador (cdr linea) (+ ganar 1)))
    (else (4enfila jugador (cdr linea) 0))))

;Funcion para buscar en todas las lineas
(define (TodasL4 jugador matriz)
  (cond ((null? matriz) #f)
        ((TodasL4 jugador (car matriz) 0) #t)
        (else (TodasL4 jugador (cdr matriz)))))





