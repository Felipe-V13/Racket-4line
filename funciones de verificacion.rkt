#lang slideshow
;verificar los espacios libres para jugar, asignados como 0
(define (libre? lista)
  (cond ((null? lista) #f)
        ((equal? (car lista) 0)#t)
        (else(libre? (cdr lista)))))

;verificar si la matriz (tablero) esta llena
(define (TableroL? matriz)
  (cond ((and (list? matriz) (list? (car matriz)))
         (TableroL_aux))))
;funcion auxiliar
(define (TableroL_aux matriz)
  (cond ((null? matriz) #t)
        ((libre? (car matriz))#f)
        (else (TableroL_aux(cdr matriz )))))


; longitud de una lista
(define (Longitud lista)
  (cond ( (null? lista) 0)
        (else (+ 1 (Longitud (cdr lista))))))

;#Funciones de matrices ###########################

;Funcion para generar matriz
(define (GenerarMatriz m n)
  (cond ((validarFC m n) (Generar_aux m n))
        (else #f)))
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
  (cond(( null? matriz) '())
       (else (cons (cdr (car matriz)) (borrar-1f (cdr matriz))))))

;Funcion para aplicar una funcion a todos los elementos de una lista (reemplazo para map)
(define (aplicar-f fun lista)
  (cond (( null? lista) '())
        (else (cons(fun (car lista)) (aplicar-f fun (cdr lista))))))

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
