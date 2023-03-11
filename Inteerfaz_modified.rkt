#lang racket/base

(require racket/gui)

(require racket/include)

(require (prefix-in htdp: 2htdp/image))

(require "Logica.rkt")


; #INICIALIZACION DEL JUEGO#

;se inicializa la variable con false

(define gameStart #f)

;ciclo que se encarga de actualizar el valor de gameStart

(define (asignar_gameStart bool)
    (set! gameStart (cons bool gameStart)))

;se define la matrix como null
(define matrix null)

;define la matriz al momento de juego
(define (matriz_game matriz)
    (set! matrix (cons matriz matrix)))

; #MENU PRINCIPAL DEL JUEGO#


; Caracteristicas de la ventana principal
(define frame (new frame%
                   [label "Racket 4 en Linea [Noel, Jose, Felipe]"]
                   [width 350]
                   [height 300]))

; Mensaje para indicar al jugador que se le pide en los text fields
(new message% [parent frame][label "Por favor indique el número de filas y columnas deseado"])


; slider para las columnas
(define columnas_4line (new slider%
                    (label "Columnas")
                    (parent frame)
                    (min-value 8)
                    (max-value 16)
                    (init-value 11)))

; slider para las filas
(define filas_4line (new slider%
                    (label "Filas")
                    (parent frame)
                    (min-value 8)
                    (max-value 16)
                    (init-value 11)))

; Mensaje para indicar al jugador que se le pide en los text fields
(new message% [parent frame][label "Color deseado:[1: Azul] [2: Rojo] [3: Celste] [4: verde]"])
(define color_SL (new slider%
                    (label "Eleccion:")
                    (parent frame)
                    (min-value 1)
                    (max-value 4)
                    (init-value 2)))

;colores a escoger por el jugador
(define (actualizar_imagen)
        (cond [(= (send color_SL get-value) 1) "azul.jpg"]
              [(= (send color_SL get-value) 2) "rojo.jpg"]
              [(= (send color_SL get-value) 3) "celeste.jpg"]
              [(= (send color_SL get-value) 4) "verde.jpg"]))


(define info-btn (new button%
                   (parent frame)
                   (label "Manual Rapido")
                   ;[min-width 50]
                   ;[min-height 50]
                   (callback (lambda (button event)
                               (send info-frame show #t)))))

(define info-frame (new frame% [label "Información Basica del juego"]
                               [width 300]
                               [height 50]))


(define info-msg (new message% [parent info-frame] [label "Este es mi párrafo de texto\nEste es mi párrafo de texto\nEste es mi párrafo de texto\nEste es mi párrafo de texto\nEste es mi párrafo de texto\nEste es mi párrafo de texto."]))


;(Botón para salir del menu rapido)
(new button% [parent info-frame]
             [label "Salir"]
            [callback (lambda (button event)
                        (send info-frame show #f)
                         (send frame show #t))])







;Boton con las caracteristicas para el incio del juego
(new button% [parent frame]
     [label "Iniciar"]
     [callback (lambda (button event)
                 (if (and (>= (send columnas_4line get-value) 8) (>= (send filas_4line get-value) 8) (<= (send filas_4line get-value) 16) (<= (send columnas_4line get-value) 16) (not gameStart) (not (false? (generateMatrx (send columnas_4line get-value) (send filas_4line get-value)))))
                     (begin
                       (crear_boton1 (send columnas_4line get-value) 0)
                       (matriz_game (generateMatrx (send filas_4line get-value) (send columnas_4line get-value)))
                       (interfaz_matrix (car matrix))
                       ;juego es True
                       (asignar_gameStart #t))
                     (begin
                       ;Juego es False
                       (writeln #f))))])


;Definir el área de los botones de interfaz
(define lugar_boton(new horizontal-panel%	 
   	 	[parent frame]	 
                [style '(border)]
                [alignment '(left top)]))


;Definir el área de juego de interfaz
(define panel(new vertical-panel%	 
   	 	[parent frame]	 
                [style '(border)]
                [alignment '(left top)]))

;mensaje cuando el jugador gana
(define ventana_win (new dialog% [label "Ganaste"]
                       [parent frame]
                       [width 200]))

; Función para mostrar la ventana que ganó
(define (ventana_gane)
  
   (send ventana_win show #t))

;Dialog box cuando el jugador gana
(define ventana_loss (new dialog% [label "Perdiste"]
                       [parent frame]
                       [width 200]))

; Función para mostrar la ventana que ganó
(define (ventana_pierde)
  (send ventana_loss show #t))

;mensaje de empate al usuario
(define empate (new dialog% [label "Empate"]
                       [parent frame]
                       [width 200]))

; Función para mostrar la ventana que ganó
(define (vetana_n)
   (send empate show #t))



;COnexion con el Algoritmo codicioso y retorna donde colocar la ficha
(define (IA_pc)
;; Se asume que la variable "matrix" está definida y contiene una matriz válida
  (cadr (greedybegin (reverse (car matrix)) ;; Se invierte la fila para que la PC empiece desde la última columna
                     (length (car matrix)) ;; Se obtiene el ancho de la matriz
                     (length (car matrix)) ;; Se obtiene el alto de la matriz
                     0 0))) ;; Los últimos dos argumentos son las coordenadas de inicio para la búsqueda (en este caso, 0,0)

;#JUGADA DE LA PC#

;;Función que utiliza el retorno de la IA para mostrar la jugada de la PC
(define (juega_IA_pc)
  (let ((col (IA_pc))) ;; Se obtiene la columna elegida por la PC
    (case (play 2 col (car matrix))
      ((1) (ventana_gane)) ;; Si la PC gana, se muestra una ventana indicándolo
      ((2) (ventana_pierde)) ;; Si la PC pierde, se muestra una ventana indicándolo
      ((0) (empate)) ;; Si hay empate, se muestra una ventana indicándolo
      (else
       ;; Si se sigue jugando, se muestran algunas cosas en la consola y se actualiza la matriz y el tablero gráfico
       (printf "Turno de la PC\n")
       (writeln (play 2 col (car matrix))) ;; Se muestra en la consola la matriz resultante de la jugada de la PC
       (define posibilidades (candi matrix)) ;; Se obtienen las posibles jugadas del jugador humano
       (matriz_game (play 2 col (car matrix))) ;; Se actualiza la matriz principal con la jugada de la PC
       (actualizacion_tablero) ;; Se actualiza el tablero gráfico con la matriz principal actualizada
       (interfaz_matrix (car matrix))))))

;asigna un valor al jugador que esta en ese momento, si es null es color plata, si es jugador(num=1) es del color elegido y si es la IA_pc es de color rosa

(define (asignacion num panelx)
  (let ((imagen_eleccion (actualizar_imagen)))
    (cond [(= num 0)
           (new message% [parent panelx]
                [label (read-bitmap "plata.jpg" )])]
          [(= num 1)
           (new message% [parent panelx]
                [label (read-bitmap imagen_eleccion)])]
          [(= num 2)
           (new message% [parent panelx]
                [label (read-bitmap "rosa (1).jpg" )])])))


;FBotones de la interfaz
(define (crear_boton1 num cont)
  (cond
    ((<= num 0) 1)
    (else
      (boton cont)
      (crear_boton1 (- num 1) (+ cont 1)))))


;Crea un widget que representa una fila de la matriz en la interfaz
(define (widget_fil)
   (new horizontal-panel%	 
   	 	[parent panel]	 
                [style '(border)]
                [alignment '(left top)]))

;Logica crear fila matriz interfaz
(define (fila_matriz fila widget)
  (cond
    ((null? fila ) 1)
    (else
     (asignacion (car fila) widget)
     (fila_matriz (cdr fila) widget))))

;Función que reinicia el tablero de juego en la interfaz, cada vez que se hace un movimiento

(define (actualizacion_tablero)
    (send frame delete-child panel)
     (set! panel (new vertical-panel%	 
   	 	[parent frame]	 
                [style '(border)]
                [alignment '(left top)])))


; Matriz en interfaz

(define (interfaz_matrix matriz)
  ; Comprueba si la matriz es nula
  (cond
    ((null? matriz) 1)
    ; Comprueba si la matriz no es una lista
    ((not (list? matriz)) #t)
    (else
     ; Crea una fila de la matriz en la interfaz utilizando la función fila_matriz y widget_fil
     (fila_matriz (car matriz) (widget_fil))
     ; Llama recursivamente a interfaz_matrix para crear las filas restantes de la matriz en la interfaz
     (interfaz_matrix (cdr matriz)))))


; #General JUEGO jugador

(define (boton pilar)
  (new button% [parent lugar_boton]   ; Se crea un nuevo botón en el panel "lugar_boton"
             [label (number->string pilar)]   ; Se asigna un label al botón con el valor de "pilar" convertido a string
             [horiz-margin 1]   ; Se asigna un margen horizontal de 1
             ; Callback procedure for a button click:
             [callback (lambda (button event)   ; Se crea un procedimiento de callback para el evento click del botón
                         (printf "turno jug\n")   ; Se imprime un mensaje en la consola
                         (writeln (play 1 pilar (car matrix))); Se llama a la función "play" con los argumentos 1, "col" y el primer elemento de "matrix". El resultado se imprime en la consola
                         (cond
                           ((equal? (play 1 pilar (car matrix)) 1)(ventana_gane))   ; Si el resultado de "play" es igual a 1, se llama a la función "ventana_gane"
                           ((equal? (play 1 pilar (car matrix)) 2)(ventana_pierde))   ; Si el resultado de "play" es igual a 2, se llama a la función "ventana_pierde"
                           ((equal? (play 1 pilar (car matrix)) 0) (empate))   ; Si el resultado de "play" es igual a 0, se llama a la función "empate"
                           ((equal? (play 1 pilar (car matrix)) (car matrix)) (send button enable #f))   ; Si el resultado de "play" es igual al primer elemento de "matrix", se deshabilita el botón
                           (else
                            (matriz_game (play 1 pilar (car matrix)))   ; Se llama a la función "matriz_game" con los argumentos 1, "pilar" y el primer elemento de "matrix"
                            (actualizacion_tablero)   ; Se llama a la función "actualizacion_tablero"
                            (interfaz_matrix (car matrix))   ; Se llama a la función "interfaz_matrix" con el primer elemento de "matrix"
                            (juega_IA_pc)   ; Se llama a la función "juega_IA_pc"
                            )))]))   

(send frame show #t)   ; Se muestra el frame

         

