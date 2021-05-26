#lang racket
;
;Descrip:
;Dom:
;Rec:
;Recursion:

;TDA Date
;date(dia,mes,año)

;constructor
;Descr: Funcion que genera una fecha
;Dom: Dia <int> X mes <int> X año <int>
;Rec: date
;Recursion: NA
(define date(lambda (dd mm yyyy) (list dd mm yyyy)))

;Pertenencia
;Descr: Funcion que verifica si los argumentos cumplen el formato fecha
;Dom: date
;Rec: Bool
;Recursion: NA
(define date? (lambda(fecha)
              (if(and(number? (getDay fecha))(getMonth fecha)(getYear fecha))
                 #t
                 #f
                 )))

;Selectores
(define getDay car)
(define getMonth cadr)
(define getYear caddr)




;Funcion encryptFn
;Descr: que permite encriptar/desencriptar un mensaje
;Dom: String
;Rec: String
;Recursion: NA 
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))


;TDA SocialNetwork
;(name, date, encryptFn decryptFn,usuarios, publicaciones,gente_que_participo_en_post,posts-compartidos-a-usuarios)


;Constructor
;Dom: String X Date X EncryptFunction X DecryptFunction
;Rec: SocialNetwork

(define socialnetwork(lambda(name date encryptFn decryptFn)
                       (if(and(esString? name)(date? date)(funcion? encryptFn)(funcion? decryptFn))
                          (list name date encryptFn decryptFn '() '() '() '())
                          '()
                          )
                       )
  )

(define socialnetworkActualizado (lambda(snName snDate fn1 fn2 users posts usersActivity posts-compartidos)
                                   (list snName snDate fn1 fn2 users posts usersActivity posts-compartidos)
  ))

;----------------------------------PERTENENCIA-----------------------
(define esString?(lambda(palabra)
                  (if (string? palabra) #t
                      #f)
                  )
  )

(define funcion? (lambda (funcion)
                   (if (procedure? funcion) #t
                       #f
                       )
                   )
  )

(define esNumero?(lambda(numero)
                  (if (number? numero) #t
                      #f)
                  )
  )

(define esRedSocial? (lambda(socialN)
                       (if(and
                           (esString? (get_snName socialN))
                           (date? (get_snDate socialN))
                           (funcion? (get_encryptFn socialN))
                           (funcion? (get_decryptFn socialN))
                           (son_UsuariosValidos? (get_snUsuarios socialN))
                           (sonPublicaciones? (get_snPublicaciones socialN))
                           ) #t
                             #f)
 
                       )

  )

(define son_UsuariosValidos?(lambda(lista_usuarios)
                              (if (null? lista_usuarios)#t
                                  (and (es_UserValido? (car lista_usuarios)) (son_UsuariosValidos? (cdr lista_usuarios)))

                                  )))

(define sonPublicaciones? (lambda (lista_publicaciones)
                            (if(null? lista_publicaciones)#t
                               (and (es_PublicacionValida? (car lista_publicaciones)) (sonPublicaciones? (cdr lista_publicaciones)))
                               )
                            )
  )

;-------------------------------SELECTORES-------------------------------
(define get_snName car)
(define get_snDate cadr)
(define get_encryptFn caddr)
(define get_decryptFn cadddr)
(define get_snUsuarios (lambda(sn)(car(cdr(cdr(cdr(cdr sn)))))))
(define get_snPublicaciones(lambda(sn)(car(cdr(cdr(cdr(cdr(cdr sn))))))))
(define get_snGente_que_participo_en_post(lambda(sn)(car(cdr(cdr(cdr(cdr(cdr(cdr sn)))))))))
(define get_snPosts_Compartidos(lambda(sn)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr sn))))))))))




;-----------------------------OTRAS FUNCIONES-----------------------------


;descr:funcion que agrega un elemento en la cabeza de la lista
;dom: list x number/string/list
;rec: list
;recursion: NA
;referencia: funcion vista en clase del profesor Gonzalo Martinez

(define agregar_cabeza
  (lambda (lista elemento)
    (cons elemento lista)))


;descr:funcion que agrega elementos a la cola de la lista
;dom: list x list/number/string
;rec: list
;recursion: natural
(define agregar_cola (lambda(lista elemento)
                            (if(empty? lista)
                               elemento
                               (cons (car lista) (agregar_cola (cdr lista) elemento))
                               )
                            ))



;descr: funcion que permite hacer un filtrado a partir de un predicado
;dom: predicado X list
;rec: list
;;Referencia: funcion vista en clase del profesor Gonzalo Martinez

(define (my-filter pred lst)
  (cond
    [(empty? lst) '()] ;null? null
    [(pred (car lst))  ;; si predicado es verdero, ejecuto la siguiente linea, caso contrario, ejecuto else
     (cons (car lst) (my-filter pred (cdr lst)))]
    [else (my-filter pred (cdr lst))]))

;; Descripción: Función que aplica una función fn a todos y cada uno de los elementos
;;              de una lista lst
;; Dom: Función a aplicar a una colección de datos <función>,
;;      Lista de datos <lista>
;; Rec: Nueva lista con cada elemento
;; Tipo de recursión: Natural
;; Referencia: funcion vista en clase del profesor Gonzalo Martinez
(define (mi-map funcion lista)
  (cond
    [(null? lista) null]
    [else (cons (funcion (car lista))
                (mi-map funcion (cdr lista)))]))

;Descripcion: Funcion que aplica el conector logico "and" a cada uno de los elementos de una lista
;Dom: list
;Rec: boolean
;Recursion: natural

(define mi-andmap(lambda(lista)
              (if (null? lista) #t   
             (and(car lista)(mi-andmap (cdr lista)))
                   )
  ))

;Descripcion: Funcion que verifica si un item se encuentra en la lista
;Dom: string/number X list
;Rec: boolean
;Recursion: NA
;Referencia: funcion vista en clase del profesor Gonzalo Martinez
(define member?
  (lambda (item lst)
    (cond
      [(null? lst) #f]
      [(equal? item (car lst)) #t]
      [else (member? item (cdr lst))])))


;descripcion: funcion que actualiza el socialnetwork con el amigo que agrega un usuario
(define sn-agregarAmigo (lambda (sn nombreUsuario amigos-que-agrega)
                          (socialnetworkActualizado
                           (get_snName sn)
                           (get_snDate sn)
                           (get_encryptFn sn)
                           (get_decryptFn sn)
                           (agregar_amigo nombreUsuario (get_snUsuarios sn) amigos-que-agrega)
                           (get_snPublicaciones sn)
                           (get_snGente_que_participo_en_post sn)

                                                    )
                          
                          )
  )


;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;-----------------------------TDA USER-------------------------------------

;user(sesionActiva,idUser,username,password,date,amigos,followers)
;donde amigos = (usernarme1,username2,....,usernameN)
;followers = ((date,IDPOST,username1),(date,IDPOST,username2,....))
;----------------------------------CONSTRUCTORES--------------------------------
;
;Descrip:funcion que crea un user
;Dom: sesionActiva <bool> X userID <number> X username <string> X password <string> X date X amigos
;Rec:user
;Recursion: NA

(define user(lambda(sesionActiva userID username password date amigos followers)
              (if (and (boolean? sesionActiva)
                       (esNumero? userID)
                       (esString? username)
                       (esString? password)
                       (date? date)
                       (listaAmigos_valida? amigos)
                        ) 
                       (list sesionActiva userID username password date amigos followers)
                  '()
                  )
              )
 
  )


;-------------------------------------PERTENENCIA-----------------------------
(define es_UserValido? (lambda(usuario)
                  (if
                    (and(boolean? (getUser_sesionActiva usuario))
                        (esNumero? (getUser_idUser usuario))
                        (esString?(getUser_username usuario))
                        (esString? (getUser_password usuario))
                        (date? (getUser_date usuario))
                        (listaAmigos_valida? (getUser_amigos usuario))) #t
                         #f)))
                   


  
(define listaAmigos_valida? (lambda (lista_amigos)
                              (if(empty? lista_amigos)#t
                                 (and(esString? (car lista_amigos))(listaAmigos_valida? (cdr lista_amigos)))
                                 )
                              ))
 
(define existe-usuario? (lambda(nombreUsuarioAVerificar contraseñaUsuario)
                          (lambda(usuario)
                            (and(eqv? nombreUsuarioAVerificar (getUser_username usuario)) (eqv? contraseñaUsuario (getUser_password usuario))))))

(define sesion-activa? (lambda (usuario)
                         (eqv? #t (getUser_sesionActiva usuario))
                         ))

(define son-amigos? (lambda(lista-amigos-usuario)
                      (lambda(personaAVerificar)
                      (member? personaAVerificar lista-amigos-usuario)
                      )))


;-----------------------------------MODIFICADORES-------------------------
(define set_sesionActiva_True (lambda(username lista_usuarios)
                                (if (eqv? username (getUser_username(car lista_usuarios)))
                                    ;caso base
                                    (cons (user
                                           #t
                                          (getUser_idUser (car lista_usuarios))
                                          (getUser_username(car lista_usuarios))
                                          (getUser_password(car lista_usuarios))
                                          (getUser_date(car lista_usuarios))
                                          (getUser_amigos(car lista_usuarios))
                                          (getUser_followers(car lista_usuarios))
                                          )
                                          (cdr lista_usuarios))
                                    ;caso recursivo
                                    (cons (car lista_usuarios)(set_sesionActiva_True username (cdr lista_usuarios)))
                                    
                                )
                                
  ))
       
(define set_sesionActiva_False (lambda(username lista_usuarios)
                                (if (eqv? username (getUser_username(car lista_usuarios)))
                                    ;caso base
                                    (cons (user
                                           #f
                                          (getUser_idUser (car lista_usuarios))
                                          (getUser_username(car lista_usuarios))
                                          (getUser_password(car lista_usuarios))
                                          (getUser_date(car lista_usuarios))
                                          (getUser_amigos(car lista_usuarios))
                                          (getUser_followers(car lista_usuarios))
                                          )
                                          (cdr lista_usuarios))
                                    ;caso recursivo
                                    (cons (car lista_usuarios)(set_sesionActiva_False username (cdr lista_usuarios)))
                                    
                                )
                                
  ))
  
                           

;---------------------------------SELECTORES----------------------------
(define getUser_sesionActiva car)
(define getUser_idUser cadr)
(define getUser_username caddr)
(define getUser_password cadddr)
(define getUser_date (lambda(usuario)
                    (car(cdr(cdr(cdr(cdr usuario)))))
                    ))

(define getUser_amigos (lambda(usuario)
                    (car(cdr(cdr(cdr(cdr (cdr usuario)))))
                    )))
 
(define getUser_followers (lambda(usuario)
                    (car(reverse usuario))))



(define getUsers_lastUser(lambda(usuarios)
                          (car(reverse usuarios))
                          )
  )

(define getUsers_lastID (lambda(usuarios)
                         (getUser_idUser(getUsers_lastUser usuarios ))
                         )
  )

(define getUser_sesionIniciada (lambda(usuarios)
                                 ;no existe un usuario con sesion iniciada
                                (if (null? usuarios) '()
                               (if(eqv? #t (getUser_sesionActiva(car usuarios)))
                                  (car usuarios)
                                  (getUser_sesionIniciada (cdr usuarios))
                               )
                                )))


;---------------------------------OTRAS FUNCIONES----------------------------

(define usuario-agregar-amigo(lambda(nombreUsuario lista_usuarios amigosNuevos)
                               
                               (if(eqv? nombreUsuario (getUser_username(car lista_usuarios)))
                                  (cons (user
                                         (getUser_sesionActiva (car lista_usuarios))
                                         (getUser_idUser (car lista_usuarios))
                                         (getUser_username (car lista_usuarios))
                                         (getUser_password (car lista_usuarios))
                                         (getUser_date (car lista_usuarios))
                                         (agregar_cola (getUser_amigos (car lista_usuarios)) amigosNuevos)
                                         (getUser_followers (car lista_usuarios))
                                         )
                                        
                                   (cdr lista_usuarios))
                                  (cons (car lista_usuarios)(usuario-agregar-amigo nombreUsuario (cdr lista_usuarios) amigosNuevos))
                                  )
                               ))



(define agregar_amigo(lambda(nombreUsuario sn amigosNuevos)
                          (socialnetworkActualizado
                           (get_snName sn)
                           (get_snDate sn)
                           (get_encryptFn sn)
                           (get_decryptFn sn)                       
                           (usuario-agregar-amigo nombreUsuario (get_snUsuarios sn) amigosNuevos)
                           (get_snPublicaciones sn)
                           (get_snGente_que_participo_en_post sn)
                           (get_snPosts_Compartidos sn)

                           )
                          ))


(define agregar_seguidor(lambda(nombreUsuario nombreNuevoSeguidor date lista_usuarios)
                          (if(eqv? nombreUsuario (getUser_username(car lista_usuarios)))
                             (cons (user (getUser_sesionActiva (car lista_usuarios))
                                         (getUser_idUser (car lista_usuarios))
                                         (getUser_username(car lista_usuarios))
                                         (getUser_password(car lista_usuarios))
                                         (getUser_date(car lista_usuarios))
                                         (getUser_amigos(car lista_usuarios))
                                         (agregar_cola (getUser_followers(car lista_usuarios)) (list (agregar_cabeza (list nombreNuevoSeguidor) date)))         
                                         )
                                   (cdr lista_usuarios)
                                   )
                             (cons (car lista_usuarios)(agregar_seguidor nombreUsuario nombreNuevoSeguidor date (cdr lista_usuarios)))
                             )))
                          
                          
;(agregar_cola (list (agregar_cabeza (list "a" "b") (date 1 1 1111))) (list (agregar_cabeza (list "c" "d") (date 2 2 2222))))
;SALIDA = (((1 1 1111) "a" "b") ((2 2 2222) "c" "d"))
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;------------------------------TDA PUBLICACION------------------------------
;publicacion(idPost,date,autorPost,contenido,reacciones,comments)
;donde contenido = mensaje encriptado
;reacciones (username1,username2,...usernameN) -> usernamei = string
;comments = TDA COMMENT

;------------------------------CONSTRUCTOR----------------------------------
;Descripcion: funcion que crea una publicacion
;Dom: int X date X string X string X string 
;Rec: publicacion
;Recursion: NA

(define inicializarPublicacion(lambda(idPost date autorPost contenido)
                               (if(and(esNumero? idPost) (date? date) (esString? autorPost) (esString? contenido))
                                   (list idPost date autorPost contenido '() '())
                                  '())
                                )

  )



 ;primero se crea la publicacion usando inicializarPublicacion
(define sn-agregarPublicacion (lambda (publicacionNueva lista-publicaciones)
                                (if(null? lista-publicaciones)
                                   (agregar_cabeza lista-publicaciones publicacionNueva)
                                 (if (eqv? (getPost_id publicacionNueva) (getPost_id(car lista-publicaciones)))
                                    (cons publicacionNueva (cdr lista-publicaciones))
                                    (cons (car lista-publicaciones) (sn-agregarPublicacion publicacionNueva (cdr lista-publicaciones)))

                                )
                                )
  )
  )




;------------------------------SELECTORES----------------------------------
(define getPost_id car)
(define getPost_date cadr)
(define getPost_autor caddr)
(define getPost_contenido cadddr)
(define getPost_reacciones (lambda(p)(car(cdr(cdr(cdr(cdr p)))))))
(define getPost_comments(lambda(p)(car(cdr(cdr(cdr(cdr(cdr p))))))))



(define getPost_lastPost(lambda(publicaciones)
                          (car(reverse publicaciones))
                          )
  )

(define getPost_lastID (lambda(publicaciones)
                         (getPost_id(getPost_lastPost publicaciones))
                         )
  )
;------------------------------PERTENENCIA----------------------------------
(define es_PublicacionValida? (lambda(publicacion)
                                (if(null? publicacion) #t
                                (and(esNumero? (getPost_id publicacion))
                                    (date? (getPost_date publicacion))
                                    (esString? (getPost_autor publicacion))
                                    (esString? (getPost_contenido publicacion))
                                    (list? (getPost_reacciones publicacion))
                                    (list? (getPost_comments publicacion))
                                 ))
                                ))
;------------------------------MODIFICADORES----------------------------------

;------------------------------OTRAS FUNCIONES----------------------------------
;user-que-reacciona = (user1,user2,.....)
(define agregar-nueva-reaccion(lambda(idPost lista_publicaciones user-que-reacciona)
                                (if(eqv? idPost (getPost_id(car lista_publicaciones)))
                                   (cons(inicializarPublicacion idPost
                                                     (getPost_date (car lista_publicaciones))
                                                     (getPost_autor (car lista_publicaciones))
                                                     (getPost_contenido (car lista_publicaciones))
                                                     (agregar_cola (getPost_reacciones (car lista_publicaciones)) user-que-reacciona)
                                                     (getPost_comments (car lista_publicaciones))
                                         )
                                    (cdr lista_publicaciones))
                                   '()
                                   )
                                )

  )

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;------------------------------TDA COMMENT----------------------------------
;comment(idPost,date,contenido,((autorComentario1,date,contenido),(autorComentario2,date,contenido),....,(autorComentarioN,date,contenido))

;;CONSTRUCTOR
;;un comentario tiene 
;;(idpost+1,autorPost,fecha,mensajedescriptivo,((autor,fecha,mensajedescriptivo),(autor,fecha,mensajedescriptivo)))
;este es el primer comentario que se le agrega a una publicacion
(define primerComentario(lambda(lista-publicaciones date postID mensajeDescriptivo)
                          (if(eqv? postID (getPost_id(car lista-publicaciones)))
                             (if (and (date? date) (esNumero? postID) (esString? mensajeDescriptivo))
                                 (cons (agregar_cola (car lista-publicaciones)
                                                     (list postID (getPost_autor (car lista-publicaciones)) date mensajeDescriptivo '()))
                                       (cdr lista-publicaciones))
                                 '()
                                 )
                           (primerComentario (cdr lista-publicaciones) date postID mensajeDescriptivo)
                           )
                          )
  )

;SELECTORES
(define getComment_ID car)
(define getCommet_autor cadr)
(define getComment_date caddr)
(define getComment_content cadddr)
(define getComment_comments (lambda (c)(car(cdr(cdr(cdr(cdr c)))))))
(define getComments_lastID(lambda(comentarios)
                            (getComment_ID(car(reverse comentarios)))
                            ))

;CAMBIAR DE LADO
;descripcion: funcion que busca la ultima id en base a las publicaciones y comentarios que contiene dicha publicacion
(define getLastID (lambda(publicaciones)
                    (if (null? (getPost_comments (getPost_lastPost publicaciones)) )
                              (getPost_lastID publicaciones)
                              (if(null? (getComment_comments(getPost_comments(getPost_lastPost publicaciones))))
                                        (getComment_ID (getPost_comments(getPost_lastPost publicaciones)))
                                        (getComments_lastID (getComment_comments(getPost_comments(getPost_lastPost publicaciones)))
                                        )
                              )
  )))
;------------------------------------------------------------------------
;DEFINITIVA
;TDA PARTICIPACION POST
;donde R: valor actual que tienen las participaciones
;se usa para el post/share

(define noExistePostParticipado? (lambda(idPost lista-personas-que-participan-en-post)
                                 (if(null? lista-personas-que-participan-en-post)#t
                                    (if(eqv? idPost (car(car lista-personas-que-participan-en-post)))#f
                                       (noExistePostParticipado? idPost  (cdr lista-personas-que-participan-en-post)
                                    )
                                 ))))
;se usa cuando ya existe la idPost con participantes
;los nuevos participantes los agrega a la cola
(define añadir-participante-por-idPost (lambda(idPost lista-personas participante)
                                         (if(eqv? idPost (car(car lista-personas)))
                                            (cons (agregar_cola (car lista-personas) participante) (cdr lista-personas))
                                            (cons(car lista-personas) (añadir-participante-por-idPost idPost (cdr lista-personas) participante))
                                         )))

(define agregarNuevaParticipacion(lambda(idPost lista-participantes nuevos-participantes)
                                   (if(null? lista-participantes)
                                      ;la lista esta vacia, por lo tanto corresponde a la primera
                                      (agregar_cabeza lista-participantes (agregar_cabeza nuevos-participantes idPost))
                                      ;se verifica si ya existe una lista de participantes con una idPOST en particular
                                      (if(noExistePostParticipado? idPost lista-participantes)
                                      (agregar_cola lista-participantes (list(agregar_cabeza nuevos-participantes idPost))
                                      
                                      )
                                      (añadir-participante-por-idPost idPost lista-participantes nuevos-participantes)
                                      ))))
#|
(define usuarios (list "user1" "user2" "user3"))
(define a (agregarNuevaParticipacion 4 '() usuarios))
(define b(agregarNuevaParticipacion 5 a (list "user7" "user8" "user9")))
(agregarNuevaParticipacion 5 b (list "user0" "userA" "userb"))
salida = '((4 "user1" "user2" "user3") (5 "user7" "user8" "user9" "user0" "userA" "userb"))|#
;--------------------------FUNCIONES OBLIGATORIAS--------------------------------------



;--------------------EJEMPLO DE COMO USAR LAS FUNCIONES OBLIGATORIAS-------------------------------------

;-------------------------------------------FUNCION REGISTER--------------------------------------
;PRIMERO SE DEFINE LA RED SOCIAL--> EN ESTE CASO LA RED SOCIAL SE ENCUENTRA EN emptyFB
;(define emptyFB (socialnetwork "fb" (date 25 10 2021) encryptFn encryptFn))

;SE REGISTRAN LOS USUARIOS EN LA RED SOCIAL CREADA
#|(define accionRegistrar(register (register (register emptyFB (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2"
"pass2") (date 25 10 2021) "user3" "pass3"))
|#

;AÑADIR AMIGOS --> SE REQUIERE ANTES DE HACER UN POST YA QUE SE DEBE VERIFICAR SI A LAS PERSONAS A LAS QUE VA DIRIGIDAS EL POST
;SE ENCUENTRAN EN LA LISTA DE AMIGOS DEL USUARIO

;(define accionAgregarAmigos (agregar_amigo "user1" accionRegistrar (list "alejo" "valentina")))
;(define accionAgregarAmigos2(agregar_amigo "user3" accionAgregarAmigos (list "elviejo" "carlitox")))
;(define agregarAmigos3(agregar_amigo "user2" accionAgregarAmigos2 (list "matias" "gregory")))

;--------------------------------------------FUNCION POST--------------------------------------------
;CASO POST MISMO USUARIO
;(((login accionAgregarAmigos "user1" "pass1" post)(date 30 10 2020)) "mi primer post")

;CASO POST DIRIGIDO A AMIGOS
;usando el login anterior
;(define login1 (((login accionAgregarAmigos "user1" "pass1" post)(date 30 10 2020)) "mi primer post" "alejo" "valentina"))
;(define login2(((login login1 "user2" "pass2" post)(date 11 22 2020)) "segundo post"))

;-------------------------------------------FUNCION FOLLOW-------------------------------------------
;USUARIO1 SIGUE A USUARIO2
#|
(define follow1(((login login2 "user1" "pass1" follow) (date 30 10 2020))
 "user2"))

|#
;
;USUARIOS1 SIGUE A USUARIO3 Y USUARIO3 SIGUE A USUARIO2
;(define follow2(((login follow1 "user1" "pass1" follow)(date 25 5 2021))"user3"))
;(define follow3(((login follow2 "user3" "pass3" follow)(date 15 02 2021))"user2"))

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------REGISTER----------------------------------------------------
;Descripcion: funcion que registra a un nuevo usuario en la red social
;Dom: socialnetwork X date X string X string
;Rec: socialnetwork
;encabezado = (register socialnetwork date username password)


(define register(lambda(socialnetwork date username password)
                   (if(and(esRedSocial? socialnetwork) (date? date) (esString? username) (esString? password))
                      (if(null? (my-filter (existe-usuario? username password) (get_snUsuarios socialnetwork)))
                         (if (null? (get_snUsuarios socialnetwork))
                             ;primero usuario en registrarse
                             (socialnetworkActualizado (get_snName socialnetwork)
                                                   (get_snDate socialnetwork)
                                                   (get_encryptFn socialnetwork)
                                                   (get_decryptFn socialnetwork)
                                                   (agregar_cola (get_snUsuarios socialnetwork)(list(user #f 0 username password date '() '())))
                                                   (get_snPublicaciones socialnetwork)
                                                   (get_snGente_que_participo_en_post socialnetwork)
                                                   (get_snPosts_Compartidos socialnetwork)
                                                   )
                             ;ya existe 1 o mas usuarios
                             (socialnetworkActualizado (get_snName socialnetwork)
                                                   (get_snDate socialnetwork)
                                                   (get_encryptFn socialnetwork)
                                                   (get_decryptFn socialnetwork)
                                                   (agregar_cola (get_snUsuarios socialnetwork)
                                                                 (list(user #f
                                                                       (+ 1 (getUsers_lastID (get_snUsuarios socialnetwork)))
                                                                       username
                                                                       password
                                                                       date
                                                                       '()
                                                                       '())))
                                                   (get_snPublicaciones socialnetwork)
                                                   (get_snGente_que_participo_en_post socialnetwork)
                                                   (get_snPosts_Compartidos socialnetwork)
                                                   )
                             


                             )
                       
                         
                         "este usuario ya existe"



                         )
                      "parametro(s) no valido(s)"
                      )
                    )

  )

;--------------------------LOGIN----------------------------------------------------
;Descripcion: Función que permite autenticar a un usuario registrado iniciar sesión
;y junto con ello permite la ejecución de comandos concretos dentro de la red social.

;Dom:socialnetwork X string X string X function
;Rec:function
;Encabezado = (login socialnetwork username password operation)

(define login (lambda(socialnetwork username password operation)
                (if (and (esRedSocial? socialnetwork) (esString? username) (esString? password) (funcion? operation))
                    
                (if (not(null? (my-filter (existe-usuario? username password) (get_snUsuarios socialnetwork))))
                    (operation (socialnetworkActualizado (get_snName socialnetwork)
                                                   (get_snDate socialnetwork)
                                                   (get_encryptFn socialnetwork)
                                                   (get_decryptFn socialnetwork)
                                                   (set_sesionActiva_True username (get_snUsuarios socialnetwork))
                                                   (get_snPublicaciones socialnetwork)
                                                   (get_snGente_que_participo_en_post socialnetwork)
                                                   (get_snPosts_Compartidos socialnetwork)
                                                   ))
                    operation
                )
                "parametros no validos")))

;--------------------------POST----------------------------------------------------
#|(define bimbo(lambda (a b c)(list a b c)))
(define a(lambda(valor1)(lambda(valor2 . valor3)
                          (bimbo valor1 valor2 valor3)
                          )))
salida a = ((a 1)"hola" "alejo" "valentina")


(define getUser_sesionIniciada (lambda(usuarios)
                                (if (null? usuarios) '()
                               (if(eqv? #t (getUser_sesionActiva(car usuarios)))
                                  (car usuarios)
                                  (getUser_sesionIniciada (cdr usuarios))
                               )
                                )))

(define son-amigos? (lambda(lista-amigos-usuario)
                      (lambda(personaAVerificar)
                      (member? personaAVerificar lista-amigos-usuario)
                      )))

|#
;Descripcion: Función que permite a un usuario con sesión iniciada en
;la plataforma realizar una nueva publicación propia o dirigida a otros usuarios
;Dom: socialnetwork
;rec: socialnetwork
                                  
(define post(lambda (sn)
              (lambda(date)(lambda(content . users)
              (if(and(esRedSocial? sn) (date? date) (esString? content))
              ;se verifica que exista un usuario con sesion iniciada
              (if(not(null? (getUser_sesionIniciada(get_snUsuarios sn))))
                 ;esta condicion verifica si el post es dirigido a otros usuarios
                 ;null = post dirigido a mismo usuario
                 ;not null =  post dirigido a otros usuarios incluyendo al que lo creo
                 (if(not(null? users))
                    ;ahora se verifica si los usuarios ingresados pertenecen a la lista de amigos de la persona que realiza
                    ;el post
                    (if(mi-andmap(mi-map
                                  (son-amigos? (getUser_amigos(getUser_sesionIniciada(get_snUsuarios sn)))) users ))
                       ;es el primer post que se hace en la red social?           
                       (if(null? (get_snPublicaciones sn))        
                          (socialnetworkActualizado
                           (get_snName sn)
                           (get_snDate sn)
                           (get_encryptFn sn)
                           (get_decryptFn sn)
                           (set_sesionActiva_False (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) (get_snUsuarios sn))
                           (sn-agregarPublicacion (inicializarPublicacion 0 date (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) ((get_encryptFn sn)content))          
                                                  (get_snPublicaciones sn))
                           (agregarNuevaParticipacion 0 (get_snGente_que_participo_en_post sn) users  )
                           (get_snPosts_Compartidos sn)
                           )
                       ;existe una o mas publicaciones
                           (socialnetworkActualizado
                           (get_snName sn)
                           (get_snDate sn)
                           (get_encryptFn sn)
                           (get_decryptFn sn)
                           (set_sesionActiva_False (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) (get_snUsuarios sn))
                           (sn-agregarPublicacion (inicializarPublicacion (+ 1 (getLastID (get_snPublicaciones sn))) date (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) ((get_encryptFn sn)content))                         
                                                  (get_snPublicaciones sn) )

                           (agregarNuevaParticipacion (+ 1 (getLastID (get_snPublicaciones sn))) (get_snGente_que_participo_en_post sn) users)
                            (get_snPosts_Compartidos sn)
                           )
                          )

                       
                       ;las personas ingresadas no se encuentran en la lista de amigos del user actual
                       sn
                       )
                    
                    ;publicacion dirigida al mismo usuario
                    ;es primera publicacion que se hace en red social?
                    (if(null? (get_snPublicaciones sn))
                          (socialnetworkActualizado
                           (get_snName sn)
                           (get_snDate sn)
                           (get_encryptFn sn)
                           (get_decryptFn sn)
                           (set_sesionActiva_False (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) (get_snUsuarios sn))
                           (sn-agregarPublicacion (inicializarPublicacion 0 date (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))  ((get_encryptFn sn)content))        
                                                  (get_snPublicaciones sn))
                           (agregarNuevaParticipacion 0 (get_snGente_que_participo_en_post sn) (list (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))))
                            (get_snPosts_Compartidos sn)
                           )

                          ;existe una publicacion
                          (socialnetworkActualizado
                           (get_snName sn)
                           (get_snDate sn)
                           (get_encryptFn sn)
                           (get_decryptFn sn)
                           (set_sesionActiva_False (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) (get_snUsuarios sn))
                           (sn-agregarPublicacion (inicializarPublicacion (+ 1 (getLastID (get_snPublicaciones sn))) date (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) ((get_encryptFn sn)content))
                                                  (get_snPublicaciones sn))

                           (agregarNuevaParticipacion (+ 1 (getLastID (get_snPublicaciones sn))) (get_snGente_que_participo_en_post sn)  (list (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))))
                            (get_snPosts_Compartidos sn)
                           )
                

                    
                    )
                    )


                 ;no se encontro algun usuario con sesion iniciada
                 sn
                 )
                ;la red social ingresada no es valida
              '()
                )


              )
  )))

                               
;--------------------------FOLLOW----------------------------------------------------    
;descripcion: funcion que permite a un usuario con sesion iniciada en la plataforma seguir a otro usuario
;dom: social network
;recorrido: socialnetwork
(define follow(lambda(sn)
         (lambda(date)
           (lambda(user)
             ;se verifica que exista una sesion activa
             (if(and(esRedSocial? sn) (esString? user) (date? date))
             (if(not(null? (getUser_sesionIniciada(get_snUsuarios sn))))
                ;se verifica que el usuario a seguir con el usuario login no sean iguales
                (if (not(eqv? user (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))))
                    (socialnetworkActualizado
                     (get_snName sn)
                     (get_snDate sn)
                     (get_encryptFn sn)
                     (get_decryptFn sn)
                     (set_sesionActiva_False (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))
                                             (agregar_seguidor user (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) date (get_snUsuarios sn)))
                     
                     
                     (get_snPublicaciones sn)
                     (get_snGente_que_participo_en_post sn)
                     (get_snPosts_Compartidos sn)
                     )
                    "un usuario no puede seguirse a si mismo"
                    )
                ;no se ha encontrado una sesion activa
                sn
                )
             ;la red social recibida como entrada no es valida
             '()
             )
             )

             ))
              )
  

