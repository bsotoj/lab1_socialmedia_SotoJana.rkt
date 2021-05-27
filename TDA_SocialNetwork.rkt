#lang racket
(provide (all-defined-out))

;LOS EJEMPLOS DE USO DE LAS FUNCIONES OBLIGATORIAS SE ENCUENTRAN EN main.rkt

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
;date(dia,mes,año)
(define getDay car)
(define getMonth cadr)
(define getYear caddr)




;Funcion encryptFn y decryptFn
;Descr: que permite encriptar/desencriptar un mensaje
;Dom: String
;Rec: String
;Recursion: NA 
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))





;TDA SocialNetwork
;socialnetwork(name, date, encryptFn decryptFn,usuarios, publicaciones,gente_que_participo_en_post,posts-compartidos-a-usuarios)


;Constructor
;Dom: String X Date X EncryptFunction X DecryptFunction
;Rec: SocialNetwork
;recursion: NA
;ejemplo de uso: (socialnetwork "fb" (date 12 1 2000) encryptFunction decryptFunction)
#|

 el tda socialnetwork ademas de incluir los parametros de entrada con los que inicializa, se le agregan otros 4 que son
users, posts, usersActivity posts-compartidos.


*users contiene toda la informacion relacionada con un usuario esto es -> el estado de su sesion
, la id que tiene, nombre de usuario, contraseña, date (en que fecha se creo dicho usuario), amigos (se utilizan en funciones posteriores
que basicamente es para indicar si a las personas a las que va dirigido un post son amigos del usuario que lo envia) y los seguidores
que tiene dicho usuario

*publicacion es usado a la hora de crear un post,cada publicacion contine la id del post, date, el autor del post, contenido/mensajedescriptivo
,reacciones (su uso es para las funciones opcionales) y comentarios.

*usersActivity se utiliza al hacer un post, que indica el idPost y a los usuarios que fueron
dirigidos -> (0 alejo valentina) -> el post con id 0 fue dirigido a alejo y valentina

*posts-compartidos se usa al operar con un share, donde cada posicion de posts-compartidos contiene
(idPost date usuarios), por lo tanto, en usersActivity y posts-compartidos no habria problema para saber
cual es el post en el que participaron/se les compartio y el autor original de la publicacion, ya que el id
y el nombre del autor se incluyen en el TDA PUBLICACION

|#
(define socialnetwork(lambda(name date encryptFn decryptFn)
                       (if(and(esString? name)(date? date)(funcion? encryptFn)(funcion? decryptFn))
                          (list name date encryptFn decryptFn '() '() '() '())
                          '()
                          )
                       )
  )

;descripcion: funcion que genera un nuevo socialnetwork incluyendo los nuevos parametros con los que inicializo socialnetwork
;dom: string X date X encryptFn X decryptFn X list X list X list X list
;recorrido: socialnetwork
;recursion: NA
;ejemplo de uso: las funciones obligatorias lo utilizan de forma interna
(define socialnetworkActualizado (lambda(snName snDate fn1 fn2 users posts usersActivity posts-compartidos)
                                   (list snName snDate fn1 fn2 users posts usersActivity posts-compartidos)
  ))

;----------------------------------PERTENENCIA-----------------------
;descripcion:funcion que verifica si un argumento es string
;dom: palabra
;recorrido: boolean
;recursion: NA
(define esString?(lambda(palabra)
                  (if (string? palabra) #t
                      #f)
                  )
  )

;descripcion: funcion que verifica si un argumento es un procedure/funcion
;dom: funcion
;recorrido: boolean
;recursion: NA
(define funcion? (lambda (funcion)
                   (if (procedure? funcion) #t
                       #f
                       )
                   )
  )
;descripcion: funcion que verifica si un argumento es un numero
;dom: numero
;recorrido: boolean
;recursion: NA
(define esNumero?(lambda(numero)
                  (if (number? numero) #t
                      #f)
                  )
  )

;descripcion:funcion que verifica si un argumento es una red social
;dom: socialN
;recorrido: boolean
;recursion: NA
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
;descripcion: funcion que verifica si la lista recibida como argumento son usuarios validos
;dom: lista_usuarios 
;recorrido: boolean
;recursion: natural

(define son_UsuariosValidos?(lambda(lista_usuarios)
                              (if (null? lista_usuarios)#t
                                  (and (es_UserValido? (car lista_usuarios)) (son_UsuariosValidos? (cdr lista_usuarios)))

                                  )))
;descripcion:funcion que verifica si la lista recibida como argumento son publicaciones validas
;dom: lista_publicaciones
;recorrido: boolean
;recursion: natural
(define sonPublicaciones? (lambda (lista_publicaciones)
                            (if(null? lista_publicaciones)#t
                               (and (es_PublicacionValida? (car lista_publicaciones)) (sonPublicaciones? (cdr lista_publicaciones)))
                               )
                            )
  )

;-------------------------------SELECTORES-------------------------------
;ingresar a las distintas partes de socialnetwork
;estructura: socialnetwork(snName snDate fn1 fn2 users posts usersActivity posts-compartidos)
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
;dom: socialnetwork X string X list
;recorrido: socialnetwork
;recursion: NA
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
;----------------------------------CONSTRUCTOR--------------------------------
;
;Descrip:funcion que crea un user
;Dom: boolean X number X string X string X date X list
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
;descripcion:funcion que verifica si el argumento es un usuario valido
;dom: usuario (list)
;recorrido: boolean
;rec:NA
(define es_UserValido? (lambda(usuario)
                  (if
                    (and(boolean? (getUser_sesionActiva usuario))
                        (esNumero? (getUser_idUser usuario))
                        (esString?(getUser_username usuario))
                        (esString? (getUser_password usuario))
                        (date? (getUser_date usuario))
                        (listaAmigos_valida? (getUser_amigos usuario))) #t
                         #f)))
                   


;descripcion: funcion que verifica si la lista de amigos de un usuario es valida
;dom:list
;recorrido:boolean
;rec: natural
(define listaAmigos_valida? (lambda (lista_amigos)
                              (if(empty? lista_amigos)#t
                                 (and(esString? (car lista_amigos))(listaAmigos_valida? (cdr lista_amigos)))
                                 )
                              ))
;descripcion:funcion currificada que verifica la existencia de un usuario
;dom:string X string X list
;recorrido: boolean
;rec:NA
(define existe-usuario? (lambda(nombreUsuarioAVerificar contraseñaUsuario)
                          (lambda(usuario)
                            (and(eqv? nombreUsuarioAVerificar (getUser_username usuario)) (eqv? contraseñaUsuario (getUser_password usuario))))))

;descripcion:funcion que verifica si la sesion de un usuario esta activa
;dom:list
;recorrido:boolean
;rec:NA
(define sesion-activa? (lambda (usuario)
                         (eqv? #t (getUser_sesionActiva usuario))
                         ))
;descripcion:funcion currificada que verifica si una persona se encuentra en la lista de amigos de un usuario
;dom:list X string
;recorrido:boolean
;rec:NA
(define son-amigos? (lambda(lista-amigos-usuario)
                      (lambda(personaAVerificar)
                      (member? personaAVerificar lista-amigos-usuario)
                      )))


;-----------------------------------MODIFICADORES-------------------------

;descripcion:funcion que modifica el estado de sesion de un usuario a activo
;dom:string X (lista X listas)
;recorrido:lista X listas
;rec:natural
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
;descripcion:funcion que modifica el estado de sesion de un usuario a inactivo
;dom:string X (lista X listas)
;recorrido:lista X listas
;rec:natural       
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
;selectores que permiten ingresar a los distintos componentes de un usuario
;user(sesionActiva,idUser,username,password,date,amigos,followers)
;donde amigos = (usernarme1,username2,....,usernameN)
;followers = ((date,IDPOST,username1),(date,IDPOST,username2,....))
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


;descripcion:funcion que obtiene el ultimo usuario de una lista de usuarios
;dom: users
;recorrido:user
;rec:NA
(define getUsers_lastUser(lambda(usuarios)
                          (car(reverse usuarios))
                          )
  )
;descripcion:funcion que obtiene la ID del ultimo usuario
;dom:users 
;recorrido: number
;rec:NA
(define getUsers_lastID (lambda(usuarios)
                         (getUser_idUser(getUsers_lastUser usuarios ))
                         )
  )
;descripcion:funcion que obtiene al usuario con sesion activa de una lista de usuarios
;dom:users
;recorrido:user
;rec:cola
(define getUser_sesionIniciada (lambda(usuarios)
                                 ;no existe un usuario con sesion iniciada
                                (if (null? usuarios) '()
                               (if(eqv? #t (getUser_sesionActiva(car usuarios)))
                                  (car usuarios)
                                  (getUser_sesionIniciada (cdr usuarios))
                               )
                                )))


;---------------------------------OTRAS FUNCIONES----------------------------
;descripcion:funcion que permite añadir a la lista de amigos de un usuario una nueva persona
;dom:string X users X list (contiene strings)
;recorrido: users
;rec:natural
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


;descripcion: funcion que agrega amigos a un usuario especifico y actualiza la socialnetwork con dicho cambio
;dom:string X socialnetwork X list
;recorrido:socialnetwork
;rec:na
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

;descripcion:funcion que agregar un nuevo seguidor a un usuario
;dom:string X string X date X users
;recorrido: users
;rec:natural
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
;descripcion:funcion que agrega una publicacion nuevo a la lista de publicaciones
;dom: publicacion X publicaciones
;recorrido: publicaciones
;rec:natural
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
;selectores para acceder a las distintas partes de una publicacion
;publicacion(idPost,date,autorPost,contenido,reacciones,comments)
;donde contenido = mensaje encriptado
;reacciones (username1,username2,...usernameN) -> usernamei = string
;comments = TDA COMMENT

(define getPost_id car)
(define getPost_date cadr)
(define getPost_autor caddr)
(define getPost_contenido cadddr)
(define getPost_reacciones (lambda(p)(car(cdr(cdr(cdr(cdr p)))))))
(define getPost_comments(lambda(p)(car(cdr(cdr(cdr(cdr(cdr p))))))))


;descripcion:funcion que obtiene la ultima publicacion en la lista de publicaciones
;dom:publicaciones
;recorrido:publicacion
;rec:NA
(define getPost_lastPost(lambda(publicaciones)
                          (car(reverse publicaciones))
                          )
  )
;descripcion:funcion que obtiene la ID del ultimo post
;dom:publicaciones
;recorrido:number
;rec:NA
(define getPost_lastID (lambda(publicaciones)
                         (getPost_id(getPost_lastPost publicaciones))
                         )
  )

;ESTA FUNCION ORIGINALMENTE FUE PENSADA PARA BUSCAR LA ID EN UNA PUBLICACION, YA QUE DENTRO DE UNA PUBLICACION
;HAY UN COMENTARIO (QUE TIENE UNA ID), Y DENTRO DE ESE COMENTARIO HAY MAS COMENTARIOS CON SU PROPIA ID (GENERADO CON LA ID
;DEL ULTIMO COMENTARIO O SI ES EL PRIMERO CON LA IDPOST)
;PERO POR TEMAS DE TIEMPO SOLO SE IMPLEMENTO EN LAS PUBLICACIONES

;descripcion: funcion que busca la ultima id en base a las publicaciones hechas
;dom:publicaciones
;recorrido:number
;rec:NA
(define getLastID (lambda(publicaciones)
                              (getPost_lastID publicaciones)
                              ))
;------------------------------PERTENENCIA----------------------------------
;descripcion:funcion que verifica si una publicacion es valida
;dom:publicacion
;recorrido:boolean
;rec:NA
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
;descripcion:funcion currificada que verifica si la publicacion fue creada por el usuario recibido como argumento
;dom:string X publicacion
;recorrido:boolean
;rec:NA
(define es_publicacionUsuario?(lambda (nombreUsuario)
                                (lambda(publicacion)
                                  (eqv? nombreUsuario (getPost_autor publicacion))
                                  )
                                ))


;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;------------------------------TDA PARTICIPACION POST----------------------------------
;PARTICIPACIONES SE USA CUANDO SE HACE UN POST, COMO UN POST VA DIRIGIDO A OTROS USUARIOS O AL MISMO USUARIO,
;Y PARA EVITAR SEGUIR COMPLICANDO EL TDA PUBLICACION, SE HACE ESTE TDA EL CUAL CONTIENE LA ID DEL POST Y A LAS PERSONAS
;QUE FUE DIRIGIDA DICHO POST.
;ES MAS SENCILLO DE ESTA FORMA VER LA IDPOST PARA VER EL POST ORIGINAL CON SU CONTENIDO Y A LAS PERSONAS A LAS QUE FUE DIRIGIDO.
;
;participacion((ID username1 username2),(ID username3 username4),....)

;PERTENENCIA
;descripcion:funcion que verifica si existe una participacion con la id que se recibe como argumento
;dom: number X participaciones
;recorrido: boolean
;rec:cola

(define noExistePostParticipado? (lambda(idPost lista-personas-que-participan-en-post)
                                 (if(null? lista-personas-que-participan-en-post)#t
                                    (if(eqv? idPost (car(car lista-personas-que-participan-en-post)))#f
                                       (noExistePostParticipado? idPost  (cdr lista-personas-que-participan-en-post)
                                    )
                                 ))))
;descripcion:funcion currificada que verifica si un usuario pertenece a la participacion
;dom: string X participacion
;recorrido:boolean
;rec:NA

(define usuarioParticipa?(lambda(nombreUsuario)
                           (lambda(participacion)
                             (member? nombreUsuario participacion)
                             )
                           ))

;OTRAS FUNCIONES
;añadir-participante-por-idPost se usa cuando ya existe la idPost con participantes
;los nuevos participantes los agrega a la cola

;descripcion: funcion que agrega a un participante a participaciones a traves de un IDPost
;dom:number X participaciones X string
;recorrido: participaciones
;rec:natural

(define añadir-participante-por-idPost (lambda(idPost lista-personas participante)
                                         (if(eqv? idPost (car(car lista-personas)))
                                            (cons (agregar_cola (car lista-personas) participante) (cdr lista-personas))
                                            (cons(car lista-personas) (añadir-participante-por-idPost idPost (cdr lista-personas) participante))
                                         )))
;CONSTRUCTOR
;descripcion:funcion que crea una nueva participacion y la agrega al TDA PARTICIPACION POST
;dom:number X participantes X list
;recorrido:participantes
;rec:NA

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



