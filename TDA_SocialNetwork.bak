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
;(name, date, encryptFn decryptFn,usuarios, publicaciones)


;Constructor
;Dom: String X Date X EncryptFunction X DecryptFunction
;Rec: SocialNetwork

(define socialnetwork(lambda(name date encryptFn decryptFn)
                       (if(and(esString? name)(date? date)(funcion? encryptFn)(funcion? decryptFn))
                          (list name date encryptFn decryptFn '() '())
                          '()
                          )
                       )
  )

;Pertenencia
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

(define sonPublicaciones? (lambda (x)#t))
;sonusuarios?
     ;esusuario?
;sonpublicaciones?
     ;espublicacion?

;LINEA 126 METER TODO EL IF EN UNA FUNCION DE PERTENENCIA 
;(DEFINE ESREDSOCIAL?)

;Selectores
(define get_snName car)
(define get_snDate cadr)
(define get_encryptFn caddr)
(define get_decryptFn cadddr)
(define get_snUsuarios (lambda(sn)(car(cdr(cdr(cdr(cdr sn)))))))
(define get_snPublicaciones(lambda(sn)(car(cdr(cdr(cdr(cdr(cdr sn))))))))



;TDA user
;user(sesionActiva,idUser,username,password,date,amigos,publicaciones_que_participa)
;donde amigos = (user1,user2,....,userN)
;publicaciones_que_participa= (idPublicacion1,idPublicacion2,......,idPublicacionN)

;constructor
;
;Descrip:funcion que crea un user
;Dom: sesionActiva <bool> X userID <number> X username <string> X password <string> X date
;Rec:user
;Recursion: NA
(define user(lambda(sesionActiva userID username password date)
              (if (and (boolean? sesionActiva)(esNumero? userID) (esString? username) (esString? password) (date? date)) 
                  (list sesionActiva userID username password date '() '())
                  '()
                  )
              )
 
  )
;pertenencia
;sonusuarios?
     ;esusuario?
(define es_UserValido? (lambda(usuario)
                  (if
                    (and(boolean? (getUser_sesionActiva usuario))
                        (esNumero? (getUser_idUser usuario))
                        (esString?(getUser_username usuario))
                        (esString? (getUser_password usuario))
                        (date? (getUser_date usuario))) #t
                         #f)))
                   

(define son_UsuariosValidos?(lambda(lista_usuarios)
                              (if (null? lista_usuarios)#t
                                  (and (es_UserValido? (car lista_usuarios)) (son_UsuariosValidos? (cdr lista_usuarios)))

                                  )))

;modificadores

;selectores
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

;user(sesionActiva,idUser,username,password,date,amigos,publicaciones_que_participa)
(define getUser_publicaciones (lambda(usuario)
                    (car(cdr(cdr(cdr(cdr (cdr(cdr usuario)))))))
                    ))


;TDA USERS?
(define getUser_lastUser(lambda(usuario)
                          (car(reverse usuario))
                          )
  )
(define getUser_lastID (lambda(usuario)
                         (getUser_idUser(getUser_lastUser usuario ))
                         )
  )


