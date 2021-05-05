#lang racket

;TDA Date
;Constructor TDA Date
;Dom: Dia <number> X mes <number> X año <number>
;Rec:Date
(define date(lambda(dd mm yyyy)(
                                cond [(date? dd mm yyyy)(list dd mm yyyy)]
                                      [else '()]                            
                                
                                )

  ))
;Pertenencia
;Dom: Dia <number> X mes <number> X año <number>
;Rec: Bool
(define date? (lambda (dd mm yyyy)(
                                   cond [(and(number? dd)(number? mm)(number? yyyy)(list dd mm yyyy))]

                                         [else #f])               )
                )
  

;Funcion encryptFn
;funcion que permite encriptar/desencriptar un mensaje
;Dom: String
;Rec: String
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))

;TDA SocialNetwork
;(name,date,encryptFn,decryptFn, users, posts)
;Constructor
;Dom:String X Date X EncryptFunction X DecryptFunction
;Rec:SocialNetwork

(define socialnetwork(lambda(name date encryptFn decryptFn)
                       (list name date encryptFn decryptFn '() '() )

                       ))

;Selectores
(define snName car)
(define snDate cadr)
(define snUsers (lambda(s)(car(cddddr s))))
(define snPosts (lambda(s)(car (cdr(cddddr s)))))

;TDA Usuario
;(idUser,username,pass,amigos)
;donde amigos= (idUser1,idUser2,....,udUserN)

;constructor
;Dom: number X string X string X amigos
;Rec: user
(define user(lambda(lastUserID username pass amigos)
              (list lastUserID username pass amigos)


;modificadores

(define addUser(lambda users newUserName pass)


  )




