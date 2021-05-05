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



