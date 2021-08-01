#lang racket

;candidate functions for the toy dataset
(provide y1)
(provide y2)
(provide y3)
(provide y4>62)

(define y1 (cons "feature1" (lambda (z) (first z)))) ; returns the value of feature 1 for a given test sample
(define y2 (cons "feature2" (lambda (z) (second z))))
(define y3 (cons "feature3" (lambda (z) (third z))))
(define y4>62 (cons "feature4>62" (lambda (z) (if (> (fourth z) 62) 1 0)))); returns 1 if the value of feature 4 > 62, else 0

;candidate functions for the titanic dataset
(provide pclass)
(provide sex)
(provide age>25)
(provide sibsp)
(provide parch)  
(provide fare>50)
(provide emb)

(define pclass (cons "pclass" (lambda (z) (first z)))) ; returns the value of pclass for a given test sample
(define sex (cons "sex" (lambda (z) (second z))))
(define age>25 (cons "age>25" (lambda (z) (if (> (third z) 25) 1 0)))) 
(define sibsp (cons "sibsp" (lambda (z) (fourth z))))
(define parch (cons "parch" (lambda (z) (fifth z))))
(define fare>50 (cons "fare>50" (lambda (z) (if (> (sixth z) 50) 1 0))))
(define emb (cons "emb" (lambda (z) (seventh z))))

;candidate functions for the mushroom dataset
(provide cshape)
(provide csurf)
(provide bruise)
(provide odor)
(provide gatch)
(provide gspace)
(provide gsize)
(provide sshape) 
(provide nring)
(provide pop)
(provide hab)

(define cshape (cons "cshape" (lambda (z) (first z))))
(define csurf (cons "csurf" (lambda (z) (second z))))
(define bruise (cons "bruise" (lambda (z) (third z))))
(define odor (cons "odor" (lambda (z) (fourth z))))
(define gatch (cons "gatch" (lambda (z) (fifth z))))
(define gspace (cons "gspace" (lambda (z) (sixth z))))
(define gsize (cons "gsize" (lambda (z) (seventh z))))
(define sshape (cons "sshape" (lambda (z) (eighth z))))
(define nring (cons "nring" (lambda (z) (ninth z))))
(define pop (cons "pop" (lambda (z) (tenth z))))
(define hab (cons "hab" (lambda (z) (last z))))
