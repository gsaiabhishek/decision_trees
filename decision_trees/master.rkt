#lang racket

(require 2htdp/batch-io)

(require "decision_functions.rkt")

;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings
(provide toy-raw)
(define toy-raw (cdr (read-csv-file toytrain)))

(provide titanic-raw)
(define titanic-raw (map (lambda (z) (cddr z)) (cdr (read-csv-file titanictrain))))

(provide mushroom-raw)
(define mushroom-raw (cdr (read-csv-file mushroomtrain)))

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data) (cons (map (lambda (z) (string->number z)) (cdr data)) (string->number (car data))))

;list of (features . result)
(provide toy)
(define toy (map (lambda (z) (format z)) toy-raw))

(provide titanic)
(define titanic (map (lambda (z) (format z)) titanic-raw))

(provide mushroom)
(define mushroom (map (lambda (z) (format z)) mushroom-raw))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  (/ (get-leaf-prob1 data 0) (length data)))
(define (get-leaf-prob1 l a )
  (if (null? l) a (get-leaf-prob1 (cdr l) (if (= 1 (cdar l)) (+ a 1) a))))

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  (let ([1prob (get-leaf-prob data)])
  (cond [(or (= 1prob 0) (= 1prob 1)) 0]
        [else (+ (* 1prob (log (/ 1 1prob) 2)) (* (- 1 1prob) (log (/ 1 (- 1 1prob)) 2)))])))

;find the difference in entropy achieved
;by applying a decision function f to the data
(provide entropy-diff)
(define (entropy-diff f data)
  (- (get-entropy data) (foldr + 0 (map (lambda (z) (* (/ (length z) (length data)) (get-entropy z))) (group-by (lambda (z) (f (car z))) data))))) 
   
;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
 (let ([car (car candidates)]) (cf1 (cdr candidates) data (entropy-diff (cdr car) data) car)))
(define (cf1 l data c f)
(if (null? l) f (let* ([car (car l)]
                      [entr (entropy-diff (cdr car) data)])
                  (if (> entr c) (cf1 (cdr l) data entr car) (cf1 (cdr l) data c f)))))
                 

(provide DTree)
(struct DTree (desc func kids))

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (build-tree candidates data depth)
 (if (or (null? candidates) (= 0 depth)) (DTree (~a (get-leaf-prob data)) '() '()) (let* ([chf (choose-f candidates data)]
                                                                  [gb (group-by (lambda (z) ((cdr chf) (car z))) data)])
                                                        (DTree (car chf) (cons chf (map (lambda (z) ((cdr chf) (caar z))) gb)) (map (lambda (z) (build-tree (remove chf candidates) z (- depth 1)))
                                                                                   gb)))))

;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
(provide make-decision)
(define (make-decision tree test)
  (match tree
    [(DTree desc func kids) (if (equal? kids '()) (string->number desc) (let ([coeff (find-coeff ((cdar func) test) (cdr func) 0)])
                                                          (if (>= coeff 0) (make-decision (list-ref  kids coeff ) test) 0)))]))
(define (find-coeff ch func c)
  (cond [(null? func) -1]
        [(equal? ch (car func)) c]
        [else (find-coeff ch (cdr func) (+ c 1))])) 

;============================================================================================================
;============================================================================================================
;============================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append
         (map (lambda (t)
                (string-append tabs
                               "r" prefix
                               "--"
                               "r" prefix "t" (~a (cdr t))
                               "[label=\"" (~a (cdr t)) "\"];" "\n"
                               (dot-helper (car t)
                                           (string-append prefix "t" (~a (cdr t)))
                                           (string-append tabs "\t")
                                           )
                               )
                ) children
                  )
         )
  )

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [d (car node)]
         [c (cdr node)])
    (string-append tabs
                   "r"
                   prefix
                   "[label=\"" d "\"];" "\n\n"
                   (dot-child (pair-idx c 0) prefix tabs)
                   )
    )
  )

;output tree (dot file)
(provide display-tree)
(define (display-tree tree outfile)
  (write-file outfile (string-append "graph \"decision-tree\" {" "\n"
                                     (dot-helper tree "" "\t")
                                     "}"
                                     )
              )
  )
;============================================================================================================
;============================================================================================================
;============================================================================================================
