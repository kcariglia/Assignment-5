;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 5 Part 2 With Bug|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;CS1102 Assignment 5
; Katherine Cariglia and Emmanuel Ola

; Part 2
;=========================

(define-struct widget (name quantity price))
;; a widget is a (make-widget String Natural Number)
; same as assignment #3, except no parts component -- let's skip mutual recursion for now :-)

(define-struct bst (widget left right))
;; a BST is either
;;          false, or
;;          a (make-bst widget bst bst)


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define BST-A1 (make-bst A1 false false))
(define BST-Z1 (make-bst Z1 false false))
(define BST-D1 (make-bst D1 BST-A1 false))
(define BST-W1 (make-bst W1 BST-D1 BST-Z1))

; function: smaller?
; signature: String Bst → Boolean.
; purpose: Determines whether the string (representing a widget’s name) should go in the left branch of the current Bst
; test cases:
(check-expect (smaller? "D1" BST-A1) #f)
(check-expect (smaller? "A1" BST-D1) #t)

(define (smaller? key bst)
  (if (string<? key (widget-name (bst-widget bst)))
      #t
      #f))

; function: same?
; signature: String Bst →  Boolean
; purpose: Determines whether the string (representing the widget’s name) is equal to the value of the current Bst
; test cases:
(check-expect (same? "A1" BST-D1) #f)
(check-expect (same? "W1" BST-W1) #t)

(define (same? key bst)
  (if (string=? key (widget-name (bst-widget bst)))
      #t
      #f))

(define-struct db (field lt? eq? bst))
;; a db is (make-db widget-field smaller-function? same-function? bst)
; interp.
; - widget-field is one of the fields in the widget structure
; - smaller-function? is function that compares if one value is less than another
; - same-function? is function that compares if two values are equal
; - bst is bst structure

; example:
(define DB-quantity (make-db widget-quantity < = false))
(define DB-name (make-db widget-name < = false))
(define DB-price (make-db widget-price < = false))

; template:
(define (fn-for-db db)
  (...
   (db-field db)
   (db-lt? (db-field db))
   (db-eq? (db-field db))
   (db-bst db)))


; function: find
; signature: X DB → Widget|false
; purpose: Consumes the value to look for and a DB.  This function does a normal find operation, and looks for X within the BST
; test cases:
(check-expect (find "A1" DB-name) false)
(check-expect (find "A1" (make-db widget-name smaller? same? BST-W1)) A1)
(check-expect (find "Z1" (make-db widget-name smaller? same? BST-W1)) Z1)
;(check-expect (find "Z1" (make-db widget-quantity smaller? same? BST-W1)) false)

(define (find x db)
  (cond
    [(false? (db-bst db)) false]
    [((db-eq? db) x (db-bst db)) (bst-widget (db-bst db))]
    [((db-lt? db) x (db-bst db))
     (find x (make-db (db-field db) (db-lt? db) (db-eq? db) (bst-left (db-bst db))))]
    [else
     (find x (make-db (db-field db) (db-lt? db) (db-eq? db) (bst-right (db-bst db))))]))


; function: insert
; signature: Widget DB →  DB
; purpose: Inserts a widget into the database’s BST.  The function will need to return a database with the updated BST
; test cases:
(check-expect (db-bst (insert A1 DB-quantity)) (db-bst (make-db (db-field DB-quantity) (db-lt? DB-quantity) (db-eq? DB-quantity) (make-bst A1 false false))))
(check-expect (db-bst (insert A1 (make-db widget-quantity < = BST-Z1))) (db-bst (make-db widget-quantity < = (make-bst Z1 BST-A1 false))))
(check-expect (db-bst (insert Z1 (make-db widget-quantity < = BST-A1))) (db-bst (make-db widget-quantity < = (make-bst A1 false BST-Z1))))


(define (insert k db)
  (local[(define (insert0 k runningdb)
  (cond
    [(false? (db-bst runningdb)) (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-abstract k (db-bst db) db))]
    [((db-lt? db) ((db-field db) k) ((db-field db) (bst-widget (db-bst db))))
     (insert0 k (make-db (db-field runningdb) (db-lt? runningdb) (db-eq? runningdb) (bst-left (db-bst runningdb))))]
    [else
     (insert0 k (make-db (db-field runningdb) (db-lt? runningdb) (db-eq? runningdb) (bst-right (db-bst runningdb))))]))]
    (insert0 k db)))

(define (insert-abstract k bst db)
  (cond
    [(false? (db-bst bst)) (make-bst k false false)]
    [((db-lt? db) ((db-field db) k) ((db-field db) (bst-widget bst)))
     (make-bst (bst-widget bst)
               (insert-abstract k (bst-left bst) db) 
               (bst-right bst))]
    [else
     (make-bst (bst-widget bst)
               (bst-left bst)
               (insert-abstract k (bst-right bst) db))]))

; function: find-name
; signature: String Bst →  Widget | false
; purpose: Returns the widget whose name corresponds to the given string, or false if that widget does not exist
; test cases:
(check-expect (find-name "D1" BST-A1) #f)
(check-expect (find-name "W1" BST-W1) W1)
(check-expect (find-name "A1" BST-W1) A1)

(define (find-name k b)
  (cond
    [(false? b) false]
    [(same? k b) (bst-widget b)]
    [(smaller? k b)
     (find-name k (bst-left b))]
    [else
     (find-name k (bst-right b))]))


; function: insert-name
; signature: Widget Bst →  Bst
; purpose: Adds the widget to an existing binary search tree in the correct position
; test cases:
(check-expect (insert-name D1 BST-A1) (make-bst A1 false (make-bst D1 false false)))
(check-expect (insert-name D1 BST-Z1) (make-bst Z1 (make-bst D1 false false) false))

(define (insert-name k b)
  (cond
    [(false? b) (make-bst k false false)]
    [(smaller? (widget-name k) b)
     (make-bst (bst-widget b)
               (insert-name k (bst-left b)) 
               (bst-right b))]
    [else
     (make-bst (bst-widget b)
               (bst-left b)
               (insert-name k (bst-right b)))]))
