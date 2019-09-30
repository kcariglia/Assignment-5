;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |starter pt3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;CS1102 Assignment 5
; Katherine Cariglia and Emmanuel Ola

; Part 3
;=========================
(require math/statistics)


(define-struct bst (widget left right))
;; a bst is a (make-bst widget bst bst)
;; widget is the data to be stored
;; left and right are the left and right subtrees

(define-struct widget (name quantity price))
;; a widget is a (make-widget Natural Natural Natural)
; same as assignment #3, except no parts component

(define-struct db (field lt? eq? bst))

(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define BST-A1 (make-bst A1 false false))
(define BST-Z1 (make-bst Z1 false false))
(define BST-D1 (make-bst D1 BST-A1 false))
(define BST-W1 (make-bst W1 BST-D1 BST-Z1))

(define DB-quantity (make-db widget-quantity < = false))
(define DB-name (make-db widget-name < = false))
(define DB-price (make-db widget-price < = false))

;; you probably want to copy code from part 1 about creating trees
;; with random widgets

;; for the extra credit
;; BST -> Natural
;; computes the height of b
;; uses accumulator approach
(define (height b)
  (local
    [(define (height-helper b d)
       (cond
         [(false? b) d]
         [else
          (max d
               (height-helper (bst-left b) (add1 d))
               (height-helper (bst-right b) (add1 d)))]))]
    (height-helper b 0)))


;; can create a list of random widgets.  very helpful for part 3.
;; may be useful earlier to stress test
(define (random-widgets num max)
  (local
    [(define (rnd val)
       (add1 (random val)))]
    (build-list num
                (lambda (dummy)
                  (make-widget 
                   (number->string (random max))
                   (rnd max)
                   (rnd max))))))

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

; function: insert-name
; signature: Widget Bst →  Bst
; purpose: Adds the widget to an existing binary search tree in the correct position
; test cases:
(check-expect (insert-name D1 BST-A1) (make-bst A1 false (make-bst D1 false false)))

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

;; (listof widget) bst -> bst
;; inserts all widgets in low into bst
(define (build-tree low)
  (foldr insert-name false low))


;; BST -> Natural
;; computes the difference in heights between the left and right branches of b
;; negative values indicate a right-heavy tree
;; positive values indicate a left-heavy tree
(define (height-diff b)
  (- (height (bst-left b)) (height (bst-right b))))

;; BST -> Boolean
;; determines whether a BST requires balancing
(define (balanced? b)
  (if (false? b)
      true
      (<= (abs (height-diff b)) 1)))


; widget lists
(define widget-list10k (random-widgets 10000 99999))
(define widget-list30k (random-widgets 30000 99999))
(define widget-list100k (random-widgets 100000 999999))
(define widget-list300k (random-widgets 300000 999999))
(define widget-list1m (random-widgets 1000000 9999999))


; function: depth
; signature: x db -> natural
; purpose: consumes a value to look for in the DB, and returns the depth of the node it finds
; test cases:
(check-expect (depth A1 (make-db widget-quantity < = false)) 0)
(check-expect (depth Z1 (make-db widget-quantity < = BST-W1)) 1)
(check-expect (depth A1 (make-db widget-quantity < = BST-W1)) 2)


(define (depth x db)
  (local [
          (define (depth0 x0 db0 a)
            (cond [(false? (db-bst db0)) a]
              [((db-eq? db0) ((db-field db0) x0) ((db-field db0) (bst-widget (db-bst db0)))) a]
              [((db-lt? db0) ((db-field db0) x0) ((db-field db0) (bst-widget (db-bst db0)))) (depth0 x0 (make-db (db-field db0) (db-lt? db0) (db-eq? db0) (bst-left (db-bst db0))) (add1 a))]
              [else (depth0 x0 (make-db (db-field db0) (db-lt? db0) (db-eq? db0) (bst-right (db-bst db0))) (add1 a))]))] (depth0 x db 0)))
 

; function: avg-depth
; signature: (listof widget) db -> number
; purpose: for every element of the list compute its depth in the tree, and then take the average

(define (avg-depth low db)
  (mean (map (lambda (x) (depth x db)) low)))


; function: time0
; signature: (listof Widget) db -> time
; purpose: returns time taken to use insert! function with list of widgets and a db

(define (time0 low db)
  (time (begin (set-db-bst! db false) (foldr insert! db low))))

                 
; function: insert!
; signature: Widget DB → DB
; purpose: Inserts a widget into the database’s BST.  The function will need to return a database with the updated BST

(define (insert! k db)
  (begin
  (local [(define b (db-bst db))
          (define smaller? (db-lt? db))
          (define field (db-field db))
          (define (insert-k bst acc)
            (cond
              [(false? bst)
               (if (false? acc)
                   (set-db-bst! db (make-bst k false false))
                   (if (smaller? (field (bst-widget acc)) (field k))
                       (set-bst-left! acc (make-bst k false false))
                       (set-bst-right! acc (make-bst k false false))))]
              [(smaller? (field k) (field (bst-widget bst)))
               (make-bst (bst-widget bst)
                         (insert-k (bst-left bst) bst) 
                         (bst-right bst)) ]
              [else
               (make-bst (bst-widget bst)
                         (bst-left bst)
                         (insert-k (bst-right bst) bst))]))]
    (make-db (db-field DB-quantity) (db-lt? DB-quantity) (db-eq? DB-quantity) (insert-k b false))) db))
    

