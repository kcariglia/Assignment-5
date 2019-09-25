;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 5 Part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;CS1102 Assignment 5
; Katherine Cariglia and Emmanuel Ola

; Part 1
;=========================

(require 2htdp/image)
;; constants for rendering the tree
(define TEXT-SIZE 20)    
(define TEXT-COLOR "black") 
(define TAB 5)

(define-struct widget (name quantity price))
;; a widget is a (make-widget String Natural Number)
;; same as assignment #3, except no parts component -- let's skip
;; mutual recursion in our BSTs for now :-)

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

;; (listof widget) bst -> bst
;; inserts all widgets in low into bst
(define (build-tree low)
  (foldr insert-name false low))
 
;; your (hopefully former) colleagues couldn't figure this part out
;(define (insert-name v b) b)
;;             ^^^^^
;; !!!

;; here is some code related to displaying a tree
;; you might find it helpful for debugging
;; (render bst) -> image


;; helper functions, can ignore
(define (blanks n)
  (list->string (build-list n (lambda (x) #\ ))))

(define (to-text side w t)
  (text  (string-append (blanks t) side (widget-name w)) TEXT-SIZE TEXT-COLOR))

(define (render-helper b t img side)
  (if (false? b)
      img
      (above/align "left"
                   (to-text side (bst-widget b) t)
                   (render-helper (bst-left b) (+ t TAB) img "L: ")
                   (render-helper (bst-right b) (+ t TAB) img "R: "))))
;; end of helper functio

;; render:  BST -> image
;; provides a graphical representation of the tree
(define (render b)
  (render-helper b 0 (square 0 "solid" "white") "T: "))


; function: smaller?
; signature: String BST → Boolean.
; purpose: Determines whether the string (representing a widget’s name) should go in the left branch of the current BST
; test cases:
(check-expect (smaller? "D1" BST-A1) #f)
(check-expect (smaller? "A1" BST-D1) #t)

(define (smaller? key bst)
  (if (string<? key (widget-name (bst-widget bst)))
      #t
      #f))


;; function: same?
;; signature: String BST →  Boolean
;; purpose: Determines whether the string (representing the widget’s name) is equal to the value of the current BST
;; test cases:
(check-expect (same? "A1" BST-D1) #f)
(check-expect (same? "W1" BST-W1) #t)

(define (same? key bst)
  (if (string=? key (widget-name (bst-widget bst)))
      #t
      #f))


;; function: find-name
;; signature: String BST →  Widget | false
;; purpose: Returns the widget whose name corresponds to the given string, or false if that widget does not exist
;; test cases:
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


;; function: insert-name
;; signature: Widget BST →  BST
;; purpose: Adds the widget to an existing binary search tree in the correct position
;; test cases:
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