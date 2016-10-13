#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student IDs for each of your group members below.***
Alec Brunelle, brunell3, 999241315
|#
#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
;(define (And x y) (and x y))
;(define (Or x y) (or x y))
;(define (If x y z) (if x y z))

; Correction Oct 5 2016
(define-syntax If
  (syntax-rules ()
  ((If a b c)
  (if a b c)))
)
(define-syntax And
  (syntax-rules ()
     [
      (And p q)
      (if p q #f)
     ]
   )
)
(define-syntax Or
  (syntax-rules ()
    [
      (Or p q)
      (if p #t q)
    ]
  )
)

; Please do define And, Or as syntactic forms
; We have actually done this in class you may use the class code and this week's lab code for this.


; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size
         SELECT
         index-in-list
         join-attrs)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
  (first table)
)

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
  (rest table)
)

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
  (length (tuples table))
)

#|
(index-in-list list element)
  list: list to search
  element: element to find in the list

  Returns the index of element in the list. Returns -1 if not found.
|#
(define (index-in-list list element)
  (if
    (member element list) (- (length list) (length (member element list))) -1
  )
)

#|
(list-ref-table table attrs)
  table: a valid table

  Returns a table with attrs ordered the same as attrs.

  Notes:
  - build a new table with:
    - attrs as the first list element
    - tuple:
      - element ordering the same as the ordering in attrs
|#
(define (select-query table attrs)
  (foldl
    (lambda (a b)
      (append
        b
        (list
          (foldl
            (lambda (current-attr result-tuple)
              (append
                result-tuple
                (list (list-ref a (index-in-list (attributes table) current-attr)))
              )
            )
            '()
            attrs
          )
        )
      )
    )
    '()
    (tuples table)
  )
)


#|
(list-of-attrs tables)
  tables: list of tables

  Returns a list of attributes found in all of the tables
|#
(define (list-of-attrs tables)
  (foldl
    (lambda (current-table result)
      (append
        result
        (attributes current-table)
      )
    )
    '()
    tables
  )
)

#|
(join-attrs tables-char-id-pairs)
  tables-char-id-pairs: list of table, char-id pairs

  Returns a list of joined attributes from all of the tables in the order
  in which it was inputted. If duplicates arise, it use the char-id of the table
  to uniquely id itself.
|#
(define (join-attrs tables char-ids)
  (foldl
    (lambda (current-table current-char-id result)
      (append
        result
        (foldl
          (lambda (current-attr result)
            (if
              ; if more than twice in the list of attrs, we need to id this column
              (>
                (count
                  (lambda (x)
                    (equal? x current-attr)
                  )
                  (list-of-attrs tables)
                )
                1
              )
              (append result (list (string-append current-char-id "." current-attr)))
              (append result (list current-attr))
            )
          )
          '()
          (attributes current-table)
        )
      )
    )
    '()
    tables
    char-ids
  )
)

#|
(join-table tables)
  Returns the cartesian-product of the list of tables in table-tuple form.
  https://piazza.com/class/is1wjow0bqh48n?cid=134
|#
(define (join-table tables)
  (foldl
    (lambda (current-tuple result)
      (append
        result
        (map
          (lambda (current)
            (append current-tuple current)
          )
          ;; TODO
          (select-query (second tables) (attributes (second tables)))
        )
      )
    )
    '()
    (select-query (first tables) (attributes (first tables)))
  )
)


#|
(define-syntax SELECT)
  Defines syntax for SELECT statements
|#
(define-syntax SELECT
  (syntax-rules (FROM)
    [
      (SELECT <attrs> FROM <table>)
      (cond
        [(empty? <attrs>) '()]
        [(list? <attrs>) (append (list <attrs>) (select-query <table> <attrs>))]
        [(equal? (id->string <attrs>) "*") <table>]
      )
    ]
    [
      (SELECT <attrs> FROM [<table> <char-id>] ...)
      (cond
        [(empty? <attrs>) '()]
        ;; attributes are joined together, duplicates are changed to
        ;; <char-id>.<attribute-name>
        [(equal? (id->string <attrs>) "*")
          (append
            (list
              (join-attrs
                (list <table> ...)
                (list <char-id> ...)
              )
            )
            (join-table
              (list <table> ...)
            )
          )
        ]
      )
    ]
  )
)

; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes:
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#

#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#

#|
A function 'replace-attr' that takes:
  - x
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#


; Starter for Part 3; feel free to ignore!

; What should this macro do?
(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     ; Change this!
     (void)]
    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     ; Change this!
     (void)]))

(define-syntax id->string
 (syntax-rules ()
   [
     (id->string <id>)
     (symbol->string (quote <id>))
   ]
 )
)
