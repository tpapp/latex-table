;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:common-lisp-user)
(in-package #:lxt)

(table `((,(multicolumn :center "name" 2) will-be-ignored)
         (1 2)
         (3 4))
       :rules '((0 . :top)
                (1 . :middle)
                (-1 . :bottom)))

(table `((,pi 3)
         (110.7 ,(align :right 'symbol))
         ("text" 99.1))
       :column-types (numprint 2))

(labeled-vertical #("a" "b" "c") #(1 2 3))

(labeled-vertical #("nice" "b" "c") #(1 2 3) :header? 'multicolumn)
