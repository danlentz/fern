;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(defpackage :fern
  (:shadow :get :map)
  (:use :common-lisp :unicly)
  (:export
    :arc
    :fern
    :put
    :get
    :get*
    :drop
    :map
    :map-values
    :map-nodes
    :map-nodes-remove-if
    :all-values
    :set-value
    :get-value
    :deep-copy
    :make-fern
    :make-spinlock
    :lock-spinlock
    :unlock-spinlock
    :with-spinlock
    :make-recursive-spinlock
    :lock-recursive-spinlock
    :unlock-recursive-spinlock
    :with-recursive-spinlock))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some potential unicode vanity-glyphs to assign...  looking for a
;; good, readable set of arrows to incorporate into some type of
;; traversal grammar macro symtax such as symbol macros or macro
;; character.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"   Graph Traversal Primitives ? "
"------------------------------- "
" ⇠ ⇡ ⇢ ⇣ ⇔ ➜ ⏎ ⌫ ⦙ ✖ ✚ ✪        "
" ⇠ ⇡ ⇢ ⇣                        "
" ⬅ ⬆ ⬇  ❶ ❷ ❸ ➀                 "
"                                "
"   21st Century Lispoid Man     "
"------------------------------- "
" ❰ ❱                            "
" ❮ ❯                            "
"                                "
"        Uncatgorized            "
"------------------------------- "
" ▶ ◀ ❉ ✱ ✖ ✔ ☹ ∞ ⌀ ∅ ∀ ∃ ─      "
"                                "
"   Hexagrams of the I-Ching     "
"------------------------------- "
" ☰  2630 #\TRIGRAM-FOR-HEAVEN   "
" ☱  2631 #\TRIGRAM-FOR-LAKE     "
" ☲  2632 #\TRIGRAM-FOR-FIRE     "
" ☳  2633 #\TRIGRAM-FOR-THUNDER  "
" ☴  2634 #\TRIGRAM-FOR-WIND     "
" ☵  2635 #\TRIGRAM-FOR-WATER    "
" ☶  2636 #\TRIGRAM-FOR-MOUNTAIN "
" ☷  2637 #\TRIGRAM-FOR-EARTH    "
