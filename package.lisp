;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(defpackage :fern
  (:shadow :get :map :values)
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
    :make-fern))
 
