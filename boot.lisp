(uiop:define-package #:tetris
  (:use #:cl
        #:tetris/app)
  (:export #:start))
(in-package #:tetris)

(defun start (&rest args)
  (main args))
