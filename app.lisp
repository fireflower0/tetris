(uiop:define-package #:tetris/app
  (:use #:cl
        #:tetris/utils)
  (:export #:main))
(in-package #:tetris/app)

;; ビースの横と縦のマス数
(defparameter *piece-width* 4)
(defparameter *piece-height* 4)

;; フィールドの横と縦マス数
(defparameter *field-width* 14)
(defparameter *field-height* 24)

;; マスのピクセル数
(defparameter *cell-width* 20)
(defparameter *cell-height* 20)

;; ゲームフィールド
(defparameter *field*
  (make-array
   `(,*field-width* ,*field-height*)
   :initial-element 0))

;; 現在移動中のブロック
(defparameter *piece*
  (make-array
   `(,*piece-width* ,*piece-height*)
   :initial-element 0))

;; 現在移動中のブロックの位置
(defparameter *location-x* 0)
(defparameter *location-y* 0)

;; 次のブロック
(defparameter *next*
  (make-array
   `(,*piece-width* ,*piece-height*)
   :initial-element 0))

;; ブロックをクリア
(defun crear-block ()
  (dotimes (y *piece-height*)
    (dotimes (x *piece-width*)
      (setf (aref *next* x y) 0))))

;; ブロックを生成
(defun create-block (&key x1 y1 x2 y2 x3 y3 x4 y4 color)
  (setf (aref *next* x1 y1) 1
        (aref *next* x2 y2) 1
        (aref *next* x3 y3) 1
        (aref *next* x4 y4) 1))

;; 次のピースを予め作っておく
(defun create-piece ()
  (crear-block)
  (case (rem (random 100) 7)
    (0 (create-block :x1 1 :y1 1     ; ----
                     :x2 2 :y2 1     ; -@@-
                     :x3 1 :y3 2     ; -@@-
                     :x4 2 :y4 2))   ; ----
    (1 (create-block :x1 1 :y1 0     ; -@--
                     :x2 1 :y2 1     ; -@--
                     :x3 1 :y3 2     ; -@--
                     :x4 1 :y4 3))   ; -@--
    (2 (create-block :x1 1 :y1 1     ; ----
                     :x2 1 :y2 2     ; -@--
                     :x3 2 :y3 2     ; -@@-
                     :x4 1 :y4 3))   ; -@--
    (3 (create-block :x1 1 :y1 1     ; ----
                     :x2 2 :y2 1     ; -@@-
                     :x3 1 :y3 2     ; -@--
                     :x4 1 :y4 3))   ; -@--
    (4 (create-block :x1 1 :y1 1     ; ----
                     :x2 2 :y2 1     ; -@@-
                     :x3 2 :y3 2     ; --@-
                     :x4 2 :y4 3))   ; --@-
    (5 (create-block :x1 2 :y1 1     ; ----
                     :x2 1 :y2 2     ; --@-
                     :x3 2 :y3 2     ; -@@-
                     :x4 1 :y4 3))   ; -@--
    (6 (create-block :x1 1 :y1 1     ; ----
                     :x2 1 :y2 2     ; -@--
                     :x3 2 :y3 2     ; -@@-
                     :x4 2 :y4 3)))) ; --@-

;; 次のブロックへ
(defun next-piece (is-first)
  (when is-first (create-piece))
  (setf *piece* *next*
        *location-x* 5
        *location-y* 3)
  (create-piece))

;; *piece*内のブロックの最上部の位置を返す
(defun get-piece-top ()
  (dotimes (y *piece-height*)
    (dotimes (x *piece-width*)
      (when (= (aref *piece* x y) 1)
        (return-from get-piece-top y)))))

;; *piece*内のブロックの最下部の位置を返す
(defun get-piece-bottom ()
  (do ((y (- *piece-height* 1) (- y 1)))
      ((= y 0))
    (dotimes (x *piece-width*)
      (when (= (aref *piece* x y) 1)
        (return-from get-piece-bottom y)))))

;; *piece*内のブロックの左側の位置を返す
(defun get-piece-left ()
  (dotimes (x *piece-width*)
    (dotimes (y *piece-height*)
      (when (= (aref *piece* x y) 1)
        (return-from get-piece-left x)))))

;; *piece*内のブロックの右側の位置を返す
(defun get-piece-right ()
  (do ((x (- *piece-width* 1) (- x 1)))
      ((= x 0))
    (dotimes (y *piece-height*)
      (when (= (aref *piece* x y) 1)
        (return-from get-piece-right x)))))

(defconstant +piece-left+ 2)
(defconstant +piece-right+ 4)
(defconstant +piece-dowm+ 8)

(defun move-piece (move)
  (cond ((= move +piece-left+)
         (unless (<= (+ *location-x* (get-piece-left)) 0)
           (dotimes (y *piece-height*)
             (dotimes (x *piece-width*)
               (when (and (= (aref *piece* x y) 1)
                          (>= (- (+ *location-x* x) 1) 0)
                          (>= (+ *location-y* y) 0)
                          (= (aref *field*
                                   (- (+ *location-x* x) 1)
                                   (+ *location-y* y))
                             1))
                 (return-from move-piece)))))
         (decf *location-x*))
        ((= move +piece-right+)
         (unless (>= (+ *location-x* (get-piece-right))
                     (- *field-width* 1))
           (dotimes (y *piece-height*)
             (dotimes (x *piece-width*)
               (when (and (= (aref *piece* x y) 1)
                          (< (+ (+ *location-x* x) 1) *field-width*)
                          (>= (+ *location-y* y) 0)
                          (= (aref *field*
                                   (+ (+ *location-x* x) 1)
                                   (+ *location-y* y))
                             1))
                 (return-from move-piece)))))
         (incf *location-x*))
        ((= move +piece-dowm+)
         (unless (>= (+ *location-y* (get-piece-bottom))
                     (- *field-height* 1))
           (dotimes (y *piece-height*)
             (dotimes (x *piece-width*)
               (when (and (= (aref *piece* x y) 1)
                          (>= (+ (+ *location-y* y) 1) 0)
                          (< (+ (+ *location-y* y) 1) *field-height*)
                          (= (aref *field*
                                   (+ *location-x* x)
                                   (+ (+ *location-y* y) 1))
                             1))
                 (return-from move-piece)))))
         (incf *location-y*))))

(defun main (&rest args)
  (with-window-renderer (window renderer)
    (sdl2:with-event-loop (:method :poll)
      (:keydown (:keysym keysym)
                (if (sdl2:scancode= (sdl2:scancode-value keysym)
                                    :scancode-escape)
                    (sdl2:push-event :quit)))
      (:idle ()
             (sdl2:set-render-draw-color renderer 0 0 0 255)
             (sdl2:render-clear renderer)
             (sdl2:render-present renderer))
      (:quit () t))))
