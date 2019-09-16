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

;; ブロックの移動判定
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

;; ブロックを回転させる
(defun turn-piece ()
  (let ((piece-turn (make-array
                     `(,*piece-width* ,*piece-height*)
                     :initial-element 0))
        (offset-x 0) (offset-y 0))
    ;; 回転したブロックを生成する
    (dotimes (y *piece-height*)
      (dotimes (x *piece-width*)
        (setf (aref piece-turn (- (- *piece-height* 1) y) x)
              (aref *piece* x y))))
    ;; 回転可能かどうかを調べる
    (dotimes (y *piece-height*)
      (dotimes (x *piece-width*)
        (when (= (aref piece-turn x y) 1)
          (setf offset-x (+ *location-x* x)
                offset-y (+ *location-y* y))
          (when (or (< offset-x 0)
                    (>= offset-x *field-width*)
                    (>= offset-y *field-height*)
                    (and (>= offset-y 0)
                         (= (aref *field* offset-x offset-y) 1)))
            (return-from turn-piece)))))
    (setf *piece* piece-turn)))

;; ブロックを位置情報に従ってフィールドにコピーする
(defun piece-to-field ()
  (dotimes (y *piece-height*)
    (dotimes (x *piece-width*)
      (when (and (= (aref *piece* x y) 1)
                 (>= (+ *location-y* y) 0))
        (setf (aref *field* (+ *location-x* x) (+ *location-y* y))
              (aref *piece* x y))))))

;; 各行を調べ、行が埋まっている場合は行を削除する
(defun delete-line ()
  (let ((del-count 0))
    (do ((y (- *field-height* 1) (- y 1))) ((= y 0))
      (let ((line-count 0))
        (dotimes (x *field-width*)
          (incf line-count (aref *field* x y)))
        ;; これより上にブロックはない
        (when (= line-count 0) (return))
        (when (/= line-count *field-width*) (go continue01))
        ;; 1行削除する
        (incf del-count)
        (dotimes (x *field-width*)
          (setf (aref *field* x y) 0)))
     continue01)
    del-count))

;; 削除した行を詰める
(defun shift-line (del-count)
  (do ((y (- *field-height* 1) (- y 1)))
      ((or (< y 0) (< del-count 0)))
    (let ((line-count 0))
      (dotimes (x *field-width*)
        (incf line-count (aref *field* x y)))
      (when (/= line-count 0)
        (decf y) (go continue01))
      (decf del-count)
      (do ((iy y (- iy 1))) ((< iy 0))
        (dotimes (x *field-width*)
          (if (>= (- iy 1) 0)
              (setf (aref *field* x iy) (aref *field* x (- iy 1)))
              (setf (aref *field* x 0) 0)))))
   continue01))

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
