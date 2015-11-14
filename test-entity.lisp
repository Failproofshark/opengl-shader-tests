(in-package :cl-user)
(defpackage :test-entity
  (:use :cl
        :sb-cga
        :split-vao
        :kit.gl.vao
        :kit.gl.shader)
  (:export :test-entity
           :draw))
(in-package :test-entity)

(defclass test-entity ()
  ((entity-x
    :initarg :entity-x
    :type float
    :accessor entity-x)
   (entity-y
    :initarg :entity-y
    :type float
    :accessor entity-y)
   (entity-width
    :initarg :entity-width
    :type float
    :accessor entity-width)
   (entity-height
    :initarg :entity-height
    :type float
    :accessor entity-height)
   (verticies
    :initform (make-array 12
                          :element-type 'single-float
                          :initial-contents '(-0.5 -0.5  
                                              0.5 -0.5  
                                              0.5  0.5  
                                              0.5  0.5  
                                              -0.5 0.5  
                                              -0.5 -0.5))
    :allocation :class
    :accessor verticies)
   (color
    :initarg :color
    :initform (make-array 18
                          :element-type 'single-float
                          :initial-contents '(1.0 0.0 0.0
                                              0.0 1.0 0.0
                                              0.0 0.0 1.0  
                                              0.0 0.0 1.0
                                              1.0 1.0 0.0
                                              1.0 0.0 0.0))
    :accessor color)
   (entity-vao
    :allocation :class
    :accessor entity-vao)))

(defmethod initialize-instance :after ((entity test-entity) &key &allow-other-keys)
  (with-slots (entity-vao verticies color) entity
    (setf entity-vao (make-instance 'vao :type 'my-vao))
    (vao-buffer-vector entity-vao
                       0
                       (* 4 (length verticies))
                       verticies)
    (vao-buffer-vector entity-vao
                       1
                       (* 4 (length color))
                       color)))

(defgeneric generate-mvp-matrix (Entity projection-matrix)
  (:documentation "Generate a projection matrix"))

(defmethod generate-mvp-matrix ((entity test-entity) projection-matrix)
  (with-accessors ((entity-x entity-x) (entity-y entity-y) (entity-width entity-width) (entity-height entity-height)) entity
    ;; REMEMBER MATRIX MULTIPLICATION IS NOT COMMUTATIVE AND WE MULTIPLE OUR TRANSFORMATION IN REVERSE ORDER OF WHATA WE WANT!
    (matrix* projection-matrix
             (translate* entity-x entity-y 0.0)
             (scale* entity-width entity-height 0.0))))

(defgeneric draw (Entity projection-matrix the-shader)
    (:documentation "How to draw the entity to the screen"))

(defmethod draw ((entity test-entity) projection-matrix the-shader)
  (with-accessors ((color color) (entity-width entity-width) (entity-height entity-height) (verticies verticies) (vertex-data-bound vertex-data-bound) (entity-vao entity-vao)) entity
    (uniform-matrix the-shader
                    :model-view-projection-matrix
                    4
                    (cl:vector (generate-mvp-matrix entity projection-matrix)))
    (vao-draw entity-vao
              :primitive :triangles
              :count 6)))

