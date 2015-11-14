(in-package :cl-user)
(defpackage :split-render
  (:use :cl
        :sdl2
        :kit.math
        :kit.gl.shader
        :kit.gl.vao
        :sb-cga
        :test-entity
        :split-shader)
  (:export :main))

(in-package :split-render)

(defun main ()
  (with-init (:everything)
    (with-window (window :title "split render test" :flags '(:shown :opengl))
      (with-gl-context (gl-context window)
        (gl-make-current window gl-context)
        (gl:viewport 0 0 800 600)

        (let* ((shader (compile-shader-dictionary 'my-shader))
               (projection-matrix (ortho-matrix 0 800 0 600 -1 1))
               (entity (make-instance 'test-entity
                                      :entity-x 400.0
                                      :entity-y 300.0
                                      :entity-width 100.0
                                      :entity-height 100.0)))
          
          (use-program shader :basic-shader)

          (with-event-loop (:method :poll)
            (:idle ()
                   (gl:clear-color 0.0 0.0 0.0 0.0)
                   (gl:clear :color-buffer)
                   (draw entity projection-matrix shader)
                   (gl-swap-window window))
            (:quit ()
                   t)))))))
