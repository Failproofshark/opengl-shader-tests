;; Even though cl-sdl2 provides a binding to gl-bindtexture, create-texture-from-surface requires a renderer which I could not find when using opengl to render stuff
(in-package :cl-user)
(defpackage :texture-test
  (:use :cl
        :sdl2
        :alexandria)
  (:export :main))

(in-package :texture-test)

(require :cl-opengl)
(require :sdl2-image)

(defun create-gl-array (type lisp-array)
  (let ((gl-array (gl:alloc-gl-array type (length lisp-array))))
    (dotimes (i (length lisp-array))
      (setf (gl:glaref gl-array i) (aref lisp-array i)))
    gl-array))

(defun main ()
  (let ((vertex-attribute-array (create-gl-array :float #(-0.5 -0.5 1.0 1.0 1.0 1.0 1.0
                                                          0.5 -0.5 1.0 1.0 1.0 0.0 1.0
                                                          -0.5  0.5 1.0 1.0 1.0 1.0 0.0
                                                          0.5  0.5 1.0 1.0 1.0 0.0 0.0)))
        (element-attribute-array (create-gl-array :unsigned-short #(0 1 2 3))))
    (with-init (:everything)
      (sdl2-image:init '(:jpg))
      (with-window (my-window :title "CL-OpenGL test" :flags '(:shown :opengl))
        (with-gl-context (gl-context my-window)
          (gl-make-current my-window gl-context)
          (gl:viewport 0 0 800 600)
          ;;the texture-surface is the actual loaded image object
          (let ((texture-surface (sdl2-image:load-image (asdf:system-relative-pathname :opengl-shader-test "image.jpg")))
                ;;The first buffer is our verticies, the second is our elements
                (buffers (gl:gen-buffers 2))
                (vao (car (gl:gen-vertex-arrays 1)))
                (texture (car (gl:gen-textures 1)))
                (vertex-shader (gl:create-shader :vertex-shader))
                (fragment-shader (gl:create-shader :fragment-shader))
                (shader-program (gl:create-program)))
            
            (gl:shader-source vertex-shader (read-file-into-string (asdf:system-relative-pathname 'opengl-shader-test
                                                                                                  "texture-vertex-shader.glsl")))
            (gl:compile-shader vertex-shader)
            
            (gl:shader-source fragment-shader (read-file-into-string (asdf:system-relative-pathname 'opengl-shader-test
                                                                                                    "texture-fragment-shader.glsl")))
            (gl:compile-shader fragment-shader)
            
            (gl:attach-shader shader-program vertex-shader)
            (gl:attach-shader shader-program fragment-shader)
            
            (gl:link-program shader-program)
            (gl:use-program shader-program)

            (gl:bind-vertex-array vao)
            
            (gl:bind-buffer :array-buffer (first buffers))
            (gl:buffer-data :array-buffer :static-draw vertex-attribute-array)
            (gl:free-gl-array vertex-attribute-array)
            
            (gl:vertex-attrib-pointer (gl:get-attrib-location shader-program "position")
                                      2
                                      :float
                                      :false
                                      (* 7 (cffi:foreign-type-size :float))
                                      (cffi:null-pointer))
            (gl:enable-vertex-attrib-array (gl:get-attrib-location shader-program "position"))
            
            (gl:vertex-attrib-pointer (gl:get-attrib-location shader-program "input_color")
                                      3
                                      :float
                                      :false
                                      (* 7 (cffi:foreign-type-size :float))
                                      (* 2 (cffi:foreign-type-size :float)))
            (gl:enable-vertex-attrib-array (gl:get-attrib-location shader-program "input_color"))
            
            ;;Texture coordinates
            (gl:vertex-attrib-pointer (gl:get-attrib-location shader-program "tex_coord")
                                      2
                                      :float
                                      :false
                                      (* 7 (cffi:foreign-type-size :float))
                                      (* 5 (cffi:foreign-type-size :float)))
            (gl:enable-vertex-attrib-array (gl:get-attrib-location shader-program "tex_coord"))

            ;;Binding the texture object for configuration
            (gl:bind-texture :texture-2d texture)
            (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-border)
            (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-border)
            (gl:generate-mipmap :texture-2d)
            (gl:tex-parameter :texture-2d :texture-min-filter :linear)
            (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
            (gl:tex-image-2d :texture-2d
                             0
                             :rgb
                             (surface-width texture-surface)
                             (surface-height texture-surface)
                             0
                             :rgb
                             :unsigned-byte
                             (surface-pixels texture-surface))

            (gl:bind-buffer :element-array-buffer (second buffers))
            (gl:buffer-data :element-array-buffer :static-draw element-attribute-array)
            (gl:free-gl-array element-attribute-array)

            (with-event-loop (:method :poll)
              (:idle ()
                     (gl:clear-color 0.0 0.0 0.0 0.0)
                     (gl:clear :color-buffer)
                     (gl:draw-elements :triangle-strip
                                       (gl:make-null-gl-array :unsigned-short)
                                       :count 4)
                     (gl-swap-window my-window))
              (:quit ()
                     (free-surface texture-surface)
                     (gl:delete-textures `(,texture))
                     (gl:delete-shader vertex-shader)
                     (gl:delete-shader fragment-shader)
                     (gl:delete-program shader-program)
                     (gl:delete-buffers buffers)
                     (gl:delete-vertex-arrays `(,vao))
                     (sdl2-image:quit)
                     t))))))))
