(defpackage :opengl-dev1
  (:use :cl))

(in-package :opengl-dev1)

(defparameter *shader-path* "/home/johannes/common-lisp/arcimoog/lisp/")
(defparameter *texture-path* "/home/johannes/common-lisp/arcimoog/lisp/")


(defun array-to-gl-array (lisp-array data-type)
  (declare (type simple-vector lisp-array))
  (let ((gl-array (gl:alloc-gl-array data-type (length lisp-array))))
    (loop for item across lisp-array
          for i from 0 do
            (setf (gl:glaref gl-array i) item))
    gl-array))


;; Necessary, because gl:draw-elements contains a problematic implementation of the offset
;; parameter.

(defun my-gl-draw-elements (mode count type &key (offset 0))
  (%gl:draw-elements mode count type offset))


;; #include <glad/glad.h>

(asdf:load-system "cl-opengl")

;; #include <GLFW/glfw3.h>

(asdf:load-system "cl-glfw3")

;; #include <stb_image.h>


(asdf:load-system "cl-jpeg")

;; #include <learnopengl/shader_s.h>

(defun validp (shader)
  (> shader -1))

(defun check-shader-error (shader)
  "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader)))
    (unless (equalp error-string "")
      (progn
        (format t "~A~%" error-string)
        (error 'compile-error :message error-string)))))


(defclass shader-class ()
  ((id :accessor id
       :initform -1
       :documentation "OpenGL-identifier for shader program.")
   (vertex-source-path :initarg :vertex-source
                       :reader vertex-source-path)
   (fragment-source-path :initarg :fragment-source
                         :reader fragment-source-path)))

(defgeneric use (shader))

(defgeneric destroy (shader))

(defgeneric set-uniform (shader name type &rest parameters))


(defmethod initialize-instance :after ((shader shader-class) &key)
  (let ((vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader)))
    (loop while (not (validp (id shader))) do
      (with-simple-restart
          (retry "Retry compiling shaders.")
        (gl:shader-source vertex-shader (uiop:read-file-string (vertex-source-path shader)))
        (gl:shader-source fragment-shader (uiop:read-file-string (fragment-source-path shader)))
        (gl:compile-shader vertex-shader)
        (gl:compile-shader fragment-shader)
        (check-shader-error vertex-shader)
        (check-shader-error fragment-shader)
        (setf (id shader) (gl:create-program))
        (gl:attach-shader (id shader) vertex-shader)
        (gl:attach-shader (id shader) fragment-shader)
        (gl:link-program (id shader))
        (gl:use-program (id shader))
        (gl:delete-shader vertex-shader)
        (gl:delete-shader fragment-shader)))))

(defmethod use ((shader shader-class))
  (gl:use-program (id shader)))

(defmethod destroy ((shader shader-class))
  (gl:delete-program (id shader)))

(defmethod set-uniform ((shader shader-class) name type &rest parameters)
  (use shader)
  (apply #'gl:uniformf (append (list (gl:get-uniform-location (id shader) name)) parameters)))

(defmethod set-uniform-matrix ((shader shader-class) name matrix-array)
  (use shader)
  (gl:program-uniform-matrix-4fv (id shader) (gl:get-uniform-location (id shader) name) matrix-array))

(defmacro with-shader (shader &body body)
  `(progn
     (use ,shader)
     ,@body
     (gl:use-program 0)))


;; #include <iostream>


;; // glfw: whenever the window size changed (by OS or user resize) this callback function executes
;; // ---------------------------------------------------------------------------------------------
;; void framebuffer_size_callback(GLFWwindow* window, int width, int height)

;; {
;;     // make sure the viewport matches the new window dimensions; note that width and
;;     // height will be significantly larger than specified on retina displays.
;;     glViewport(0, 0, width, height);
;; }

(glfw:def-window-size-callback framebuffer-size-callback (window width height)
  (declare (ignore window))
  (gl:viewport 0 0 width height))

;; void processInput(GLFWwindow *window);

(defun process-input ()

  ;; {
  ;;     if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
  ;;         glfwSetWindowShouldClose(window, true);
  ;; }

  (when (eq (glfw:get-key :escape) :press)
    (glfw:set-window-should-close)))


;; // settings
;; const unsigned int SCR_WIDTH = 800;
;; const unsigned int SCR_HEIGHT = 600;

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)

;; int main()
(defun main ()
  ;; {
  ;;     // glfw: initialize and configure
  ;;     // ------------------------------
  ;;     glfwInit();
  ;;     glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  ;;     glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  ;;     glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  ;; #ifdef __APPLE__
  ;;     glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  ;; #endif

  ;;     // glfw window creation
  ;;     // --------------------
  ;;     GLFWwindow* window = glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, "LearnOpenGL", NULL, NULL);
  ;;     if (window == NULL)
  ;;     {
  ;;         std::cout << "Failed to create GLFW window" << std::endl;
  ;;         glfwTerminate();
  ;;         return -1;
  ;;     }
  ;;     glfwMakeContextCurrent(window);
  (glfw:with-init-window (:title "LearnOpenGL" :width *screen-width* :height *screen-height*)
    ;;     glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);

    (glfw:set-framebuffer-size-callback 'framebuffer-size-callback)

    ;;     // glad: load all OpenGL function pointers
    ;;     // ---------------------------------------
    ;;     if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    ;;     {
    ;;         std::cout << "Failed to initialize GLAD" << std::endl;
    ;;         return -1;
    ;;     }

    ;;     // build and compile our shader zprogram
    ;;     // ------------------------------------
    ;;     Shader ourShader("4.2.texture.vs", "4.2.texture.fs");
    (let ((our-shader (make-instance 'shader-class
                                     :vertex-source (concatenate 'string *shader-path*
                                                                 "shader-texture.vs")
                                     :fragment-source (concatenate 'string *shader-path*
                                                                   "shader-texture.fs"))))

      ;;     // set up vertex data (and buffer(s)) and configure vertex attributes
      ;;     // ------------------------------------------------------------------
      ;;     float vertices[] = {
      ;;         // positions          // colors           // texture coords
      ;;          0.5f,  0.5f, 0.0f,   1.0f, 0.0f, 0.0f,   1.0f, 1.0f, // top right
      ;;          0.5f, -0.5f, 0.0f,   0.0f, 1.0f, 0.0f,   1.0f, 0.0f, // bottom right
      ;;         -0.5f, -0.5f, 0.0f,   0.0f, 0.0f, 1.0f,   0.0f, 0.0f, // bottom left
      ;;         -0.5f,  0.5f, 0.0f,   1.0f, 1.0f, 0.0f,   0.0f, 1.0f  // top left
      ;;     };

      ;;     unsigned int indices[] = {
      ;;         0, 1, 3, // first triangle
      ;;         1, 2, 3  // second triangle
      ;;     };

      (let ((vertices #(0.5 0.5 0.0    1.0 0.0 0.0    1.0 1.0
                        0.5 -0.5 0.0   0.0 1.0 0.0    1.0 0.0
                        -0.5 -0.5 0.0  0.0 0.0 1.0    0.0 0.0
                        -0.5 0.5 0.0   1.0 1.0 0.0    0.0 1.0))
            (indices #(0 1 3
                       1 2 3)))
        ;;     unsigned int VBO, VAO, EBO;


        ;;     glGenVertexArrays(1, &VAO);
        ;;     glGenBuffers(1, &VBO);
        ;;     glGenBuffers(1, &EBO);

        (let ((vao (gl:gen-vertex-array))
              (vbo (gl:gen-buffer))
              (ebo (gl:gen-buffer)))

          ;;     glBindVertexArray(VAO);

          (gl:bind-vertex-array vao)

          ;;     glBindBuffer(GL_ARRAY_BUFFER, VBO);

          (gl:bind-buffer :array-buffer vbo)

          ;;     glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

          (gl:buffer-data :array-buffer :static-draw (array-to-gl-array vertices :float))

          ;;     glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);

          (gl:bind-buffer :element-array ebo)

          ;;     glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices, GL_STATIC_DRAW);

          (gl:buffer-data :element-array :static-draw (array-to-gl-array indices :unsigned-int))

          ;;     // position attribute
          ;;     glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void*)0);
          ;;     glEnableVertexAttribArray(0);

          (gl:vertex-attrib-pointer 0 3 :float nil (* 8 (cffi:foreign-type-size :float))
                                    (cffi:null-pointer))
          (gl:enable-vertex-attrib-array 0)

          ;;     // color attribute
          ;;     glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void*)(3 * sizeof(float)));
          ;;     glEnableVertexAttribArray(1);

          (gl:vertex-attrib-pointer 1 3 :float nil (* 8 (cffi:foreign-type-size :float))
                                    (cffi:inc-pointer (cffi:null-pointer)
                                                      (* 3 (cffi:foreign-type-size :float))))
          (gl:enable-vertex-attrib-array 1)

          ;;     // texture coord attribute
          ;;     glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void*)(6 * sizeof(float)));
          ;;     glEnableVertexAttribArray(2);

          (gl:vertex-attrib-pointer 2 2 :float nil (* 8 (cffi:foreign-type-size :float))
                                    (cffi:inc-pointer (cffi:null-pointer)
                                                      (* 6 (cffi:foreign-type-size :float))))
          (gl:enable-vertex-attrib-array 2)


          ;;     // load and create a texture
          ;;     // -------------------------
          ;;     unsigned int texture1, texture2;
          ;;     // texture 1
          ;;     // ---------
          ;;     glGenTextures(1, &texture1);

          (let ((texture1 (gl:gen-texture))
                (texture2 (gl:gen-texture)))

            ;;     glBindTexture(GL_TEXTURE_2D, texture1);

            (gl:bind-texture :texture-2d texture1)

            ;;      // set the texture wrapping parameters
            ;;     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);	// set texture wrapping to GL_REPEAT (default wrapping method)

            (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)

            ;;     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

            (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)

            ;;     // set texture filtering parameters
            ;;     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

            (gl:tex-parameter :texture-2d :texture-min-filter :linear)

            ;;     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

            (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

            ;;     // load image, create texture and generate mipmaps
            ;;     int width, height, nrChannels;

            ;;     stbi_set_flip_vertically_on_load(true); // tell stb_image.h to flip loaded texture's on the y-axis.
            ;;     // The FileSystem::getPath(...) is part of the GitHub repository so we can find files on any IDE/platform; replace it with your own image path.
            ;;     unsigned char *data = stbi_load(FileSystem::getPath("resources/textures/container.jpg").c_str(), &width, &height, &nrChannels, 0);

            (multiple-value-bind (data height width)
                (cl-jpeg:decode-image (concatenate 'string *texture-path* "vicentino-test.jpg"))

              (cond (data
                     (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data)
                     (gl:generate-mipmap :texture-2d))

                    ;;     if (data)
                    ;;     {
                    ;;         glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
                    ;;         glGenerateMipmap(GL_TEXTURE_2D);
                    ;;     }
                    ;;     else
                    ;;     {
                    ;;         std::cout << "Failed to load texture" << std::endl;

                    (t (format t "~&Failed to load texture.")))

              ;;     }
              ;;     stbi_image_free(data);
              )
            ;;     // texture 2
            ;;     // ---------
            ;;     glGenTextures(1, &texture2);
            ;;     glBindTexture(GL_TEXTURE_2D, texture2);

            (gl:bind-texture :texture-2d texture2)

            ;;     // set the texture wrapping parameters
            ;;     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);	// set texture wrapping to GL_REPEAT (default wrapping method)

            (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)

            ;;     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

            (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)

            ;;     // set texture filtering parameters
            ;;     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

            (gl:tex-parameter :texture-2d :texture-min-filter :linear)

            ;;     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

            (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

            ;;     // load image, create texture and generate mipmaps
            ;;     data = stbi_load(FileSystem::getPath("resources/textures/awesomeface.png").c_str(), &width, &height, &nrChannels, 0);

            (multiple-value-bind (data width height)
                (cl-jpeg:decode-image (concatenate 'string *texture-path* "vicentino-test.jpg"))
              (cond (data
                     (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigend-byte data)
                     (gl:generate-mipmap :texture-2d))
                    ;;     if (data)
                    ;;     {
                    ;;         // note that the awesomeface.png has transparency and thus an alpha channel, so make sure to tell OpenGL the data type is of GL_RGBA
                    ;;         glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);
                    ;;         glGenerateMipmap(GL_TEXTURE_2D);
                    ;;     }
                    ;;     else
                    ;;     {
                    ;;         std::cout << "Failed to load texture" << std::endl;
                    (t (format t "~&Failed to load texture2."))
                    ;;     }
                    ;;     stbi_image_free(data);

                    ))

            ;;     // tell opengl for each sampler to which texture unit it belongs to (only has to be done once)
            ;;     // -------------------------------------------------------------------------------------------
            ;;     ourShader.use(); // don't forget to activate/use the shader before setting uniforms!
            ;;     // either set it manually like so:
            ;;     glUniform1i(glGetUniformLocation(ourShader.ID, "texture1"), 0);

            (use our-shader)
            (gl:uniformi (gl:get-uniform-location (id our-shader) "texture1") 0)

            ;;     // or set it via the texture class
            ;;     ourShader.setInt("texture2", 1);

            (set-uniform our-shader "texture2" :unsigned-int 1)




            ;;     // render loop
            ;;     // -----------
            ;;     while (!glfwWindowShouldClose(window))
            ;;     {
            (loop until (glfw:window-should-close-p)
                  do (progn

                       ;;         // input
                       ;;         // -----
                       ;;         processInput(window);

                       (process-input)

                       ;;         // render
                       ;;         // ------
                       ;;         glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
                       ;;         glClear(GL_COLOR_BUFFER_BIT);

                       (gl:clear-color 0.2 0.3 0.3 1.0)
                       (gl:clear :color-buffer-bit)

                       ;;         // bind textures on corresponding texture units
                       ;;         glActiveTexture(GL_TEXTURE0);
                       ;;         glBindTexture(GL_TEXTURE_2D, texture1);
                       ;;         glActiveTexture(GL_TEXTURE1);
                       ;;         glBindTexture(GL_TEXTURE_2D, texture2);

                       (gl:active-texture :texture0)
                       (gl:bind-texture :texture-2d texture1)
                       (gl:active-texture :texture1)
                       (gl:bind-texture :texture-2d texture2)

                       ;;         // render container
                       ;;         ourShader.use();

                       (use our-shader)

                       ;;         glBindVertexArray(VAO);

                       (gl:bind-vertex-array vao)

                       ;;         glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

                       (my-gl-draw-elements :triangles 6 :unsigned-int)

                       ;;         // glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
                       ;;         // -------------------------------------------------------------------------------
                       ;;         glfwSwapBuffers(window);

                       (glfw:swap-buffers)

                       ;;         glfwPollEvents();

                       (glfw:poll-events)

                       ;;     }

                       ))


            ;;     // optional: de-allocate all resources once they've outlived their purpose:
            ;;     // ------------------------------------------------------------------------
            ;;     glDeleteVertexArrays(1, &VAO);
            ;;     glDeleteBuffers(1, &VBO);
            ;;     glDeleteBuffers(1, &EBO);

            (gl:delete-vertex-arrays (list vao))
            (gl:delete-buffers (list vbo ebo))
            (destroy our-shader))))))

  ;;     // glfw: terminate, clearing all previously allocated GLFW resources.
  ;;     // ------------------------------------------------------------------
  ;;     glfwTerminate();

  ;;     return 0;
  ;; }

  )

;; // process all input: query GLFW whether relevant keys are pressed/released this frame and react accordingly
;; // ---------------------------------------------------------------------------------------------------------
;; void processInput(GLFWwindow *window)
