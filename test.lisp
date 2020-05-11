;;
;;; Functions used while developing, debugging
;;

(defun read-image (&optional (path  "~/Dev/Python/Epicycles/a.png"))
  (o:read-image-file path))

(defun save-to-temp (image)
  (o:write-png-file #p"E:/tmp/tmp-cl.png" image))

(defmacro with-tmp-image ((var image)&rest body)
  `(let ((,var ,image))
	 (save-to-temp (progn ,@body
						  ))))

(defun threshold-to-8big (image)
  "1-bit to 8-bigt grayscale image"
  (o:with-image-bounds (y x) image
	(let* ((gray-image (o:make-8-bit-gray-image y x)))
	  (declare (type o:8-bit-gray-image gray-image)
			   (type o:1-bit-gray-image image))
	  (o:do-pixels (i j) image
		(setf (o:pixel gray-image i j)
			  (if (zerop (o:pixel image i j))
				  0
				  255)))
	  gray-image)))


(defun draw-contours (contours ymax xmax)
  (let ((gray-image (o:make-1-bit-gray-image ymax xmax :initial-element 0)))
	(loop for c in contours do
	  (loop for (y x) in c do
		(setf (o:pixel gray-image y x)
			  1)))
	gray-image))


(defun print-boundary (y x image)
  (flet ((p (dir)
		   (multiple-value-bind (yn xn) (point-yx y x dir)
			 (o:pixel image yn xn))))
	(format t "
~d  ~d  ~d ~% 
~d |~d| ~d ~%
~d  ~d  ~d 
" (p 0) (p 1) (p 2) (p 7) (o:pixel image y x) (p 3) (p 6) (p 5) (p 4))))

(defun threshold (image level)
  (declare (type o:8-bit-gray-image image))
  (let ((p 0))
	(declare (type (integer 0 255) p))
	(o:do-pixels (i j) image
	  (setf p (o:pixel image i j))
	  (setf (o:pixel image i j) (if (> p level)
									255
									0))))
  image)

(defun test ()
  (with-tmp-image (im (read-image "E:/Dev/Python/friends/images/Binod Ghale.png")) ;;"~/Dev/Python/Epicycles/ashvin.png"))
				  (setf im (o:coerce-image im 'o:8-bit-gray-image))
				  ;;(setf im (o:threshold-image im 127))
				  ;;(setf im (invert im))
				  ;; (o:with-image-bounds (y x) im 
				  ;; 	(setf im (draw-contours (find-contours im) y x)))
				  ;;(setf im (threshold-to-8big im))
				  (setf im (o:blur-image im))
				  (setf im (o:edge-detect-image im))
				  (setf im (invert (o:threshold-image im 20)))
				  (threshold-to-8big im)))


				  
