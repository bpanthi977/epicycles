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


(defun test ()
  (with-tmp-image (im (read-image "~/Dev/Python/Epicycles/a.png"))
				  (setf im (o:coerce-image im 'o:8-bit-gray-image))
				  (setf im (o:threshold-image im 127))
				  (setf im (invert im))
				  (setf *img-a* im)
				  (o:with-image-bounds (y x) im 
					(setf im (draw-contours (find-contour im) y x)))
				  (threshold-to-8big im)))
