;;;; epicycles.lisp

(in-package #:epicycles)

;;
;;; Discrete Fourier Transform 
;; 

;; Defintion Method / Bruteforce method
;; TODO : USE FFT 
(defun dft (data)
  "Compute dft of array of numbers (complex/real) and return array of complex"
  (let ((transform (make-array (length data))))
	(loop with N = (length data)
		  for |n| from  0 to (1- N) do
			(setf (aref transform |n|)
				  (loop for k from 0 to (1- N)
						with coeff = (* -2 pi #C(0 1) |n| (/ N))
						summing (* (aref data k) (exp (* coeff k))))))
	transform))

(defun power-of-two (x)
  "Is x a power of two number"
  (and (> x 0)
       (zerop (logand x (1- x)))
       (1- (integer-length x))))

(defun pads (n)
  "Number to add to n to make is a power of 2"
  (if (= n 1)
	  1
	  (if (power-of-two n)
		  0
		  (- (expt 2 (integer-length n)) n))))

(defun pad-data (data)
  (let ((pads (pads (length data))))
	(unless (= 0 pads)
	  (loop with new-data = (make-array (+ (length data) pads)
										:element-type 'complex
										:initial-element #C(0.0 0.0))
			for i from 0 to (1- (length new-data)) do
			  (setf (aref new-data i) (aref data (mod i (length data))))
			finally (setf data new-data)))							 
	data))

(defun fft (data)
  (bordeaux-fft:sfft (pad-data data)))

;;
;;; Contour finding from an image file 
;;

(defun invert (image)
  "flip 1's and 0's "
  (declare (type o:1-bit-gray-image image))
  (let (p)
	(o:do-pixels (i j) image
	  (setf p (o:pixel image i j))
	  (setf (o:pixel image i j) (if (= p 0) 1 0))))
  image)

(defun find-first-point (image)
  "Find first 1 point from top left of image"
  (declare (type o:1-bit-gray-image image))
  (o:do-pixels (i j) image
	(if (eql (o:pixel image i j) 1)
		(return-from find-first-point (values i j)))))

(declaim (inline point-xy))
(defun point-yx (y x dir)
  "Get the coordinate of point at `dir' from base point `(y,x)'
Direction number of points surrounding a point
| 0 | 1 | 2 |
| 7 | # | 3 |
| 6 | 5 | 4 |"
  (declare (type integer x y dir))
  (ecase dir
	(0 (values (+ y -1) (+ x -1)))
	(1 (values (+ y -1) (+ x +0)))
	(2 (values (+ y -1) (+ x +1)))
	(3 (values (+ y +0) (+ x +1)))
	(4 (values (+ y +1) (+ x +1)))
	(5 (values (+ y +1) (+ x +0)))
	(6 (values (+ y +1) (+ x -1)))
	(7 (values (+ y +0) (+ x -1)))))

(declaim(inline next-dir))
(defun next-dir (d)
  "return starting search direction when entered to a pixel from direction `d'"
  (case d
	(1 0)(2 0)(3 2)(4 2)(5 4)(6 4)(7 6)(0 6)))

(declaim (inline out-of-bounds))
(defun out-of-bounds (y x ymax xmax)
  "Check if (y,x) is out of bound for image of size ymax * xmax"
  (or (> y ymax) (> x xmax) (< x 0) (< y 0)))

(defun find-contours (image)
  "Find contours of image; currently finds only one contour"
  (declare (type o:1-bit-gray-image image))
  (let (sx sy sdir dir contour x y p)
	;;(declare (type integer sx sy x y p sdir dir))
	(o:with-image-bounds (ymax xmax) image
	  (decf ymax) (decf xmax) 
	  ;; find first point and add it to contour
	  (setf (values sy sx) (find-first-point image)
			dir 2 sdir 2)			
	  (push (list  sy sx) contour)
	  ;; now walk through its boundary
	  (loop with xb = sx
			with yb = sy do
			  (setf (values y x) (point-yx yb xb dir))
			  (setf p (if (out-of-bounds y x ymax xmax)
						  0
						  (o:pixel image y x)))
			  (if (= p 1)
				  (if (and (= x sx)
						   (= y sy))
					  (return) ;; if reached the first point, end 
					  (progn                      ;; else, 
						(push (list y x) contour) ;; add point to contours 
						(setf xb x yb y           ;; this is now the new base point 
							  dir (next-dir dir)
							  sdir dir)))
				  (progn
					(setf dir (mod (1+ dir) 8)) ;; change direction 
					(if (= dir sdir)            ;; if reached staring direction, means no new 1-points
						(return)))))
	  ;; finally return contours
	  (list contour))))

;;
;;; Find DFT of Contour points of an image file 
;;

(defun points-to-complex (contours)
  "Convert points `(y,x)' on the contours to complex numbers `#C(x, y)'"
  (let ((data (make-array (loop for c in contours summing (length c))
						  :element-type 'complex
						  :adjustable nil
						  :initial-element  #C(0 1))))
	(loop for c in contours
		  with i = 0 do
	  (loop for p in c do 
		(setf (aref data i) (complex (second p) (first p)))
		(incf i)))
	data))

(defun find-1-points (image)
  "Returns array of all white points `#C(x,y)' in image"
  (let (points (count 0))
	(declare (type o:1-bit-gray-image image))
	(o:do-pixels (i j) image
	  (when (= (o:pixel image i j) 1)
		(incf count)
		(push (complex j i) points)))
	(make-array count :element-type 'complex :initial-contents points)))

(defun find-nearest-point (image y0 x0)
  "Find the 1-point nearest to given (y0,x0) point"
  (macrolet ((pixel-check ()
			   `(when (= 1 (o:pixel image y x))
				  (setf (o:pixel image y x) 0)
				  (return-from find-nearest-point (values y x)))))
	(o:with-image-bounds (ymax xmax) image
	  (declare (type integer ymax xmax))
	  (loop for s integer from 2 to (max x0 y0 (- ymax y0) (- xmax x0)) by 2
			with x integer = 0 
			with y integer = 0 do
			  ;; top row 
			  (setf y (- y0 (1- s)))
			  (when (>= y 0) 
				(loop for x from (max 0 (- x0 (1- s))) below (min xmax (+ x0 s)) do
				  (pixel-check)))
			  ;; right column
			  (setf x (+ x0 (1- s)))
			  (when (< x xmax)
				(loop for y from (max 0(- y0 (1- s))) below (min ymax (+ y0 s)) do
				  (pixel-check)))
			  ;; bottom row
			  (setf y (+ y0 (1- s)))
			  (when (< y ymax)
				(loop for x from (min (1- xmax) (+ x0 (1- s))) downto (max 0 (- x0 (1- s))) do
				  (pixel-check)))
			  ;; left column
			  (setf x (- x0 (1- s)))
			  (when (>= x 0)
				(loop for y from (min (1- ymax) (+ y0 (1- s))) downto (max 0 (- y0 (1- s))) do
				  (pixel-check)))))))

(defun walk-all-points (image)
  "Returns array of all points #C(x,y) in image, arranged in a path"
  (let ((points (make-array 1 :element-type 'complex
							  :initial-element  #C(0 1)
							  :fill-pointer t))
		x y (count 0))
	(setf (values y x) (find-first-point image))
	(setf (o:pixel image y x) 0)
	(when (and x y)
	  (vector-push-extend (complex x y) points)
	  (incf count)
	  (loop do
		(setf (values y x) (find-nearest-point image y x))
		(if x
			(vector-push-extend (complex x y) points)
			(return))
		(incf count)))
	(make-array (length points)
				:element-type 'complex
				:displaced-to points)))

(defun center-data (data)
  (let ((avg (/ (reduce #'+ data) (length data))))
	(loop for i from 0 to (1- (length data)) do
	  (decf (aref data i) avg))))

(defun read-image-and-caluate-dft (image type fft)
  (let (data)
	;; change to 8 bit gray i.e. average out the rgb values
	(setf image (o:coerce-image image 'o:8-bit-gray-image))
	(setf image (o:blur-image image))
	(setf image (o:edge-detect-image image))
	;; threshold to 1-bit image i.e values above threshold are 1 and below are zero
	(setf image (o:threshold-image image 20))
	(ecase type
	  (:contour 
	   ;; get contour points as complex numbers
	   (setf data (points-to-complex (find-contours image))))
	  (:direct
	   (setf data (find-1-points image)))
	  (:walk-all
	   (setf data (walk-all-points image))))
	(center-data data)
	;; return dft
	(if fft
		(fft data)
		(dft data))))

;;
;;; Circle Drawing mechanisms
;;


(defstruct circle
  (radius  0 :type integer)
  (exact-radius 0.0 :type float)
  (phase 0.0 :type float)
  (del-phase 0.0 :type float))

(declaim (inline complex-point))
(defun complex-point (complex)
  "Converts `complex' to `sdl:point'"
  (sdl:point :x (truncate (realpart complex))
			 :y (truncate (imagpart complex))))

(defmethod draw-circle ((c circle) origin)
  (sdl:draw-circle (complex-point origin)
				   (circle-radius c)
				   :color sdl:*black*
				   ))

(defmethod update-phase((c circle))
  (incf (circle-phase c) (circle-del-phase c)))

(defmethod draw-circles-end-to-end (circles origin)
  (let (end)
	(loop for c in circles do
	  (if (> (circle-radius c) 1) (draw-circle c origin))
	  (setf end (* *scale* (circle-exact-radius c) (exp (complex 0 (circle-phase c)))))
	  (sdl:draw-line (complex-point origin) (complex-point (incf origin end)) :color sdl:*red*)
	  (update-phase c)
		  finally (return origin))))

(declaim (inline amplitude))
(defun amplitude (complex)
  (sqrt (+ (expt (realpart complex) 2)
		   (expt (imagpart complex) 2))))

(declaim (inline argument))
(defun argument (complex)
  (atan (imagpart complex)
		(realpart complex)))

(defun create-circles-from-dft (dft &optional (size-factor 500) (speed-factor 1))
  (loop for f across dft
		with length = (length dft)
		for i from 0
		for radius = (/ (amplitude f) size-factor)
		collect (make-circle :radius (round radius)
							 :exact-radius radius
							 :phase (argument f)
							 :del-phase (* speed-factor 2 pi (/ i length)))))

;;
;;; Finally
;;
(defparameter *project-dir* (asdf:system-source-directory "epicycles"))
(defparameter *default-file* (merge-pathnames "a.png" *project-dir*))

(defparameter *translation* #C(0 0))
(defparameter *scale* 1)

(defun scale (circles scale)
  (mapcar #'(lambda (c)
			  (setf (circle-radius c)
					(round (* (circle-exact-radius c) scale))))
		  circles))

(defun main(&key (file *default-file*) (speed-factor 1) (type :contour) (scale 1) (fft t))
  (setf *translation* #C(500 500)
		*scale* 1)
  (let* ((image (o:read-image-file file))
		 (dft (read-image-and-caluate-dft image type fft))
		 (circles (create-circles-from-dft dft (* 5 (/ scale) (array-dimension image 0)) speed-factor))
		 (final-points nil)
		 (dds (make-instance 'gui:dragndropnscale
							 :scale *scale*
							 :translation (sdl:point :x (realpart *translation*)
													 :y (imagpart *translation*))
							 :origin (sdl:point :x 0 :y 0)
							 :callback (lambda (tr s)
										 (setf *translation* (complex (sdl:x tr) (sdl:y tr))
											   *scale* s)
										 (scale circles *scale*))))
		 scale-widget)
	;; sort circle to look nice
	(setf circles (sort circles #'> :key #'circle-radius))	
	;; Initialize sdl 
	(sdl:with-init ()
	  (sdl:window 1000 1000)
	  (sdl:enable-unicode)
	  (sdl:initialise-default-font)
	  (setf sdl:*default-color* sdl:*black*)
	  (sdl:with-events ()
		(:quit-event () t)
		(:key-down-event (:key key :unicode code)
						 (case key
						   (:sdl-key-q (sdl:push-quit-event))
						   (:sdl-key-k (scale circles (setf *scale* (* *scale* 1.2))))
						   (:sdl-key-l (scale circles (setf *scale* (/ *scale* 1.2))))
						   (t (if (not scale-widget)
								  (setf scale-widget (make-instance 'gui::number-entry
																	:value *scale*
																	:x (sdl:mouse-x)
																	:y (sdl:mouse-y)
																	:selected-range t))																	
								  (progn
									(let ((char (code-char code)))
									  (if (or (char= char #\Return) (char= char #\Esc))
										  (progn (scale circles (setf *scale* (gui::value scale-widget))) 
												 (setf scale-widget nil))
										  (gui::key-down scale-widget :char char))))))))
		(:mouse-motion-event
		 (:x x :y y)
		 (gui:mouse-motion dds x y))
		(:mouse-button-down-event
		 (:button button :x x :y y)
		 (gui:mouse-button-down dds button x y))
		(:mouse-button-up-event
		 (:button button :x x :y y)
		 (gui:mouse-button-up dds button x y))
		(:idle ()
			   (sdl:clear-display sdl:*white*)
			   ;; draw circles and add the farend point 
			   (push (complex-point (draw-circles-end-to-end circles *translation*))
					 final-points)
			   ;; draw the farend points i.e. the points on the image
			   (loop for p in (rest final-points)
					 with prevp = (first final-points) do
					   (sdl:draw-line prevp p
									  :color sdl:*blue*
									  :aa t)
					   (setf prevp p))
			   ;; widgets
			   (if scale-widget
				   (gui::draw scale-widget))

											  
			   (sdl:update-display))))))
