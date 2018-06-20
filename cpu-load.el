
;; Joel Svensson 2018


;; elisp
(require 'seq)


;; Global variables 
(defvar last-val ())

(defvar cpu-load-buffer ())

(defvar cpu-load-timer ())

(defvar cpu-load-num-cpus ())

(defconst cpu-load-bar-width 20) ;must divide 100

;; Functions 

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))


(defun cpu-lines (ls)
  "Takes a list of strings as input and returns a list of those"
  "strings that begin with the prefix cpu"
  (seq-filter (lambda (str) (string-prefix-p "cpu" str)) ls))

;; TODO: error checking ?? (anywhere ??) 
(defun cpu-values (string)
  "Takes a string (one cpu line) from the stat file and parses out the values"
  (mapcar 'string-to-number (cdr (split-string string " " t))))

(defun total-usage-percentage-string (old-values values)
  "Returns a string representing cpu usage percentage"
  (let ((idle-old-val (car (nthcdr 3 old-values)))
	(tot-old-val  (seq-reduce #'+ old-values 0)))
    (let ((idle-val (- (car (nthcdr 3 values)) idle-old-val))
	  (tot-val  (- (seq-reduce #'+ values 0) tot-old-val)))
      (number-to-string (* 100 (- 1.0 (/ (float idle-val) (float tot-val)))))))
  )

(defun total-usage-percentage (old-values values)
  "Returns cpu usage percentage"
  (let ((idle-old-val (car (nthcdr 3 old-values)))
	(tot-old-val  (seq-reduce #'+ old-values 0)))
    (let ((idle-val (- (car (nthcdr 3 values)) idle-old-val))
	  (tot-val  (- (seq-reduce #'+ values 0) tot-old-val)))
      (* 100 (- 1.0 (/ (float idle-val) (float tot-val))))))
  )

(defun cpu-load-bar-string (p)
  "returns a bar representing the load percentage as a string"
  (let ((bar-string "[")
	(tick-size (/ 100 cpu-load-bar-width)))
    (let ((ticks (/ p tick-size)))
      (concat
       (concat "["
	       (make-string ticks ?#))
       (concat (make-string (- cpu-load-bar-width ticks) ?\s) "]" )))))


(cpu-load-bar-string 30)
    ;;(while (> p (/ 100 cpu-load-bar-width))
    ;;  (setq bar-string (cons "#" bar-string
    

(defun measure-cpu-init ()
  "Initialise some state related to the cpu-load measurements"
  (if cpu-load-timer
      (cancel-timer cpu-load-timer)
    ())

  (let ((tmp (cpu-lines (read-lines "/proc/stat"))))
    (setq cpu-load-num-cpus (length tmp)))
  
  (setq last-val (make-list cpu-load-num-cpus '(0 0 0 0 0 0 0 0 0 0)))
  
  (setq cpu-load-buffer (get-buffer-create "cpu-load"))
  (setq cpu-load-timer (run-at-time t 1 #'measure-cpu cpu-load-buffer))
  )


;; timer controller function
;; TODO: improve
(defun measure-cpu (buffer)
  "Presents cpu usage information in buffer"
  (with-current-buffer buffer
    (let ((values (mapcar 'cpu-values (cpu-lines (read-lines "/proc/stat")))))
      (progn
	(setq buffer-read-only nil)
	(erase-buffer)
	(let ((p (total-usage-percentage (car last-val) (car values))))
	  (progn
	    (insert "Total") 
	    (insert (cpu-load-bar-string (round p)))
	    (insert (format "%.3f%%\n" p))))

	(setq i 1)
	(while (< i cpu-load-num-cpus)
	  (let ((p (total-usage-percentage (car (nthcdr i last-val)) (car (nthcdr i values)))))
	    (progn
	      (insert (concat " CPU" (int-to-string (- i 1))))
	      (insert (cpu-load-bar-string (round p)))
	      (insert (format "%.3f%%\n" p))))
	  (setq i (+ i 1))
	  )
	
	(setq buffer-read-only t)
	(setq last-val values)))
    )
  )
  

(cancel-timer cpu-load-timer)
;; (measure-cpu cpu-load-buffer)

(cancel-timer (car timer-list))
timer-list

