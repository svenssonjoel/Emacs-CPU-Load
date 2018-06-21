
;; Joel Svensson 2018


;; elisp
(require 'seq)


;; Global variables 
(defvar last-val ())

(defvar cpu-load-buffer ())

(defvar cpu-load-timer ())

(defvar cpu-load-num-cpus ())

(defconst cpu-load-bar-width 39) 

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

(defun usage-part (old-values values)
  "returns load over last period of time as value between 0 and 1"
  (let ((idle-old-val (car (nthcdr 3 old-values)))
	(tot-old-val  (seq-reduce #'+ old-values 0)))
    (let ((idle-val (- (car (nthcdr 3 values)) idle-old-val))
	  (tot-val  (- (seq-reduce #'+ values 0) tot-old-val)))
      (- 1.0 (/ (float idle-val) (float tot-val)))))
  )


(defun usage-percentage (old-values values)
  "Returns cpu usage percentage"
  (* 100 (usage-part old-values values)))

(defun cpu-load-bar-string (old-values values)
  "returns a bar representing the load percentage as a string"
  (let ((bar-string "[")
	(load (usage-part old-values values))
	(tick-size (/ 100 cpu-load-bar-width)))
    (let ((ticks (/ p tick-size))
	  (bar-length (* load cpu-load-bar-width)))
      (concat
       (concat "["
	       (make-string (round bar-length) ?#))
       (concat
	(make-string
	 (- cpu-load-bar-width (round bar-length)) ?\s) "]" )))))
    

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


;; Function launched periodically from timer
(defun measure-cpu (buffer)
  "Presents cpu usage information in buffer"
  (with-current-buffer buffer
    (let ((values (mapcar 'cpu-values (cpu-lines (read-lines "/proc/stat")))))
      (progn
	(setq buffer-read-only nil)
	(erase-buffer)
	(let ((p (usage-percentage (car last-val) (car values))))
	  (progn
	    (insert "Total") 
	    (insert (cpu-load-bar-string (car last-val) (car values)))
	    (insert (format "%.3f%%\n" p))))

	(setq i 1)
	(while (< i cpu-load-num-cpus)
	  (let ((p (usage-percentage
		    (car (nthcdr i last-val))
		    (car (nthcdr i values)))))
	    (progn
	      (insert (concat " CPU" (int-to-string (- i 1))))
	      (insert (cpu-load-bar-string
		       (car (nthcdr i last-val))
		       (car (nthcdr i values))))
	      (insert (format "%.3f%%\n" p))))
	  (setq i (+ i 1))
	  )
	
	(setq buffer-read-only t)
	(setq last-val values)))
    )
  )
  

;; debug
;; (cancel-timer cpu-load-timer)
;; (measure-cpu cpu-load-buffer)

;; (cancel-timer (car timer-list))
;; timer-list

