
;; Joel Svensson 2018


;; elisp
(require 'seq)

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

(mapcar (lambda (cv) (seq-reduce #'+ cv 0)) (mapcar 'cpu-values (cpu-lines (read-lines "/proc/stat"))))

(setq last-val (cpu-values (car (cpu-lines (read-lines "/proc/stat")))))
(length (cpu-lines (read-lines "/proc/stat")))
(cpu-lines (read-lines "/proc/stat"))


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


(defvar last-val ())


(total-usage-percentage-string last-val (cpu-values (car (cpu-lines (read-lines "/proc/stat")))))

(nthcdr 0 '(1 2 3))

(setq cpu-load-buffer (get-buffer-create "cpu-load"))

;; timer controller function
(defun measure-cpu (buffer)
  "Presents cpu usage information in buffer"
  (if (not last-val)
      (setq last-val (0 0 0 0 0 0 0 0 0 0))) 
  (with-current-buffer buffer
    (erase-buffer)
    (let ((values (cpu-values (car (cpu-lines (read-lines "/proc/stat"))))))
      (insert (format "CPU-LOAD: %.3f%%" (total-usage-percentage last-val values)))
      (setq last-val values))
    )
  )

(setq cpu-load-timer (run-at-time t 1 #'measure-cpu cpu-load-buffer))

(cancel-timer cpu-load-timer)
;; (measure-cpu cpu-load-buffer)

;; timer-list


