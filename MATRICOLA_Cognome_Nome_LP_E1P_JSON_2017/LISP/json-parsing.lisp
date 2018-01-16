(defvar *cpar* '(#\}))
(defvar *apar* '(#\]))
(defvar *spaces* '(#\Space #\Newline #\Tab))
(defvar *apix* '(#\" #\'))
(defvar *digits* '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
(defvar *symbols* '(#\. #\+ #\-))
 

;; Convert a string to list
(defun string2list (string)
  (coerce string 'list))
 
;; Builds a list reverted
(defun consend (e l)
  (if (null l) (list e)
    (cons (first l) (consend e (rest l)))))

;; Gets everything but last and first element 
(defun rest-but-last (list)
  (rest (but-last list)))
 
;; Gets everything but last element
(defun but-last (list)
  (reverse (rest (reverse list))))

;; Gets last elements of list 
(defun endlist (list)
  (first (reverse list)))

;; Delete all whitespaces
(defun del-whitespaces (l)
  (let ((ch (car l)))
    (cond ((or (eql ch #\Space)
               (eql ch #\Newline)
               (eql ch #\Tab)) (del-whitespaces (cdr l)))
          ((null ch) nil)
          ((or (eql ch #\")
               (eql ch #\'))
           (cons ch
                 (del-whitespaces-i (cdr l))))
          (T (cons ch (del-whitespaces (cdr l)))))))

(defun del-whitespaces-i (l)
  (let ((ch (car l)))
    (cond ((null ch) nil)
          ((eql ch #\\)
           (cons (car l)
                 (cons (second l) 
                       (del-whitespaces-i (cdr (cdr l))))))
          ((or (eql ch #\")
               (eql ch #\')) (cons ch 
                                   (del-whitespaces (cdr l))))
          (T (cons ch (del-whitespaces-i (cdr l)))))))

;; Parse a json object
(defun json-object (list)
  (let ((i (first list)) (jsonobj list))
    (cond
     ((and (member i *cpar*) (null (rest jsonobj))) 
      (list (list 'json-obj)))
     (T (append
         (list 'json-obj)
         (json-members jsonobj))))))

;; Parse all members of an object
(defun json-members (list)
  (let ((i (first list)))
    (cond
     ((null list) nil)
     ((member i *apix*)  (json-pair (rest list) (list i)))
     (T (error "~%MALFORMED OBJECT")))))

;; Parse all pairs of a JSON object 
(defun json-pair (list accum)
  (let ((i (first list)))
    (cond ((and (member i *apix*) (eql (cadr list) '#\:))
           (json-pair-comma (rest (rest list)) 
                            (consend i accum) nil))
          ((and (member i *apix*) (not (eql (cadr list) '#\:)))
           (error "~%MALFORMED OBJECT"))
          (T (json-pair (rest list) (consend i accum)))))) 

;; Finds a comma, parsing what is before and after
(defun json-pair-comma (list &optional (before nil) (accum nil))
  (let ((i (first list)) (e (rest list)))
    (cond
     ((and (null e) (eql i '#\}))
      (list (append (analyze-payload before)
                    (analyze-payload accum))))
     ((eql i '#\,)
      (append (list (append (analyze-payload before)
                            (analyze-payload accum)))
              (json-members e)))
     ((eql i '#\[)       
      (append (list (append (analyze-payload before)
                            (analyze-payload accum)))
              (json-elements-nested list)))
     ((and (null e) (not (eql i '#\})))
      (error "~%MISSING LAST PARENTHESIS"))
     (T (json-pair-comma (rest list) before (consend i accum))))))

;; Fids the type of the value (number, string, object or array)
(defun analyze-payload (list)
  (let ((i (first list)))
    (cond ((member i *apix*) (list (json-string list)))
          ((or (member i *digits*) (member i *symbols*)) 
           (list (json-numbers list)))
          ((or (eql i '#\[) (eql i '#\{)) 
           (list (json-parser list))))))

;; Parse a number
(defun json-numbers (list &optional (accum nil))
  (let ((i (first list)))
    (cond
     ((eql i '#\.) (json-numbers-float list accum))
     ((null list) (parse-integer (coerce accum 'string)))
     ((member i *spaces*) (error "~%NOT VALID VALUE"))
     (T (json-numbers (rest list) (consend i accum))))))

;; Parse a rational number
(defun json-numbers-float (list &optional (accum nil))
  (parse-float (coerce (append accum list) 'string)))

;; Parse a string, contained in ' or "
(defun json-string (list)
  (let ((i (first list)) (e (endlist list)))
    (cond
     ((and (eql i '#\") (eql e '#\")) 
      (coerce (rest-but-last list) 'string))
     ((and (eql i '#\') (eql e '#\')) 
      (coerce (rest-but-last list) 'string))
     (T (error  "~% MISMATCHED APIX")))))

;; Parse a JSON array
(defun json-array (list)
  (let ((i (first list)) (jsonobj list))
    (cond
     ((and (member i *apar*) (null (rest jsonobj))) 
      (list '(json-array)))
     (T (append '(json-array) (json-elements jsonobj))))))

;; Parse all elements
(defun json-elements (list &optional (accum nil))
  (let ((i (first list)) (e (rest list)))
    (cond
     ((null e)
      (analyze-payload accum))
     ((or (eql i '#\{) (eql i '#\[))
      (json-elements-nested list)) 
     ((eql i '#\,)
      (append (analyze-payload accum)
              (json-elements e)))
     (T (json-elements e (consend i accum))))))

;; Parse nested elements in objects or array
(defun json-elements-nested (list &optional accum)
  (let ((i (first list)) (e (rest list)))
    (cond
     ((or (eql i '#\}) (eql i '#\]))
      (append (list (json-parser (consend i accum)))
              (json-elements (rest list))))
     (T (json-elements-nested e (consend i accum))))))

;; Parse a JSON from a list of chars
(defun json-parser (list)
  (cond ((eql (first list) '#\{)
         (json-object (rest list)))
        ((eql (first list) '#\[)
         (json-array (rest list)))
        (T (error  "~% NOT A JSON OBJECT"))))

;; Main function to parse a JSON from string
(defun json-parse (json)
  (let ((list (string2list json)))
    (json-parser (del-whitespaces list))))

;; Load strings from file and parse JSON
(defun json-load (filename)
  (json-parse (read-file filename)))

;; Read all strings from file
(defun read-file (filename)
  (if (null filename)
      (error "Filename is NIL")
    (with-open-file
        (in filename
            :if-does-not-exist :error)
      (let ((json (make-string (file-length in))))
        (read-sequence json in)
        json))))

;; Write JSON to a file
(defun json-write (list filename)
  (cond ((null filename) (error "Filename is NIL"))
        (T (with-open-file
               (stream filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
             (format stream (list-to-string list))))))

;; Transform JSON into a string
(defun list-to-string (list)
  (cond 
   ((eql (first list) 'json-obj)
    (concatenate 'string "{" (write-obj(rest list)) "}"))
   ((eql (first list) 'json-array)
    (concatenate 'string "[" (write-array(rest list)) "]"))
   (T (error "Malformed object"))))

;; Transform back an object
(defun write-obj (list)
  (let ((i (first list)) (r (rest list)))
    (cond
     ((null list) "")
     ((concatenate 'string (write-pair i)(write-obj r))))))

;; Transform back an array
(defun write-array(list)
  (let ((i (first list)) (r (rest list)))
    (cond
     ((null list) "")
     ((null (rest list)) (write-value i))
     (T (concatenate 'string (write-value i) "," 
                     (write-array r))))))

;; Transform back a pair
(defun write-pair(list)
  (concatenate 'string "\"" (car list) "\"" ":" 
               (write-value(cadr list))))

;; Transform back a value
(defun write-value (list)
  (cond 
   ((numberp list) (write-to-string list))
   ((stringp list) (concatenate 'string "\"" list "\""))
   (T (list-to-string list))))

;; Get back an element
(defun json-get (list &rest fields)
  (json-fields list fields))

;; Parse request
(defun json-fields (lista val)
  (cond ((null val) lista)
	((stringp (first val)) 
         (json-fields (search-key (first val) lista) (rest val)))
	((and (numberp (first val)) (< (first val) 0)) 
         (error "CAN'T USE NEGATIVE INDEXES"))
	((numberp (first val))
	 (json-fields 
          (search-index lista (first val)) (rest val)))))

;; Search using a key in a JSON object
(defun search-key (key list)
  (let ((l (car list)))
    (cond ((null list) (error "~%KEY NOT FOUND"))
          ((equal 'json-obj (first list)) 
           (search-key key (rest list)))
          ((equal key (first l))
           (first (rest l))) 
          (T (search-key key (cdr list))))))

;; Search using the index in a JSON array
(defun search-index (list index)
  (cond 
   ((eql 'json-array (first list)) 
    (search-index (rest list) index))
   ((null list) (error "not found"))
   ((eq index 0) (car list))
   (T (search-index (cdr list) (- index 1)))))



