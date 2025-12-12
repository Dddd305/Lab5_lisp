(require "uiop")

;;; Завдання 1: Читання CSV у Геш-таблицю
(defun split-csv-line (line)
  (mapcar (lambda (s) (string-trim '(#\Space #\Tab #\Return) s))
          (uiop:split-string line :separator '(#\,))))

(defun read-csv-to-hash-table (file-path key)
  (let ((db (make-hash-table))
        (header-skipped nil))
    (with-open-file (stream file-path :direction :input :if-does-not-exist :error)
      (loop for line = (read-line stream nil)
            while line do
            (if (not header-skipped)
                (setf header-skipped t)
                (let ((fields (split-csv-line line))
                      (record (make-hash-table)))
                  
                  (cond
                    ((eq key :specialties)
                     (let ((id (parse-integer (first fields)))
                           (code (second fields))
                           (name (third fields)))
                       (setf (gethash :id record) id)
                       (setf (gethash :code record) code)
                       (setf (gethash :name record) name)
                       ;; Add to main DB using ID as key
                       (setf (gethash id db) record)))

                    ((eq key :articles)
                     (let ((id (parse-integer (first fields)))
                           (spec-id (parse-integer (second fields)))
                           (title (third fields))
                           (author (fourth fields))
                           (year (parse-integer (fifth fields))))
                       (setf (gethash :id record) id)
                       (setf (gethash :specialty-id record) spec-id)
                       (setf (gethash :title record) title)
                       (setf (gethash :author record) author)
                       (setf (gethash :year record) year)
                       (setf (gethash id db) record))))))))
    db))

;;; Завдання 2: Запис Геш-таблиці у CSV
(defun write-hash-table-to-csv (file-path db key)
  (with-open-file (stream file-path :direction :output 
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
    (cond 
      ((eq key :specialties) (format stream "id,code,name~%"))
      ((eq key :articles) (format stream "id,specialty_id,title,author,year~%")))
    
    (maphash (lambda (k v)
               (declare (ignore k))
               (cond
                 ((eq key :specialties)
                  (format stream "~D,~A,~A~%"
                          (gethash :id v)
                          (gethash :code v)
                          (gethash :name v)))
                 ((eq key :articles)
                  (format stream "~D,~D,~A,~A,~D~%"
                          (gethash :id v)
                          (gethash :specialty-id v)
                          (gethash :title v)
                          (gethash :author v)
                          (gethash :year v)))))
             db))
  file-path)

;;; Завдання 3: Конвертація у Асоціативний список (Alist)
(defun hash-table-to-alist (ht)
  (let ((result nil))
    (maphash (lambda (k record-hash)
               (declare (ignore k))
               (let ((record-alist nil))
                 (maphash (lambda (f v) (push (cons f v) record-alist)) record-hash)
                 (push record-alist result)))
             ht)
    result))

;;; Завдання 4: Вибірка (Select)
(defun select (file-path key)
  (let ((db (read-csv-to-hash-table file-path key)))
    (lambda (field value)
      (let ((results nil))
        (maphash (lambda (k record)
                   (declare (ignore k))
                   (let ((field-val (gethash field record)))
                     (when (cond 
                             ((functionp value) (funcall value field-val))
                             ((equal field-val value) t))
                       (push record results))))
                 db)
        results))))

(defun print-hash-tables (records)
  (if (null records)
      (format t "No records found.~%")
      (dolist (rec records)
        (format t "~%Record:~%")
        (maphash (lambda (k v) (format t "  ~A: ~A~%" k v)) rec))))

;;; Тести
(defun test-lab5 ()
  (format t "~%--- Lab 5 Testing: Scientific Articles ---~%")
  (unless (and (probe-file "specialties.csv") (probe-file "articles.csv"))
    (format t "ERROR: CSV files not found! Please create 'specialties.csv' and 'articles.csv'.~%")
    (return-from test-lab5))

  (format t "~%[Test 1] Select Specialty with Code '121':~%")
  (let ((selector (select "specialties.csv" :specialties)))
    (print-hash-tables (funcall selector :code "121")))

  (format t "~%[Test 2] Select Articles for Specialty ID 1:~%")
  (let ((selector (select "articles.csv" :articles)))
    (print-hash-tables (funcall selector :specialty-id 1)))

  (format t "~%[Test 3] Select Articles published after 1980:~%")
  (let ((selector (select "articles.csv" :articles)))
    (print-hash-tables (funcall selector :year (lambda (y) (> y 1980)))))

  (format t "~%[Test 4] Conversion to Alist (showing first record):~%")
  (let* ((db (read-csv-to-hash-table "articles.csv" :articles))
         (alist (hash-table-to-alist db)))
    (format t "~A~%" (first alist)))

  (format t "~%[Test 5] Writing filtered articles to 'output_articles.csv'...~%")
  (let ((db (make-hash-table))
        (selector (select "articles.csv" :articles)))
    (dolist (rec (funcall selector :specialty-id 1))
      (setf (gethash (gethash :id rec) db) rec))
    (write-hash-table-to-csv "output_articles.csv" db :articles)
    (format t "File written. Contents:~%")
    (with-open-file (stream "output_articles.csv")
      (loop for line = (read-line stream nil)
            while line do (format t "~A~%" line)))))
