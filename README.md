<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПІСКС</b></p>
<p align="center">
<b>Звіт лабораторної роботи 4</b><br/>
з дисципліни "Вступ до функціонального програмування"<br/>
Тема: "Функції вищого порядку та замикання"
</p>
<p align="right"><b>Студент:</b> КВ-23 Домущі Дмитро</p>
<p align="right"><b>Рік:</b> 2025</p>

## Загальне завдання
1. Реалізувати утиліти для роботи з базою даних, яка зберігається у CSV файлі.
2. База даних повинна завантажуватись у асоціативний список або геш-таблицю.
3. Реалізувати функції для:
   - Читання бази з файлу.
   - Запису бази у файл.
   - Конвертації між структурами (наприклад, геш-таблиця -> список).
   - Вибірки записів за критерієм (select).

## Варіант 8
**Тема:** Наукові статті (Геш-таблиця).
**База даних:** Наукові статті за спеціальностями.
**Сутності:**
1. **Спеціальності** (Specialties): `id`, `code`, `name`.
2. **Наукові статті** (Articles): `id`, `specialty_id`, `title`, `author`, `year`.

## Лістинг реалізації завдання

### Файли .csv

#### article.csv
```
id,specialty_id,title,author,year
1,1,Lisp in AI Systems,McCarthy,1960
2,1,Functional Patterns,Graham,2001
3,2,Sorting Algorithms,Knuth,1973
4,3,Graph Theory,Euler,1736
5,1,Closure Compilation,Steele,1978
```

#### specialties.csv
```
id,code,name
1,121,Software Engineering
2,122,Computer Science
3,113,Applied Mathematics
```

### 1. Читання з CSV у Геш-таблицю
```lisp
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
```

### 2. Запис Геш-таблиці у CSV
```lisp
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
```

### 3. Конвертація у Асоціативний список (Alist)
```lisp
(defun hash-table-to-alist (ht)
  (let ((result nil))
    (maphash (lambda (k record-hash)
               (declare (ignore k))
               (let ((record-alist nil))
                 (maphash (lambda (f v) (push (cons f v) record-alist)) record-hash)
                 (push record-alist result)))
             ht)
    result))
```

### 4. Вибірка (Select) з використанням функціоналів
```lisp
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
```

### Тестові набори та утиліти
```lisp
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
```

### Тестування
```lisp
CL-USER> (test-lab5)

--- Lab 5 Testing: Scientific Articles ---

[Test 1] Select Specialty with Code '121':

Record:
  ID: 1
  CODE: 121
  NAME: Software Engineering

[Test 2] Select Articles for Specialty ID 1:

Record:
  ID: 5
  SPECIALTY-ID: 1
  TITLE: Closure Compilation
  AUTHOR: Steele
  YEAR: 1978

Record:
  ID: 2
  SPECIALTY-ID: 1
  TITLE: Functional Patterns
  AUTHOR: Graham
  YEAR: 2001

Record:
  ID: 1
  SPECIALTY-ID: 1
  TITLE: Lisp in AI Systems
  AUTHOR: McCarthy
  YEAR: 1960

[Test 3] Select Articles published after 1980:

Record:
  ID: 2
  SPECIALTY-ID: 1
  TITLE: Functional Patterns
  AUTHOR: Graham
  YEAR: 2001

[Test 4] Conversion to Alist (showing first record):
((YEAR . 1978) (AUTHOR . Steele) (TITLE . Closure Compilation)
 (SPECIALTY-ID . 1) (ID . 5))

[Test 5] Writing filtered articles to 'output_articles.csv'...
File written. Contents:
id,specialty_id,title,author,year
5,1,Closure Compilation,Steele,1978
2,1,Functional Patterns,Graham,2001
1,1,Lisp in AI Systems,McCarthy,1960
NIL
```
