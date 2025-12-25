;; ==================== ПАРСЕР PYTHON ====================
;; Обновленный для работы с лексером lexer_python.lsp

;; ==================== БАЗОВЫЕ КОМБИНАТОРЫ ====================

(defun parse-suc (val)
  "Элементарный парсер - успешный разбор со значением val"
  #'(lambda (stream) (cons val stream)))

(defun parse-fail ()
  "Элементарный парсер - неудачный разбор"
  #'(lambda (stream) (declare (ignore stream)) nil))

(defun parse-pred (pred)
  "Парсер по предикату"
  #'(lambda (stream)
      (if (null stream) 
          nil
          (let ((token (car stream)))
            (if (funcall pred token) 
                (cons token (cdr stream))
                nil)))))

(defun parse-and (&rest parsers)
  "Последовательный комбинатор"
  #'(lambda (stream)
      (labels ((apply-parser (parsers stream res)
                 (if (null parsers) 
                     (cons (reverse res) stream)
                     (let ((parser-res (funcall (car parsers) stream)))
                       (if (null parser-res) 
                           nil
                           (apply-parser (cdr parsers) 
                                         (cdr parser-res)
                                         (cons (car parser-res) res)))))))
        (apply-parser parsers stream nil))))

(defun parse-or (&rest parsers)
  "Параллельный комбинатор"
  (unless parsers (error "parse-or: no parsers"))
  #'(lambda (stream)
      (labels ((try-parsers (parsers)
                 (if (null parsers) 
                     nil
                     (let ((res (funcall (car parsers) stream)))
                       (if res res (try-parsers (cdr parsers)))))))
        (try-parsers parsers))))

(defun parse-many (parser)
  "0 или более повторений"
  #'(lambda (stream)
      (labels ((apply-parser (stream res)
                 (let ((res-parser (funcall parser stream)))
                   (if (null res-parser)
                       (cons (reverse res) stream)
                       (apply-parser (cdr res-parser) 
                                     (cons (car res-parser) res))))))
        (apply-parser stream nil))))

(defun parse-many1 (parser)
  "1 или более повторений"
  (parse-and parser (parse-many parser)))

(defun parse-optional (parser)
  "0 или 1 применение"
  #'(lambda (stream)
      (let ((res (funcall parser stream)))
        (if res 
            res
            (cons nil stream)))))

(defun parse-sep-by (parser sep)
  "Парсер, разделенный сепаратором"
  #'(lambda (stream)
      (let ((first (funcall parser stream)))
        (if (null first)
            (cons nil stream)
            (labels ((parse-rest (stream res)
                       (let ((sep-res (funcall sep stream)))
                         (if (null sep-res)
                             (cons (reverse (cons (car first) res)) (cdr first))
                             (let ((next (funcall parser (cdr sep-res))))
                               (if (null next)
                                   (cons (reverse (cons (car first) res)) (cdr first))
                                   (parse-rest (cdr next) 
                                               (cons (car next) res))))))))
              (parse-rest (cdr first) nil))))))

(defun parse-app (parser fn)
  "Применение функции к результату парсинга"
  #'(lambda (stream)
      (let ((res (funcall parser stream)))
        (if (null res) 
            nil
            (cons (funcall fn (car res)) (cdr res))))))

(defun parse-lazy (parser-fn)
  "Ленивый парсер для рекурсивных определений"
  #'(lambda (stream)
      (funcall (funcall parser-fn) stream)))

;; ==================== ПАРСЕРЫ ТОКЕНОВ (адаптированы под лексер) ====================

(defun parse-identifier ()
  "Парсер для идентификаторов (ожидает '(IDENTIFIER name))"
  (parse-pred #'(lambda (token) 
                  (and (listp token) (eq (car token) 'IDENTIFIER)))))

(defun parse-number ()
  "Парсер для чисел (ожидает '(INTEGER value) или '(FLOAT value))"
  (parse-pred #'(lambda (token) 
                  (and (listp token) 
                       (or (eq (car token) 'INTEGER)
                           (eq (car token) 'FLOAT))))))

(defun parse-string ()
  "Парсер для строк (ожидает '(STRING value))"
  (parse-pred #'(lambda (token) 
                  (and (listp token) (eq (car token) 'STRING)))))

(defun parse-keyword (kw)
  "Парсер для ключевых слов (ожидает символ, например 'IF)"
  (parse-pred #'(lambda (token) (eq token kw))))

(defun parse-operator (op)
  "Парсер для операторов (ожидает символ, например 'OP-PLUS)"
  (parse-pred #'(lambda (token) (eq token op))))

(defun parse-punctuation (punct)
  "Парсер для пунктуации (ожидает символ, например 'LPAREN)"
  (parse-pred #'(lambda (token) (eq token punct))))

;; ==================== ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ====================

(defun adapt-tokens (tokens)
  "Преобразует токены лексера в формат, понятный парсеру"
  (labels ((adapt-token (token)
             (cond
              ;; Атомарные токены (ключевые слова, операторы, пунктуация) оставляем как есть
              ((symbolp token) token)
              
              ;; Числа
              ((and (listp token) (eq (car token) 'INTEGER))
               token)
              
              ((and (listp token) (eq (car token) 'FLOAT))
               token)
              
              ;; Строки
              ((and (listp token) (eq (car token) 'STRING))
               token)
              
              ;; Идентификаторы
              ((and (listp token) (eq (car token) 'IDENTIFIER))
               token)
              
              ;; INDENT - игнорируем
              ((and (listp token) (eq (car token) 'INDENT))
               nil)
              
              ;; LF - игнорируем
              ((eq token 'LF)
               nil)
              
              ;; Неизвестный формат
              (t token))))
    
    (remove nil (mapcar #'adapt-token tokens))))

;; ==================== ДЕКЛАРАЦИИ РЕКУРСИВНЫХ ФУНКЦИЙ ====================

(defun parse-expr () nil)
(defun parse-block () nil)
(defun parse-stmt () nil)
(defun parse-simple-stmt () nil)

;; ==================== ВЫРАЖЕНИЯ ====================

(defun parse-atom ()
  "Атомарные выражения: числа, строки, идентификаторы, константы"
  (parse-app
   (parse-or (parse-number)
             (parse-string)
             (parse-identifier)
             (parse-keyword 'TRUE)
             (parse-keyword 'FALSE)
             (parse-keyword 'NONE))
   #'(lambda (result)
       (cond
        ((eq result 'TRUE) t)
        ((eq result 'FALSE) nil)
        ((eq result 'NONE) nil)
        ((listp result) 
         (cond
          ((eq (car result) 'IDENTIFIER)
           (intern (string-upcase (second result))))
          ((or (eq (car result) 'INTEGER) (eq (car result) 'FLOAT))
           (read-from-string (second result)))
          ((eq (car result) 'STRING)
           (second result))
          (t result)))
        (t result)))))

(defun parse-parenthesized-expr ()
  "Выражение в скобках"
  (parse-app
   (parse-and (parse-punctuation 'LPAREN)
              (parse-lazy #'parse-expr)
              (parse-punctuation 'RPAREN))
   #'(lambda (result) (second result))))

(defun parse-list-expr ()
  "Выражение списка"
  (parse-app
   (parse-and (parse-punctuation 'LBRACKET)
              (parse-optional 
               (parse-sep-by (parse-lazy #'parse-expr) 
                             (parse-punctuation 'COMMA)))
              (parse-punctuation 'RBRACKET))
   #'(lambda (result)
       (let ((elements (second result)))
         (if elements
             `(list ,@elements)
             '(list))))))

(defun parse-call-expr ()
  "Вызов функции"
  (parse-app
   (parse-and (parse-atom)
              (parse-punctuation 'LPAREN)
              (parse-optional 
               (parse-sep-by (parse-lazy #'parse-expr) 
                             (parse-punctuation 'COMMA)))
              (parse-punctuation 'RPAREN))
   #'(lambda (result)
       (let ((func (first result))
             (args (third result)))
         `(,func ,@(if args args '()))))))

(defun parse-primary-expr ()
  "Первичные выражения"
  (parse-or (parse-parenthesized-expr)
            (parse-list-expr)
            (parse-call-expr)
            (parse-atom)))

(defun parse-unary-expr ()
  "Унарные выражения"
  (parse-app
   (parse-or
    (parse-and (parse-operator 'OP-MINUS) (parse-lazy #'parse-unary-expr))
    (parse-and (parse-keyword 'NOT) (parse-lazy #'parse-unary-expr))
    (parse-primary-expr))
   #'(lambda (result)
       (if (listp result)
           (cond
            ((eq (first result) 'OP-MINUS)
             `(- ,(second result)))
            ((eq (first result) 'NOT)
             `(not ,(second result)))
            (t result))
           result))))

(defun parse-power-expr ()
  "Выражение возведения в степень"
  (labels ((parse-rest (left)
             (parse-app
              (parse-optional
               (parse-and (parse-operator 'OP-POWER)
                          (parse-lazy #'parse-unary-expr)))
              #'(lambda (result)
                  (if result
                      (parse-rest `(expt ,left ,(second result)))
                      left)))))
    (parse-app (parse-unary-expr) #'parse-rest)))

(defun parse-mul-expr ()
  "Мультипликативные выражения"
  (labels ((parse-rest (left)
             (parse-app
              (parse-optional
               (parse-and (parse-or (parse-operator 'OP-MULT)
                                    (parse-operator 'OP-DIV)
                                    (parse-operator 'OP-FLOOR-DIV)
                                    (parse-operator 'OP-MOD))
                          (parse-lazy #'parse-power-expr)))
              #'(lambda (result)
                  (if result
                      (let ((op (first result))
                            (right (second result)))
                        (parse-rest 
                         (cond
                          ((eq op 'OP-MULT) `(* ,left ,right))
                          ((eq op 'OP-DIV) `(/ ,left ,right))
                          ((eq op 'OP-FLOOR-DIV) `(floor ,left ,right))
                          ((eq op 'OP-MOD) `(mod ,left ,right))
                          (t left))))
                      left)))))
    (parse-app (parse-power-expr) #'parse-rest)))

(defun parse-add-expr ()
  "Аддитивные выражения"
  (labels ((parse-rest (left)
             (parse-app
              (parse-optional
               (parse-and (parse-or (parse-operator 'OP-PLUS)
                                    (parse-operator 'OP-MINUS))
                          (parse-lazy #'parse-mul-expr)))
              #'(lambda (result)
                  (if result
                      (let ((op (first result))
                            (right (second result)))
                        (parse-rest 
                         (if (eq op 'OP-PLUS)
                             `(+ ,left ,right)
                             `(- ,left ,right))))
                      left)))))
    (parse-app (parse-mul-expr) #'parse-rest)))

(defun parse-comparison-expr ()
  "Выражения сравнения"
  (labels ((parse-rest (left)
             (parse-app
              (parse-optional
               (parse-and (parse-or (parse-operator 'OP-EQUAL)
                                    (parse-operator 'OP-NOT-EQUAL)
                                    (parse-operator 'OP-LESS)
                                    (parse-operator 'OP-GREATER)
                                    (parse-operator 'OP-LESS-EQUAL)
                                    (parse-operator 'OP-GREATER-EQUAL)
                                    (parse-keyword 'IS)
                                    (parse-keyword 'IN))
                          (parse-lazy #'parse-add-expr)))
              #'(lambda (result)
                  (if result
                      (let ((op (first result))
                            (right (second result)))
                        (parse-rest
                         (cond
                          ((eq op 'OP-EQUAL) `(equal ,left ,right))
                          ((eq op 'OP-NOT-EQUAL) `(not (equal ,left ,right)))
                          ((eq op 'OP-LESS) `(< ,left ,right))
                          ((eq op 'OP-GREATER) `(> ,left ,right))
                          ((eq op 'OP-LESS-EQUAL) `(<= ,left ,right))
                          ((eq op 'OP-GREATER-EQUAL) `(>= ,left ,right))
                          ((eq op 'IS) `(eq ,left ,right))
                          ((eq op 'IN) `(member ,left ,right))
                          (t left))))
                      left)))))
    (parse-app (parse-add-expr) #'parse-rest)))

(defun parse-not-expr ()
  "Выражения с NOT"
  (parse-app
   (parse-or
    (parse-and (parse-keyword 'NOT) (parse-lazy #'parse-not-expr))
    (parse-comparison-expr))
   #'(lambda (result)
       (if (and (listp result) (eq (first result) 'NOT))
           `(not ,(second result))
           result))))

(defun parse-and-expr ()
  "Выражения с AND"
  (labels ((parse-rest (left)
             (parse-app
              (parse-optional
               (parse-and (parse-keyword 'AND)
                          (parse-lazy #'parse-not-expr)))
              #'(lambda (result)
                  (if result
                      (parse-rest `(and ,left ,(second result)))
                      left)))))
    (parse-app (parse-not-expr) #'parse-rest)))

(defun parse-or-expr ()
  "Выражения с OR"
  (labels ((parse-rest (left)
             (parse-app
              (parse-optional
               (parse-and (parse-keyword 'OR)
                          (parse-lazy #'parse-and-expr)))
              #'(lambda (result)
                  (if result
                      (parse-rest `(or ,left ,(second result)))
                      left)))))
    (parse-app (parse-and-expr) #'parse-rest)))

;; Определение parse-expr
(setf (symbol-function 'parse-expr) #'parse-or-expr)

;; ==================== ИНСТРУКЦИИ ====================

(defun parse-simple-stmt ()
  "Простая инструкция"
  (parse-app
   (parse-and (parse-lazy #'parse-expr)
              (parse-punctuation 'SEMICOLON))
   #'(lambda (result) (first result))))

(defun parse-assignment ()
  "Присваивание"
  (parse-app
   (parse-and (parse-identifier)
              (parse-operator 'OP-ASSIGN)
              (parse-lazy #'parse-expr))
   #'(lambda (result)
       (let ((var-name (second (first result))))
         `(setq ,(intern (string-upcase var-name)) ,(third result))))))

(defun parse-return-stmt ()
  "Инструкция return"
  (parse-app
   (parse-and (parse-keyword 'RETURN)
              (parse-optional (parse-lazy #'parse-expr)))
   #'(lambda (result)
       (if (second result)
           `(return-from nil ,(second result))
           '(return-from nil)))))

(defun parse-if-stmt ()
  "Инструкция if"
  (parse-app
   (parse-and (parse-keyword 'IF)
              (parse-lazy #'parse-expr)
              (parse-punctuation 'COLON)
              (parse-lazy #'parse-block)
              (parse-optional
               (parse-and (parse-keyword 'ELSE)
                          (parse-punctuation 'COLON)
                          (parse-lazy #'parse-block))))
   #'(lambda (result)
       (let ((condition (second result))
             (then-block (fourth result))
             (else-part (fifth result)))
         (if else-part
             `(if ,condition
                  (progn ,@then-block)
                  (progn ,@(third else-part)))
             `(if ,condition
                  (progn ,@then-block)))))))

(defun parse-while-stmt ()
  "Инструкция while"
  (parse-app
   (parse-and (parse-keyword 'WHILE)
              (parse-lazy #'parse-expr)
              (parse-punctuation 'COLON)
              (parse-lazy #'parse-block))
   #'(lambda (result)
       `(loop while ,(second result)
              do (progn ,@(fourth result))))))

(defun parse-for-stmt ()
  "Инструкция for"
  (parse-app
   (parse-and (parse-keyword 'FOR)
              (parse-identifier)
              (parse-keyword 'IN)
              (parse-lazy #'parse-expr)
              (parse-punctuation 'COLON)
              (parse-lazy #'parse-block))
   #'(lambda (result)
       (let ((var-name (second (second result)))
             (iterable (fourth result))
             (body (sixth result)))
         `(dolist (,(intern (string-upcase var-name)) ,iterable)
            ,@body)))))

(defun parse-def-stmt ()
  "Определение функции"
  (parse-app
   (parse-and (parse-keyword 'DEF)
              (parse-identifier)
              (parse-punctuation 'LPAREN)
              (parse-optional 
               (parse-sep-by (parse-identifier) 
                             (parse-punctuation 'COMMA)))
              (parse-punctuation 'RPAREN)
              (parse-punctuation 'COLON)
              (parse-lazy #'parse-block))
   #'(lambda (result)
       (let ((func-name (second (second result)))
             (params (fourth result))
             (body (seventh result)))
         `(defun ,(intern (string-upcase func-name)) 
              ,(mapcar #'(lambda (p) (intern (string-upcase (second p)))) 
                       (if params params '()))
            ,@body)))))

(defun parse-import-stmt ()
  "Инструкция import"
  (parse-app
   (parse-and (parse-keyword 'IMPORT)
              (parse-sep-by (parse-identifier) 
                            (parse-punctuation 'COMMA)))
   #'(lambda (result)
       `(progn
          ,@(mapcar #'(lambda (module) 
                        `(require ',(intern (string-upcase (second module))))) 
                    (second result))))))

(defun parse-expr-stmt ()
  "Инструкция-выражение"
  (parse-lazy #'parse-expr))

;; Определение parse-stmt
(setf (symbol-function 'parse-stmt)
      #'(lambda ()
          (parse-or (parse-if-stmt)
                    (parse-while-stmt)
                    (parse-for-stmt)
                    (parse-def-stmt)
                    (parse-return-stmt)
                    (parse-assignment)
                    (parse-import-stmt)
                    (parse-expr-stmt))))

;; Определение parse-block
(setf (symbol-function 'parse-block)
      #'(lambda ()
          (parse-app
           (parse-many (parse-lazy #'parse-stmt))
           #'(lambda (result) result))))

;; ==================== ИНТЕРФЕЙС ДЛЯ РАБОТЫ С ЛЕКСЕРОМ ====================

(defun parse-python-code (code)
  "Главная функция: принимает строку с кодом Python, возвращает AST"
  (let* ((raw-tokens (python-lexer code))
         (adapted-tokens (adapt-tokens raw-tokens))
         (result (funcall (parse-block) adapted-tokens)))
    
    (if result
        (car result)  ; Возвращаем AST
        (error "Ошибка парсинга: не удалось разобрать код"))))

(defun parse-python-tokens (tokens)
  "Функция для парсинга уже готовых токенов"
  (let* ((adapted-tokens (adapt-tokens tokens))
         (result (funcall (parse-block) adapted-tokens)))
    
    (if result
        (car result)
        (error "Ошибка парсинга"))))

;; ==================== ФУНКЦИИ ТЕСТИРОВАНИЯ ====================

(defun test-lexer-parser ()
  "Тестирование связки лексер+парсер"
  (format t "~%=== Тестирование лексера и парсера Python ===~%")
  
  (let ((test-cases
         '(("x = 5 + 3;" 
            . "Простое арифметическое выражение")
           
           ("def square(x): return x * x;" 
            . "Определение функции")
           
           ("if x > 0: y = 1" 
            . "Условный оператор без else")
           
           ("if a: b = 1 else: b = 0" 
            . "Условный оператор с else")
           
           ("for i in range(10): print(i)" 
            . "Цикл for")
           
           ("while x < 10: x = x + 1" 
            . "Цикл while")
           
           ("[1, 2, 3, 4]" 
            . "Список")
           
           ("not x and y" 
            . "Логическое выражение")
           
           ("x is None" 
            . "Оператор is")
           
           ("x in [1, 2, 3]" 
            . "Оператор in"))))
    
    (dolist (test test-cases)
      (let* ((code (car test))
             (desc (cdr test))
             (tokens nil)
             (result nil))
        
        (format t "~%~A~%" (make-string 60 :initial-element #\-))
        (format t "Тест: ~A~%" desc)
        (format t "Код: ~S~%" code)
        
        ;; Шаг 1: Лексический анализ
        (format t "~%1. Лексический анализ:~%")
        (setq tokens (python-lexer code))
        (format t "Токены: ~S~%" tokens)
        
        ;; Шаг 2: Адаптация токенов
        (format t "~%2. Адаптированные токены:~%")
        (let ((adapted (adapt-tokens tokens)))
          (format t "~S~%" adapted))
        
        ;; Шаг 3: Синтаксический анализ
        (format t "~%3. Синтаксический анализ:~%")
        (handler-case
            (progn
              (setq result (parse-python-code code))
              (format t "AST: ~S~%" result))
          (error (e)
            (format t "ОШИБКА: ~A~%" e)))
        
        (format t "~%")))))

(defun quick-test ()
  "Быстрый тест простых выражений"
  (format t "~%=== Быстрый тест парсера ===~%")
  
  (dolist (code '("x = 5 + 3;"
                  "y = a * b / c;"
                  "z = (a + b) * c;"
                  "return x;"
                  "if x: y = 1"))
    (format t "~%Код: ~S~%" code)
    (format t "Результат: ~S~%" (parse-python-code code))))

;; ==================== ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ====================

(defun debug-tokens (code)
  "Отладочная функция: показывает токены для кода"
  (let ((tokens (python-lexer code)))
    (format t "~%Код: ~S~%" code)
    (format t "Токены: ~S~%" tokens)
    (format t "Адаптированные: ~S~%" (adapt-tokens tokens))
    tokens))

(defun test-individual-parser (parser-name code)
  "Тестирование отдельного парсера"
  (let* ((tokens (adapt-tokens (python-lexer code)))
         (parser (cond
                  ((eq parser-name 'expr) (parse-expr))
                  ((eq parser-name 'stmt) (funcall (parse-stmt)))
                  (t (error "Неизвестный парсер: ~A" parser-name))))
         (result (funcall parser tokens)))
    
    (format t "~%Парсер: ~A~%" parser-name)
    (format t "Код: ~S~%" code)
    (format t "Токены: ~S~%" tokens)
    (format t "Результат: ~S~%" result)
    result))

;; Инициализация - запустить тесты при загрузке
;; (test-lexer-parser)  ; Раскомментировать для автоматического тестирования

;; Для использования:
;; 1. Загрузите лексер
;; 2. Загрузите этот парсер
;; 3. Вызовите (parse-python-code "ваш код на Python")


