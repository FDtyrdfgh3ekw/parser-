;; lexer_python.lsp
;; Лексический анализатор Python

(defun is_whitespace (ch)
  (or (eql ch #\Space)
      (eql ch #\Tab)
      (eql ch #\Newline)
      (eql ch #\Return)))

(defun is_digit (ch)
  (and (characterp ch) 
       (>= (char-code ch) (char-code #\0))
       (<= (char-code ch) (char-code #\9))))

(defun is_alpha (ch)
  (and (characterp ch)
       (or (and (>= (char-code ch) (char-code #\a))
                (<= (char-code ch) (char-code #\z)))
           (and (>= (char-code ch) (char-code #\A))
                (<= (char-code ch) (char-code #\Z))))))

(defun is_alnum (ch)
  (or (is_alpha ch) (is_digit ch)))

(defun stream-from-string (str)
  (list :str str :pos 0 :line 1 :col 0))

(defun stream-peek (stream)
  (let ((str (getf stream :str))
        (pos (getf stream :pos)))
    (if (< pos (length str))
        (char str pos)
        nil)))

(defun stream-next (stream)
  (let ((str (getf stream :str))
        (pos (getf stream :pos))
        (line (getf stream :line))
        (col (getf stream :col)))
    (when (< pos (length str))
      (let ((ch (char str pos)))
        (setf (getf stream :pos) (1+ pos))
        (if (eql ch #\Newline)
            (progn
              (setf (getf stream :line) (1+ line))
              (setf (getf stream :col) 0))
            (setf (getf stream :col) (1+ col)))
        ch))))

(defun stream-skip-whitespace (stream)
  (when (and (stream-peek stream) 
             (is_whitespace (stream-peek stream))
             (not (eql (stream-peek stream) #\Newline)))
    (stream-next stream)
    (stream-skip-whitespace stream)))

(defun lex-indentation (stream)
  (let ((indent 0))
    (labels ((read-spaces ()
               (when (and (stream-peek stream) 
                          (eql (stream-peek stream) #\Space))
                 (stream-next stream)
                 (setq indent (+ indent 1))
                 (read-spaces))))
      (read-spaces))
    (list 'INDENT indent)))

(defun lex-number (stream)
  (let ((buffer ""))
    (labels ((read-digits ()
               (when (and (stream-peek stream) 
                          (is_digit (stream-peek stream)))
                 (setq buffer (concatenate 'string 
                                           buffer 
                                           (string (stream-next stream))))
                 (read-digits))))
      (read-digits))
    
    (if (and (stream-peek stream) 
             (eql (stream-peek stream) #\.))
        (progn
          (setq buffer (concatenate 'string buffer "."))
          (stream-next stream)
          (labels ((read-fraction ()
                     (when (and (stream-peek stream) 
                                (is_digit (stream-peek stream)))
                       (setq buffer (concatenate 'string 
                                                 buffer 
                                                 (string (stream-next stream))))
                       (read-fraction))))
            (read-fraction))
          (list 'FLOAT buffer))
        (list 'INTEGER buffer))))

(defun lex-string (stream)
  (let ((quote-char (stream-next stream))
        (buffer ""))
    (labels ((read-chars ()
               (when (and (stream-peek stream) 
                          (not (eql (stream-peek stream) quote-char)))
                 (let ((ch (stream-next stream)))
                   (if (eql ch #\\)
                       (let ((escaped (stream-next stream)))
                         (cond
                           ((eql escaped #\n) 
                            (setq buffer (concatenate 'string buffer (string #\Newline))))
                           ((eql escaped #\t) 
                            (setq buffer (concatenate 'string buffer (string #\Tab))))
                           ((eql escaped #\r) 
                            (setq buffer (concatenate 'string buffer (string #\Return))))
                           ((eql escaped #\\) 
                            (setq buffer (concatenate 'string buffer (string #\\))))
                           (t 
                            (setq buffer (concatenate 'string buffer (string escaped))))))
                       (setq buffer (concatenate 'string buffer (string ch)))))
                 (read-chars))))
      (read-chars))
    
    (when (and (stream-peek stream) 
               (eql (stream-peek stream) quote-char))
      (stream-next stream))
    
    (list 'STRING buffer)))

(defun lex-identifier (stream)
  (let ((buffer ""))
    (when (and (stream-peek stream) 
               (or (is_alpha (stream-peek stream)) 
                   (eql (stream-peek stream) #\_)))
      (setq buffer (concatenate 'string 
                                buffer 
                                (string (stream-next stream)))))
    
    (labels ((read-chars ()
               (when (and (stream-peek stream) 
                          (or (is_alnum (stream-peek stream))
                              (eql (stream-peek stream) #\_)))
                 (setq buffer (concatenate 'string 
                                           buffer 
                                           (string (stream-next stream))))
                 (read-chars))))
      (read-chars))
    
    ;; Проверка на ключевые слова
    (cond
     ((equal buffer "if") 'IF)
     ((equal buffer "else") 'ELSE)
     ((equal buffer "elif") 'ELIF)
     ((equal buffer "while") 'WHILE)
     ((equal buffer "for") 'FOR)
     ((equal buffer "def") 'DEF)
     ((equal buffer "class") 'CLASS)
     ((equal buffer "return") 'RETURN)
     ((equal buffer "import") 'IMPORT)
     ((equal buffer "from") 'FROM)
     ((equal buffer "as") 'AS)
     ((equal buffer "with") 'WITH)
     ((equal buffer "try") 'TRY)
     ((equal buffer "except") 'EXCEPT)
     ((equal buffer "finally") 'FINALLY)
     ((equal buffer "raise") 'RAISE)
     ((equal buffer "assert") 'ASSERT)
     ((equal buffer "pass") 'PASS)
     ((equal buffer "break") 'BREAK)
     ((equal buffer "continue") 'CONTINUE)
     ((equal buffer "lambda") 'LAMBDA)
     ((equal buffer "and") 'AND)
     ((equal buffer "or") 'OR)
     ((equal buffer "not") 'NOT)
     ((equal buffer "is") 'IS)
     ((equal buffer "None") 'NONE)
     ((equal buffer "True") 'TRUE)
     ((equal buffer "False") 'FALSE)
     ((equal buffer "global") 'GLOBAL)
     ((equal buffer "nonlocal") 'NONLOCAL)
     ((equal buffer "del") 'DEL)
     ((equal buffer "yield") 'YIELD)
     ((equal buffer "async") 'ASYNC)
     ((equal buffer "await") 'AWAIT)
     ((equal buffer "in") 'IN)
     (t (list 'IDENTIFIER buffer)))))

(defun lex-operator (stream)
  (let ((ch1 (stream-next stream))
        (ch2 (stream-peek stream)))
    (cond
     ((and (eql ch1 #\=) ch2 (eql ch2 #\=))
      (stream-next stream)
      'OP-EQUAL)
     ((and (eql ch1 #\!) ch2 (eql ch2 #\=))
      (stream-next stream)
      'OP-NOT-EQUAL)
     ((and (eql ch1 #\<) ch2 (eql ch2 #\=))
      (stream-next stream)
      'OP-LESS-EQUAL)
     ((and (eql ch1 #\>) ch2 (eql ch2 #\=))
      (stream-next stream)
      'OP-GREATER-EQUAL)
     ((and (eql ch1 #\+) ch2 (eql ch2 #\=))
      (stream-next stream)
      'OP-PLUS-EQUAL)
     ((and (eql ch1 #\-) ch2 (eql ch2 #\=))
      (stream-next stream)
      'OP-MINUS-EQUAL)
     ((and (eql ch1 #\*) ch2 (eql ch2 #\=))
      (stream-next stream)
      'OP-MULT-EQUAL)
     ((and (eql ch1 #\/) ch2 (eql ch2 #\=))
      (stream-next stream)
      'OP-DIV-EQUAL)
     ((and (eql ch1 #\/) ch2 (eql ch2 #\/))
      (stream-next stream)
      'OP-FLOOR-DIV)
     ((and (eql ch1 #\*) ch2 (eql ch2 #\*))
      (stream-next stream)
      'OP-POWER)
     ((eql ch1 #\+) 'OP-PLUS)
     ((eql ch1 #\-) 'OP-MINUS)
     ((eql ch1 #\*) 'OP-MULT)
     ((eql ch1 #\/) 'OP-DIV)
     ((eql ch1 #\=) 'OP-ASSIGN)
     ((eql ch1 #\<) 'OP-LESS)
     ((eql ch1 #\>) 'OP-GREATER)
     ((eql ch1 #\%) 'OP-MOD)
     ((eql ch1 #\@) 'OP-AT)
     (t nil))))

(defun lex-punctuation (stream)
  (let ((ch (stream-next stream)))
    (cond
     ((eql ch #\() 'LPAREN)
     ((eql ch #\)) 'RPAREN)
     ((eql ch #\[) 'LBRACKET)
     ((eql ch #\]) 'RBRACKET)
     ((eql ch #\{) 'LBRACE)
     ((eql ch #\}) 'RBRACE)
     ((eql ch #\:) 'COLON)
     ((eql ch #\,) 'COMMA)
     ((eql ch #\;) 'SEMICOLON)
     ((eql ch #\.) 'DOT)
     (t nil))))

(defun member-char (ch lst)
  (if (null lst)
      nil
      (or (eql ch (car lst))
          (member-char ch (cdr lst)))))

(defun python-lexer (input-str)
  "Основная функция лексического анализатора Python"
  (let ((stream (stream-from-string input-str))
        (tokens nil)
        (at-line-start t))
    
    (labels ((process-stream ()
               (when (stream-peek stream)
                 ;; Пропускаем пробелы (кроме новых строк)
                 (labels ((skip-whitespace ()
                            (when (and (stream-peek stream) 
                                       (is_whitespace (stream-peek stream))
                                       (not (eql (stream-peek stream) #\Newline)))
                              (stream-next stream)
                              (skip-whitespace))))
                   (skip-whitespace))
                 
                 (let ((ch (stream-peek stream)))
                   (cond
                    ;; Конец строки
                    ((eql ch #\Newline)
                     (stream-next stream)
                     (setq tokens (cons 'LF tokens))
                     (setq at-line-start t))
                    
                    ;; Начало строки - добавляем INDENT
                    (at-line-start
                     (setq tokens (cons (lex-indentation stream) tokens))
                     (setq at-line-start nil))
                    
                    ;; Числа
                    ((is_digit ch)
                     (setq tokens (cons (lex-number stream) tokens)))
                    
                    ;; Строки
                    ((or (eql ch #\") (eql ch #\'))
                     (setq tokens (cons (lex-string stream) tokens)))
                    
                    ;; Идентификаторы и ключевые слова
                    ((or (is_alpha ch) (eql ch #\_))
                     (setq tokens (cons (lex-identifier stream) tokens)))
                    
                    ;; Операторы
                    ((member-char ch '(#\+ #\- #\* #\/ #\% #\= #\< #\> #\! #\& #\| #\^ #\~ #\@))
                     (let ((op (lex-operator stream)))
                       (when op (setq tokens (cons op tokens)))))
                    
                    ;; Пунктуация
                    ((member-char ch '(#\( #\) #\[ #\] #\{ #\} #\: #\, #\; #\.))
                     (let ((punct (lex-punctuation stream)))
                       (when punct (setq tokens (cons punct tokens)))))
                    
                    ;; Пропускаем остальные символы
                    (t
                     (stream-next stream))))
                 
                 (process-stream))))
      
      (process-stream))
    
    ;; Возвращаем токены в правильном порядке
    (reverse tokens)))
 
