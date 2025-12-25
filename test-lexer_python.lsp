(defun lexer-python-test ()
  "Полный тест лексера Python"
  (print (python-lexer 
    (concatenate 'string 
      "d = 81 * 6 "
      "if a == 10: "
      "    num = 0.245 / 2 "
      "\"lexxxer\"       "
      "a or b "
      "while a == True:"
    ))
   ;; '(
   ;;   (INDENT 0) (IDENTIFIER "d") OP-ASSIGN (INTEGER "81") OP-MULT (INTEGER "6") LF
   ;;   (INDENT 0) IF (IDENTIFIER "a") OP-EQUAL (INTEGER "10") LF
   ;;   (INDENT 4) (IDENTIFIER "num") OP-ASSIGN (FLOAT "0.245") OP-DIV (INTEGER "2") LF
   ;;   (INDENT 0) (STRING "lexxxer") LF
   ;;   (INDENT 0) (IDENTIFIER "a") OR (IDENTIFIER "b") LF
   ;;   (INDENT 0) WHILE (IDENTIFIER "a") OP-EQUAL TRUE LF

     
   ;;   )
   ))
(lexer-python-test)

     

