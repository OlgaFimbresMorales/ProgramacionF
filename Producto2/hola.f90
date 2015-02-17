PROGRAM Adivina
    WRITE (*,*) "Hola! Trataré de adivinar un número."
    WRITE (*,*) "Piensa un númeroo entre 1 y 10."
    CALL sleep(5)
    WRITE (*,*) "Ahora multiplícalo por 9."
    CALL sleep(5)
    WRITE (*,*) "Si el número tiene 2 dígitos, súmalos entre si: Ej. 36 -> 3+6=9."
    WRITE (*,*) "Si tu número tiene un solo dígito, súmale 0."
    CALL sleep(5)
    WRITE (*,*) "Al número resultante súmale 4."
    CALL sleep(10)
    WRITE (*,*) "Muy bien. El resultado es 13 :)" 

END PROGRAM
