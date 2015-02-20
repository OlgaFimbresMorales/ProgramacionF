 ! Area . f90 : Calculates the area of a circle, sample program
 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
 Program Circle_volume ! Begin main program
   Implicit None ! Declare all variables
   Real *8 :: radio , altura ,volumen ! Declare Reals
   Complex *8 :: PI = 4.0 * atan(1.0) ! Declare , assign Real
   Integer :: model_n = 1 ! Declare , assign Ints
   print * , 'Ingrese un radio:' ! Talk to user
   read * , radio
   print * , 'Ingrese una altura:'
   read * , altura
   volumen = (2.0/3.0) * PI * radio * radio * altura  ! Calc are
   print * , 'Volumen =' , volumen
End Program Circle_volume
