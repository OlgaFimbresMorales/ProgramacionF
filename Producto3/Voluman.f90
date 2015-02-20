 ! Area . f90 : Calculates the area of a circle, sample program
 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
 Program Circle_volume ! Begin main program
   Implicit None ! Declare all variables
   Real *8 :: radius , hight ,volume ! Declare Reals
   Complex *8 :: PI = 4.0 * atan(1.0) ! Declare , assign Real
   Integer :: model_n = 1 ! Declare , assign Ints
   print * , 'Enter a radius:' ! Talk to user
   read * , radius
   print * , 'Enter a hight:'
   read * , hight
   volume = (2.0/3.0) * PI * radius * radius * hight  ! Calc are
   print * , 'Volumen =' , volume
End Program Circle_volume
