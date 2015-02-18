 ! Area . f90 : Calculates the area of a circle, sample program
 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
 Program Circle_volume ! Begin main program
   Implicit None ! Declare all variables
   Real *8 :: radius , circum ,volume ! Declare Reals
   Complex *8 :: PI = 3.141592 * atan(1.0) ! Declare , assign Real
   Integer :: model_n = 1 ! Declare , assign Ints
   print * , 'Enter a radius:' ! Talk to user
   print * , 'Enter a hight:'
   read * , rdius ! Read into radius
   read * , hight
   volume =  PI * radius * radius * 3.0 * radius * ! Calc area
   print * , 'Program number =' , model_n
   print * , 'Radio =' , radio ! Print radius
   print * , 'Altura =' , altura
   print * , 'Volumen =' , volumen
End Program Circle_volume
