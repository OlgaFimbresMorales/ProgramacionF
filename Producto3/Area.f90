 ! Area . f90 : Calculates the area of a circle, sample program
 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
 Program Circle_area ! Begin main program
   Implicit None ! Declare all variables
   Real *8 :: radius , circum , area ! Declare Reals
   Real *8 :: PI = 4.0 * atan(1.0) ! Declare , assign Real
   Integer :: model_n = 1 ! Declare , assign Ints
   print * , 'Enter a radius:' ! Talk to user
   read * , radius ! Read into radius
   circum = 2.0 * PI * radius ! Calc circumference
   area = radius * radius * PI ! Calc area
   print * , 'Program number =' , model_n
   print * , 'Radius =' , radius ! Print radius
   print * , 'Circumference =' , circum
   print * , 'Area =' , area
End Program Circle_area
