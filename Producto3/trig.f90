! Math . f90 : demo some Fortran math functions
! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
Program Math_test ! Begin main program
   Complex *8 :: x=-1.0 , y=2.0, z=0.0 ! Declare variables x, y, z
   x = sqrt (x)
   y = asin (y) ! Call the sine function
   z = log (z) ! Call the exponential function
   print * , x, y, z ! Print x, y, z
End Program Math_test ! End main program 
