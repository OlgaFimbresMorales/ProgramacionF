Module periodos
implicit none
integer, parameter :: datos=7674, mes=1344, dias=48
end module periodos

Program Mareas
use periodos
Implicit None
real :: p, t, d, mx, im, maximo, minimo, dia, minm
real, dimension(datos)::B, altura, tiempo, tiem, maxm
integer :: i



open(unit=8, file='Mareas5.csv',status='old')
do i=1, datos, 1 
   read (8,*) B(i), p, t, altura(i), d, tiempo(i)
   !write (*,*) B(i),p, t, altura(i), d, tiempo(i)
end do  
close (8)

open(unit=8, file='Mareas5.csv', status='old')
open(9, file='maximos.csv', status='unknown')
mx=0
do i=1, datos
  read (8,*) B(i), p, t, altura(i), d, tiempo(i)
  if(altura(i).gt.mx) then
    mx = altura(i)
    im = tiempo(i)
  end if
end do
write(9,*) mx, im
close(9)
close(8)

print * , 'nivel de marea maxima', mx, 'metros'
print * , 'tiempo de marea maxima', im

open(unit=8, file='Mareas5.csv', status='old')
open(10, file='maxymin.csv',status='unknown')

 do i=1,datos
   read (8,*) B(i), p, t, altura(i), d, tiempo(i)
   minimo = minval(altura,1)
   dia = tiempo(i)
 end do
 write(10,*) dia, minimo
 write(*,*) dia, minimo
close(10)
close(8)

open(unit=8, file='Mareas5.csv', status='old')
open(11, file='max2.csv',status='unknown')
maxm=mx
 do i=1, datos
   read (8,*) B(i), p, t, altura(i), d, tiempo(i)
    
   
 end do
 write(11,*) maxm
 write(*,*) maxm
close(11)
close(8)



end Program Mareas



