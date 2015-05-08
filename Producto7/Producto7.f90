Module periodos
implicit none
integer, parameter :: datos=7632, mes=1344, dias=159, dia=48, mediodia=24, mesesc=5376!datos en los meses completos
end module periodos

Program Mareas
use periodos
Implicit None
real :: p, t, d, mx, im, maximo, minimo, minimod, maximod, maximd
real, dimension(1:datos):: B, altura, tiempo, tiem, maxm, min
real, dimension(1:5):: mm, tm, periodomaxm,  periodominm
real, dimension(1:dias)::  periodomaxd, periodomind
real, dimension(1:318):: periodomd
integer :: i, j



open(unit=8, file='Mareas.csv',status='old')
do i=1, datos, 1 
   read (8,*) B(i), p, t, altura(i), d, tiempo(i)
  !write (*,*) B(i),p, t, altura(i), d, tiempo(i)
end do  

print * , '-------------------------------'
print * , 'Niveles máximos por mes'
open(unit=10, file='max.csv', status='unknown')
do j=0, mesesc, mes
maximo = -1
  do i=1,mes,1
     if(altura(i+j).gt.maximo) then
     maximo = altura(i+j)
     mx = tiempo(i+j)
     end if
     continue
  end do

      write(10,*) mx, maximo
     ! write(*,*) mx, maximo
end do  
close(10)


print * , 
print * , '-------------------------------'
print * , 'Niveles minimos por mes'
open(unit=11,file='min.csv',status='unknown')


do j=0, mesesc, mes
minimo=0
  do i=1,mes,1
   if(altura(i+j).lt.minimo) then
   minimo = altura(i+j)
   im = tiempo(i+j)
   end if
  end do
  write(11,*) im, minimo
   !write(*,*) im, minimo
end do
close(11)

print * , '--------------------------------'
print * , '!Mínimos diarios'
open (unit=12, file='mindia.csv', status='unknown')
do j=0,datos-1,dia
minimod=0
  do i=1,dia,1
  if(altura(i+j).lt.minimod) then
  minimod = altura(i+j)
  im = tiempo(i+j) 
  end if
  end do
  write (12,*) im, minimod
  !write (*,*) im, minimod
end do
close(12)


print * , '--------------------------------'
print * ,'!Máximos diarios'
open (unit=13, file='maxdia.csv', status='unknown')
do j=0,datos-1,dia
maximod=-1
  do i=1,dia,1
  if(altura(i+j).gt.maximod) then
  maximod = altura(i+j)
  im = tiempo(i+j)
  end if
  end do
  write (13,*) im, maximod
 !write (*,*) im, maximod
end do
close(13)
close (8)



print * , 'nivel de marea máxima', maximo
print * , 'mivel de marea mímina', minimo
print * , 'tiempo de marea mínima', dia

print * , '-------------------------------'
print * , 'Periodos entre máximos por mes'
open(unit=10, file='max.csv', status='unknown')

do i=1,5, 1
   read(10,*) tm(i), mm(i)
 if (i .gt. 1) then
   periodomaxm(i) = tm(i) - tm(i-1)
 end if
   !write(*,*) periodomaxm(i)
  
end do
  !write(*,*) periodomaxm
  print * , 'Periodo promedio entre maximos mensuales'
  write(*,*) sum(periodomaxm)/4, 'dias'
close(10)

print * , '-------------------------------'
print * , 'Periodos entre mínimos por mes'
open(unit=11,file='min.csv',status='unknown')
do i=1,5,1
  read(11,*) tm(i), mm(i)
  periodominm(i) = tm(i) - tm(i-1)
  !write(*,*) periodominm(i)
end do
  !write(*,*) periodominm(i)
close(11)

print * , '-------------------------------'
print * , 'Periodos entre mínimos por dias'
open (unit=12, file='mindia.csv', status='unknown')
do i =1,159,1
   read(12,*) tm(i), mm(i)
   periodomind(i) = tm(i) - tm(i-1)
  !write(*,*) periodomind(i)
end do
  !write(*,*) periodomind(i)
close(12)

print * , '-------------------------------'
print * , 'Periodos entre máximos por dias'
open (unit=13, file='maxdia.csv', status='unknown')
do i=1,dias,1
   read(13,*) tm(i), mm(i)
if (i .gt. 1) then
   periodomaxd(i) = tm(i)-tm(i-1)
end if
   !write(*,*) periodomaxd(i)
end do
   print * , '-------------------------------'
   print * , 'Periodo promedio entre maximos diarios'
  !write(*,*) sum(periodomaxd)/(dias-1), 'dias'
close(13)

print * , '-------------------------------'

print * , 'mareas semidiurnas'
open(unit=8, file='Mareas.csv',status='old')
do i=1, datos, 1 
   read (8,*) B(i), p, t, altura(i), d, tiempo(i)
  !write (*,*) B(i),p, t, altura(i), d, tiempo(i)
end do  


open (unit=14, file='semidiurna.csv', status='unknown')

do i=1,318,1
   read(14,*) tm(i),mm(i)
end do

do j=0,datos-1,mediodia
 maximd=-1
 do i=1,mediodia,1
  if(altura(i+j).gt.maximd) then
  maximd = altura(i+j)
  im = tiempo(i+j)
  end if
 end do
 
  write (14,*) im, maximd
  !write (*,*) im, maximd
end do

print * , '-------------------------------'
print * , 'diferencias entre mareas semidiurnas'
!promedio semidiurnas
do i=1,318,1
 
  if (i .gt. 1) then
   periodomd(i) = tm(i)-tm(i-1)
  end if
  write(*,*) periodomd(i)
end do
   print * , '-------------------------------'
   print * , 'Periodo promedio entre mareas semidiurnas'
  write(*,*) sum(periodomd)/317, 'dias' 

close(14)
close(8)



end Program Mareas




