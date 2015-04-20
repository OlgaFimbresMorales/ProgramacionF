module Cte
implicit none 
real, parameter :: g = 9.81, p =1.1644, pi = 4.0*atan(1.0), CD = 0.47
! g = atiende al valor general que se le asigna a la gravedad terrestre, esta puede variar dependiendo de la altura o del planeta donde deseemos posicionar la simulación.
!p = es la densidad del aire a una temperatura de 30 grados centigrados, la cual varia junto con la temperatura.
!CD = atiende al coeficiente de fricción de un cuerpo esférico, dependiendo del la forma del proyectil esta varia.
integer, parameter :: ntps=5000 ! este número solo atienda a la cantidad de puntos que se le desee poner como máximo para realizar las operaciones.
end module Cte



program Tiro_parabolico
use Cte
implicit none 
real :: dt, x0, y0, v0, v0x, v0y, m, dt_r, D, Xs, Ts, Ys, Xf, Tf, Yf, A, r, ttotal, xsf, ysf, tt
real :: vxf(0:ntps), vyf(0:ntps), ax(0:ntps), ay(0:ntps), xx(0:ntps), yy(0:ntps), ft(0:ntps), Vo(0:ntps)


print * , 'Datos iniciales'
write (*,*) 'Ingrese la masa del proyectil'
read *, m
print * , '-------------------------------------'
write (*,*) 'Identifique el radio del proyectil'
read *, r
print * , '-------------------------------------'
write (*,*) 'Ingrese una velocidad inicial para el proyectil'
read *, v0
print * , '-------------------------------------'
write (*,*) 'Ingrese un ángulo inicial para el proyectil'
read *, dt
print * , '-------------------------------------'
write (*,*) 'Determine una posición inicial en el eje x'
read *, x0
print * , '-------------------------------------'
write (*,*) 'Determine una posición inicial en el eje y'
read *, y0
print * , '-------------------------------------'

dt_r =(dt*pi)/180 !fortran trabaja las funciones trigonométricas con radianes, asi que debemos convertir los grados a radianes. 
v0x = v0*cos(dt_r) !velocidad inicial en x.
v0y = v0*sin(dt_r) !velocidad inicial en y.
A = pi*r*r !area transversal .
D = p*CD*A*0.5 !constante de fricción.

 

print * , '-------------------------------------'
print * , '-------------------------------------'
print * , '-------------------------------------'
print * , '-------------------------------------'
print * , '-------------------------------------'


call Tiro_sfriccion(v0, dt_r, v0x, v0y, Xs, Ts, Ys, ttotal, xsf, ysf) !llamamos a la subrutina pertinente para que nos muestre los valores que estamos pidiendo.
print * , 'Modelo Ideal'
print * , 'Tiempo de vuelo', ttotal, 'segundos'
print * , 'Alcance', xsf, 'metros'
print * , 'Altura máxima', ysf, 'metros'
print * , '-------------------------------------'
print * , '-------------------------------------'
print * , '-------------------------------------'
call Tiro_friccion1(v0, v0x, v0y, ax, ay, Xf, Tf, Yf, x0, y0, vxf, vyf, ft, D, m, Vo) !llamamos a la subrutina.
print * , 'Modelo real'
print * , 'Tiempo de vuelo',Tf ,'segundos'
print * , 'Alcance',Xf ,'metros'
print * , 'Altura máxima',Yf ,'metros'
end Program Tiro_parabolico



subroutine Tiro_sfriccion(v0, dt_r, v0x, v0y, Xs, Ts, Ys, ttotal, xsf, ysf)!deben de estar espeficicados todos aquellas variables que estará pidiendo desde el programa para poder realizar las operaciones que le especifiquemos.
use Cte !llamamos al módulo de los varoles constantes.
implicit none
real, intent(in) :: v0x, v0y, v0, dt_r !aquellas variables que llamará solamente para trabajar.
real, intent(inout) :: Xs, Ts, Ys, ttotal, xsf, ysf!aquellas variables que nos regresara dentro del programa con un valor asignado.
real, dimension (0:ntps) :: xx, yy, tt !las variables que utilizaremos en la operacion DO.
integer :: i

Ts = 2*v0*sin(dt_r)*(1/g) !Tiempo de vuelo sin fricción.
Ys = v0*v0*sin(dt_r)*sin(dt_r)*(1/(2*g)) !Posición en el eje y sin fricción.
Xs = v0*v0*sin(2*dt_r)*(1/g) !Posición en el eje x sin fricción.

open (1, file='tirosinfriccion.dat') !Abrimos el archivo a donde iran todos los datos necesarios para gráficar.
do i=0, ntps, 1
  
  tt(i) = (float(i)*0.01) !Espeficamos el intervalo de tiempo en que realizara las operaciones.
  xx(i) = v0x*tt(i) !Y sustituimos en cada operacion el tiempo total por el intervalo de tiempo nuevo.
  yy(i) = v0y*tt(i) - 0.5*g*tt(i)*tt(i)
write (1,*) xx(i), yy(i) !Nos escribe dentro del archivo los datos en dos columnas que utilizara para gráficar.
if (yy(i)<0) exit !Condicionamos el programa para que deje de trabajar cuando la posición en y sea menor a 0.
end do
close (1) !cerramos el archivo.

!Y pedimos que nos de los ultimos valores que encontro y el máximo valor en Y.
ttotal =tt(i) 
xsf = xx(i)
ysf = maxval(yy, 1, (yy(i)<0))

end subroutine Tiro_sfriccion

subroutine Tiro_friccion1(v0,v0x, v0y, ax, ay, Xf, Tf, Yf, x0, y0, vxf, vyf, ft, D, m, Vo) 
use cte
implicit none
real, intent(in) :: v0, v0x, v0y, x0, y0, D, m
real, intent(inout) :: Xf, Tf, Yf
real, dimension (0:ntps) :: fx, ft, fy, vxf, vyf, ax, ay, Vo
integer :: i
   
!Es necesario especificar los valores iniciales antes de que comienze a calcular tiempos y posiciones. Estos dependen de las variables que el programa nos pide al inciar.
   fy = 0
   ft(0)=0
   fx(0)=x0
   fy(0)=y0
   vxf(0)=v0x
   vyf(0)=v0y
   ax(0) = -(D/m)*(v0x)*(v0x) !Aceleración inicial en el eje X con fricción.
   ay(0) = -g - ((D/m)*(v0y)*(v0y)) !Aceleración inicial en el eje Y con fricción.

open (2, file='tirofriccion.dat')  !Archivo para graficar.
do i = 0, ntps, 1
    

   ft(i+1) = (ft(i)*0.01) + 0.01 !Intervalo de tiempo
  
   vxf(i+1) = vxf(i) + (ax(i)*ft(i+1)) !Velocidad X.
   vyf(i+1) = vyf(i) + (ay(i)*ft(i+1)) !Velocidad Y.
   fx(i+1) = fx(i) + (vxf(i)*ft(i+1)) + ((1/2)*ax(i)*ft(i+1)*ft(i+1)) !Posición X.
   fy(i+1) = fy(i) + (vyf(i)*ft(i+1)) + ((1/2)*ay(i)*ft(i+1)*ft(i+1)) !Posición Y.
   ax(i+1) = -(D/m)*(vxf(i))*(vxf(i)) !Aceleracion X.
   ay(i+1) = -g - ((D/m)*(vyf(i))*(vyf(i))) !Aceleración Y.
  


write (2, 1001) fx(i), fy(i)
if (fy(i)<0) exit !Condición.

end do
1001 format (2f10.6)
close (2)

!Últimos valores encontrados y el máximo valor de Y.

Tf = ft(i) * 10.0
Xf = fx(i+1)
Yf = maxval(fy, 1, (fy(i)<0))
end subroutine Tiro_friccion1


