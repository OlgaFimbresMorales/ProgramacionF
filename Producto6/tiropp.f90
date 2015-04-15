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

dt_r =(dt*pi)/180
v0x = v0*cos(dt_r) !velocidad inicial en x
v0y = v0*sin(dt_r) !velocidad inicial en y
A = pi*r*r !area transversal 
D = p*CD*A*0.5 !

 

print * , '-------------------------------------'
print * , '-------------------------------------'
print * , '-------------------------------------'
print * , '-------------------------------------'
print * , '-------------------------------------'


call Tiro_sfriccion(v0, dt_r, v0x, v0y, Xs, Ts, Ys, ttotal, xsf, ysf)
print * , 'Modelo Ideal'
print * , 'Tiempo de vuelo', ttotal, 'segundos'
print * , 'Alcance', xsf, 'metros'
print * , 'Altura máxima', ysf, 'metros'
print * , '-------------------------------------'
print * , '-------------------------------------'
print * , '-------------------------------------'
call Tiro_friccion1(v0, v0x, v0y, ax, ay, Xf, Tf, Yf, x0, y0, vxf, vyf, ft, D, m, Vo)
print * , 'Modelo real'
print * , 'Tiempo de vuelo',Tf ,'segundos'
print * , 'Alcance',Xf ,'metros'
print * , 'Altura máxima',Yf ,'metros'
end Program Tiro_parabolico

module Cte
implicit none 
real, parameter :: g = 9.81, p =1.1644, pi = 4.0*atan(1.0), CD = 0.47
integer, parameter :: ntps=5000
end module Cte

subroutine Tiro_sfriccion(v0, dt_r, v0x, v0y, Xs, Ts, Ys, ttotal, xsf, ysf)
use Cte
implicit none
real, intent(in) :: v0x, v0y, v0, dt_r
real, intent(inout) :: Xs, Ts, Ys, ttotal, xsf, ysf
real, dimension (0:ntps) :: xx, yy, tt
integer :: i

Ts = 2*v0*sin(dt_r)*(1/g)
Ys = v0*v0*sin(dt_r)*sin(dt_r)*(1/(2*g))
Xs = v0*v0*sin(2*dt_r)*(1/g)

open (1, file='tirosinfriccion.dat')
do i=0, ntps, 1
  
  tt(i) = (float(i)*0.01)
  xx(i) = v0x*tt(i) 
  yy(i) = v0y*tt(i) - 0.5*g*tt(i)*tt(i)
write (1,*) xx(i), yy(i)
if (yy(i)<0) exit
end do
close (1)

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


open (2, file='tirofriccion.dat')

do i = 0, ntps, 1
    
   ft(0) =0
    fx(0) = 0
    fy(0) = 0
    vxf(0) = v0x
    vyf(0) = v0y
    Vo(0) = v0
 

   ft(i+1) = ft(i) + 0.01
   vxf(i+1) = vxf(i+1) + (ax(i+1)*ft(i+1))
   vyf(i+1) = vxf(i+1) + (ay(i+1)*ft(i+1))
   ax(i+1) = -(D/m)*(Vo(i+1))*(vxf(i+1))
   ay(i+1) = -g - ((D/m)*(Vo(i+1))*(vyf(i+1)))

   Vo(i+1) = sqrt((vxf(i+1)*vxf(i+1)) + (vyf(i+1)*vyf(i+1)))
   
   fx(i+1) = fx(i) + (vxf(i+1)*ft(i+1)) + ((1/2)*ax(i+1)*ft(i)*ft(i+1))
   fy(i+1) = fy(i) + (vyf(i+1)*ft(i+1)) + ((1/2)*ay(i+1)*ft(i)*ft(i+1))
  
write (2, 1001) fx(i), fy(i)
if (fy(i)<0) exit

end do
1001 format (2f10.6)
close (2)

Tf = ft(i+1)
Xf = fx(i+1)
Yf = maxval(fy, 1, (fy(i)<0))
end subroutine Tiro_friccion1



