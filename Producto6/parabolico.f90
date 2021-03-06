Program Tiro_p
Implicit None
real, parameter :: g = 9.81, p =1.1644, pi = 4.0*atan(1.0), CD = 0.47
real :: dt, x0, y0, v0, v0x, v0y, ax, ay, m, dt_r, FD, x, y, t, tv, A, r, D, ty, n, tt, dy, delt, ot, xsf, ysf, tsf
real ::  ox(200), oy(200)
integer, parameter :: npts= 5000
integer :: i, l, z
write (*,*) 'Ingrese la masa del proyectil'
read *, m
write (*,*) 'Identifique el radio del proyectil'
read *, r
write (*,*) 'Ingrese una velocidad inicial para el proyectil'
read *, v0
write (*,*) 'Ingrese un ángulo inicial para el proyectil'
read *, dt
write (*,*) 'Determine una posición inicial en el eje x'
read *, x0
write (*,*) 'Determine una posición inicial en el eje y'
read *, y0

dt_r =(dt*pi)/180
v0x = v0*cos(dt_r) !velocidad inicial en x
v0y = v0*sin(dt_r) !velocidad inicial en y
A = pi*r*r !area transversal
FD = D*v0*v0 !fuerza de arrastre 
D = p*CD*A*0.5
n = (v0y + sqrt((v0y*v0y) + (2*g*y0)))/g !tiempo total de vuelo
ty = sqrt(m/(g*D))*atan(v0y*sqrt(D/(m*g))) !tiempo altura maxima
tv = sqrt((2*y)/(-ay)) !tiempo en caer

print * , 'Datos sin fricción'
print * , 'Tiempo total de vuelo', tsf
print * , 'Distancia máxima recorrida', xsf
print * , 'Altura máxima alcanzada', ysf

End Program Tiro_p  

subroutine friccion_no (i, v0x, v0y, g, ot, ox, oy, ntps, xsf, ysf, tsf)
implicit none
real, intent(in) :: v0y, v0x, g
real, intent(inout) :: ot, xsf, ysf, tsf
integer, intent(in) :: ntps
integer :: i
real :: ox(200),oy(200)
do i=0,ntps,1
    ot = (float(i)*0.01)
    ox(i) = v0x*ot
    oy(i) = v0y*ot - 0.5*g*ot*ot
    write(1,10001) ox(i), oy(i)
    if (oy(i)<0) exit
   end do
10001 format (2f10.6)


xsf = ox(i)
ysf = oy(i)

end subroutine friccion_no

Subroutine y_max(v0y, ty, ay, y, D, m)
real, intent(in) :: v0y, ty, ay, D, m
real, intent(inout) :: y

y = ((v0y*ty)+((1/2)*ay*ty*ty))/2 !da el doble de lo que deberia

end subroutine y_max 

Subroutine Tiro_d(dt_r, x0, y0, v0x, v0y, ax, ay, t, x, g, ty, tv, m, D)
real, intent(in) :: dt_r, x0, y0, v0x, v0y, ax, ay, g, tv, ty, m, D
real, intent(inout) :: t, x

t = ty+tv
x = x0 + (v0x*t) + ((m/D)*log(t))

end subroutine Tiro_d
 


Subroutine Tiro_a(m, p, A, D, v0, v0x, v0y, ax, ay, g)
real, intent(in) :: m, p, A, D, v0, v0x, v0y, g
real, intent(inout) :: ax, ay


ax = -(D/m)*v0*v0x
ay = -g-(D*v0*v0y*(1/m))

end subroutine Tiro_a
