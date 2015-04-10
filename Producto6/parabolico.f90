Program Tiro_p
Implicit None
real, parameter :: g = 9.81, p =1.1644, pi = 4.0*atan(1.0)
real :: dt, x0, y0, v0, v0x, v0y, ax, ay, m, CD, dt_r, FD, x, y, n, A 
integer :: t
write (*,*) 'Ingrese la masa del proyectil'
read *, m
write (*,*) 'Elija un coeficiente de fricción:'
write (*,*) 'Esfera: 0.47, Media esfera: 0.42, Cono: 0.5, Cubo: 1.05, Cilindro: 0.82, Aerodinámico: 0.04'
read *, CD
write (*,*) 'Ingrese una velocidad inicial para el proyectil'
read *, v0
write (*,*) 'Ingrese un ángulo inicial para el proyectil'
read *, dt
write (*,*) 'Determine una posición inicial en el eje x'
read *, x0
write (*,*) 'Determine una posición inicial en el eje y'
read *, y0


dt_r =(dt*pi)/180
v0x = v0*cos(dt_r)
v0y = v0*sin(dt_r)
FD = (1/2)*p*v0*v0*CD*A

n = (v0y + sqrt((v0y*v0y) + (2*g*y0)))/g
call Tiro_d (v0, v0y, v0x, x0, y0, m, CD, FD, n, g, x, y, t)

print * , 'Tiempo total de vuelo',t
print * , 'Distancia máxima recorrida', x
print * , 'Altura máxima alcanzada', y

End Program Tiro_p  

Subroutine Tiro_d (v0, v0y, v0x, x0, y0, m, CD, FD, n, g, x, y, t)
real, intent(in) :: v0, v0y, v0x, x0, y0, m, CD, FD, n, g
real, intent(inout) :: x, y
   integer, intent(inout) :: t
t = (2/FD)*log(1+((FD*v0y)/g))
y = (v0y*t) + ((m/FD)*(Log(cos(sqrt((g*FD)/m)*t))))
x = (v0x*t) + ((m/FD)*(Log(t)))
end subroutine Tiro_d
