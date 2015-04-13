Program Tiro_p
Implicit None
real, parameter :: g = 9.81, p =1.1644, pi = 4.0*atan(1.0), CD = 0.47
real :: dt, x0, y0, v0, v0x, v0y, ax, ay, m, dt_r, FD, x, y, t, tv, A, r, D, ty
integer :: n
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
call Tiro_a(m, p, A, D, v0, v0x, v0y, ax, ay, g)
ty = -v0y/ay !tiempo altura maxima

tv = sqrt(m/(g*D))*acos(y)*(exp((y*D)/m)) !tiempo en caer

print * , 'tiempo subir', ty
print * , 'tiempo caer', tv

print * , 'ay =', ay
print * , 'ax =', ax
print * , 'D=', D

call Tiro_d(dt_r, x0, y0, v0x, v0y, ax, ay, t, x, y, g, ty, tv, m, D)

print * , 'Tiempo total de vuelo', t
print * , 'Distancia máxima recorrida', x
print * , 'Altura máxima alcanzada', y

End Program Tiro_p  

Subroutine Tiro_d(dt_r, x0, y0, v0x, v0y, ax, ay, t, x, y, g, ty, tv, m, D)
real, intent(in) :: dt_r, x0, y0, v0x, v0y, ax, ay, g, tv, ty, m, D
real, intent(inout) :: t, x, y

t = ty+tv
x = x0 + (v0x*t) + ((m/D)*log(t))
y = (v0y*ty) + ((1/2)*ay*ty*ty)

end subroutine Tiro_d

Subroutine Tiro_a(m, p, A, D, v0, v0x, v0y, ax, ay, g)
real, intent(in) :: m, p, A, D, v0, v0x, v0y, g
real, intent(inout) :: ax, ay

ax = -(D/m)*v0*v0x
ay = -g-(D*v0*v0y*(1/m))

end subroutine Tiro_a
