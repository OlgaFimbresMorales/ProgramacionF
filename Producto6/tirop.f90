Program Tiro_Parabolico
Implicit None 
real, parameter :: g = 9.81, p = 1.1644, pi = 4.0*atan(1.0), A=1
real :: dt, x0, y0, v0, v0x, v0y, ax, ay, t, x, y, m, CD, dt_r, FD 
real :: z(3000), w(3000), vx(100), vy(100)
   integer :: i, c
write (*,*) 'Ingrese la masa del proyectil'
read *, m
write (*,*) 'Elija un coeficiente de fricción:'
write (*,*) ' Esfera: 0.47, Media esfera: 0.42, Cono: 0.5, Cubo: 1.05, Cilindro: 0.82, Aerodinámico: 0.04'
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




call Tiro_a (dt_r, x0, y0, v0, v0x, v0y, ax, ay, t, x, y, g, FD)

print * , 'Tiempo total de vuelo', t
print * , 'Distancia máxima recorrida', x
print * , 'Altura máxima alcanzada', y

open(1, file='tirop.dat')
do c=1,100
    
    t = (float(c)*0.01)
    vx(c) = (v0x*t) + (m/(FD*t))
    vy(c) = (v0y) - ((sqrt((m*g)/FD))*(tan(sqrt((g*FD)/m)*t))) 
    write(1,*) vx(c), vy(c)
    if (vy(c)>(-v0y)) exit
end do
close(1)
End program Tiro_Parabolico


Subroutine x (v0x, v0y, v0, dt_r, g, t)
Implicit None
real, intent(in) :: v0x, v0y, v0, dt_r, g
real, intent(inout) :: t
real :: z(3000), w(3000)
   integer :: i
do i=1,3000
    
    
      t = (float(i)*0.018)   
      z(i) = v0x*t
      w(i) =  v0*sin(dt_r)*t - 0.5*g*t*t   
      write(1,*) z(i), w(i) 
      if (w(i)<0) exit        
 end do  
end subroutine x  


Subroutine Tiro_p(dt_r, x0, y0, v0, v0x, v0y, ax, ay, t, x, y, g)
Implicit None 
real, intent(in) :: dt_r, x0, y0, v0, v0x, v0y, g, ax, ay
real, intent(inout) :: t, x, y
y = y0 + ((v0y*v0y)/(2*g))
t = (v0y + sqrt((v0y*v0y) + (2*g*y0)))/g
x = x0 + (v0x*t)
End Subroutine Tiro_p 

Subroutine Tiro_a (dt_r, x0, y0, v0, v0x, v0y, ax, ay, t, x, y, g, FD)
real, intent(in) :: dt_r, x0, y0, v0, v0x, v0y, g, ax, ay, FD
real, intent(inout) :: t, x, y
y = (v0y*t) + ((m/FD)*(Log(cos(sqrt((g*FD)/m)*t))))
t = (v0y + sqrt((v0y*v0y) + (2*g*y0)))/g
x = (v0x*t) + ((m/FD)*(Log(t)))
End Subroutine Tiro_a


Subroutine Tir_op(m, p, A, CD, v0, v0x, v0y, ax, ay,g)
Implicit None
real, intent(in) :: m, p, A, CD, v0, v0x, v0y, g
real, intent(inout) :: ax, ay
ax = -(CD/m)*v0*v0x
ay = -(g)-((CD/m)*v0*v0y)
End Subroutine Tir_op  
