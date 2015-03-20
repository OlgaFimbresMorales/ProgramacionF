Program Tiro_Parabolico
Implicit None 
real, parameter :: g = 9.81, p = 1.1644, pi = 4.0*atan(1.0)
real :: dt, x0, y0, v0, v0x, v0y, ax, ay, t, x, y, m, A, CD, dt_r 
real :: z(200), w(200)
   integer :: i
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


call Tiro_p (dt_r, x0, y0, v0, v0x, v0y, ax, ay, t, x, y, g)

print * , 'Tiempo total de vuelo', t
print * , 'Distancia máxima recorrida', x
print * , 'Altura máxima alcanzada', y

open(1, file='tirop.dat')

do i=1,500
    
    
      t = (float(i)*0.01)   
      z(i) = (((v0*v0*(sin(2*dt_r)))) + (v0x*(sqrt((v0y*v0y) + (2*g*y)))))/g
      w(i) = ((v0y*v0y)/(2*g))    
      write(1,*) z(i), w(i)      
      if (w(i)<0) exit   
 end do   
close(1)   

End program Tiro_Parabolico

Subroutine Tiro_p(dt_r, x0, y0, v0, v0x, v0y, ax, ay, t, x, y, g)
Implicit None 
real, intent(in) :: dt_r, x0, y0, v0, v0x, v0y, g, ax, ay
real, intent(inout) :: t, x, y
y = y0 + ((v0y*v0y)/(2*g))
t = (v0y + sqrt((v0y*v0y) + (2*g*y)))/g
x = x0 + (((v0*v0*(sin(2*dt_r)))) + (v0x*(sqrt((v0y*v0y) + (2*g*y)))))/g

End Subroutine Tiro_p 


Subroutine Tir_op(m, p, A, CD, v0x, v0y, ax, ay)
Implicit None
real, intent(in) :: m, p, A, CD, v0x, v0y
real, intent(inout) :: ax, ay
ax = 1
ay = 2
End Subroutine Tir_op  
