Program proyectil_2
    implicit none
    real, parameter :: pi = 4.0*atan(1.0)
    real :: v, a, t, h, r, a_grados
    real, parameter :: g = 9.81
    real :: x(150),y(150)
    write (*,*) 'Ingrese un ángulo para el proyectil'
    read *, a_grados
    write (*,*) 'Ingrese una velocidad para el proyectil'
    read *, v
    a = a_grados*pi/180.0
    open(1, file='proy.dat')
    t = 2*v*sin(a)*(1/g)
    h = v*v*sin(a)*sin(a)*(1/(2*g))
    r = v*v*sin(2*a)*(1/g)
    print * , 'Tiempo total de vuelo=' , t
    print * , 'Altura máxima alcanzada=' , h
    print * , 'Distancia máxima alcanzada=' , r
End Program proyectil_2
