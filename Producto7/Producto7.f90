Module periodos
implicit none
integer, parameter :: datos=8000
end module periodos

Program Mareas
use periodos
Implicit None
real :: p, t, d
real, dimension(datos)::B, altura, tiempo
integer :: i



open(unit=8, file='mareas.csv',status='old')
do i=1, datos, 1 
   read (8,*) B(i), p, t, altura(i), d, tiempo(i)
   write (*,*) B(i),p, t, altura(i), d, tiempo(i)
end do  
close (8)

end Program Mareas


subroutine periodo_dias
implicit none
real, allocatable :: dia (:,:)

end subroutine periodo_dias
