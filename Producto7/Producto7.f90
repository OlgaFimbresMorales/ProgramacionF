Module periodos
implicit none
integer, parameter :: dia=48, mes=30
end module periodos

Program Mareas
use periodos
Implicit None
real, dimension(54) :: a, b
integer :: i



open(2,file="mareas3.csv",status="old")
do i=1, dia, 1
   read (2,*) a(i), b(i)  
   write (*,*) a(i), b(i)

end do
close(2)

end Program Mareas
