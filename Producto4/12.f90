program taylor_aprox
    implicit none
open(1, file='taylor.dat')
sin(x)
(%o6)                                done
(%i7) fortran(p(x));
      x-x**3/6.0E+0
(%o7)                                done
(%i8) fotran(t(x));
                                     5     3
                                    x     x
(%o8)                        fotran(--- - -- + x)
                                    120   6
(%i9) fortran(l(x));
      x
(%o9)                                done
(%i10) fotran(s(x));
                                    7     5     3
                                   x     x     x
(%o10)                   fotran(- ---- + --- - -- + x)
                                  5040   120   6f(x):= sin(x);


close(1)

end program taylor_aprox
