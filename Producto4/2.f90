f(x):= log(1+x);
t(x):=taylor(f(x), x, 0, 4);
l(x):=taylor(f(x), x, 0, 7);
m(x):=taylor(f(x), x, 0, 11);
s(x):=taylor(f(x), x, 0, 16);
fortran(t(x);
fortran(l(x));
fotran(m(x));
fortran(s(x));
tex(f(x),p(x),t(x),l(x),s(x));
plot2d ([f(x),t(x),l(x),m(x),s(x)], 
   [x, -1.5, 1.5], [y, -4, 2], 
   [legend, log(1+x),T4, T7, T11, T16]);
