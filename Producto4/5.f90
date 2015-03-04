f(x):= (1+x)*(exp(x));
t(x):=taylor(f(x), x, 0, 1);
l(x):=taylor(f(x), x, 0, 3);
m(x):=taylor(f(x), x, 0, 6);
s(x):=taylor(f(x), x, 0, 10);
fortran(t(x));
fotran(l(x));
fotran(m(x));
fotran(s(x));
tex(f(x),p(x),t(x),l(x),s(x));
plot2d ([f(x),t(x),l(x), m(x), s(x)], 
   [x, -3, 3], [y, -4, 4], 
   [legend, f(x), T1, T3, T6, T10]);
