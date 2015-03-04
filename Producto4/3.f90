f(x):= log(cos(x));
t(x):=taylor(f(x), x, 0, 1);
l(x):=taylor(f(x), x, 0, 3);
m(x):=taylor(f(x), x, 0, 6);
s(x):=taylor(f(x), x, 0, 10);
fortran(t(x);
fotran(l(x));
fotran(m(x));
fotran(s(x));
tex(f(x),p(x),t(x),l(x),s(x));
plot2d ([f(x),t(x),l(x),m(x),s(x)], 
   [x, -5, 5], [y, -%pi/2, %pi/2], 
   [legend, log(cos(x)), T1, T3, T6, T10]);
