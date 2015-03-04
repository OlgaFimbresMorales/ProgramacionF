f(x):= (exp(x))/(cos(x));
t(x):=taylor(f(x), x, 0, 1);
l(x):=taylor(f(x), x, 0, 5);
m(x):=taylor(f(x), x, 0, 9);
s(x):=taylor(f(x), x, 0, 13);
fortran(t(x));
fotran(l(x));
fortran(m(x));
Fortran(s(x));
tex(f(x),p(x),t(x),l(x),s(x));
plot2d ([f(x),t(x),l(x),m(x),s(x)], 
   [x, -5, 5], [y, -%pi, %pi], 
   [legend, (exp(x)/cos(x)), T1, T5, T9, T13]);
