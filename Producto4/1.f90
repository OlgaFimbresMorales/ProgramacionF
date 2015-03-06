
f(x):= sin(x);
l(x):=taylor(f(x), x, 0, 1);
p(x):=taylor(f(x), x, 0, 3);
t(x):=taylor(f(x), x, 0, 5);
s(x):=taylor(f(x), x, 0, 7);
fortran(f(x));
fortran(p(x));
fotran(t(x));
fortran(l(x));
fotran(s(x));
tex(f(x),p(x),t(x),l(x),s(x));
plot2d ([f(x),l(x),p(x),t(x),s(x)],    
        [x, -4, 4], [y, -2, 2],
        [style, [lines,2]],  
        [color, red, green, blue, magenta, cyan],  
        [legend, Sin(x), P1(x), P3(x), P5(x), P7(x)]);

