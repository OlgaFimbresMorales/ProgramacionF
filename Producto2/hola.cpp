#include <iostream>
#include <unistd.h>

main()
{
  std::cout << "¡ Hola! Trataré de adivinar un número." << std::endl;
  std::cout << "Piensa un número entre 1 y 10." << std::endl;
  sleep(5);
  std::cout << "Ahora multiplícalo por 9." << std::endl;
  sleep(5);
  std::cout << "Si el número tiene 2 dígitos, súmalos entre si: Ej. 36 -> 3+6=9." << std::endl;
  std::cout << "Si tu número tiene un solo dígito, súmale 0." << std::endl;
  sleep(5);
  std::cout << "Al número resultante súmale 4." << std::endl;
  sleep(10);
  std::cout << "Muy bien. El resultado es 13 :)" << std::endl; 
  return 0;
}


  
