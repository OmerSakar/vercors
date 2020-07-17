// Example program
#include <iostream>
#include <string>


int main()
{
  int total = 0;
  int i = 0;

  /*@ 
    loop_invariant 0 <= i && i <= 5;
    loop_invariant (i>0) ==> total == \old(total) + i-1);
  */
  while (i < 5) {
    total = total + i;
    i = i + 1;
  }
  /*@  assert false; */
  return total;
}

