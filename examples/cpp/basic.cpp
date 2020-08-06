// Example program
#include <iostream>
#include <string>

/*@
 requires true;
 ensures 1 != 1;
 */
int main()
{
  int total = 0;
  
  /*@ 
    loop_invariant 0 <= i && i <= 5;
    loop_invariant (i>1) ==> total == \old(total) + i;
  */
  for (int i = 0; i < 5; i++) {
    total = total + 1;
  }
  return total;
}

class Please {
    int k = 16;
    
    int plz()
    {
       int pl = 23423;
       return pl;
    }
};
