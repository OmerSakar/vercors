// Example program
#include <iostream>
#include <string>


//int x = 123;

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

  Please p;
  
  vector<double> a;
  //std::vector<double> a;

  // cout<<p.getK();
    
  return total;
}

class Please {
    int k;
    
    int plz()
    {
       int pl = 23423;
       return pl;
    }
};
