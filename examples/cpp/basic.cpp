// Example program
#include <iostream>
#include <string>


//int x = 123;

/*
 requires true;
 ensures 2 == 1+2-1;
 */
//int main()
//{
////  int total = 0, plz = 2;
////
////  /*
////    loop_invariant 0 <= i && i <= 5;
////    loop_invariant (i>1) ==> total == \old(total) + i;
////  */
////  for (int i = 0; i < 5; i++) {
////    total = total + 1;
////  }
//
////  Please p;
//
//  //vector<double> a;
//  std::vector<double> a;
//
//  // cout<<p.getK();
//
//  return total;
//}

class Counter {
	int count;

    /*@
	requires Perm(count, 1);
	ensures Perm(count, 1\2);
	ensures count == \old(count) + n;
	*/
	void incr(int n) {
		count = count + n;
	}

    /*@
	requires Perm(count, 1);
	ensures Perm(count, 1\2);
	ensures count == \old(count) + 2 * n;
	*/
	void incr2(int n) {
		incr(n);
		incr(n); // problem
	}
};

class Array {

    /*@
    context_everywhere A != nullptr;
	context_everywhere (\forall* int j; 0 <= j && j < A.length; Perm(A[j], write));
	ensures (\forall int j; 0 <= j && j < A.length; A[j] == 0);
	*/
	void clear(int A[]) {
		int i = 0;

        /*@
		loop_invariant 0 <= i && i <= A.length;
		loop_invariant (\forall int j; 0 <= j && j < i; A[j] == 0);
        */
		while (i < A.length) {
			A[i] = 0;
			i = i + 1;
		}
	}

};