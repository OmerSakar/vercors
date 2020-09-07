// Example program
#include <iostream>
#include <string>


//int x = 123;

int main()
{
  int total = 0, plz = 2;

  /*@
    loop_invariant 0 <= i && i <= 5;
    loop_invariant (i>1) ==> total == \old(total) + i;
  */
  for (int i = 0; i < 5; i++) {
    total = total + 1;
  }

  //Counter* p;

  //vector<double> a;
  //std::vector<double> a;

  // cout<<p.getK();

  return total;
}

class Counter { //demo 1
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

class Array { //demo 2

    /*@
    context_everywhere A != nullptr;
	context_everywhere (\forall* int j; 0 <= j && j < A.length; Perm(A[j], write));
	ensures (\forall int j; 0 <= j && j < A.length; A[j] == 0);
	*/
	void clear(int A[]) {
		int i = 0;
        //string* ptr = &food;
        /*@
		loop_invariant 0 <= i && i <= A.length;
		loop_invariant (\forall int j; 0 <= j && j < i; A[j] == 0);
        */
        //TODO .length is not actually CPP.
        //TODO See how we can easily support different ops on e.g. arrays
		while (i < A.length) {
			A[i] = 0;
			i = i + 1;
		}
	}

};

class ArraySum { //demo 3
	int sum;

    /*@
    context_everywhere A != nullptr;
	context_everywhere 0 <= i && i <= A.length;
    requires (\forall* int j; 0 <= j && j < A.length; Perm(A[j], 1\2));
	*/
	int /*@ pure */ sum_contrib(int A[], int i) {
	  return (i == A.length) ? 0 : A[i] + sum_contrib(A, i + 1);
    }
};