// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases SignalsMustPropagate
//:: tools silicon
//:: verdict Fail

class C {
    void m1() {
        m2();
    }

    //@ signals (RuntimeException e) true;
    void m2();
}
