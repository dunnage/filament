package dunnage;

import clojure.lang.Compiler;
import clojure.lang.IFn;

public class Filament {
    IFn[] tasks;
    Object[] state;
    IFn makeFiber;
    IFn unwrapState;
    IFn getMode;
    IFn getIndex;

    public Filament(IFn[] t,Object[] s, IFn makeFiberIn, IFn unwrapStateIn, IFn getModeIn, IFn getIndexIn) {
        tasks = t;
        state = s;
        makeFiber = makeFiberIn;
        unwrapState = unwrapStateIn;
        getMode = getModeIn;
        getIndex = getIndexIn;
    }

    public void reuse(Object[] s) {
        state = s;
    }

    public Object  run() {
        while (true) {
            int oldmode = ((Long) getMode.invoke(state)).intValue();
            switch (oldmode) {
                case 0:
                case 1:
                case 2:
                    int idx = ((Long) getIndex.invoke(state)).intValue();
                    IFn nextfun = tasks[idx];
                    state = (Object[]) nextfun.invoke(state);
                    break;
                case 3:
                    return makeFiber.invoke(state);
                case 4:
                    return unwrapState.invoke(state);


            }

        }

    }

}
