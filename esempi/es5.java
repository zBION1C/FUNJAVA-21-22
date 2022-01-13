interface I {
	int m(int x);
}

interface Z {
	boolean m(int x);
}

interface J {
	boolean m(Z x, I y, int z);
}

class P {
	public static void main(String[] args){
		J w = (x, y, z) -> x.m(y.m(z));
		System.out.println(w.m((x) -> false, (y) -> y+1, 10));
	}
}
