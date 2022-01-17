interface I {
	int m (int x);
}

interface J {
	int m (I x, I y, int z);
}

class P {
	public static void main(String[] args) {
	J w = (x, y, z) -> x.m(y.m(z));
	System.out.println(w.m((u)-> u, (v) -> 41, 8));
	}
}