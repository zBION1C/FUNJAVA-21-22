interface I {
	int m(int x);
}

interface J {
	int m(I x, I y, int z);
}

class P {
	public static void main(String[] args){
		J w = (x,y,z) -> x.m(z + y.m(z));
		System.out.println(w.m((x) -> x+1, (y) -> w.m((x) -> 2, (y) -> 3, 1), 4));
	}
}