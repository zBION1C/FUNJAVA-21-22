interface I {
	int m(int x);
}

interface J {
	int m(I x, I y, boolean z);
}

class P {
	public static void main(String[] args){
		J w = (x,y,z) -> x.m(y.m(z) + z);
		System.out.println(w.m((x) -> x+1, (u) -> u+2, 4));
	}
}