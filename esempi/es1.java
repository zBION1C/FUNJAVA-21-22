interface I {
	int m(int x);
}

interface J {
	int m(I x, I y, boolean z);
}

class P {
	public static void main(String[] args){
		J w = (x,y,z) -> z.m(y.m(x)+x);
		System.out.println(w.m(2, (x) -> x+1, (u) -> u+3));
	}
}