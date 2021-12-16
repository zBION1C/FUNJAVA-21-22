interface I {
	int m(int x);
}

interface J {
	int m(I x, I y, boolean z);
}

class P {
	public static void main(String[] args){
		J w = (x,y,z) -> x.m(y.m(2+1337, (z) -> 3+1024));
		System.out.println(w.m(2, (x,y) -> x+y), z);
	}
}