interface I {
	int m(int x);
}

interface J {
	int m(I x, I y, boolean z);
}

class P {
	public static void main(String[] args){
		int w = (x,y) -> x.m(y);
		System.out.println(true);
	}
}