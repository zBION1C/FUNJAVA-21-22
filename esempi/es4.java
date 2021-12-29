interface I {
	int m(int x);
}

interface J {
	int m(int x, int y);
}

class P {
	public static void main(String[] args){
		J w = (x,y) -> x+y;
		System.out.println(w.m(2, 3));
	}
}