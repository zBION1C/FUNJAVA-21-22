interface I {
	int m(int x);
}

interface J {
	int m(I x, int y);
}

interface Z {
	int m(int x, int y);
}

class P {
	public static void main(String[] args){
		J w = (x,y) -> x.m(y);
		System.out.println(w.m((u) -> u+1, 2));
	}
}
