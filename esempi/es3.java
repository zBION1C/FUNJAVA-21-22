interface I {
	int m(int x);
}

interface J {
	int m(I x, int y, int z);
}

class P {
	public static void main(String[] args){
		J w = (x,y,z) -> x.m(y+z);
		System.out.println(w.m((x) -> x+x, 2, 3+4));
	}
}