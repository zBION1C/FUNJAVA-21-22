interface I{
	int m(int x);
}

interface J{
	int m(I x, int y);
}

class P{
	public static void main(String[] args){
		J w = (u, y) -> u.m(1);
		System.out.println(w.m((u) -> u, 2));
	}
}