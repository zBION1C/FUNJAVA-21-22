interface I {
	int m(int x);
}

class P {
	public static void main(String[] args) {
		I w = (x) -> x;
		System.out.println(w.m(y));
	}
}