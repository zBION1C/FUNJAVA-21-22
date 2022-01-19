interface J{
	J m(J x);
}

class P{
	public static void main(String[] args){
		J w = (u) -> u.m(u);
		System.out.println(w.m((x) -> x.m(x)));
	}
}