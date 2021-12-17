interface I {
	int m (int x);
} 
interface J {
	int m (I x, I y, int z);
} 
class P {
	public static void main(String[] args) {
		I x = (y) -> y+1;
		System.out.println(x.m(3));
	}
}