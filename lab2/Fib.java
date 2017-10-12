public class Fib {
  public static void main(String[] args) {
    System.out.println(fib(13));
  }

  public static int fib(int n) {
    int x = 1, y = 1;
    while (n != 0) {
      int z = x + y;
      x = y;
      y = z;
      n--;
    }
    return x;
  }
}
