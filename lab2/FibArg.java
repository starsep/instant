public class FibArg {
    public static void main(String[] args) {
      try {
        int n = Integer.parseInt(args[0]);
        System.out.println(Fib.fib(n));
      } catch(Exception e) {
        System.err.println("Failure :C");
      }
    }
}
