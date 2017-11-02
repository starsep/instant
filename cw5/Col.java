public class Col {
    public static void f(int[] a, int i, int n) {
        a[n - 1] = (!(i < 0 || i >= n || a[i] > 0) ? 1 : 0);
    }
}