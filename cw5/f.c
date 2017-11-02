int f(int a[], int n, int *r) {
    for (int i = 0; i < n; i++) {
        if (a[i] < 0) {
            *r = a[i];
            return i;
        }
    }
    return -1;
}
