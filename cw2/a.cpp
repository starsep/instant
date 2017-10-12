#include <cstdio>

extern int f(int *a, int b, int c, int d, int e);

int main() {
    printf("%d", f(0, 1, 2, 3, 4));
}
