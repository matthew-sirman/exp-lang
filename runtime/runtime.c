#include <stdio.h>

typedef long long i64;

extern i64 emain();

int main() {
    printf("%lld\n", emain());
    return 0;
}
