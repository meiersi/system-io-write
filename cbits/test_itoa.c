#include <stdio.h>
#include <limits.h>
#include <string.h>

void hex2(unsigned int x) {
    printf("hex2: %x\n", x);
    do {
        char c = "0123456789abcdef" [x & 0xf];
        printf("char %c for %d\n", c, x);
        x >>= 4; 
        printf("char %c for %d\n", c, x);
    } while ( x );
    printf("what!");
};

void hex(int x) {
    int i = 8 * sizeof(x) - 4;

    printf("hex: %x\n", x);
    for (; i >= 0; i -= 4) {
        int y = (x >> i) & 0xf;
        char c = "0123456789abcdef" [y];
        printf("char %d: %c\n", i, c);
    }
    hex2(x);
};


char* uint_hex (unsigned int x, char* buf) {
    // write hex representation in reverse order
    char* end = buf;
    do {
        *end++ = "0123456789abcdef" [x & 0xf];
        x >>= 4; 
    } while ( x );
    // store pointer to next free byte
    char* result = end;
    // invert written digits
    end--;
    char* start = buf; 
    while(start < end) {
        char c = *end;
        *end-- = *start;
        *start++ = c;
    }
    return result;
};

char* int_hex (int x, char* buf) {
    if (x == INT_MIN) {
        strcpy(buf, "-80000000");
        return buf + 10;
    } else if (x < 0) {
        *buf++ = '-';
        return uint_hex((unsigned int)(-x), buf);
    } else 
        return uint_hex((unsigned int)x, buf);
};

void test_int_hex(int x) {
    char buf[20];

    *int_hex(x, buf) = '\0';
    printf("test_int_hex: %x --> '%s'\n", x, buf);
}

int main() {

  test_int_hex(0xff);

  test_int_hex(0xdeadbeef);

  printf("hello world");
 
  return 0;
}
