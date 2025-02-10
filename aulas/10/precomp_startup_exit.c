#include <stdio.h>
#include <stdlib.h>

void func1();
void func2();

void __attribute__((constructor)) func1();
void __attribute__((destructor)) func2();

void func1() {
    printf("Dentro da func1()\n");
}

void func2() {
    printf("Dentro da func2()\n");
}

int main() {
    // atexit(exitFunction);

    printf("Dentro do main()\n");
    return 0;
}

void exitFunction() {
    printf("Código no final da execução.\n");
}