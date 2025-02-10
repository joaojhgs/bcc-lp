#include <stdio.h>

int main() {
    int a = 10;
    int b = 20;
    int result;

    asm("addl %1, %2;"   // Adiciona o valor de a em b
        "movl %2, %0;"   // Move o resultado para a variável result
        : "=r" (result)  // Output
        : "r" (a), "r" (b)  // Inputs
    );

    printf("O resultado é: %d\n", result);

    return 0;
}
