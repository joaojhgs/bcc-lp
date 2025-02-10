#include<stdio.h>

// Pragmas eclusivos do Microsoft Visual C++:

// #pragma warn +xxx (para mostrar o aviso)
// #pragma warn -xxx (para ocultar o aviso)
// #pragma warn .xxx (para alternar entre ocultar e mostrar)

// #pragma warn -rvl // valor de retorno
// #pragma warn -par // parâmetro não utilizado
// #pragma warn -rch // código não atingido

// Pragmas do GCC:

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreturn-type"
#pragma GCC diagnostic ignored "-Wunused-variable"

int show(int x) {
    // parameter x nunca será utilizado

    printf("Olá pessoal de BCC!\n");

    // função não possui a expressão de retorno
}


int main() {
    int temp = show(10);
    // printf("%d\n", temp);
    return 0;
}

#pragma GCC diagnostic pop