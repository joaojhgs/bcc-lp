#include<stdio.h>

// Pragmas do GCC

#pragma GCC diagnostic ignored "-Wunused-variable"
int unusedVariableGCC;

#pragma GCC optimize("O3")
void myFunction() {
    // código da função
}

// Pragmas do Clang

#pragma clang diagnostic ignored "-Wunused-variable"
int unusedVariableClang;

int main() {
    #pragma clang loop vectorize(disable)
    for (int i = 0; i < 10; ++i) {
        // corpo do loop
    }
    return 0;
}