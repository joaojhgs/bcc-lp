#include<stdio.h>

#pragma GCC poison printf
int main() {
    int a = 21;
    if(a == 21)
        printf("função não será reconhecida");
    else
        printf("Algum erro...");
    return 0;
}