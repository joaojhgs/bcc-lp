unsigned int fibo(unsigned short int n) {
    if (n < 2)
        return n;
    return fibo(n - 1) + fibo(n - 2);
}

unsigned long long factorial(unsigned int n) {
    if (n == 0)
        return 1;
    else
        return n * factorial(n - 1);
}

