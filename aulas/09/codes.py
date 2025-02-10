import sys

sys.setrecursionlimit(500000)


def py_fact_r(n):
    if n == 0:
        return 1
    else:
        return n * py_fact_r(n - 1)

def py_fact_i(n):
    result = 1
    for i in range(1, n+1):
        result *= i
    return result