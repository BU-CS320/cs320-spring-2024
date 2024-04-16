def factorial(n):
    if n == 0:
        return 1
    rec_call = factorial(n - 1)
    out = n * rec_call
    return out

fact_10 = factorial(10)