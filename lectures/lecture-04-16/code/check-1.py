def f(x):
    if x == 0:
        return 1
    return f(x // 4) + f(x // 2)

f(16)