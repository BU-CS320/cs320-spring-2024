def f():
    def g():
        return x
    x = 1
    y = g()
    x = 2
    z = g()
    return y == z

out = f()