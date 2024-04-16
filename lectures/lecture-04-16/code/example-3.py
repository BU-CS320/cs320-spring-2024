def f():
    x = 1
    def g():
        return x
    return g

h = f()

def a():
    x = 2
    out = h()
    return out

a()