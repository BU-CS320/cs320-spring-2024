def f():
    return x

def g(x):
    y = f()
    out = x + y
    return out

def h(x, y):
    return g(x + y)

x = 1
x = h(x, x)