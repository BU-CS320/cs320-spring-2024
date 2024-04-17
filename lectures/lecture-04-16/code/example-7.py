def f():
    x = 0
    def g():
        nonlocal x
        x += 1
        return x
    def g():
        nonlocal x
        x += 1
        return x
    return (g, h)


g = f()
print(g())
print(g())