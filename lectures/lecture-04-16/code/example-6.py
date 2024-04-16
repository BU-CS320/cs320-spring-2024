x = 0
def f():
    x = 1
    def g():
        x = 2
        def h():
            return x
        y = h()
        return x
    y = g()
    return x

y = f()