x = 3
def f():
    y = 4
    def g():
        x = 5
        def h():
            a = x
            b = y
            return a + b
        return h()
    a = g()
    b = x
    return a + b

f()