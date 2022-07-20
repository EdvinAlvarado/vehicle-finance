from typing import Callable, Dict, List
import time

def investment(contributions: List[float], rate: float) -> float:
    ret = contributions[0]
    for i in range(1, len(contributions)):
        ret = ret*(1+rate) + contributions[i]
    return ret

def discount(l: List[float], rate: float) -> List[float]:
    return [n/((1+rate)**i) for i,n in enumerate(l)]

def net_present_value(rate: float, l: List[float]) -> float:
    return sum(discount(l, rate))

def iterative_solution(func: Callable, init_val: float, step: float, *args) -> float:
    init = init_val
    ret = func(init, *args)
    while abs(ret) > 10:
        # print(init, ret)
        if ret < 0:
            init -= step
        elif ret > 0:
            init += step
        ret = func(init, *args)
    return init

def roroi(l: List[float]) -> float:
    return iterative_solution(net_present_value, 0., 0.01/100, l)


if __name__ == "__main__":
    l = [-1000.] + [1000.]*10
    # print(net_present_value(l,0.07))
    n = roroi(l)
