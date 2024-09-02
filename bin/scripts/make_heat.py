#!/usr/bin/python3

import multiprocessing
from multiprocessing import Process


    
def make_heat() -> None:
    while True:
        pass


def main() -> None:
    thread_count: int = multiprocessing.cpu_count()
    heat_makers: list[Process] = [Process(target=make_heat) for _ in range(thread_count)]

    for heat_maker in heat_makers:
        heat_maker.start()

    for heat_maker in heat_makers:
        heat_maker.join()


main()
