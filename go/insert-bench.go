package main

import (
    "fmt"
)

func main() {
    var size int
    fmt.Scanf("%d", &size)
    m := map[int32]int32{}
    rng := new(213)
    for i:=0; i<size; i++ {
        m[int32(rng.operate())] = 0;
    }
    fmt.Println(len(m))
    m = make(map[int32]int32)
    fmt.Println(len(m))
    state := rng
    for i:=0; i<size; i++ {
        m[int32(rng.operate())] = 0;
    }
    fmt.Println(len(m))
    rng = state
    for i:=0; i<size; i++ {
        delete(m, int32(rng.operate()));
    }
}

type sfc64 struct {
    a uint64
    b uint64
    c uint64
    counter uint64
}

func new(seed uint64) sfc64 {
    x := sfc64{a: seed, b: seed, c:seed, counter:1}
    for i := 0; i < 12; i++ {
        _ = x.operate()
    }
    return x
}

func (state *sfc64) operate() uint64 {
    tmp := state.a + state.b + state.counter
    state.counter += 1
    state.a = state.b ^ (state.b >> 11)
    state.b = state.c + (state.c << 3)
    state.c = ((state.c << 24) | (state.c >> 40)) + tmp
    return tmp
}
