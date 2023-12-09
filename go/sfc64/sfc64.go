package sfc64

type Sfc64 struct {
    a uint64
    b uint64
    c uint64
    counter uint64
}

func New(seed uint64) Sfc64 {
    x := Sfc64{a: seed, b: seed, c:seed, counter:1}
    for i := 0; i < 12; i++ {
        _ = x.Gen()
    }
    return x
}

func (state *Sfc64) Gen() uint64 {
    tmp := state.a + state.b + state.counter
    state.counter += 1
    state.a = state.b ^ (state.b >> 11)
    state.b = state.c + (state.c << 3)
    state.c = ((state.c << 24) | (state.c >> 40)) + tmp
    return tmp
}
