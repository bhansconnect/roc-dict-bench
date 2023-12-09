package main

import (
    "fmt"

    "sfc64"
)

func main() {
    var size int
    fmt.Scanf("%d", &size)
    m := map[int32]int32{}
    rng := sfc64.New(213)
    for i:=0; i<size; i++ {
        m[int32(rng.Gen())] = 0;
    }
    fmt.Println(len(m))
    m = make(map[int32]int32)
    fmt.Println(len(m))
    state := rng
    for i:=0; i<size; i++ {
        m[int32(rng.Gen())] = 0;
    }
    fmt.Println(len(m))
    rng = state
    for i:=0; i<size; i++ {
        delete(m, int32(rng.Gen()));
    }
}

