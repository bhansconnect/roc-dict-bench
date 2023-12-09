package main

import (
    "fmt"
    "strconv"
    "os"

    "sfc64"
)

func main() {
    size, err := strconv.Atoi(os.Args[1])
    if err != nil {
        panic(err)
    }
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

