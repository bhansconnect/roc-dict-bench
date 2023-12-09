interface Sfc64
    exposes [
        new,
        gen,
        slightlyBiasedShuffle,
        boundedSlightlyBiased,
    ]
    imports []

Sfc64 := {
    a: U64,
    b: U64,
    c: U64,
    counter: U64,
}

new = \seed ->
    @Sfc64 {a: seed, b: seed, c: seed, counter: 1}
    |> repGen 12

repGen = \state, remaining ->
    if remaining > 0 then
        state
        |> gen
        |> .0
        |> repGen (remaining - 1)
    else
        state

gen = \@Sfc64 state ->
    tmp =
        state.a
        |> Num.addWrap state.b
        |> Num.addWrap state.counter
    (@Sfc64 {
        a :
            state.b 
            |> Num.shiftRightZfBy 11
            |> Num.bitwiseXor state.b,
        b :
            state.c 
            |> Num.shiftLeftBy 3
            |> Num.addWrap state.c,
        c :
            state.c 
            |> Num.shiftLeftBy 24
            |> Num.bitwiseOr
                (Num.shiftRightZfBy state.c 40)
            |> Num.addWrap tmp,
        counter : Num.addWrap state.counter 1,
    }, tmp)

slightlyBiasedShuffle = \l0, s0 ->
    helper = \l1, s1, i ->
        if i > 0 then
            (s2, value) =
                boundedSlightlyBiased s1 (Num.toU64 (i + 1))
            List.swap l1 i (Num.toNat value)
            |> helper s2 (i - 1)
        else
            (s1, l1)

    helper l0 s0 (List.len l0 - 1)

boundedSlightlyBiased = \s0, range ->
    (s1, r32) = gen s0
    rand =
        r32 
        |> Num.toU32
        |> Num.toU64 
        |> Num.mulWrap range
        |> Num.shiftRightZfBy 32
        |> Num.toU32
    (s1, rand)
