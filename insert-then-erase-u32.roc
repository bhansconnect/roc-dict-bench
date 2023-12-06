# Ported from https://github.com/martinus/map_benchmark/blob/master/src/benchmarks/Insert.cpp
app "insert-erase-u32"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        cli.Stdin,
        cli.Stdout,
        cli.Task,
        Random,
    ]
    provides [main] to cli

main =
    in <- Stdin.line |> Task.await
    size =
        when in is
            Input line ->
                line
                |> Str.trim
                |> Str.toU32
                |> unwrap
            End ->
                0
    _ <- insertErase size |> Task.await
    Stdout.line "done"

insertErase = \size ->
    s0 = Random.seed32 213
   
    {dict: d0, rand: s1} = insertRandomElems (Dict.empty {}) size s0
    _ <- Dict.len d0 |> Num.toStr |> Stdout.line |> Task.await
    d1 = Dict.clear d0
    _ <- Dict.len d1 |> Num.toStr |> Stdout.line |> Task.await
    {dict: d2} = insertRandomElems d1 size s1
    _ <- Dict.len d2 |> Num.toStr |> Stdout.line |> Task.await
    # This intentionally reuses the state from the last insert
    {dict: d3} = removeRandomElems d2 size s1
    _ <- Dict.len d3 |> Num.toStr |> Stdout.line |> Task.await
    Task.ok d3


insertRandomElems = \dict, size, rand ->
    if size > 0 then
        fullRange = Random.u32 0 Num.maxU32
        {state: next, value} = fullRange rand
        insertRandomElems
            (Dict.insert dict value 0u32)
            (size - 1)
            next
    else
        {dict, rand}
    
removeRandomElems = \dict, size, rand ->
    if size > 0 then
        fullRange = Random.u32 0 Num.maxU32
        {state: next, value} = fullRange rand
        removeRandomElems
            (Dict.remove dict value)
            (size - 1)
            next
    else
        {dict, rand}
    
unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            Inspect.toStr res
            |> crash
