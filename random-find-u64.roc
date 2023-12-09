# Ported from https://github.com/martinus/map_benchmark/blob/master/src/benchmarks/RandomFind.cpp
app "random-find-u64"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        cli.Arg,
        cli.Stdout,
        cli.Task,
        Sfc64,
    ]
    provides [main] to cli

main =
    args <- Arg.list |> Task.await
    inserts =
        args
        |> List.get 1
        |> unwrap
        |> Str.toU32
        |> unwrap
    findsPerInsert =
        args
        |> List.get 2
        |> unwrap
        |> Str.toU32
        |> unwrap
    _ <- randomFind inserts findsPerInsert |> Task.await
    Stdout.line "done"

randomFind = \inserts, findsPerInsert ->
    lower = 0x_0000_0000_FFFF_FFFFu64
    upper = 0xFFFF_FFFF_0000_0000u64
    _ <- randomFindInternal 4 lower inserts findsPerInsert |> Task.await
    _ <- randomFindInternal 4 upper inserts findsPerInsert |> Task.await

    _ <- randomFindInternal 3 lower inserts findsPerInsert |> Task.await
    _ <- randomFindInternal 3 upper inserts findsPerInsert |> Task.await
    
    _ <- randomFindInternal 2 lower inserts findsPerInsert |> Task.await
    _ <- randomFindInternal 2 upper inserts findsPerInsert |> Task.await

    _ <- randomFindInternal 1 lower inserts findsPerInsert |> Task.await
    _ <- randomFindInternal 1 upper inserts findsPerInsert |> Task.await

    _ <- randomFindInternal 0 lower inserts findsPerInsert |> Task.await
    _ <- randomFindInternal 0 upper inserts findsPerInsert |> Task.await
    Task.ok {}

randomFindInternal = \numRandom, bitMask, inserts, findsPerInsert ->
    numTotal = 4
    numSeq = numTotal - numRandom

    findsPerIter = numTotal * findsPerInsert
    _ <- Stdout.line "\(numSeq * 100 // numTotal |> Num.toStr)% success, \(Inspect.toStr bitMask)" |> Task.await

    insertRandom =
        List.range {start: At 0, end: Length numTotal }
        |> List.map \i -> i < numRandom
   
    base = Sfc64.new 123
    unrelated = Sfc64.new 987654321
    findInitState = unrelated
    numFound =
        randomFindInternalHelper
            (Dict.empty {})
            base
            unrelated
            findInitState
            findInitState
            insertRandom
            bitMask
            0
            0
            0
            inserts
            findsPerIter
    _ <- Stdout.line "\(numFound |> Num.toStr) found" |> Task.await
    Task.ok {}

randomFindInternalHelper = \d0, baseS0, unrelatedS0, findS0, findInitState, insertRandom, bitMask, i0, findCount0, numFound0, inserts, findsPerIter ->
    # insert NumTotal entries: some random, some sequential
    (baseS1, list) = Sfc64.slightlyBiasedShuffle insertRandom baseS0
    (d2, baseS4, unrelatedS3) =
        List.walk list (d0, baseS1, unrelatedS0) \(d1, baseS2, unrelatedS1), isRandomInsert ->
            (unrelatedS2, unrelatedVal) = Sfc64.gen unrelatedS1
            if isRandomInsert then
                (baseS3, value) = Sfc64.gen baseS2
                masked = Num.bitwiseAnd value bitMask
                (Dict.insert d1 masked 1u64, baseS3, unrelatedS2)
            else
                masked = Num.bitwiseAnd unrelatedVal bitMask
                (Dict.insert d1 masked 1u64, baseS2, unrelatedS2)

    i1 =  i0 + (Num.toU32 (List.len insertRandom))
    
    (findS1, findCount1, numFound1) = findHelper d2 findS0 0 findCount0 numFound0 bitMask i1 findsPerIter findInitState
            
    if i1 < inserts then
        randomFindInternalHelper d2 baseS4 unrelatedS3 findS1 findInitState insertRandom bitMask i1 findCount1 numFound1 inserts findsPerIter
    else
        numFound1


# the actual benchmark code which sohould be as fast as possible
findHelper = \dict, findS0, j, findCount0, numFound0, bitMask, i, findsPerIter, findInitState ->
    if j < findsPerIter then
        findCount1 = findCount0 + 1
        (findCount2, findS1) =
            if findCount1 > i then
                (0, findInitState)
            else
                (findCount1, findS0)
        (findS2, value) = Sfc64.gen findS1
        masked = Num.bitwiseAnd value bitMask
        numFound1 =
            when Dict.get dict masked is
                Ok v -> numFound0 + v
                Err _ -> numFound0
        findHelper dict findS2 (j + 1) findCount2 numFound1 bitMask i findsPerIter findInitState
    else
        (findS0, findCount0, numFound0)

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            Inspect.toStr res
            |> crash
