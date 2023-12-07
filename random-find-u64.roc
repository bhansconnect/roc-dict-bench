# Ported from https://github.com/martinus/map_benchmark/blob/master/src/benchmarks/RandomFind.cpp
app "random-find-u64"
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
    (inserts, findsPerInsert) =
        when in is
            Input line ->
                {before, after} =
                    line
                    |> Str.trim
                    |> Str.splitFirst " "
                    |> unwrap
                (
                    before
                    |> Str.trim
                    |> Str.toU32
                    |> unwrap,
                    after
                    |> Str.trim
                    |> Str.toU32
                    |> unwrap,
                )
            End ->
                crash "bad input: \(Inspect.toStr in)"
    _ <- randomFind inserts findsPerInsert |> Task.await
    Stdout.line "done"

randomFind = \inserts, findsPerInsert ->
    _ <- randomFindInternal 4 Lower inserts findsPerInsert |> Task.await
    _ <- randomFindInternal 4 Upper inserts findsPerInsert |> Task.await

    _ <- randomFindInternal 3 Lower inserts findsPerInsert |> Task.await
    _ <- randomFindInternal 3 Upper inserts findsPerInsert |> Task.await
    
    _ <- randomFindInternal 2 Lower inserts findsPerInsert |> Task.await
    _ <- randomFindInternal 2 Upper inserts findsPerInsert |> Task.await

    _ <- randomFindInternal 1 Lower inserts findsPerInsert |> Task.await
    _ <- randomFindInternal 1 Upper inserts findsPerInsert |> Task.await

    _ <- randomFindInternal 0 Lower inserts findsPerInsert |> Task.await
    _ <- randomFindInternal 0 Upper inserts findsPerInsert |> Task.await
    Task.ok {}

randomFindInternal = \numRandom, bitLoc, inserts, findsPerInsert ->
    numTotal = 4
    numSeq = numTotal - numRandom

    findsPerIter = numTotal * findsPerInsert
    _ <- Stdout.line "\(numSeq * 100 // numTotal |> Num.toStr)% success, \(Inspect.toStr bitLoc)" |> Task.await

    insertRandom =
        List.range {start: At 0, end: Length numTotal }
        |> List.map \i -> i < numRandom
   
    base = Random.seed32 123
    unrelated = Random.seed32 987654321
    findInitState = unrelated
    numFound =
        randomFindInternalHelper
            (Dict.empty {})
            base
            unrelated
            findInitState
            findInitState
            insertRandom
            bitLoc
            0
            0
            0
            inserts
            findsPerIter
    _ <- Stdout.line "\(numFound |> Num.toStr) found" |> Task.await
    Task.ok {}

randomFindInternalHelper = \d0, baseS0, unrelatedS0, findS0, findInitState, insertRandom, bitLoc, i0, findCount0, numFound0, inserts, findsPerIter ->
    # insert NumTotal entries: some random, some sequential
    {state: baseS1, list} = Random.slightlyBiasedShuffle insertRandom baseS0
    (d2, baseS4, unrelatedS3) =
        List.walk list (d0, baseS1, unrelatedS0) \(d1, baseS2, unrelatedS1), isRandomInsert ->
            fullRange = Random.u32 0 Num.maxU32
            if isRandomInsert then
                {state: baseS3, value} = fullRange baseS2
                shifted = applyBitShift value bitLoc
                (Dict.insert d1 shifted 1u64, baseS3, unrelatedS1)
            else
                {state: unrelatedS2, value} = fullRange unrelatedS1
                shifted = applyBitShift value bitLoc
                (Dict.insert d1 shifted 1u64, baseS2, unrelatedS2)

    i1 =  i0 + (Num.toU32 (List.len insertRandom))
    
    (findS1, findCount1, numFound1) = findHelper d2 findS0 0 findCount0 numFound0 bitLoc i1 findsPerIter findInitState
            
    if i1 < inserts then
        randomFindInternalHelper d2 baseS4 unrelatedS3 findS1 findInitState insertRandom bitLoc i1 findCount1 numFound1 inserts findsPerIter
    else
        numFound1


# the actual benchmark code which sohould be as fast as possible
findHelper = \dict, findS0, j, findCount0, numFound0, bitLoc, i, findsPerIter, findInitState ->
    if j < findsPerIter then
        findCount1 = findCount0 + 1
        (findCount2, findS1) =
            if findCount1 > i then
                (0, findInitState)
            else
                (findCount1, findS0)
        fullRange = Random.u32 0 Num.maxU32
        {state: findS2, value} = fullRange findS1
        shifted = applyBitShift value bitLoc
        numFound1 =
            when Dict.get dict shifted is
                Ok v -> numFound0 + v
                Err _ -> numFound0
        findHelper dict findS2 (j + 1) findCount2 numFound1 bitLoc i findsPerIter findInitState
    else
        (findS0, findCount0, numFound0)


applyBitShift = \val, bitLoc ->
    when bitLoc is
        Upper ->
            val
            |> Num.toU64
            |> Num.shiftLeftBy 32
        Lower ->
            val
            |> Num.toU64
    
unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            Inspect.toStr res
            |> crash
