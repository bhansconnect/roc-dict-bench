# Ported from https://github.com/martinus/map_benchmark/blob/master/src/benchmarks/RandomFind.cpp
app "random-find-u64"
    packages {
        # cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        cli: "../roc-basic-cli/src/main.roc"
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

randomFindInternalHelper = \dict, baseRng, unrelatedRng, findRng, findInitState, insertRandom, bitMask, i, findCount, numFound, inserts, findsPerIter ->
    # insert NumTotal entries: some random, some sequential
    (baseRng, list) = Sfc64.slightlyBiasedShuffle insertRandom baseRng
    (dict, baseRng, unrelatedRng) =
        List.walk list (dict, baseRng, unrelatedRng) \(dict, baseRng, unrelatedRng), isRandomInsert ->
            (unrelatedRng, unrelatedVal) = Sfc64.gen unrelatedRng
            if isRandomInsert then
                (baseRng, value) = Sfc64.gen baseRng
                masked = Num.bitwiseAnd value bitMask
                (Dict.insert dict masked 1u64, baseRng, unrelatedRng)
            else
                masked = Num.bitwiseAnd unrelatedVal bitMask
                (Dict.insert dict masked 1u64, baseRng, unrelatedRng)

    i =  i + (Num.toU32 (List.len insertRandom))
    
    (findRng, findCount, numFound) = findHelper dict findRng 0 findCount numFound bitMask i findsPerIter findInitState
            
    if i < inserts then
        randomFindInternalHelper dict baseRng unrelatedRng findRng findInitState insertRandom bitMask i findCount numFound inserts findsPerIter
    else
        numFound


# the actual benchmark code which sohould be as fast as possible
findHelper = \dict, findRng, j, findCount, numFound, bitMask, i, findsPerIter, findInitState ->
    if j < findsPerIter then
        findCount = findCount + 1
        (findCount, findRng) =
            if findCount > i then
                (0, findInitState)
            else
                (findCount, findRng)
        (findRng, value) = Sfc64.gen findRng
        masked = Num.bitwiseAnd value bitMask
        numFound =
            when Dict.get dict masked is
                Ok v -> numFound + v
                Err _ -> numFound
        findHelper dict findRng (j + 1) findCount numFound bitMask i findsPerIter findInitState
    else
        (findRng, findCount, numFound)

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            Inspect.toStr res
            |> crash
