module Validation exposing
    ( ValidationError(..)
    , append, errorToList
    , errorMap
    , Validation(..)
    , success, failure, failureWithList, validation, toResult, toList
    , map, mapError, mapValidationError
    , andMap, andMapAcc, andSkip, andSkipAcc
    )

{-| This library aims to provide basic `Validation` in Elm


# ValidationError

@docs ValidationError


### Common Helpers

@docs append, errorToList


### Map

@docs errorMap


# Validation

@docs Validation


### Common Helpers

@docs success, failure, failureWithList, validation, toResult, toList


### Map

@docs map, mapError, mapValidationError


### Chaining

@docs andMap, andMapAcc, andSkip, andSkipAcc

-}


{-| A `ValidationError` can be a simple `Error` or an `ErrorList`

    type YourError
        = ErrorX
        | ErrorY String Int

    Error ErrorX

    Error (ErrorY "some message" 15)

    ErrorList [ ErrorX, ErrorY "some message" 15 ]

-}
type ValidationError err
    = Error err
    | ErrorList (List err)



-- Common Helpers


{-| Combines two `ValidationError`

    append (Error 1) (Error 2)
        |> Expect.equal (ErrorList [ 1, 2 ])

    append (Error 1) (ErrorList [ 2, 3 ])
        |> Expect.equal (ErrorList [ 1, 2, 3 ])

    append (ErrorList [ 2, 3 ]) (Error 1)
        |> Expect.equal (ErrorList [ 1, 2, 3 ])

    append (ErrorList [ 1, 2 ]) (ErrorList [ 3, 4 ])
        |> Expect.equal (ErrorList [ 1, 2, 3, 4 ])

-}
append : ValidationError err -> ValidationError err -> ValidationError err
append ve1 ve2 =
    case ( ve1, ve2 ) of
        ( Error err1, Error err2 ) ->
            ErrorList [ err1, err2 ]

        ( Error err, ErrorList l ) ->
            ErrorList (err :: l)

        ( ErrorList l, Error err ) ->
            ErrorList (err :: l)

        ( ErrorList l1, ErrorList l2 ) ->
            ErrorList (l1 ++ l2)


{-| Converts a `ValidationError` into a `List err`

    errorToList (Error 1)
        |> Expect.equal [ 1 ]

    errorToList (ErrorList [ 1, 2, 3 ])
        |> Expect.equal [ 1, 2, 3 ]

-}
errorToList : ValidationError err -> List err
errorToList ve =
    case ve of
        Error e ->
            [ e ]

        ErrorList l ->
            l



-- Map


{-| Map a function over a `ValidationError`

    errorMap ((+) 1) (Error 1)
        |> Expect.equal (Error 2)

    errorMap ((+) 1) (ErrorList [ 1, 2, 3 ])
        |> Expect.equal (ErrorList [ 2, 3, 4 ])

-}
errorMap : (err1 -> err2) -> ValidationError err1 -> ValidationError err2
errorMap f ve =
    case ve of
        Error err1 ->
            Error (f err1)

        ErrorList l ->
            ErrorList (List.map f l)


{-| A `Validation` can be a `Success` or a `Failure`

    Success "output value"

    Failure (Error "not valid")

    Failure (ErrorList [ "too short", "forbidden character" ])

-}
type Validation err a
    = Failure (ValidationError err)
    | Success a



-- Common Helpers


{-| Returns a successful `Validation`

    success "output value"
        |> Expect.equal (Success "output value")

-}
success : a -> Validation err a
success =
    Success


{-| Returns a failed `Validation`

    failure "not valid"
        |> Expect.equal (Failure (Error "not valid"))

-}
failure : err -> Validation err a
failure err =
    Failure (Error err)


{-| Returns a failed `Validation` with an `ErrorList`

    failureWithList [ "error1", "error2" ]
        |> Expect.equal (Failure (ErrorList [ "error1", "error2" ]))

-}
failureWithList : List err -> Validation err a
failureWithList l =
    Failure (ErrorList l)


{-| Helps creating a basic `Validation` function

    type YourError
        = ErrorX

    emptyValidation : String -> Validation YourError String
    emptyValidation =
        validation ErrorX String.isEmpty

    emptyValidation ""
        |> Expect.equal (Success "")

    emptyValidation "notempty"
        |> Expect.equal (Failure (Error ErrorX ))

-}
validation : err -> (a -> Bool) -> a -> Validation err a
validation err valid a =
    if valid a then
        Success a

    else
        Failure (Error err)


{-| Converts a `Validation` to a `Result`

    toResult (Success "valid")
        |> Expect.equal (Ok "valid")

    toResult (Failure (Error "error"))
        |> Expect.equal (Err [ "error" ])

-}
toResult : Validation err a -> Result (List err) a
toResult va =
    case va of
        Success a ->
            Ok a

        Failure ve ->
            Err (errorToList ve)


{-| Converts a `Validation` to a `List`. The `List` is empty if it's a
successful validation or contains the errors if it's failed validation.

    toList (Success "valid")
        |> Expect.equal []

    toList (Failure (Error "error"))
        |> Expect.equal [ "error" ]

-}
toList : Validation err a -> List err
toList va =
    case va of
        Success a ->
            []

        Failure ve ->
            errorToList ve



-- Map


{-| Map a function over a successful `Validation`

    map ((+) 1) (Success 1)
        |> Expect.equal (Success 2)

    map ((+) 1) (Error 1)
        |> Expect.equal (Error 1)

-}
map : (a -> b) -> Validation err a -> Validation err b
map f va =
    case va of
        Success a ->
            Success (f a)

        Failure ve ->
            Failure ve


{-| Map a function over a failed `Validation`

    mapError ((+) 1) (Success 1)
        |> Expect.equal (Success 1)

    mapError ((+) 1) (Failure (Error 1))
        |> Expect.equal (Failure (Error 2))

    mapError ((+) 1) (Failure (ErrorList [ 1, 2 ]))
        |> Expect.equal (Failure (ErrorList [ 2, 3 ]))

-}
mapError : (err1 -> err2) -> Validation err1 a -> Validation err2 a
mapError f va =
    mapValidationError (errorMap f) va


{-| Map a function over a failed `Validation` on the `ValidationError`

    tranform : ValidationError Int -> ValidationError Int
    tranform v =
        case v of
            Error n ->
                ErrorList (List.repeat 5 n)

            ErrorList l ->
                ErrorList l

    mapValidationError tranform (Success 1)
        |> Expect.equal (Success 1)

    mapValidationError tranform (Failure (Error 1))
        |> Expect.equal (Failure (ErrorList [ 1, 1, 1, 1, 1 ]))

-}
mapValidationError : (ValidationError err1 -> ValidationError err2) -> Validation err1 a -> Validation err2 a
mapValidationError f va =
    case va of
        Success a ->
            Success a

        Failure ve ->
            Failure (f ve)



-- Chaining


{-| Chain together many `Validation` by binding them. This means we only
continue if the validations are successful and we stop at the first `Failure`

    type alias Example =
        { a : String
        , b : String
        }

    success Example
        |> andMap (success "valid1")
        |> andMap (success "valid2")
        |> Expect.equal (Success (Example "valid1" "valid2"))

    success Example
        |> andMap (failure "error1")
        |> andMap (success "valid2")
        |> Expect.equal (failure "error1")

    success Example
        |> andMap (success "valid1")
        |> andMap (failure "error2")
        |> Expect.equal (failure "error2")

    success Example
        |> andMap (failure "error1")
        |> andMap (failure "error2")
        |> Expect.equal (failure "error1")

-}
andMap : Validation err a -> Validation err (a -> b) -> Validation err b
andMap va vf =
    case ( va, vf ) of
        ( _, Failure ve ) ->
            Failure ve

        ( _, Success f ) ->
            map f va


{-| Chain together many `Validation` by accumulating them. This means we always
continue and accumulate all the `Failure`s

    type alias Example =
        { a : String
        , b : String
        }

    success Example
        |> andMapAcc (success "valid1")
        |> andMapAcc (success "valid2")
        |> Expect.equal (Success (Example "valid1" "valid2"))

    success Example
        |> andMapAcc (failure "error1")
        |> andMapAcc (success "valid2")
        |> Expect.equal (failure "error1")

    success Example
        |> andMapAcc (success "valid1")
        |> andMapAcc (failure "error2")
        |> Expect.equal (failure "error2")

    success Example
        |> andMapAcc (failure "error1")
        |> andMapAcc (failure "error2")
        |> Expect.equal (failureWithList [ "error2", "error1" ])

-}
andMapAcc : Validation err a -> Validation err (a -> b) -> Validation err b
andMapAcc va vf =
    case ( va, vf ) of
        ( Success _, Failure ve ) ->
            Failure ve

        ( Failure ve1, Failure ve2 ) ->
            Failure (append ve1 ve2)

        ( _, Success f ) ->
            map f va


{-| Chain together many `Validation` by binding them and skipping the current
result. Same behavior than `andMap` but skip the current result

    type alias Example =
        { a : String
        , b : String
        }

    success Example
        |> andMap (success "valid1")
        |> andSkip (success "whatever")
        |> andMap (success "valid2")
        |> Expect.equal (success (Example "valid1" "valid2"))

    success Example
        |> andMap (failure "error1")
        |> andSkip (success "")
        |> andMap (success "")
        |> Expect.equal (failure "error1")

    success Example
        |> andMap (success "")
        |> andSkip (failure "error2")
        |> andMap (success "")
        |> Expect.equal (failure "error2")

    success Example
        |> andMap (failure "error1")
        |> andSkip (failure "error2")
        |> andMap (success "")
        |> Expect.equal (failure "error1")

-}
andSkip : Validation err a -> Validation err b -> Validation err b
andSkip va vf =
    case ( va, vf ) of
        ( _, Failure ve ) ->
            Failure ve

        ( Failure ve, Success _ ) ->
            Failure ve

        ( Success _, Success _ ) ->
            vf


{-| Chain together many `Validation` by accumulating them and skipping the
current result. Same behavior than `andMapAcc` but skip the current result

    success Example
        |> andMapAcc (success "valid1")
        |> andSkipAcc (success "whatever")
        |> andMapAcc (success "valid2")
        |> Expect.equal (success (Example "valid1" "valid2"))

    success Example
        |> andMapAcc (failure "error1")
        |> andSkipAcc (success "")
        |> andMapAcc (success "")
        |> Expect.equal (failure "error1")

    success Example
        |> andMapAcc (success "")
        |> andSkipAcc (failure "error2")
        |> andMapAcc (success "")
        |> Expect.equal (failure "error2")

    success Example
        |> andMapAcc (failure "error1")
        |> andSkipAcc (failure "error2")
        |> andMapAcc (success "")
        |> Expect.equal (failureWithList [ "error2", "error1" ])

-}
andSkipAcc : Validation err a -> Validation err b -> Validation err b
andSkipAcc va vf =
    case ( va, vf ) of
        ( Success _, Failure ve ) ->
            Failure ve

        ( Failure ve1, Failure ve2 ) ->
            Failure (append ve1 ve2)

        ( Failure ve, Success _ ) ->
            Failure ve

        ( Success _, Success _ ) ->
            vf
