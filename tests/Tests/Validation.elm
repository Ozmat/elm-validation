module Tests.Validation exposing (..)

import Expect
import Test exposing (..)
import Fuzz exposing (string)
import Validation exposing (..)


all : Test
all =
    describe "Validation tests"
        [ describe "Validation.append"
            [ test "combines two ValidationError (Error + Error)" <|
                \_ ->
                    append (Error 1) (Error 2)
                        |> Expect.equal (ErrorList [ 1, 2 ])
            , test "combines two ValidationError (Error + ErrorList)" <|
                \_ ->
                    append (Error 1) (ErrorList [ 2, 3 ])
                        |> Expect.equal (ErrorList [ 1, 2, 3 ])
            , test "combines two ValidationError (ErrorList + Error)" <|
                \_ ->
                    append (ErrorList [ 2, 3 ]) (Error 1)
                        |> Expect.equal (ErrorList [ 1, 2, 3 ])
            , test "combines two ValidationError (ErrorList + ErrorList)" <|
                \_ ->
                    append (ErrorList [ 1, 2 ]) (ErrorList [ 3, 4 ])
                        |> Expect.equal (ErrorList [ 1, 2, 3, 4 ])
            ]
        , describe "Validation.errorToList"
            [ fuzz string "converts a ValidationError into a List of error (Error)" <|
                \s ->
                    errorToList (Error s)
                        |> Expect.equal [ s ]
            , fuzz string "converts a ValidationError into a List of error (ErrorList)" <|
                \s ->
                    errorToList (ErrorList [ s, s, s ])
                        |> Expect.equal [ s, s, s ]
            ]
        , describe "Validation.errorMap"
            [ test "map a function on a ValidationError (Error)" <|
                \s ->
                    errorMap increment (Error 1)
                        |> Expect.equal (Error 2)
            , test "map a function on a ValidationError (ErrorList)" <|
                \s ->
                    errorMap increment (ErrorList [ 1, 2, 3 ])
                        |> Expect.equal (ErrorList [ 2, 3, 4 ])
            ]
        , fuzz string "Validation.success returns a successful Validation" <|
            \s ->
                success s
                    |> Expect.equal (Success s)
        , fuzz string "Validation.failure returns a failed Validation" <|
            \s ->
                failure s
                    |> Expect.equal (Failure (Error s))
        , fuzz string "Validation.failureWithList returns a failed Validation with an ErrorList" <|
            \s ->
                failureWithList [ s, s ]
                    |> Expect.equal (Failure (ErrorList [ s, s ]))
        , describe "Validation.validation"
            [ test "helps creating a basic Validation function (Success)" <|
                \_ ->
                    validation "error" String.isEmpty ""
                        |> Expect.equal (success "")
            , test "helps creating a basic Validation function (Failure)" <|
                \_ ->
                    validation "error" String.isEmpty "eeee"
                        |> Expect.equal (failure "error")
            ]
        , describe "Validation.toResult"
            [ fuzz string "Converts a Validation to a Result (Success)" <|
                \s ->
                    toResult (success s)
                        |> Expect.equal (Ok s)
            , fuzz string "Converts a Validation to a Result (Failure)" <|
                \s ->
                    toResult (failure s)
                        |> Expect.equal (Err [ s ])
            ]
        , describe "Validation.toList"
            [ fuzz string "Converts a Validation to a List (Success)" <|
                \s ->
                    toList (success s)
                        |> Expect.equal ([])
            , fuzz string "Converts a Validation to a List (Failure)" <|
                \s ->
                    toList (failure s)
                        |> Expect.equal ([ s ])
            ]
        , describe "Validation.map"
            [ test "map a function if it's a successful validation" <|
                \_ ->
                    map increment (success 1)
                        |> Expect.equal (success 2)
            , test "has no effect otherwise" <|
                \_ ->
                    map increment (failure 1)
                        |> Expect.equal (failure 1)
            ]
        , describe "Validation.mapError"
            [ test "map a function if it's a failed validation (using errorMap)" <|
                \_ ->
                    mapError increment (failure 1)
                        |> Expect.equal (failure 2)
            , test "has no effect otherwise" <|
                \_ ->
                    mapError increment (success 1)
                        |> Expect.equal (success 1)
            ]
        , describe "Validation.mapValidationError"
            [ test "map a function, on the ValidationError, if it's a failed validation" <|
                \_ ->
                    mapValidationError tranform (failure 1)
                        |> Expect.equal (Failure (ErrorList [ 1, 1, 1, 1, 1 ]))
            , test "has no effect otherwise" <|
                \_ ->
                    mapValidationError tranform (success 1)
                        |> Expect.equal (success 1)
            ]
        , describe "Validation.andMap"
            [ fuzz string "helps binding validation (Success, Success)" <|
                \s ->
                    success Example
                        |> andMap (success s)
                        |> andMap (success s)
                        |> Expect.equal (Success (Example s s))
            , test "helps binding validation (Failure, Success)" <|
                \_ ->
                    success Example
                        |> andMap (failure "error1")
                        |> andMap (success "")
                        |> Expect.equal (failure "error1")
            , test "helps binding validation (Success, Failure)" <|
                \_ ->
                    success Example
                        |> andMap (success "")
                        |> andMap (failure "error2")
                        |> Expect.equal (failure "error2")
            , test "helps binding validation (Failure, Failure)" <|
                \_ ->
                    success Example
                        |> andMap (failure "error1")
                        |> andMap (failure "error2")
                        |> Expect.equal (failure "error1")
            ]
        , describe "Validation.andMapAcc"
            [ fuzz string "helps accumulating validation (Success, Success)" <|
                \s ->
                    success Example
                        |> andMapAcc (success s)
                        |> andMapAcc (success s)
                        |> Expect.equal (Success (Example s s))
            , test "helps binding validation (Failure, Success)" <|
                \_ ->
                    success Example
                        |> andMapAcc (failure "error1")
                        |> andMapAcc (success "")
                        |> Expect.equal (failure "error1")
            , test "helps binding validation (Success, Failure)" <|
                \_ ->
                    success Example
                        |> andMapAcc (success "")
                        |> andMapAcc (failure "error2")
                        |> Expect.equal (failure "error2")
            , test "helps binding validation (Failure, Failure)" <|
                \_ ->
                    success Example
                        |> andMapAcc (failure "error1")
                        |> andMapAcc (failure "error2")
                        |> Expect.equal (Failure (ErrorList [ "error2", "error1" ]))
            ]
        ]



-- Helpers


increment : Int -> Int
increment =
    (+) 1


tranform : ValidationError Int -> ValidationError Int
tranform v =
    case v of
        Error n ->
            ErrorList (List.repeat 5 n)

        ErrorList l ->
            ErrorList l


type alias Example =
    { a : String
    , b : String
    }
