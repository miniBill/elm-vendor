module Main exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm.Constraint as Constraint
import Elm.Package as Package
import Elm.Project as Project
import Elm.Version as Version
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withCliOptions config task


type alias CliOptions =
    { nameOrPath : String

    -- , version : Maybe String
    }


config : Program.Config CliOptions
config =
    Program.config
        |> Program.add
            (OptionsParser.build
                (\nameOrPath ->
                    -- version
                    { nameOrPath = nameOrPath

                    -- , version = version
                    }
                )
                |> OptionsParser.with
                    (Option.requiredPositionalArg
                        -- "Name or path"
                        "path"
                    )
             -- |> OptionsParser.withOptionalPositionalArg (Option.optionalPositionalArg "Version")
            )


task : CliOptions -> BackendTask FatalError ()
task cliOptions =
    let
        targetName : String
        targetName =
            -- case cliOptions.version of
            case Nothing of
                Just version ->
                    cliOptions.nameOrPath ++ ", version " ++ version

                Nothing ->
                    cliOptions.nameOrPath
    in
    Do.log ("💭 Trying to vendor " ++ targetName) <| \_ ->
    Do.do
        (File.jsonFile Project.decoder "elm.json"
            |> BackendTask.allowFatal
            |> BackendTask.andThen
                (\project ->
                    case project of
                        Project.Package _ ->
                            BackendTask.fail <| FatalError.fromString "elm-vendor can only vendor inside applications"

                        Project.Application application ->
                            BackendTask.succeed application
                )
        )
    <| \application ->
    Do.do
        (Glob.succeed identity
            |> Glob.capture (Glob.literal <| cliOptions.nameOrPath ++ "/elm.json")
            |> Glob.expectUniqueMatch
            |> BackendTask.allowFatal
            |> BackendTask.andThen
                (\elmJsonPath ->
                    File.jsonFile Project.decoder elmJsonPath
                        |> BackendTask.allowFatal
                )
            |> BackendTask.andThen
                (\project ->
                    case project of
                        Project.Application _ ->
                            BackendTask.fail <| FatalError.fromString "elm-vendor can only vendor packages"

                        Project.Package package ->
                            BackendTask.succeed package
                )
        )
    <| \package ->
    let
        { satisfiedDirect, satisfiedIndirect, unsatisfied, conflicting } =
            package.deps
                |> tripartition
                    (checkDependency application)

        lengthString : List a -> String
        lengthString list =
            String.padLeft padLength ' ' <| String.fromInt (List.length list)

        padLength : Int
        padLength =
            [ satisfiedDirect, satisfiedIndirect, unsatisfied, conflicting ]
                |> List.map
                    (\list ->
                        list
                            |> List.length
                            |> String.fromInt
                            |> String.length
                    )
                |> List.maximum
                |> Maybe.withDefault 0
    in
    [ "The package needs " ++ lengthString package.deps ++ " dependencies, of which:"
    , " - " ++ lengthString satisfiedDirect ++ " are already satisfied,"
    , " - " ++ lengthString satisfiedIndirect ++ " are compatible with the indirect dependencies,"
    , " - " ++ lengthString unsatisfied ++ " are not satisfied,"
    , " - " ++ lengthString conflicting ++ " are incompatible with your current dependencies."
    ]
        |> String.join "\n"
        |> Script.log


checkDependency : Project.ApplicationInfo -> ( Package.Name, Constraint.Constraint ) -> DependencyKind
checkDependency { depsDirect, depsIndirect } ( name, constraint ) =
    let
        findIn : List ( Package.Name, Version.Version ) -> Maybe Version.Version
        findIn list =
            List.Extra.findMap
                (\( depName, depVersion ) ->
                    if name == depName then
                        Just depVersion

                    else
                        Nothing
                )
                list
    in
    case findIn depsDirect of
        Just version ->
            if Constraint.check version constraint then
                SatisfiedDirect

            else
                Conflicting

        Nothing ->
            case findIn depsIndirect of
                Just version ->
                    if Constraint.check version constraint then
                        SatisfiedIndirect

                    else
                        Conflicting

                Nothing ->
                    Unsatisfied


type DependencyKind
    = SatisfiedDirect
    | SatisfiedIndirect
    | Unsatisfied
    | Conflicting


tripartition :
    (a -> DependencyKind)
    -> List a
    ->
        { satisfiedDirect : List a
        , satisfiedIndirect : List a
        , unsatisfied : List a
        , conflicting : List a
        }
tripartition f xs =
    let
        go queue acc =
            case queue of
                [] ->
                    { satisfiedDirect = List.reverse acc.satisfiedDirect
                    , satisfiedIndirect = List.reverse acc.satisfiedIndirect
                    , unsatisfied = List.reverse acc.unsatisfied
                    , conflicting = List.reverse acc.conflicting
                    }

                head :: tail ->
                    case f head of
                        SatisfiedDirect ->
                            go tail { acc | satisfiedDirect = head :: acc.satisfiedDirect }

                        SatisfiedIndirect ->
                            go tail { acc | satisfiedIndirect = head :: acc.satisfiedIndirect }

                        Unsatisfied ->
                            go tail { acc | unsatisfied = head :: acc.unsatisfied }

                        Conflicting ->
                            go tail { acc | conflicting = head :: acc.conflicting }
    in
    go xs
        { satisfiedDirect = []
        , satisfiedIndirect = []
        , unsatisfied = []
        , conflicting = []
        }
