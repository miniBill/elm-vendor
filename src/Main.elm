module Main exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
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
import Json.Decode
import Json.Encode
import List.Extra
import Pages.Script as Script exposing (Script)
import Pages.Script.Spinner as Spinner


run : Script
run =
    Script.withCliOptions config <| \cliOptions ->
    Spinner.steps
        |> Spinner.withStep "Checking elm.json from package" (\_ -> getPathFor cliOptions)
        |> Spinner.withStep "Gathering dependencies"
            (\packageElmJsonPath ->
                gatherDependencies packageElmJsonPath
                    |> BackendTask.map
                        (\{ application, package, satisfiedIndirect, unsatisfied } ->
                            { packageElmJsonPath = packageElmJsonPath
                            , application = application
                            , package = package
                            , satisfiedIndirect = satisfiedIndirect
                            , unsatisfied = unsatisfied
                            }
                        )
            )
        |> withStep_ "Adding folder to the local elm.json" (\{ application, package } -> addFolder application package)
        |> withStep_ "Installing package's dependencies that were indirect" (\{ satisfiedIndirect } -> installIndirectDependencies satisfiedIndirect)
        |> withStep_ "Installing package's dependencies that were missing" (\{ unsatisfied } -> installUnsatisfiedDependencies unsatisfied)
        |> withStep_ "Removing the package from the dependencies" (\{ application, package } -> removeDependency application package)
        |> withStep_ "Copying files" (\{ packageElmJsonPath, package } -> copyFiles packageElmJsonPath package)
        |> Spinner.runSteps
        |> BackendTask.map (always ())


withStep_ : String -> (a -> BackendTask FatalError b) -> Spinner.Steps FatalError a -> Spinner.Steps FatalError a
withStep_ label f previous =
    Spinner.withStep label (\input -> BackendTask.map (always input) (f input)) previous


copyFiles : String -> Project.PackageInfo -> BackendTask FatalError ()
copyFiles packageElmJsonPath package =
    let
        target : String
        target =
            targetPath package
    in
    Do.allowFatal (command <| "mkdir -p " ++ target) <| \_ ->
    Do.allowFatal (command <| "cp -r $(dirname " ++ packageElmJsonPath ++ ")/{LICENSE,src,README.md,elm.json} " ++ target) <| \_ ->
    Do.noop


targetPath : Project.PackageInfo -> String
targetPath package =
    "vendored/" ++ Package.toString package.name ++ "/" ++ Version.toString package.version


addFolder : Project.ApplicationInfo -> Project.PackageInfo -> BackendTask FatalError ()
addFolder application package =
    let
        target : String
        target =
            targetPath package ++ "/src"

        newDirs : List String
        newDirs =
            if List.member target application.dirs then
                application.dirs

            else
                application.dirs ++ [ target ]

        newBody : Json.Encode.Value
        newBody =
            { application | dirs = newDirs }
                |> Project.Application
                |> Project.encode
    in
    if newDirs == application.dirs then
        Do.noop

    else
        Script.writeFile { path = "elm.json", body = Json.Encode.encode 4 newBody }
            |> BackendTask.allowFatal


removeDependency : Project.ApplicationInfo -> Project.PackageInfo -> BackendTask FatalError ()
removeDependency application package =
    if List.any (\( depName, _ ) -> depName == package.name) application.depsDirect then
        command ("elm-json uninstall --yes " ++ Package.toString package.name)
            |> BackendTask.allowFatal

    else
        Do.noop


getPathFor : CliOptions -> BackendTask FatalError String
getPathFor cliOptions =
    let
        cut : String -> String -> String
        cut tail input =
            if String.endsWith tail input then
                String.dropRight (String.length tail) input

            else
                input
    in
    Glob.succeed identity
        |> Glob.capture (Glob.literal <| (cliOptions.nameOrPath |> cut "elm.json" |> cut "/") ++ "/elm.json")
        |> Glob.expectUniqueMatch
        |> BackendTask.allowFatal


installIndirectDependencies : List ( ( Package.Name, Constraint.Constraint ), Version.Version ) -> BackendTask FatalError ()
installIndirectDependencies satisfiedIndirect =
    if List.isEmpty satisfiedIndirect then
        Do.noop

    else
        let
            versioned : List String
            versioned =
                List.map
                    (\( ( name, _ ), version ) -> Package.toString name ++ "@" ++ Version.toString version)
                    satisfiedIndirect

            cmd : String
            cmd =
                String.join " " ("elm-json install --yes" :: versioned)
        in
        BackendTask.allowFatal (command cmd)


installUnsatisfiedDependencies : List ( Package.Name, Constraint.Constraint ) -> BackendTask FatalError ()
installUnsatisfiedDependencies unsatisfied =
    if List.isEmpty unsatisfied then
        Do.noop

    else
        let
            extractVersion : Constraint.Constraint -> String
            extractVersion constraint =
                constraint
                    |> Constraint.toString
                    |> String.split " "
                    |> List.take 1
                    |> String.concat

            versioned : List String
            versioned =
                List.map
                    (\( name, constraint ) -> Package.toString name ++ "@" ++ extractVersion constraint)
                    unsatisfied

            cmd : String
            cmd =
                String.join " " ("elm-json install --yes" :: versioned)
        in
        BackendTask.allowFatal (command cmd)


command :
    String
    -> BackendTask { fatal : FatalError, recoverable : BackendTask.Custom.Error } ()
command cmd =
    Do.log cmd <| \_ ->
    Do.do (BackendTask.Custom.run "command" (Json.Encode.string cmd) Json.Decode.string) <| \output ->
    Script.log output


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


gatherDependencies :
    String
    ->
        BackendTask
            FatalError
            { application : Project.ApplicationInfo
            , package : Project.PackageInfo
            , satisfiedIndirect : List ( ( Package.Name, Constraint.Constraint ), Version.Version )
            , unsatisfied : List ( Package.Name, Constraint.Constraint )
            }
gatherDependencies packageElmJsonPath =
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
        (File.jsonFile Project.decoder packageElmJsonPath
            |> BackendTask.allowFatal
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
            [ List.length satisfiedDirect
            , List.length satisfiedIndirect
            , List.length unsatisfied
            , List.length conflicting
            ]
                |> List.map
                    (\list ->
                        list
                            |> String.fromInt
                            |> String.length
                    )
                |> List.maximum
                |> Maybe.withDefault 0

        message =
            [ "The package needs " ++ lengthString package.deps ++ " dependencies, of which:"
            , " - " ++ lengthString satisfiedDirect ++ " are already satisfied,"
            , " - " ++ lengthString satisfiedIndirect ++ " are compatible with the indirect dependencies,"
            , " - " ++ lengthString unsatisfied ++ " are not satisfied,"
            , " - " ++ lengthString conflicting ++ " are incompatible with your current dependencies."
            ]
                |> String.join "\n"
    in
    Do.log message <| \_ ->
    if List.isEmpty conflicting then
        BackendTask.succeed
            { application = application
            , package = package
            , satisfiedIndirect = satisfiedIndirect
            , unsatisfied = unsatisfied
            }

    else
        BackendTask.fail <|
            FatalError.build
                { title = "Incompatible dependencies"
                , body =
                    "The following dependencies from the package are incompatible with your current application: "
                        ++ String.join ", "
                            (List.map
                                (\( dep, constraint ) ->
                                    Package.toString dep ++ " " ++ Constraint.toString constraint
                                )
                                conflicting
                            )
                }


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
    if
        List.member (Package.toString name) replacedByCoreExtra
            && List.any (\( depName, _ ) -> Package.toString depName == "elmcraft/core-extra") depsDirect
    then
        SatisfiedDirect

    else
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
                            SatisfiedIndirect version

                        else
                            Conflicting

                    Nothing ->
                        Unsatisfied


replacedByCoreExtra : List String
replacedByCoreExtra =
    [ "elm-community/array-extra"
    , "elm-community/basics-extra"
    , "elm-community/dict-extra"
    , "elm-community/list-extra"
    , "elm-community/maybe-extra"
    , "elm-community/result-extra"
    , "elm-community/string-extra"
    , "GlobalWebIndex/cmd-extra"
    , "hayleigh-dot-dev/tuple-extra"
    , "stoeffel/set-extra"
    ]


type DependencyKind
    = SatisfiedDirect
    | SatisfiedIndirect Version.Version
    | Unsatisfied
    | Conflicting


tripartition :
    (( Package.Name, Constraint.Constraint ) -> DependencyKind)
    -> List ( Package.Name, Constraint.Constraint )
    ->
        { satisfiedDirect : List ( Package.Name, Constraint.Constraint )
        , satisfiedIndirect : List ( ( Package.Name, Constraint.Constraint ), Version.Version )
        , unsatisfied : List ( Package.Name, Constraint.Constraint )
        , conflicting : List ( Package.Name, Constraint.Constraint )
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

                        SatisfiedIndirect version ->
                            go tail { acc | satisfiedIndirect = ( head, version ) :: acc.satisfiedIndirect }

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
