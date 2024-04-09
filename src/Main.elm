module Main exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm.Project as Project
import FatalError exposing (FatalError)
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
    Script.log ("The package needs the following dependencies: " ++ Debug.toString package.deps)
