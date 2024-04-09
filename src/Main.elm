module Main exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withCliOptions config task


type alias CliOptions =
    { nameOrPath : String
    , version : Maybe String
    }


config : Program.Config CliOptions
config =
    Program.config
        |> Program.add
            (OptionsParser.build
                (\nameOrPath version ->
                    { nameOrPath = nameOrPath
                    , version = version
                    }
                )
                |> OptionsParser.with (Option.requiredPositionalArg "Name or path")
                |> OptionsParser.withOptionalPositionalArg (Option.optionalPositionalArg "Version")
            )


task : CliOptions -> BackendTask FatalError ()
task cliOptions =
    let
        targetName : String
        targetName =
            case cliOptions.version of
                Just version ->
                    cliOptions.nameOrPath ++ ", version " ++ version

                Nothing ->
                    cliOptions.nameOrPath
    in
    Do.log ("Trying to vendor " ++ targetName) <| \_ ->
    Do.noop
