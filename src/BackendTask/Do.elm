module BackendTask.Do exposing (allowFatal, do, log, noop)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Pages.Script as Script


do : BackendTask error a -> (a -> BackendTask error b) -> BackendTask error b
do x f =
    BackendTask.andThen f x


log : String -> (() -> BackendTask error a) -> BackendTask error a
log message =
    do (Script.log message)


noop : BackendTask error ()
noop =
    BackendTask.succeed ()


allowFatal :
    BackendTask { error | fatal : FatalError } a
    -> (a -> BackendTask FatalError b)
    -> BackendTask FatalError b
allowFatal x f =
    BackendTask.andThen f (BackendTask.allowFatal x)
