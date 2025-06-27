module Server

open SAFE
open Saturn
open Shared
open Shared.Api
open FsToolkit.ErrorHandling

module Storage =

    // This would something such as a database in a real application
    let private registeredUsernames = ResizeArray<Username> []

    let addUsername username = async { registeredUsernames.Add username }

    let isUsernameAvailable (username: Username) = async { return registeredUsernames.Contains username |> not }

let registerUsername (username: Username) : Async<Result<unit, Username.RegistrationError>> = asyncResult {

    let! _ =
        username
        |> Storage.isUsernameAvailable
        |> AsyncResult.requireTrue Username.RegistrationError.Taken

    // We must revalidate the username as it was provided by the client, which cannot be trusted
    and! validatedUsername =
        username.Value
        |> Username.validate
        |> Result.mapError Username.RegistrationError.Invalid
        |> AsyncResult.ofResult

    do! Storage.addUsername validatedUsername
    return ()
}

let userApi ctx = {
    isUsernameAvailable = Storage.isUsernameAvailable
    registerUsername = registerUsername
}

let app = application {
    use_router (Api.make userApi)
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    run app
    0