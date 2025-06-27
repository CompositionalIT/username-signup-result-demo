namespace Shared

open FsToolkit.ErrorHandling

type Username =
    | Username of string
    with member this.Value = match this with Username s -> s

module Username =
    type ValidationError =
        | LengthError
        | IllegalCharacters
    type RegistrationError =
        | Taken
        | Invalid of ValidationError list

    let validate (s: string) =
        validation {
            let! _ =
                if s.Length < 3 then Error LengthError
                else if s.Length > 20 then Error LengthError
                else Ok ()

            and! _ =
                if not (s.ToCharArray() |> Array.forall System.Char.IsLetterOrDigit) then
                    Error IllegalCharacters
                else
                    Ok ()

            return Username s
        }

module Api =
    type RegisterUsernameResponse = Result<unit, Username.RegistrationError>
    type IUserApi = {
        isUsernameAvailable: Username -> Async<bool>
        registerUsername: Username -> Async<RegisterUsernameResponse>
    }

