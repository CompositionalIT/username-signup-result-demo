module Index

open Elmish
open SAFE
open Shared

// Create a module for the ValidationError extension to limit the scope of the UserName import
module Extensions =
    open Shared.Username

    type ValidationError with
        member this.Description =
            match this with
            | LengthError -> "Must be between 3 and 20 characters"
            | IllegalCharacters -> "Only letters and digits are allowed"

open Extensions

type UsernameInputValidationState =
    | NotValidated
    | LocalValidationFailed of Username.ValidationError list
    | UsernameValidAndAvailable of RemoteData<bool>

type Model = {
    UsernameInput: string
    UsernameInputValidationState: UsernameInputValidationState
    SubmitResult: RemoteData<Api.RegisterUsernameResponse>
}

type Msg =
    | SetUsernameInput of string
    | CheckUsernameAvailability of ApiCall<Username, bool>
    | Register of ApiCall<unit, Api.RegisterUsernameResponse>

let api = Api.makeProxy<Api.IUserApi> ()

let init () =
    let initialModel = {
        UsernameInput = ""
        UsernameInputValidationState = NotValidated
        SubmitResult = NotStarted
    }

    initialModel, Cmd.none

let update msg model =
    match msg with

    // User interaction handlers

    | SetUsernameInput value ->
        match Username.validate value with
        | Ok validUsername ->
            let cmd =
                Cmd.ofMsg (CheckUsernameAvailability(Start validUsername))

            let model = {
                model with
                    UsernameInput = value
                    UsernameInputValidationState = UsernameValidAndAvailable NotStarted
                    SubmitResult = NotStarted
            }

            model, cmd
        | Error validationErrors ->
            let model = {
                model with
                    UsernameInput = value
                    UsernameInputValidationState = LocalValidationFailed validationErrors
                    SubmitResult = NotStarted
            }

            model, Cmd.none

    // API calls

    | CheckUsernameAvailability apiCall ->
        match apiCall with
        | Start username ->
            let cmd = Cmd.OfAsync.perform api.isUsernameAvailable username (Finished >> CheckUsernameAvailability)
            let model = {
                model with
                    UsernameInputValidationState = UsernameValidAndAvailable(Loading None)
            }

            model, cmd

        | Finished usernameAvailable ->
            let model = {
                model with
                    UsernameInputValidationState = UsernameValidAndAvailable(Loaded usernameAvailable)
            }

            model, Cmd.none

    | Register apiCall ->
        match apiCall with
        | Start () ->
            let cmd =
                Cmd.OfAsync.perform api.registerUsername (Username model.UsernameInput) (Finished >> Register)

            let model = {
                model with
                    UsernameInputValidationState = NotValidated
                    SubmitResult = Loading None
            }

            model, cmd

        | Finished response ->
            let model = {
                model with
                    UsernameInput = ""
                    UsernameInputValidationState = NotValidated
                    SubmitResult = Loaded response
            }

            model, Cmd.none

open Feliz

let view model dispatch =

    let liError (m: string) =
        Html.li [ prop.className "text-red-500"; prop.text m ]

    let liInfo (m: string) =
        Html.li [ prop.className "text-black"; prop.text m ]

    let liSuccess (m: string) =
        Html.li [ prop.className "text-emerald-600"; prop.text m ]

    Html.div [
        prop.children [
            Html.div [
                prop.className "flex flex-col items-center justify-center h-full"
                prop.children [
                    Html.h1 [
                        prop.className "text-center text-5xl font-bold text-cyan-950 mb-4 p-4"
                        prop.text "User registration"
                    ]
                    Html.form [

                        prop.className "grid grid-cols-2 grid-flow-row w-1/2 place-items-center gap-2"
                        prop.children [

                            Html.label [
                                prop.text "Username"
                                prop.className "text-gray-700 p-2 col-span-full justify-start w-full"
                                prop.htmlFor "username"
                            ]

                            Html.input [
                                prop.id "username"
                                prop.className "border-2 border-gray-300 rounded-md p-2 h-16 w-full"
                                prop.value model.UsernameInput
                                prop.onChange (SetUsernameInput >> dispatch)
                            ]

                            Html.ul [
                                prop.className "inline-block text-sm font-bold p-2 w-full"
                                prop.children [
                                    match model.UsernameInputValidationState with
                                    | NotValidated -> ()
                                    | LocalValidationFailed errors ->
                                        yield! errors |> List.map (_.Description >> liError)
                                    | UsernameValidAndAvailable NotStarted -> liInfo "valid"
                                    | UsernameValidAndAvailable(Loading _) -> liInfo "checking availability..."
                                    | UsernameValidAndAvailable(Loaded true) -> liSuccess "available"
                                    | UsernameValidAndAvailable(Loaded false) -> liError "taken"
                                ]
                            ]

                            Html.div [
                                Html.button [
                                    prop.text "Register"
                                    prop.className "bg-cyan-950 text-white font-bold py-2 px-4 rounded m-2"
                                    prop.onClick (fun e ->
                                        dispatch (Register(Start()))
                                        e.preventDefault ())
                                ]
                            ]

                            Html.div [
                                prop.className "text-start text-sm font-bold p-2 w-full"
                                prop.children [
                                    match model.SubmitResult with
                                    | NotStarted -> Html.none
                                    | Loading _ -> Html.span "registering..."
                                    | Loaded(Ok()) -> Html.span "registration success!"
                                    | Loaded(Error errors) -> Html.span $"error(s)! %A{errors}"
                                ]
                            ]
                        ]

                    ]
                ]
            ]
        ]
    ]