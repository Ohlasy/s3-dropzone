module Upload exposing (..)

import File exposing (File)
import Html exposing (Html, text)
import Html.Attributes exposing (href, style)
import Http
import Material.Card exposing (..)
import Material.Icon exposing (icon, iconConfig)
import Material.LayoutGrid as LayoutGrid exposing (layoutGridCell)
import Material.TextField exposing (textField, textFieldConfig)
import Material.Typography as Typography
import S3
import Session exposing (Session)


type JobStatus
    = Pending
    | Running
    | Finished (Result Http.Error S3.Response)


type alias Job =
    { file : File
    , status : JobStatus
    }


type alias Model =
    { finishedJobs : List Job
    , currentJob : Maybe Job
    , pendingJobs : List Job
    }


init : Model
init =
    { finishedJobs = []
    , currentJob = Nothing
    , pendingJobs = []
    }


allJobs : Model -> List Job
allJobs model =
    case model.currentJob of
        Just job ->
            model.finishedJobs ++ [ job ] ++ model.pendingJobs

        Nothing ->
            model.finishedJobs ++ model.pendingJobs


addJobs : List File -> Model -> Model
addJobs files model =
    let
        newJobs =
            files |> List.map (\f -> { file = f, status = Pending })

        pendingJobs =
            model.pendingJobs ++ newJobs
    in
    { model | pendingJobs = pendingJobs }


finishCurrentJob : Result Http.Error S3.Response -> Model -> Model
finishCurrentJob result model =
    case model.currentJob of
        Just currentJob ->
            let
                finishedJob =
                    { currentJob | status = Finished result }

                finishedJobs =
                    model.finishedJobs ++ [ finishedJob ]
            in
            { model | currentJob = Nothing, finishedJobs = finishedJobs }

        Nothing ->
            model


startNextJob : Model -> ( Model, Maybe Job )
startNextJob model =
    case model.currentJob of
        -- No job currently running, start next if there are pending jobs
        Nothing ->
            case model.pendingJobs of
                -- Any pending jobs left?
                next :: remaining ->
                    let
                        started =
                            { next | status = Running }
                    in
                    ( { model | currentJob = Just started, pendingJobs = remaining }, Just started )

                -- No, nothing left to do
                default ->
                    ( model, Nothing )

        -- Some job already running, no change
        Just _ ->
            ( model, Nothing )



-- VIEW


viewJob : Job -> Html a
viewJob job =
    layoutGridCell [ LayoutGrid.span2Desktop ]
        [ card cardConfig
            { blocks =
                [ cardMedia { cardMediaConfig | aspect = Just SixteenToNine } (coverImageForJob job)
                , cardBlock <|
                    Html.div [ style "padding" "10px" ]
                        [ Html.h2 [] [ text (File.name job.file) ]
                        , Html.p [] [ viewStatus job.status ]
                        ]
                ]
            , actions = Nothing
            }
        ]


coverImageForJob : Job -> String
coverImageForJob job =
    case job.status of
        Finished (Ok { location }) ->
            location

        default ->
            "placeholder.jpg"


viewStatus : JobStatus -> Html a
viewStatus s =
    case s of
        Pending ->
            Html.text "Waiting."

        Running ->
            Html.text "Uploading…"

        Finished (Ok { location }) ->
            Html.a [ href location ] [ Html.text "Uploaded" ]

        Finished (Err e) ->
            Html.text ("Failed: " ++ Debug.toString e)
