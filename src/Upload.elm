module Upload exposing (..)

import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (href)
import Http
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


addJobs : Model -> List File -> Model
addJobs model files =
    let
        newJobs =
            files |> List.map (\f -> { file = f, status = Pending })

        pendingJobs =
            model.pendingJobs ++ newJobs
    in
    { model | pendingJobs = pendingJobs }


finishCurrentJob : Model -> Result Http.Error S3.Response -> Model
finishCurrentJob model result =
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
    div []
        [ Html.text (File.name job.file)
        , Html.text ": "
        , viewStatus job.status
        ]


viewStatus : JobStatus -> Html a
viewStatus s =
    case s of
        Pending ->
            Html.text "Waiting"

        Running ->
            Html.text "Uploading"

        Finished result ->
            case result of
                Ok response ->
                    Html.a [ href response.location ] [ Html.text "Uploaded" ]

                Err e ->
                    Html.text ("Failed: " ++ Debug.toString e)
