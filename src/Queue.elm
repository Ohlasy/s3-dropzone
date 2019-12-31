module Queue exposing (Queue, addJobs, allJobs, init, update, updateCurrentJob)

{-| This is all very wrong. We would like the queue to be a list of jobs in various states,
and support automatically starting new uploads while a previous one finishes, up to a given
concurrency.
-}

import File exposing (File)
import List.Zipper as LZ
import Session exposing (Session)
import Upload



-- MODEL


type alias Queue =
    Maybe (LZ.Zipper Upload.Model)


init : Queue
init =
    Nothing


allJobs : Queue -> List Upload.Model
allJobs queue =
    case queue of
        Just zipper ->
            LZ.toList zipper

        Nothing ->
            []



-- QUEUE MANAGEMENT


addJobs : Queue -> Session -> List File -> Queue
addJobs queue session files =
    let
        jobs =
            files |> List.map (Upload.init session)
    in
    case queue of
        Just zipper ->
            Just (append zipper jobs)

        Nothing ->
            LZ.fromList jobs


updateCurrentJob : Queue -> Upload.Msg -> ( Queue, Cmd Upload.Msg )
updateCurrentJob queue msg =
    case queue of
        Just zipper ->
            let
                before =
                    LZ.before zipper

                current =
                    LZ.current zipper

                after =
                    LZ.after zipper

                ( newCurrent, cmd ) =
                    Upload.update msg current

                newZipper =
                    LZ.from before newCurrent after
            in
            ( Just newZipper, cmd )

        Nothing ->
            ( queue, Cmd.none )


update : Queue -> ( Queue, Cmd Upload.Msg )
update queue =
    case queue of
        -- Queue not empty
        Just zipper ->
            let
                current =
                    LZ.current zipper
            in
            case current.status of
                -- Start current upload
                Upload.Waiting ->
                    updateCurrentJob queue Upload.Start

                -- Move to next upload
                Upload.Finished _ ->
                    case LZ.next zipper of
                        -- Start next upload
                        Just newZipper ->
                            updateCurrentJob (Just newZipper) Upload.Start

                        -- Last upload finished, no change
                        Nothing ->
                            ( queue, Cmd.none )

                -- Current upload still runinning, no change
                Upload.Running ->
                    ( queue, Cmd.none )

        -- Queue empty, nothing to do
        Nothing ->
            ( queue, Cmd.none )



-- HELPERS


append : LZ.Zipper a -> List a -> LZ.Zipper a
append zipper new =
    let
        before =
            LZ.before zipper

        current =
            LZ.current zipper

        after =
            LZ.after zipper
    in
    LZ.from before current (after ++ new)
