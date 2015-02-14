module Battle.CursorList where

import Debug (log)

import List
import World.World
import LocalChannel
import Battle.Cursor
import World.Position
import Graphics.Collage


-- MODEL

type alias Model =
    { cursors : List (ID, Battle.Cursor.Model)
    , nextID : ID
    }

type alias ID =
    Int

default : Model
default =
    { cursors = [ ]
    , nextID = 0
    }


-- UPDATE

type Action
    = NoOp
    | Clear
    | Insert Battle.Cursor.Model
    | AreaCircle Battle.Cursor.CursorType Int World.Position.Model
    | AreaWithCost Battle.Cursor.CursorType Int World.World.Model World.Position.Model
    | ModifyCursor ID Battle.Cursor.Action

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        Clear ->
            { model
                | cursors <- [ ]
                , nextID <- 0
            }

        Insert cursor ->
            model

        AreaCircle cursorType radius centre ->
            let area = World.Position.circle radius centre
                cursors' = area |> List.map (\pos -> Battle.Cursor.init cursorType pos)
            in
                { model
                    | cursors <- List.indexedMap (,) cursors'
                    , nextID <- List.length cursors'
                }

        AreaWithCost cursorType points world centre ->
            let area = World.World.getDistanceGrid world points centre
                cursors' = area |> List.map (\(cost, pos) -> Battle.Cursor.init cursorType pos)
            in
                { model
                    | cursors <- List.indexedMap (,) cursors'
                    , nextID <- List.length cursors'
                }

        ModifyCursor id cursorAction ->
            let updateCursor (cursorID, cursorModel) =
                    if cursorID == id
                        then (cursorID, Battle.Cursor.update cursorAction cursorModel)
                        else (cursorID, cursorModel)
            in
                { model |
                    cursors <- model.cursors |> List.map updateCursor
                }


-- VIEW

type alias Context =
    { actionChannel : LocalChannel.LocalChannel Action
    }

view : Context -> Model -> Graphics.Collage.Form
view context ({cursors} as model) =
    cursors
        |> List.map (viewCursor context)
        |> Graphics.Collage.group

viewCursor : Context -> (ID, Battle.Cursor.Model) -> Graphics.Collage.Form
viewCursor context (id, cursor) =
    let context' =
            Battle.Cursor.Context
                (LocalChannel.localize (ModifyCursor id) context.actionChannel)
    in
        Battle.Cursor.view context' cursor