module Main exposing (devFlags, init, main, prodFlags, reactorMain, update, view)

import Browser
import Dict exposing (update)
import Effect exposing (Effect, performEffect)
import Html exposing (Html, button, div, input, node, select, text)
import Html.Attributes exposing (href, style, type_)
import Model exposing (AppState(..), Config, LoadingPostsState, Mode(..), Model, Msg(..))
import Model.Post as Post
import Model.PostIds as PostIds exposing (HackerNewsItem(..))
import Model.PostsConfig
import View.Posts exposing (postTable, postsConfigView)


prodFlags : Config
prodFlags =
    { apiUrl = "https://hacker-news.firebaseio.com", mode = Prod }


devFlags : Config
devFlags =
    { apiUrl = "http://localhost:3000", mode = Dev }


{-| Create a program that uses the "production" configuration (uses the real hackernews API)
-}
main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> init prodFlags flags |> Tuple.mapSecond performEffect
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffect
        , subscriptions = subscriptions
        }


{-| Create a program that uses the development configuration (uses a local server that returns hardcoded hackernews posts)
-}
reactorMain : Program () Model Msg
reactorMain =
    Browser.element
        { init = \flags -> init devFlags flags |> Tuple.mapSecond performEffect
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffect
        , subscriptions = subscriptions
        }


{-| Don't modify
-}
init : Config -> () -> ( Model, Effect )
init flags _ =
    ( Model.initModel flags
    , Effect.GetTime
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getItems : String -> HackerNewsItem -> Effect
getItems apiUrl item =
    Effect.GetItems { apiUrl = apiUrl, item = item, onResult = GotPostIds, decoder = PostIds.decode }


getTopPostIds : String -> Effect
getTopPostIds apiUrl =
    getItems apiUrl Top


getPost : String -> Int -> Effect
getPost apiUrl postId =
    Effect.GetPost { apiUrl = apiUrl, postId = postId, onResult = GotPost, decoder = Post.decode }


addLoadedPost : Post.Post -> LoadingPostsState -> LoadingPostsState
addLoadedPost post state =
    { state | posts = post :: state.posts }


update : Msg -> Model -> ( Model, Effect )
update msg model =
    let
        ( newState, cmd ) =
            case ( model.state, msg ) of
                ( Model.Empty { config }, GotTime time ) ->
                    ( Model.Loading { config = config, time = time }, getTopPostIds model.config.apiUrl )

                ( Model.Loading { config, time }, GotPostIds result ) ->
                    case result of
                        Ok (Just ids) ->
                            ( Model.LoadingPosts
                                { config = config
                                , time = time
                                , postIds = ids
                                , currentId = PostIds.first ids
                                , posts = []
                                }
                            , getPost model.config.apiUrl (PostIds.first ids)
                            )

                        Ok Nothing ->
                            ( Model.Empty { config = config }, Effect.NoEffect )

                        Err err ->
                            ( Model.FailedToLoad err, Effect.NoEffect )

                ( Model.LoadingPosts loading, GotPost result ) ->
                    case result of
                        Ok post ->
                            case PostIds.advance loading.postIds of
                                Just ( nextId, nextPostIds ) ->
                                    let
                                        posts =
                                            post :: loading.posts
                                    in
                                    if List.length posts < loading.config.postsToFetch then
                                        ( Model.LoadingPosts
                                            { loading
                                                | postIds = nextPostIds
                                                , currentId = nextId
                                                , posts = posts
                                            }
                                        , getPost model.config.apiUrl nextId
                                        )

                                    else
                                        ( Model.LoadedPosts
                                            { config = loading.config
                                            , time = loading.time
                                            , posts = List.reverse (post :: loading.posts)
                                            }
                                        , Effect.NoEffect
                                        )

                                Nothing ->
                                    ( Model.LoadedPosts
                                        { config = loading.config
                                        , time = loading.time
                                        , posts = List.reverse (post :: loading.posts)
                                        }
                                    , Effect.NoEffect
                                    )

                        Err err ->
                            ( Model.FailedToLoad err, Effect.NoEffect )

                ( Model.LoadedPosts state, ConfigChanged change ) ->
                    ( Model.LoadedPosts
                        { state
                            | config = Model.PostsConfig.applyChanges change state.config
                        }
                    , Effect.NoEffect
                    )

                ( state, _ ) ->
                    ( state, Effect.NoEffect )
    in
    ( { model | state = newState }, cmd )


view : Model -> Html Msg
view model =
    let
        title =
            if model.config.mode == Dev then
                "HackerNews (DEV)"

            else
                "HackerNews"

        body =
            case model.state of
                Model.Empty _ ->
                    div [] [ text "Loading" ]

                Model.FailedToLoad _ ->
                    div [] [ text "Failed to load" ]

                Model.LoadedPosts { config, time, posts } ->
                    div []
                        [ postsConfigView config
                        , postTable config time posts
                        ]

                Model.Loading _ ->
                    div [] [ text "Loading stories" ]

                -- Model.LoadingPosts { currentId } ->
                --     div [] [ text <| "Loading post " ++ String.fromInt currentId ]
                Model.LoadingPosts { currentId, posts } ->
                    div []
                        [ text <| "Fetching post " ++ String.fromInt (List.length posts + 1) ++ "/50"
                        , div [ style "font-size" "0.8em", style "color" "#888" ]
                            [ text <| "(ID: " ++ String.fromInt currentId ++ ")" ]
                        ]

                _ ->
                    div [] [ text "Other" ]
    in
    div []
        [ node "style" [] [ text customCss ]
        , Html.h1 [] [ text title ]
        , body
        ]


customCss : String
customCss =
    """
    body {
        font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
        background-color: #f6f6ef;
        color: #333;
        margin: 0;
        padding: 20px;
    }

    h1 {
        color: #ff6600; /* HN Orange */
        font-weight: 700;
        letter-spacing: -1px;
    }

    /* Table Styling */
    table {
        width: 100%;
        border-collapse: collapse;
        background: white;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
        border-radius: 8px;
        overflow: hidden;
        margin-top: 20px;
    }

    th {
        background-color: #333;
        color: white;
        font-weight: 600;
        text-transform: uppercase;
        font-size: 0.85rem;
        padding: 15px;
        text-align: left;
    }

    td {
        padding: 12px 15px;
        border-bottom: 1px solid #eee;
        font-size: 0.95rem;
    }

    tr:hover {
        background-color: #f9f9f9;
    }

    /* Specific Column Styling */
    .post-score {
        font-weight: bold;
        color: #ff6600;
        text-align: center;
        width: 60px;
    }

    .post-title {
        font-weight: 500;
        color: #222;
    }

    .post-url a {
        color: #828282;
        text-decoration: none;
        font-size: 0.85rem;
    }

    .post-url a:hover {
        text-decoration: underline;
        color: #ff6600;
    }

    .post-time {
        color: #888;
        font-size: 0.85rem;
        white-space: nowrap;
    }

    .post-type {
        text-transform: capitalize;
        font-size: 0.85rem;
        color: #666;
        background: #eee;
        padding: 4px 8px;
        border-radius: 4px;
        text-align: center;
    }

    /* Config Section Styling */
    #select-posts-per-page, #select-sort-by {
        padding: 8px;
        border: 1px solid #ddd;
        border-radius: 4px;
        margin-right: 10px;
        background: white;
    }

    input[type="checkbox"] {
        accent-color: #ff6600;
        margin-right: 8px;
    }

    /* Helper to align the config bar */
    div > div:first-child {
        display: flex;
        align-items: center;
        gap: 20px;
        background: white;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        margin-bottom: 20px;
    }
    """
