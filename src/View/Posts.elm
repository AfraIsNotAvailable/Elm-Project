module View.Posts exposing (..)

import Char exposing (isAlphaNum)
import Html exposing (Html, a, div, table, td, text, th, tr)
import Html.Attributes exposing (class, href)
import Html.Events
import List exposing (sort)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time


postRow : Time.Posix -> Post -> Html Msg
postRow currentTime post =
    let
        postedDateStr =
            Util.Time.formatTime Time.utc post.time

        duration =
            Util.Time.durationBetween post.time currentTime

        durationStr =
            duration
                |> Maybe.map Util.Time.formatDuration
                |> Maybe.withDefault "time error"

        timeDisplay =
            postedDateStr ++ " (" ++ durationStr ++ ")"

        urlContent =
            case post.url of
                Just u ->
                    a [ href u ] [ text u ]

                Nothing ->
                    text ""
    in
    tr []
        [ td [ class "post-score" ] [ text (String.fromInt post.score) ]
        , td [ class "post-title" ] [ text post.title ]
        , td [ class "post-type" ] [ text post.type_ ]
        , td [ class "post-time" ] [ text timeDisplay ]
        , td [ class "post-url" ] [ urlContent ]
        ]


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config currentTime posts =
    let
        header =
            tr []
                [ th [] [ text "Score" ]
                , th [] [ text "Title" ]
                , th [] [ text "Type" ]
                , th [] [ text "Posted Date" ]
                , th [] [ text "Link" ]
                ]

        rows =
            posts
                |> Model.PostsConfig.filterPosts config
                |> List.map (postRow currentTime)
    in
    table [] (header :: rows)


{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsPerPageInput : PostsConfig -> Html Msg
postsPerPageInput config =
    let
        options =
            [ 10, 25, 50 ]

        current =
            String.fromInt config.postsToShow

        onChange s =
            case String.toInt s of
                Just n ->
                    ConfigChanged (ChangePostsToShow n)

                Nothing ->
                    ConfigChanged (ChangePostsToShow 10)

        postsCountOption num =
            let
                s =
                    String.fromInt num
            in
            Html.option
                [ Html.Attributes.value s
                , Html.Attributes.selected (s == current)
                ]
                [ text s ]
    in
    Html.select
        [ Html.Attributes.id "select-posts-per-page"
        , Html.Events.onInput onChange
        ]
        (List.map postsCountOption options)


sortByInput : PostsConfig -> Html Msg
sortByInput config =
    let
        currentSortStr =
            sortToString config.sortBy

        onChange s =
            case sortFromString s of
                Just sortBy ->
                    ConfigChanged (ChangeSortBy sortBy)

                Nothing ->
                    ConfigChanged (ChangeSortBy None)

        sortOption sort =
            let
                s =
                    sortToString sort
            in
            Html.option
                [ Html.Attributes.value s
                , Html.Attributes.selected (s == currentSortStr)
                ]
                [ text s ]
    in
    Html.select
        [ Html.Attributes.id "select-sort-by"
        , Html.Events.onInput onChange
        ]
        (List.map sortOption sortOptions)


showJobsInput : PostsConfig -> Html Msg
showJobsInput config =
    Html.div []
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.id "checkbox-show-job-posts"
            , Html.Events.onCheck (\b -> ConfigChanged (ChangeShowJobs b))
            , Html.Attributes.checked config.showJobs
            ]
            []
        , text "Show job posts"
        ]


showTextOnlyInput : PostsConfig -> Html Msg
showTextOnlyInput config =
    Html.div []
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.id "checkbox-show-text-only-posts"
            , Html.Events.onCheck (\b -> ConfigChanged (ChangeShowTextOnly b))
            , Html.Attributes.checked config.showTextOnly
            ]
            []
        , text "Show text-only posts"
        ]


postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    Html.div []
        [ postsPerPageInput config
        , sortByInput config
        , showJobsInput config
        , showTextOnlyInput config
        ]
