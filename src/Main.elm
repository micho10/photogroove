module Main exposing (main)

import Browser exposing (application, Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, footer,h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)


type alias Model =
  { page : Page }


type Page
    = SelectedPhoto String
    | Gallery
    | Folders
    | NotFound


view : Model -> Document Msg
view model =
  let
      content = text "This isn't even my final form!"
  in
  { title = "Photo Groove, SPA Style"
  , body =
    [ lazy viewHeader model.page
    , content
    , viewFooter
    ]
  }


viewFooter : Html msg
viewFooter =
  footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]


viewHeader : Page -> Html Msg
viewHeader page =
  let
    logo = h1 [] [ text "Photo Groove" ]

    links = ul []
      [ navLink Folders { url = "/", caption = "Folders" }
      , navLink Gallery { url = "/gallery", caption = "Gallery" }
      ]

    navLink : Page -> { url : String, caption : String } -> Html msg
    navLink route { url, caption } =
      li [ classList [ ( "active", isActive { link = route,
        page = page } ) ] ]
        [ a [ href url ] [ text caption ] ]
  in
  nav [] [ logo, links ]


isActive : { link : Page, page : Page } -> Bool
isActive { link, page } =
  case ( link, page ) of
    ( Gallery, Gallery ) -> True
    ( Gallery, _ ) -> False
    ( Folders, Folders ) -> True
    ( Folders, SelectedPhoto _ ) -> True
    ( Folders, _ ) -> False
    ( SelectedPhoto _, _ ) -> False
    ( NotFound, _ ) -> False


type Msg =
  NothingYet


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> Url -> Nav.Key -> ( Model, Cmd msg )
init flags url key =
  ( { page = urlToPage url }, Cmd.none )


urlToPage : Url -> Page
urlToPage url =
  Parser.parse parser url
    |> Maybe.withDefault NotFound


parser : Parser (Page -> a) a
parser =
  Parser.oneOf
    [ Parser.map Folders Parser.top
    , Parser.map Gallery (s "gallery")
    , Parser.map SelectedPhoto (s "photos" </> Parser.string)
    ]


main : Program () Model Msg
main =
  Browser.application
    { init = \_ _ _ -> ( { page = Folders }, Cmd.none )
    , onUrlRequest = \_ -> Debug.todo "Handle URL requests"
    , onUrlChange = \_ -> Debug.todo "Handle URL changes"
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
