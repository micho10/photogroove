module Main exposing (main)

import Browser exposing (application, Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, footer,h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)

import PhotoFolders as Folders
import PhotoGallery as Gallery


type alias Model =
  { page : Page
  , key : Nav.Key
  , version : Float
  }


type Page
    = GalleryPage Gallery.Model
    | FoldersPage Folders.Model
    | NotFound


type Route
    = Gallery
    | Folders
    | SelectedPhoto String


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

    navLink : Route -> { url : String, caption : String } -> Html msg
    navLink route { url, caption } =
      li [ classList [ ( "active", isActive { link = route, page = page } ) ] ]
        [ a [ href url ] [ text caption ] ]
  in
  nav [] [ logo, links ]


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
  case ( link,            page          ) of
  -------------------------------------------------
       ( Gallery,         GalleryPage _ ) -> True
       ( Gallery,         _             ) -> False
       ( Folders,         FoldersPage _ ) -> True
       ( Folders,         _             ) -> False
       ( SelectedPhoto _, _             ) -> False


type Msg
  = ClickedLink Browser.UrlRequest
  | ChangedUrl Url



update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    ClickedLink urlRequest ->
      case urlRequest of
        Browser.External href ->
          ( model, Nav.load href )

        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

    ChangedUrl url ->
      ( { model | page = urlToPage model.version url }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : Float -> Url -> Nav.Key -> ( Model, Cmd msg )
init version url key =
  ( { page = urlToPage version url, key = key, version = version }
  , Cmd.none
  )


urlToPage : Float -> Url -> Page
urlToPage version url =
  case Parser.parse parser url of
    Just Gallery ->
      GalleryPage (Tuple.first (Gallery.init version) )

    Just Folders ->
      FoldersPage (Tuple.first (Folders.init Nothing) )

    Just (SelectedPhoto filename) ->
      FoldersPage (Tuple.first (Folders.init (Just filename)) )

    Nothing ->
      NotFound


parser : Parser (Route -> a) a
parser =
  Parser.oneOf
    [ Parser.map Folders Parser.top
    , Parser.map Gallery (s "gallery")
    , Parser.map SelectedPhoto (s "photos" </> Parser.string)
    ]


main : Program Float Model Msg
main =
  Browser.application
    { init = init
    , onUrlRequest = ClickedLink
    , onUrlChange = ChangedUrl
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
