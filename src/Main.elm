port module Main exposing (..)

-- import Iso8601 exposing (toTime)

import Iso8601
import Platform exposing (worker)
import Rss exposing (Channel, Item)
import Time


port print : String -> Cmd msg


nodeProgram : a -> Program (Maybe String) () ()
nodeProgram _ =
    worker
        { init = \flags -> ( (), print cratePodcastRss )
        , update = \() -> \() -> ( (), Cmd.none )
        , subscriptions = \() -> Sub.none
        }


main : Program (Maybe String) () ()
main =
    nodeProgram doTheMagic


doTheMagic =
    print cratePodcastRss


cratePodcastRss : String
cratePodcastRss =
    Rss.generate
        channel


channel : Channel
channel =
    { title = "Boostagrams"
    , description = "This podcast has zero content (almost). It will consists of the host (Ville) reading out loud any boostagrams the show receives. So keep 'em coming!"
    , url = "https://elm-pages.com/blog"
    , lastBuildTime = time "20201-10-13T15:33:33"
    , generator = Just "elm-rss/elm-podcast-rss"
    , items =
        items
    , siteUrl = "https://elm-pages.com"
    , locked = { locked = True, owner = "owner@example.com" }
    , funding =
        [ { url = "example.com/paypal"
          , text = "Gimme money"
          }
        , { url = "example.com/donate"
          , text = "Donate value"
          }
        ]
    , persons =
        [ { name = "Ville"
          , role = Just "Host"
          , group = Nothing
          , href = Nothing
          , img = Just "example.com/person/img.png"
          }
        ]
    , location =
        Just
            { name = "Finland"
            , osm = Nothing
            , geo = Nothing
            }
    , license =
        { name = "WTFPL"
        , url = "http://www.wtfpl.net/"
        }
    , value =
        { type_ = "lightning"
        , method = "keysend"
        , recipients =
            [ { name = "Ville"
              , type_ = "Podcaster"
              , address = "03c457fafbc8b91b462ef0b8f61d4fd96577a4b58c18b50e59621fd0f41a8ae1a4"
              , split = 100
              }
            ]
        }
    }


items : List Item
items =
    [ { title = "Generating files with elm-pages"
      , description = "<h2>Learn all about the new generateFiles hook.</h2>"
      , url = "blog/generate-files"
      , categories = [ "Testing", "Boostagrams", "Humor" ]
      , author = "Ville from Finland"
      , pubDate = Rss.DateTime (Time.millisToPosix 1591330166000)
      , content = Just "Not encoded content"
      , contentEncoded = Just "<h1>Should there be encoded content here?</h1>"
      , transcript =
            [ { url = "todo.str"
              , type_ = "str"
              , rel = Nothing
              , language = Nothing
              }
            ]
      , enclosure =
            Just
                { url = "https://example.com/image.jpg"
                , mimeType = "image/jpeg"
                , bytes = Nothing
                }
      , chapters =
            { url = "Example.com/chapters"
            , type_ = "application/json+chapters"
            }
      , persons = []
      , soundbites = [ { startTime = 5.15, duration = 5.5, title = "The soundbite" } ]
      , location = Nothing
      , season =
            { name = "Zero content - All boostagram"
            , number = 1
            }
      , episode =
            { number = 1.0
            , display = "Boost I"
            }
      , alternateEnclosures =
            [ { type_ = "audio/mpeg"
              , length = 5000
              , title = "Smaller audio"
              , sources =
                    [ "TODO/small.mp3" ]
              }
            ]
      }
    ]


time : String -> Time.Posix
time str =
    case Iso8601.toTime str of
        Result.Ok resTime ->
            resTime

        Result.Err err ->
            Time.millisToPosix 0
