module Rss exposing
    ( generate, Item, DateOrTime(..), Enclosure
    , Channel
    )

{-| Build a feed following the RSS 2.0 format <https://validator.w3.org/feed/docs/rss2.html>.
<http://www.rssboard.org/rss-specification>

@docs generate, Item, DateOrTime, Enclosure

-}

import Date
import Dict
import Imf.DateTime
import Path
import Time
import Xml
import Xml.Encode exposing (..)


{-| Can be one of:

  - `Rss.Date` - a `Date` value from `justinmimbs/date`, or
  - `Rss.DateTime` - a `Time.Posix` from `elm/time`.

If you pass in an `Rss.Date`, it will format it at `00:00:00 GMT` on the given date (start of day).
If you pass in an `Rss.DateTime`, it will format the time in UTC.

-}
type DateOrTime
    = Date Date.Date
    | DateTime Time.Posix


{-| Data representing an RSS feed item.

contentEncoded - Use this to
[add HTML content](https://developer.mozilla.org/en-US/docs/Archive/RSS/Article/Why_RSS_Content_Module_is_Popular_-_Including_HTML_Contents)
in a `<content:encoded>` tag in the RSS feed. Some feed readers
will use this field if present to render HTML. Note that Elm doesn't
provide a way to turn `Html.Html` values into a `String`.

You can use [`zwilias/elm-html-string`](https://package.elm-lang.org/packages/zwilias/elm-html-string/latest/) to
render HTML using a drop-in replacement API and then turn that into a String.

Here's an example that shows how to [render to an HTML String
using `dillonkearns/elm-markdown`](https://github.com/dillonkearns/elm-markdown/blob/2650722990d61c8948d7998168d3bceb0ee6f298/spec-tests/OutputMarkdownHtml.elm).

<https://demo.ghost.io/author/lewis/rss/>

Encoding

enclosure - link to an attached file

<https://www.rssboard.org/rss-enclosures-use-case>

-}
type alias Item =
    { title : String
    , description : String
    , url : String
    , categories : List String
    , author : String
    , pubDate : DateOrTime
    , content : Maybe String
    , contentEncoded : Maybe String
    , transcript : List Transcript
    , chapters : { url : String, type_ : String }
    , persons : List Person
    , soundbites : List Soundbite
    , season : Season
    , episode : Episode
    , location :
        Maybe
            { name : String
            , geo : Maybe String
            , osm : Maybe String
            }
    , enclosure :
        Maybe
            { url : String
            , mimeType : String
            , bytes : Maybe Int
            }

    {-
       TODO consider adding these
          - lat optional number The latitude coordinate of the item.
          - long optional number The longitude coordinate of the item.
          - custom_elements optional array Put additional elements in the item (node-xml syntax)
    -}
    , alternateEnclosures :
        List AltEnclosure
    }


type alias AltEnclosure =
    { type_ : String
    , length : Int
    , title : String
    , sources : List String
    }


type alias Transcript =
    { url : String
    , type_ : String
    , language : Maybe String
    , rel : Maybe String
    }


{-| Represents a linked file.

<https://validator.w3.org/feed/docs/rss2.html#ltenclosuregtSubelementOfLtitemgt>

-}
type alias Enclosure =
    { url : String
    , mimeType : String
    , bytes : Maybe Int
    }


type alias Channel =
    { title : String
    , description : String
    , url : String
    , lastBuildTime : Time.Posix
    , generator : Maybe String
    , items : List Item
    , siteUrl : String
    , locked : { owner : String, locked : Bool }
    , funding : List { url : String, text : String }
    , persons : List Person
    , location : Maybe Location
    , license : License
    , value : Value
    }


{-| Generate an RSS feed from feed metadata and a list of `Rss.Item`s.
-}
generate :
    Channel
    -> String
generate feed =
    object
        [ ( "rss"
          , Dict.fromList
                [ ( "xmlns:dc", string "http://purl.org/dc/elements/1.1/" )
                , ( "xmlns:content", string "http://purl.org/rss/1.0/modules/content/" )
                , ( "xmlns:atom", string "http://www.w3.org/2005/Atom" )
                , ( "xmlns:podcast", string "https://podcastindex.org/namespace/1.0" )
                , ( "version", string "2.0" )
                ]
          , object
                [ ( "channel"
                  , Dict.empty
                  , [ [ keyValue "title" feed.title
                      , keyValue "description" feed.description
                      , keyValue "link" feed.url

                      --<atom:link href="http://dallas.example.com/rss.xml" rel="self" type="application/rss+xml" />
                      , keyValue "lastBuildDate" <| Imf.DateTime.fromPosix Time.utc feed.lastBuildTime
                      , object
                            [ ( "podcast:locked"
                              , Dict.fromList [ ( "owner", string feed.locked.owner ) ]
                              , string
                                    (if feed.locked.locked then
                                        "yes"

                                     else
                                        "no"
                                    )
                              )
                            ]
                      , licenseXml feed.license
                      , valueXml feed.value
                      ]
                    , [ feed.generator |> Maybe.map (keyValue "generator") ] |> List.filterMap identity
                    , List.map fundingXml feed.funding
                    , List.map personXml feed.persons
                    , [ feed.location |> Maybe.map locationXml ] |> List.filterMap identity
                    , List.map (itemXml feed.siteUrl) feed.items
                    ]
                        |> List.concat
                        |> list
                  )
                ]
          )
        ]
        |> Xml.Encode.encode 2


chaptersXml chapters =
    object
        [ ( "podcast:chapters"
          , Dict.fromList
                [ ( "url", string chapters.url )
                , ( "type", string chapters.type_ )
                ]
          , Xml.Encode.string ""
          )
        ]


type alias License =
    { name : String
    , url : String
    }


licenseXml : License -> Xml.Value
licenseXml license =
    object
        [ ( "podcast:license"
          , Dict.fromList
                [ ( "url", string license.url )
                ]
          , Xml.Encode.string license.name
          )
        ]


fundingXml funding =
    object
        [ ( "podcast:funding"
          , Dict.fromList
                [ ( "url", string funding.url )
                ]
          , Xml.Encode.string funding.text
          )
        ]


type alias Location =
    { name : String
    , geo : Maybe String
    , osm : Maybe String
    }


locationXml : Location -> Xml.Value
locationXml location =
    object
        [ ( "podcast:location"
          , Dict.fromList
                ([ location.geo |> Maybe.map (\geo -> ( "geo", string geo ))
                 , location.osm |> Maybe.map (\osm -> ( "osm", string osm ))
                 ]
                    |> List.filterMap identity
                )
          , Xml.Encode.string location.name
          )
        ]


type alias Soundbite =
    { title : String
    , startTime : Float
    , duration : Float
    }


soundbiteXml : Soundbite -> Xml.Value
soundbiteXml soundbite =
    object
        [ ( "podcast:soundbite"
          , Dict.fromList
                [ ( "startTime", soundbite.startTime |> String.fromFloat |> string )
                , ( "endTime", soundbite.duration |> String.fromFloat |> string )
                ]
          , Xml.Encode.string soundbite.title
          )
        ]


type alias Season =
    { number : Int
    , name : String
    }


seasonXml : Season -> Xml.Value
seasonXml season =
    object
        [ ( "podcast:season"
          , Dict.fromList
                [ ( "name", season.name |> string )
                ]
          , season.number |> String.fromInt |> string
          )
        ]


type alias Episode =
    { display : String
    , number : Float
    }


episodeXml : Episode -> Xml.Value
episodeXml episode =
    object
        [ ( "podcast:episode"
          , Dict.fromList
                [ ( "display", episode.display |> string )
                ]
          , episode.number |> String.fromFloat |> string
          )
        ]


itemXml : String -> Item -> Xml.Value
itemXml siteUrl item =
    object
        [ ( "item"
          , Dict.empty
          , list
                ([ keyValue "title" item.title
                 , keyValue "description" item.description
                 , keyValue "link" (Path.join [ siteUrl, item.url ])
                 , keyValue "guid" (Path.join [ siteUrl, item.url ])
                 , keyValue "pubDate" (formatDateOrTime item.pubDate)
                 , chaptersXml item.chapters
                 , seasonXml item.season
                 , episodeXml item.episode
                 ]
                    ++ List.map encodeTranscript item.transcript
                    ++ List.map encodeCategory item.categories
                    ++ List.map personXml item.persons
                    ++ List.map soundbiteXml item.soundbites
                    ++ List.map altEnclosureXml item.alternateEnclosures
                    ++ ([ item.content |> Maybe.map (\content -> keyValue "content" content)
                        , item.contentEncoded |> Maybe.map (\content -> keyValue "content:encoded" (wrapInCdata content))
                        , item.enclosure |> Maybe.map encodeEnclosure
                        , item.location |> Maybe.map locationXml

                        --<enclosure url="https://example.com/image.jpg" length="0" type="image/jpeg"/>
                        ]
                            |> List.filterMap identity
                       )
                )
          )
        ]


altEnclosureXml : AltEnclosure -> Xml.Value
altEnclosureXml altEnclosure =
    object
        [ ( "podcast:alternateEnclosure"
          , Dict.fromList
                [ ( "type", string altEnclosure.type_ )
                , ( "length", altEnclosure.length |> String.fromInt |> string )
                , ( "title", altEnclosure.title |> string )
                ]
          , list
                (altEnclosure.sources
                    |> List.map
                        (\uri ->
                            object
                                [ ( "source"
                                  , Dict.fromList [ ( "uri", string uri ) ]
                                  , string ""
                                  )
                                ]
                        )
                )
          )
        ]


type alias Value =
    { type_ : String
    , method : String
    , recipients : List ValueRecipient
    }


type alias ValueRecipient =
    { name : String
    , type_ : String
    , address : String
    , split : Int
    }


valueXml : Value -> Xml.Value
valueXml value =
    object
        [ ( "podcast:value"
          , Dict.fromList
                [ ( "type", string value.type_ )
                , ( "method", string value.method )
                ]
          , list <| List.map valueRecipientXml value.recipients
          )
        ]


valueRecipientXml : ValueRecipient -> Xml.Value
valueRecipientXml recipient =
    object
        [ ( "podcast:valueRecipient"
          , Dict.fromList
                [ ( "name", string recipient.name )
                , ( "type", string recipient.type_ )
                , ( "address", string recipient.address )
                , ( "split", recipient.split |> String.fromInt |> string )
                ]
          , string ""
          )
        ]


type alias Person =
    { name : String
    , role : Maybe String
    , group : Maybe String
    , img : Maybe String
    , href : Maybe String
    }


personXml : Person -> Xml.Value
personXml person =
    object
        [ ( "podcast:person"
          , Dict.fromList
                ([ person.role |> Maybe.map (\role -> ( "role", string role ))
                 , person.group |> Maybe.map (\group -> ( "group", string group ))
                 , person.img |> Maybe.map (\img -> ( "img", string img ))
                 , person.href |> Maybe.map (\href -> ( "href", string href ))
                 ]
                    |> List.filterMap identity
                )
          , Xml.Encode.string person.name
          )
        ]


encodeCategory : String -> Xml.Value
encodeCategory category =
    Xml.Encode.object
        [ ( "category", Dict.empty, Xml.Encode.string category )
        ]


encodeTranscript : Transcript -> Xml.Value
encodeTranscript transcript =
    object
        [ ( "podcast:transcript"
          , Dict.fromList
                ([ ( "url", string transcript.url )
                 , ( "type", string transcript.type_ )
                 ]
                    ++ (transcript.rel
                            |> Maybe.map (\rel -> [ ( "rel", string rel ) ])
                            |> Maybe.withDefault []
                       )
                    ++ (transcript.language
                            |> Maybe.map (\lang -> [ ( "lang", string lang ) ])
                            |> Maybe.withDefault []
                       )
                )
          , Xml.Encode.string ""
          )
        ]


encodeEnclosure : Enclosure -> Xml.Value
encodeEnclosure enclosure =
    Xml.Encode.object
        [ ( "enclosure"
          , Dict.fromList
                [ ( "url", string enclosure.url )
                , ( "length", string "0" )
                , ( "type", string enclosure.mimeType )
                ]
          , Xml.Encode.null
          )
        ]


wrapInCdata content =
    "<![CDATA[" ++ content ++ "]]>"


formatDateOrTime : DateOrTime -> String
formatDateOrTime dateOrTime =
    case dateOrTime of
        Date date ->
            formatDate date

        DateTime posix ->
            Imf.DateTime.fromPosix Time.utc posix


formatDate : Date.Date -> String
formatDate date =
    Date.format "EEE, dd MMM yyyy" date
        ++ " 00:00:00 GMT"


keyValue : String -> String -> Xml.Value
keyValue key value =
    object [ ( key, Dict.empty, string value ) ]
