module Rule exposing (Rule, getByIndex, getRandom)

import Json.Decode exposing (Decoder)
import List.Nonempty as Nonempty exposing (Nonempty)
import Random exposing (Generator)
import Task


type alias Rule =
    { game : String
    , rule : String
    , url : String
    }


getRandom : (Result String ( Int, Rule ) -> msg) -> Cmd msg
getRandom msgWrapper =
    case listOfRules of
        Err error ->
            Task.attempt msgWrapper (Task.fail error)

        Ok rules ->
            Random.int 0 (Nonempty.length rules)
                |> Random.map (\index -> Ok ( index, Nonempty.get index rules ))
                |> Random.generate msgWrapper


getByIndex : Int -> (Result String ( Int, Rule ) -> msg) -> Cmd msg
getByIndex index msgWrapper =
    case listOfRules of
        Err error ->
            Task.attempt msgWrapper (Task.fail error)

        Ok rules ->
            Task.attempt msgWrapper
                (Task.succeed <|
                    ( modBy (Nonempty.length rules) index
                    , Nonempty.get index rules
                    )
                )


ruleDecoder : Decoder Rule
ruleDecoder =
    Json.Decode.map3 Rule
        (Json.Decode.at [ "game", "name" ] Json.Decode.string)
        (Json.Decode.oneOf
            [ Json.Decode.at [ "rule", "paraphrase" ] Json.Decode.string
            , Json.Decode.at [ "rule", "quote" ] Json.Decode.string
            ]
        )
        (Json.Decode.at [ "game", "url" ] Json.Decode.string)


listOfRules : Result String (Nonempty Rule)
listOfRules =
    Json.Decode.decodeString (Json.Decode.list ruleDecoder) rulesJson
        |> Result.mapError Json.Decode.errorToString
        |> Result.map Nonempty.fromList
        |> Result.andThen (Result.fromMaybe "List of rules is empty!")


rulesJson : String
rulesJson =
    """
[
  {
    "rule": {
      "quote": "The starting player is the player who most recently constructed something (in real life).",
      "paraphrase": "The player who most recently constructed something (in real life) begins."
    },
    "game": {
      "name": "Tiny Towns",
      "url": "https://boardgamegeek.com/boardgame/265736/tiny-towns"
    }
  },
  {
    "rule": {
      "quote": "The player who proves best able - by general consent - to do a polar bear impression becomes the Starting Player.",
      "paraphrase": "The player with the the best polar bear impression begins."
    },
    "game": {
      "name": "Ice Flow",
      "url": "https://boardgamegeek.com/boardgame/31133/ice-flow"
    }
  },
  {
    "rule": {
      "quote": "The player who last ate something coconut flavored starts the round and play continues clockwise.",
      "paraphrase": "The player who last ate something coconut flavored begins."
    },
    "game": {
      "name": "Tiki Topple",
      "url": "https://boardgamegeek.com/boardgame/34373/tiki-topple"
    }
  },
  {
    "rule": {
      "quote": "The hairiest player begins a preparatory round, […]",
      "paraphrase": "The hairiest player begins."
    },
    "game": {
      "name": "Mammoth Hunters (aka, Eiszeit)",
      "url": "https://boardgamegeek.com/boardgame/5767/mammoth-hunters"
    }
  },
  {
    "rule": {
      "quote": "The person who last has been onboard of a ship starts, the other players follow in a clock-wise order.",
      "paraphrase": "The player who was last onboard a ship begins."
    },
    "game": {
      "name": "Atlantic Star",
      "url": "https://boardgamegeek.com/boardgame/2570/atlantic-star"
    }
  },
  {
    "rule": {
      "quote": "The player with the worst medical history leads the first trick.",
      "paraphrase": "The player with the worst medical history begins."
    },
    "game": {
      "name": "Quacksalbe",
      "url": "https://boardgamegeek.com/boardgame/819/quacksalbe"
    }
  },
  {
    "rule": {
      "quote": "The meanest player begins the game.",
      "paraphrase": "The meanest player begins."
    },
    "game": {
      "name": "Vegas",
      "url": "https://boardgamegeek.com/boardgame/1255/vegas"
    }
  },
  {
    "rule": {
      "quote": "Whoever can stay under water the longest goes first. If that cannot be determined now, the oldest player starts.",
      "paraphrase": "The player who can stay under water the longest begins."
    },
    "game": {
      "name": "The Reef",
      "url": "https://boardgamegeek.com/boardgame/509/reef"
    }
  },
  {
    "rule": {
      "quote": "The player with the most colorful clothing begins."
    },
    "game": {
      "name": "Flowerpower",
      "url": "https://boardgamegeek.com/boardgame/1545/flowerpower"
    }
  },
  {
    "rule": {
      "quote": "The player with the largest cowboy boots (or other shoes) begins, the others follow in clockwise direction. ",
      "paraphrase": "The player with the largest cowboy boots (or other shoes) begins."
    },
    "game": {
      "name": "Abilene",
      "url": "https://boardgamegeek.com/boardgame/906/abilene"
    }
  },
  {
    "rule": {
      "quote": "The player who can sing the sweetest high C begins."
    },
    "game": {
      "name": "Maestro",
      "url": "https://boardgamegeek.com/boardgame/569/maestro"
    }
  },
  {
    "rule": {
      "quote": "Whoever has last reached the peak of Mount Everest using nothing but blue and white checkered stilts carved from the wood of a Mammoth tree is declared the Starting Player. In case of a tie, the wisest player of the group begins the game.",
      "paraphrase": "The player who last reached the peak of Mount Everest using nothing but blue and white checkered stilts carved from the wood of a Mammoth tree begins. In case of a tie, choose the wisest player of the group."
    },
    "game": {
      "name": "The Bridges of Shangri-La",
      "url": "https://boardgamegeek.com/boardgame/8190/bridges-shangri-la"
    }
  },
  {
    "rule": {
      "quote": "The last person to feed a live duck starts the game and play proceeds clockwise",
      "paraphrase": "The last player to feed a live duck begins."
    },
    "game": {
      "name": "Duck, Duck, Bruce",
      "url": "https://boardgamegeek.com/boardgame/2114/duck-duck-bruce"
    }
  },
  {
    "rule": {
      "quote": "The person with the longest hair goes first.",
      "paraphrase": "The player with the longest hair begins."
    },
    "game": {
      "name": "Aquarius",
      "url": "https://boardgamegeek.com/boardgame/814/aquarius"
    }
  },
  {
    "rule": {
      "quote": "Give the starting leader card to the leader (player) who had the least amount of sleep last night.",
      "paraphrase": "The player who had the least amount of sleep last night begins."
    },
    "game": {
      "name": "Zombie State: Diplomacy of the Dead ",
      "url": "https://boardgamegeek.com/boardgame/61484/zombie-state-diplomacy-dead"
    }
  },
  {
    "rule": {
      "quote": "The Player who has travelled to the farthest destination in the last year becomes the First Player and receives immediately the Bartolomeu Dias Tile and 2 Victory Points.",
      "paraphrase": "The player who travelled to the farthest destination in the last year begins."
    },
    "game": {
      "name": "Vasco da Gama",
      "url": "https://boardgamegeek.com/boardgame/41002/vasco-da-gama"
    }
  },
  {
    "rule": {
      "quote": "The player that most recently visited a port in real life begins the game.",
      "paraphrase": "The player that most recently visited a port in real life begins."
    },
    "game": {
      "name": "Cargo Noir",
      "url": "https://boardgamegeek.com/boardgame/90305/cargo-noir"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently been abducted by aliens begins. In case of a tie, the player who has most recently visited a farm can go first. If you still can't figure it out, pick someone to go first.",
      "paraphrase": "The player who was most recently abducted by aliens begins. In case of a tie, choose the player who most recently visited a farm."
    },
    "game": {
      "name": "Pasture 51: They Came for our Angus",
      "url": "https://boardgamegeek.com/boardgame/152825/pasture-51-they-came-our-angus"
    }
  },
  {
    "rule": {
      "quote": "The starting player marker goes to the player who paid for something most recently.",
      "paraphrase": "The player who paid for something most recently begins."
    },
    "game": {
      "name": "Last Will",
      "url": "https://boardgamegeek.com/boardgame/97842/last-will"
    }
  },
  {
    "rule": {
      "quote": "To determine who goes first, all players will attempt to guess the current time. Whoever comes closest starts the game.",
      "paraphrase": "The player who comes closest when guessing the current time begins."
    },
    "game": {
      "name": "Chrononauts",
      "url": "https://boardgamegeek.com/boardgame/815/chrononauts"
    }
  },
  {
    "rule": {
      "quote": "The first player at the beginning of the game is the player who most recently had a great idea (or random if you prefer).",
      "paraphrase": "The player who most recently had a great idea."
    },
    "game": {
      "name": "Tempus",
      "url": "https://boardgamegeek.com/boardgame/17161/tempus"
    }
  },
  {
    "rule": {
      "quote": "The player who can tell the most honest fact his been through recives the \\"Sheriff\\" token.",
      "paraphrase": "The player who can tell the most honest fact they have been through begins."
    },
    "game": {
      "name": "Robin Hood",
      "url": "https://boardgamegeek.com/boardgame/104640/robin-hood"
    }
  },
  {
    "rule": {
      "quote": "The nicest player gets the Starting Player Token. (Or you can choose the starting player randomly.)",
      "paraphrase": "The nicest player begins."
    },
    "game": {
      "name": "Dungeon Lords",
      "url": "https://boardgamegeek.com/boardgame/45315/dungeon-lords"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently fed a pet gets the Starting Player Token. Of course, you can also choose the starting player randomly, arbitrarily, or by fiat. Just pick somebody.",
      "paraphrase": "The player who most recently fed a pet begins."
    },
    "game": {
      "name": "Dungeon Petz",
      "url": "https://boardgamegeek.com/boardgame/97207/dungeon-petz"
    }
  },
  {
    "rule": {
      "quote": "The player with either (A) the best mustache, or (B) the longest hair goes First. If one player has a very nice mustache and the other player has the longest hair, settle the issue with an arm wrestling match.",
      "paraphrase": "The player with either (A) the best mustache, or (B) the longest hair begins. If one player has a very nice mustache and the other player has the longest hair, settle the issue with an arm wrestling match."
    },
    "game": {
      "name": "The Great Heartland Hauling Co.",
      "url": "https://boardgamegeek.com/boardgame/111417/great-heartland-hauling-co"
    }
  },
  {
    "rule": {
      "quote": "The player who last visited an island goes first and play continues to the left.",
      "paraphrase": "The player who last visited an island begins."
    },
    "game": {
      "name": "Forbidden Island",
      "url": "https://boardgamegeek.com/boardgame/65244/forbidden-island"
    }
  },
  {
    "rule": {
      "quote": "The thirstiest player goes first and play continues to the left.",
      "paraphrase": "The thirstiest player begins."
    },
    "game": {
      "name": "Forbidden Desert",
      "url": "https://boardgamegeek.com/boardgame/136063/forbidden-desert"
    }
  },
  {
    "rule": {
      "quote": "The Mission Leader token is given to the hairiest player.",
      "paraphrase": "The hairiest player begins."
    },
    "game": {
      "name": "The Grizzled",
      "url": "https://boardgamegeek.com/boardgame/171668/grizzled"
    }
  },
  {
    "rule": {
      "quote": "Each player receives a game sheet and a pen. The smartest among them takes the six dice and starts the game.",
      "paraphrase": "The smartest player begins."
    },
    "game": {
      "name": "That's Pretty Clever (aka Ganz schön clever)",
      "url": "https://boardgamegeek.com/boardgame/244522/s-pretty-clever"
    }
  },
  {
    "rule": {
      "quote": "The player who was the last to eat dates will get the starting player token and takes the first turn. If neither player has eaten dates, the blue player starts.",
      "paraphrase": "The last player who ate dates begins."
    },
    "game": {
      "name": "Targi",
      "url": "https://boardgamegeek.com/boardgame/118048/targi"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently extracted DNA from a mosquito trapped in amber will be the first player. If no player has successfully accomplished that task, the player who most recently visited a theme park will be the first player.",
      "paraphrase": "The player who most recently extracted DNA from a mosquito trapped in amber begins. If no player has successfully accomplished that task, choose the player who most recently visited a theme park."
    },
    "game": {
      "name": "Dinosaur Island",
      "url": "https://boardgamegeek.com/boardgame/221194/dinosaur-island"
    }
  },
  {
    "rule": {
      "quote": "The start player is the player who was most recently on board a ship.",
      "paraphrase": "The player who was most recently on board a ship begins."
    },
    "game": {
      "name": "Maracaibo",
      "url": "https://boardgamegeek.com/boardgame/276025/maracaibo"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently visited Yokohama receives the Start Player Card and becomes the starting player. If this method does not work in determining a starting player, use any preferred method in choosing a starting player.",
      "paraphrase": "The player who most recently visited Yokohama begins."
    },
    "game": {
      "name": "Yokohama",
      "url": "https://boardgamegeek.com/boardgame/196340/yokohama"
    }
  },
  {
    "rule": {
      "quote": "The player who has seen the film “The Martian” the most times is the first player.",
      "paraphrase": "The player who has seen the film “The Martian” the most times begins."
    },
    "game": {
      "name": "On Mars",
      "url": "https://boardgamegeek.com/boardgame/184267/mars"
    }
  },
  {
    "rule": {
      "quote": "The last person to cook something goes first.",
      "paraphrase": "The last person to cook something begins."
    },
    "game": {
      "name": "The Quacks of Quedlinburg",
      "url": "https://boardgamegeek.com/boardgame/244521/quacks-quedlinburg"
    }
  },
  {
    "rule": {
      "quote": "The player who last used a needle begins."
    },
    "game": {
      "name": "Patchwork",
      "url": "https://boardgamegeek.com/boardgame/163412/patchwork"
    }
  },
  {
    "rule": {
      "quote": "The player who visited Lisboa most recently is the starting player, and takes the Starting Player marker.",
      "paraphrase": "The player who visited Lisboa most recently begins."
    },
    "game": {
      "name": "Lisboa",
      "url": "https://boardgamegeek.com/boardgame/161533/lisboa"
    }
  },
  {
    "rule": {
      "quote": "The sneakiest player gets to go first (or you may choose randomly).",
      "paraphrase": "The sneakiest player begins."
    },
    "game": {
      "name": "Clank!: A Deck-Building Adventure",
      "url": "https://boardgamegeek.com/boardgame/201808/clank-deck-building-adventure"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently been to another city goes first. Give that player the First Player marker.",
      "paraphrase": "The player who has most recently been to another city begins."
    },
    "game": {
      "name": "Lords of Waterdeep",
      "url": "https://boardgamegeek.com/boardgame/110327/lords-waterdeep"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently embarked on a voyage around the world is the start player.",
      "paraphrase": "The player who most recently embarked on a voyage around the world begins."
    },
    "game": {
      "name": "The Voyages of Marco Polo",
      "url": "https://boardgamegeek.com/boardgame/171623/voyages-marco-polo"
    }
  },
  {
    "rule": {
      "quote": "The most humble player goes first.",
      "paraphrase": "The most humble player begins."
    },
    "game": {
      "name": "Everdell",
      "url": "https://boardgamegeek.com/boardgame/199792/everdell"
    }
  },
  {
    "rule": {
      "quote": "The Starting Player is the player who lives closest to water.",
      "paraphrase": "The player who lives closest to water begins."
    },
    "game": {
      "name": "Le Havre",
      "url": "https://boardgamegeek.com/boardgame/35677/le-havre"
    }
  },
  {
    "rule": {
      "quote": "Give the Starting Player Marker to the player who has spent the least time on planet Terra, in the Sol system.",
      "paraphrase": "The player who has spent the least time on planet Terra, in the Sol system, begins."
    },
    "game": {
      "name": "Eclipse",
      "url": "https://boardgamegeek.com/boardgame/72125/eclipse"
    }
  },
  {
    "rule": {
      "quote": "Give the Starting Player Marker to the player who most recently sacrificed something. In case of a tie, give the Starting Player Marker to the player who volunteers for the next sacrifice.",
      "paraphrase": "The player who most recently sacrificed something begins. In case of a tie, choose the player who volunteers for the next sacrifice."
    },
    "game": {
      "name": "Tzolk'in: The Mayan Calendar",
      "url": "https://boardgamegeek.com/boardgame/126163/tzolk-mayan-calendar"
    }
  },
  {
    "rule": {
      "quote": "Give the First Player token to the player who was born furthest to the north.",
      "paraphrase": "The player who was born furthest to the north begins."
    },
    "game": {
      "name": "Blood Rage",
      "url": "https://boardgamegeek.com/boardgame/170216/blood-rage"
    }
  },
  {
    "rule": {
      "quote": "The player whose ears are the most pointed starts the game.",
      "paraphrase": "The player whose ears are the most pointed begins."
    },
    "game": {
      "name": "Small World",
      "url": "https://boardgamegeek.com/boardgame/40692/small-world"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently earned glory in battle receives the First Player Marker. (Alternately, select a starting player at random.)",
      "paraphrase": "The player who has most recently earned glory in battle begins."
    },
    "game": {
      "name": "Champions of Midgard",
      "url": "https://boardgamegeek.com/boardgame/172287/champions-midgard"
    }
  },
  {
    "rule": {
      "quote": "Whoever sat down first at the table goes first.",
      "paraphrase": "The player who sat down first at the table begins."
    },
    "game": {
      "name": "The Quest for El Dorado",
      "url": "https://boardgamegeek.com/boardgame/217372/quest-el-dorado"
    }
  },
  {
    "rule": {
      "quote": "The player who has visited the most European countries in his lifetime begins the game.",
      "paraphrase": "The player who has visited the most European countries in their lifetime begins."
    },
    "game": {
      "name": "Ticket to Ride: Europe",
      "url": "https://boardgamegeek.com/boardgame/14996/ticket-ride-europe"
    }
  },
  {
    "rule": {
      "quote": "The player who is the most experienced traveler goes first.",
      "paraphrase": "The player who is the most experienced traveler begins."
    },
    "game": {
      "name": "Ticket to Ride",
      "url": "https://boardgamegeek.com/boardgame/9209/ticket-ride"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently rode a bike (youngest if tied).",
        "paraphrase": "The player who most recently rode a bike begins."
      },
    "game": {
      "name": "Flamme Rouge",
      "url": "https://boardgamegeek.com/boardgame/199478/flamme-rouge"
    }
  },
  {
    "rule": {
      "quote": "Whoever was most recently on a date goes first (if tied, the youngest player wins the tie).",
        "paraphrase": "The player who was most recently on a date begins."
      },
    "game": {
      "name": "Love Letter",
      "url": "https://boardgamegeek.com/boardgame/129622/love-letter"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently visited Portugal takes the starting player marker",
      "paraphrase": "The player who most recently visited Portugal begins."
    },
    "game": {
      "name": "Azul",
      "url": "https://boardgamegeek.com/boardgame/230802/azul"
    }
  },
  {
    "rule": {
      "quote": "Randomly select a player to be the Start Player and give them the Dice Bag. One suggested method is whoever most recently visited a cathedral.",
      "paraphrase": "The player who most recently visited a cathedral begins."
    },
    "game": {
      "name": "Sagrada",
      "url": "https://boardgamegeek.com/boardgame/199561/sagrada"
    }
  },
  {
    "rule": {
      "quote": "Whoever can balance on one foot like a flamingo for the longest time, starts the game.",
      "paraphrase": "The player who can balance on one foot like a flamingo for the longest time begins."
    },
    "game": {
      "name": "Animal Upon Animal",
      "url": "https://boardgamegeek.com/boardgame/17329/animal-upon-animal"
    }
  },
  {
    "rule": {
      "quote": "The last player to have seen a circus act becomes the first player for the first act and takes the first player token",
      "paraphrase": "The last player to have seen a circus act begins."
    },
    "game": {
      "name": "Meeple Circus",
      "url": "https://boardgamegeek.com/boardgame/193214/meeple-circus"
    }
  },
  {
    "rule": {
      "quote": "Give the starting player token to the player who was most recently in a laboratory.",
      "paraphrase": "The player who was most recently in a laboratory begins."
    },
    "game": {
      "name": "Alchemists",
      "url": "https://boardgamegeek.com/boardgame/161970/alchemists"
    }
  },
  {
    "rule": {
      "quote": "The player with the huskiest voice is the starting player. The player with the next huskiest voice is second, and so on.  Wizened old gamers may decide order randomly.",
      "paraphrase": "The player with the huskiest voice begins."
    },
    "game": {
      "name": "Snow Tails",
      "url": "https://boardgamegeek.com/boardgame/38054/snow-tails"
    }
  },
  {
    "rule": {
      "quote": "A player starts, the one who has seen most recently a kung-fu movie.",
      "paraphrase": "The player who has most recently seen a kung-fu movie begins."
    },
    "game": {
      "name": "Ghost Stories",
      "url": "https://boardgamegeek.com/boardgame/37046/ghost-stories"
    }
  },
  {
    "rule": {
      "quote": "The player who last read a history book receives the start player card.",
      "paraphrase": "The player who last read a history book begins."
    },
    "game": {
      "name": "Troyes",
      "url": "https://boardgamegeek.com/boardgame/73439/troyes"
    }
  },
  {
  "rule": {
      "quote": "The player who most recently has dug a planting bed in their garden takes the Starting player token and becomes the Starting player.",
      "paraphrase": "The player who most recently has dug a planting bed in their garden begins."
    },
    "game": {
      "name": "Terra Mystica",
      "url": "https://boardgamegeek.com/boardgame/120677/terra-mystic"
    }
  },
  {
    "rule": {
      "quote": "The player wearing the most green starts the game as the first player.",
      "paraphrase": "The player wearing the most green begins."
    },
    "game": {
      "name": "Kodama: The Tree Spirits",
      "url": "https://boardgamegeek.com/boardgame/181810/kodama-tree-spirits"
    }
  },
  {
    "rule": {
      "quote": "The first player is the last person to have put their feet in the sand.",
      "paraphrase": "The player who most recently put their feet in the sand begins."
    },
    "game": {
      "name": "Archaeology: The New Expedition",
      "url": "https://boardgamegeek.com/boardgame/191300/archaeology-new-expedition"
    }
  },
  {
    "rule": {
      "quote": "The player who last watered a plant is the start player.",
      "paraphrase": "The player who last watered a plant begins."
    },
    "game": {
      "name": "Arboretum",
      "url": "https://boardgamegeek.com/boardgame/140934/arboretum"
    }
  },
  {
    "rule": {
      "quote": "The player who was the last to have a knife in his hand throws both victims into the shaft of the hotel. After that, players in turn throw two each of their own clues, one after the other, into the shaft.",
      "paraphrase": "The player who last had a knife in their hand begins."
    },
    "game": {
      "name": "Mord im Arosa",
      "url": "https://boardgamegeek.com/boardgame/80006/mord-im-arosa"
    }
  },
  {
    "rule": {
      "quote": "In the first round, the narrator is the player who looks most like a moose. In case of a tie, choose the player who looks most like a rabbit. If players are still tied, choose the narrator randomly. Or just pick the narrator any way you like.",
      "paraphrase": "The player who looks most like a moose begins. In case of a tie, choose the player who looks most like a rabbit."
    },
    "game": {
      "name": "Bunny Bunny Moose Moose",
      "url": "https://boardgamegeek.com/boardgame/59149/bunny-bunny-moose-moose"
    }
  },
  {
    "rule": {
      "quote": "The player who can best walk like a mummy may begin.",
      "paraphrase": "The player who can best walk like a mummy begins."
    },
    "game": {
      "name": "Pharaoh's Gulo Gulo",
      "url": "https://boardgamegeek.com/boardgame/175088/pharaohs-gulo-gulo"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently went swimming now chooses the start player.",
      "paraphrase": "The player who most recently went swimming begins."
    },
    "game": {
      "name": "Reef Encounter",
      "url": "https://boardgamegeek.com/boardgame/12962/reef-encounter"
    }
  },
  {
    "rule": {
      "quote": "The player who last visited a church becomes the start player.",
      "paraphrase": "The player who last visited a church begins."
    },
    "game": {
      "name": "Hamburgum",
      "url": "https://boardgamegeek.com/boardgame/30381/hamburgum"
    }
  },
  {
    "rule": {
      "quote": "The player with the longest neck is the 1st player, the second longest is 2nd and so on.",
      "paraphrase": "The player with the longest neck begins."
    },
    "game": {
      "name": "Savannah Tails",
      "url": "https://boardgamegeek.com/boardgame/54507/savannah-tails"
    }
  },
  {
    "rule": {
      "quote": "The player who has read the most science fiction books goes first. If the players can’t agree who this is, select one player at random to go first.",
      "paraphrase": "The player who has read the most science fiction books begins."
    },
    "game": {
      "name": "Android",
      "url": "https://boardgamegeek.com/boardgame/39339/android"
    }
  },
  {
    "rule": {
      "quote": "The starting player is the person who most recently had a haircut (was sheared!).",
      "paraphrase": "The player who most recently had a haircut (was sheared!) begins."
    },
    "game": {
      "name": "Shear Panic",
      "url": "https://boardgamegeek.com/boardgame/18866/shear-panic"
    }
  },
  {
    "rule": {
      "quote": "The player with the best Egyptian credentials (a nose as famous as Cleopatra's, a mummified Crocodile pet, or an extensive hieroglyphic library) starts the game. Otherwise, the youngest player goes first.",
      "paraphrase": "The player with the best Egyptian credentials (e.g. a nose as famous as Cleopatra's, a mummified Crocodile pet, or an extensive hieroglyphic library) begins."
    },
    "game": {
      "name": "Cleopatra and the Society of Architects",
      "url": "https://boardgamegeek.com/boardgame/22141/cleopatra-and-society-architects"
    }
  },
  {
    "rule": {
      "quote": "The first player is the one who won the last game, or the one who can say “Braaaaains!” with the most feeling.",
      "paraphrase": "The player who can say “Braaaaains!” with the most feeling begins."
    },
    "game": {
      "name": "Zombie Dice",
      "url": "https://boardgamegeek.com/boardgame/62871/zombie-dice"
    }
  },
  {
    "game": {
      "name": "The Pillars of the Earth",
      "url": "https://boardgamegeek.com/boardgame/24480/pillars-earth"
    },
    "rule": {
      "quote": "The last person to visit a cathedral goes first, and takes the first piece of the cathedral.",
      "paraphrase": "The player who last visited a cathedral begins."
    }
  },
  {
    "game": {
      "name": "Sator Arepo Tenet Opera Rotas",
      "url": "https://boardgamegeek.com/boardgame/18500/sator-arepo-tenet-opera-rotas"
    },
    "rule": {
      "quote": "The starting player is the one who last said a Psalm!",
      "paraphrase": "The player who last said a Psalm begins."
    }
  },
  {
    "game": {
      "name": "Carcassonne: Amazonas",
      "url": "https://boardgamegeek.com/boardgame/206940/carcassonne-amazonas"
    },
    "rule": {
      "quote": "The last player to have gone on a boat ride is the first player (or, alternately, the youngest).",
      "paraphrase": "The last player to have gone on a boat ride begins."
    }
  },
  {
    "rule": {
      "quote": "The player, who most recently visited a weekly market is the starting player.",
      "paraphrase": "The player who most recently visited a weekly market begins."
    },
    "game": {
      "name": "Fresh Fish",
      "url": "https://boardgamegeek.com/boardgame/1017/fresh-fish"
    }
  },
  {
    "rule": {
      "quote": "Whoever likes to eat cheese the most begins by rolling the die.",
      "paraphrase": "The player who likes to eat cheese the most begins."
    },
    "game": {
      "name": "Viva Topo!",
      "url": "https://boardgamegeek.com/boardgame/8195/viva-topo"
    }
  },
  {
    "rule": {
      "quote": "The starting player is the player with the most money on them (the 'dearest player')",
      "paraphrase": "The player with the most money on them begins."
    },
    "game": {
      "name": "Antler Island",
      "url": "https://boardgamegeek.com/boardgame/30483/antler-island"
    }
  },
  {
    "rule": {
      "quote": "The player with the best penguin impression (as voted by anyone and everyone present) goes first.",
      "paraphrase": "The player with the best penguin impression begins."
    },
    "game": {
      "name": "Penguin Soccer",
      "url": "https://boardgamegeek.com/boardgame/30760/penguin-soccer"
    }
  },
  {
    "rule": {
      "quote": "The player who is loudest in shouting “Olé!” starts.",
      "paraphrase": "The player who is loudest in shouting “Olé!” begins."
    },
    "game": {
      "name": "Salamanca",
      "url": "https://boardgamegeek.com/boardgame/25409/salamanca"
    }
  },
  {
    "rule": {
      "quote": "The player who last visited London starts.",
      "paraphrase": "The player who last visited London begins."
    },
    "game": {
      "name": "Portobello Market",
      "url": "https://boardgamegeek.com/boardgame/27356/portobello-market"
    }
  },
  {
    "rule": {
      "quote": "The wisest player begins."
    },
    "game": {
      "name": "Himalaya",
      "url": "https://boardgamegeek.com/boardgame/3800/himalaya"
    }
  },
  {
    "rule": {
      "quote": "Zeus on the Loose is played in rounds, starting with the person whose first name is closest to Z.",
      "paraphrase": "The player whose first name is closest to Z begins."
    },
    "game": {
      "name": "Zeus on the Loose",
      "url": "https://boardgamegeek.com/boardgame/22864/zeus-loose"
    }
  },
  {
    "rule": {
      "quote": "Decide who will be the first storyteller. This could  be  the  oldest  player,  the  youngest player, or (as is traditional — at least among bearded game designers) the player with the longest beard.",
      "paraphrase": "The player with the longest beard begins."
    },
    "game": {
      "name": "Once Upon a Time: The Storytelling Card Game",
      "url": "https://boardgamegeek.com/boardgame/1234/once-upon-time-storytelling-card-game"
    }
  },
  {
    "rule": {
      "quote": "The player who last read a portion of the Bible will be the starting player.",
      "paraphrase": "The player who last read a portion of the Bible begins."
    },
    "game": {
      "name": "The Ark of the Covenant",
      "url": "https://boardgamegeek.com/boardgame/6779/ark-covenant"
    }
  },
  {
    "rule": {
      "quote": "The game is started by the player who looks most like a pirate.",
      "note": "Translated from the Norwegian rules: Spillet startes av spilleren som ligner mest på en pirat.",
      "paraphrase": "The player looks most like a pirate begins."
    },
    "game": {
      "name": "Cartagena",
      "url": "https://boardgamegeek.com/boardgame/826/cartagena"
    }
  },
  {
    "rule": {
      "quote": "The player who can make the best animal noise goes first, with the others following in clockwise order.",
      "paraphrase": "The player who can make the best animal noise begins."
    },
    "game": {
      "name": "Two by Two",
      "url": "https://boardgamegeek.com/boardgame/66608/two-two"
    }
  },
  {
    "rule": {
      "quote": "The player who has had the worst day goes first; if you’ve all had equally miserable days, the owner of the game takes the first turn.",
      "paraphrase": "The player who has had the worst day begins."
    },
    "game": {
      "name": "Gloom",
      "url": "https://boardgamegeek.com/boardgame/12692/gloom"
    }
  },
  {
    "rule": {
      "quote": "The player who can refrain longest from laughing starts the game.",
      "paraphrase": "The player who can refrain longest from laughing begins."
    },
    "game": {
      "name": "Gheos",
      "url": "https://boardgamegeek.com/boardgame/23730/gheos"
    }
  },
  {
    "rule": {
      "quote": "The player who lives in the largest house begins and lays down any number of his Coins onto the table. Play then continues clockwise around the table.",
      "paraphrase": "The player who lives in the largest house begins."
    },
    "game": {
      "name": "For Sale",
      "url": "https://boardgamegeek.com/boardgame/172/sale"
    }
  },
  {
    "rule": {
      "quote": "Dragon Farkle is played in turns, going clockwise, starting with the one who brought the most snacks.",
      "paraphrase": "The player who brought the most snacks begins."
    },
    "game": {
      "name": "Dragon Farkle",
      "url": "https://www.boardgamegeek.com/boardgame/128174/dragon-farkle"
    }
  },
  {
    "rule": {
      "quote": "The player with the best maniacal laugh shuffles the decks, and is the Teaching Assistant (TA) for the first turn.",
      "paraphrase": "The player with the best maniacal laugh begins."
    },
    "game": {
      "name": "Mad Scientist University",
      "url": "https://boardgamegeek.com/boardgame/21666/mad-scientist-university"
    }
  },
  {
    "rule": {
      "quote": "The player who does the most convincing imitation of a giant monster starts the game.",
      "paraphrase": "The player who does the most convincing imitation of a giant monster begins."
    },
    "game": {
      "name": "Terror in Meeple City (aka Rampage)",
      "url": "https://www.boardgamegeek.com/boardgame/97903/terror-meeple-city"
    }
  },
  {
    "rule": {
      "quote": "The most recent player to caress a sheep is the First Player and takes the First Player token.",
      "paraphrase": "The most recent player to caress a sheep begins."
    },
    "game": {
      "name": "Sheepland",
      "url": "https://www.boardgamegeek.com/boardgame/123576/sheepland"
    }
  },
  {
    "rule": {
      "quote": "The player with the longest ancestry gets the start-player marker and begins.",
      "note": "From translated rules on BGG",
      "paraphrase": "The player with the longest ancestry begins."
    },
    "game": {
      "name": "New England",
      "url": "https://www.boardgamegeek.com/boardgame/5406/new-england"
    }
  },
  {
    "rule": {
      "quote": "The player with the coldest nose tip will be the start player and takes the explorer pawn.",
      "paraphrase": "The player with the coldest nose tip begins."
    },
    "game": {
      "name": "Roll to the South Pole",
      "url": "https://www.boardgamegeek.com/boardgame/118330/roll-south-pole"
    }
  },
  {
    "rule": {
      "quote": "The tallest player starts the game.",
      "paraphrase": "The tallest player begins."
    },
    "game": {
      "name": "Takenoko",
      "url": "https://www.boardgamegeek.com/boardgame/70919/takenoko"
    }
  },
  {
    "rule": {
      "quote": "The player that most recently prepared a drink is the first player and takes the First Player token.",
      "paraphrase": "The player who most recently prepared a drink begins."
    },
    "game": {
      "name": "Potion Explosion",
      "url": "https://www.boardgamegeek.com/boardgame/180974/potion-explosion"
    }
  },
  {
    "rule": {
      "quote": "The player with the best acting skills and reading voice takes the Case Booklet and the Start Player Token.",
      "paraphrase": "The player with the best acting skills and reading voice begins."
    },
    "game": {
      "name": "Watson & Holmes",
      "url": "https://www.boardgamegeek.com/boardgame/182694/watson-holmes"
    }
  },
  {
    "rule": {
      "quote": "Give the Start Player marker to the player who most recently ate chocolate.",
      "paraphrase": "The player who most recently ate chocolate begins."
    },
    "game": {
      "name": "Chocolate Factory",
      "url": "https://www.boardgamegeek.com/boardgame/240567/chocolate-factory"
    }
  },
  {
    "rule": {
      "quote": "Starting with the player who most recently wore a top hat and in a clockwise order, each player chooses a Magician with a Favorite Trick category not chosen before, then receives the respective Player Game Board and the following: […]",
      "paraphrase": "The player who most recently wore a top hat begins."
    },
    "game": {
      "name": "Trickerion: Legends of Illusion",
      "url": "https://www.boardgamegeek.com/boardgame/163068/trickerion-legends-illusion"
    }
  },
  {
    "rule": {
      "quote": "Whoever was most recently up a tower is the Starting Player and receives 2 white Bricks.",
      "paraphrase": "The player who was most recently up a tower begins."
    },
    "game": {
      "name": "Firenze",
      "url": "https://www.boardgamegeek.com/boardgame/75449/firenze"
    }
  },
  {
    "rule": {
      "quote": "The First Player marker goes to the player who last moved to a new apartment/house.",
      "paraphrase": "The player who last moved to a new apartment/house begins."
    },
    "game": {
      "name": "Rise to Nobility",
      "url": "https://www.boardgamegeek.com/boardgame/218293/rise-nobility"
    }
  },
  {
    "rule": {
      "quote": "The player who last rolled a die in a game takes the starting player token.",
      "paraphrase": "The player who last rolled a die in a game begins."
    },
    "game": {
      "name": "Dice City",
      "url": "https://www.boardgamegeek.com/boardgame/179572/dice-city"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently rode a train is first player.",
      "paraphrase": "The player who most recently rode a train begins."
    },
    "game": {
      "name": "Isle of Trains",
      "url": "https://www.boardgamegeek.com/boardgame/154906/isle-trains"
    }
  },
  {
    "rule": {
      "quote": "The player who has at some point travelled (closest) to the Forbidden City in China starts the game.",
      "paraphrase": "The player who has been closest to the Forbidden City in China begins."
    },
    "game": {
      "name": "Forbidden City",
      "url": "https://www.boardgamegeek.com/boardgame/246316/forbidden-city"
    }
  },
  {
    "rule": {
      "quote": "The player who has last performed an alchemical transmutation (or a player chosen at random) becomes the starting player and places one of their larger discs on the 1st space of the Current Round Order track on the main game board.",
      "paraphrase": "The player who has last performed an alchemical transmutation begins."
    },
    "game": {
      "name": "Trismegistus: The Ultimate Formula",
      "url": "https://www.boardgamegeek.com/boardgame/281442/trismegistus-ultimate-formula"
    }
  },
  {
    "rule": {
      "quote": "The player to last drink a cup of tea is the Starting Player, give them the Starting Player marker.",
      "paraphrase": "The player to last drink a cup of tea begins."
    },
    "game": {
      "name": "Alubari: A Nice Cup of Tea",
      "url": "https://www.boardgamegeek.com/boardgame/228959/alubari-nice-cup-tea"
    }
  },
  {
    "rule": {
      "quote": "The player that most recently smelled a flower is the first player.",
      "paraphrase": "The player that most recently smelled a flower begins."
    },
    "game": {
      "name": "Tussie Mussie",
      "url": "https://www.boardgamegeek.com/boardgame/257614/tussie-mussie"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently worked the hardest becomes the first active player.",
      "note": "Rule from the 'Royal Goods' version of the game",
      "paraphrase": "The player who most recently worked hard begins."
    },
    "game": {
      "name": "Oh My Goods!",
      "url": "https://www.boardgamegeek.com/boardgame/183840/oh-my-goods"
    }
  },
  {
    "rule": {
      "quote": "Whoever was most recently in Strasbourg (or a random player) is the Starting Player.",
      "paraphrase": "The player who was most recently in Strasbourg begins."
    },
    "game": {
      "name": "Strasbourg",
      "url": "https://www.boardgamegeek.com/boardgame/91873/strasbourg"
    }
  },
  {
    "rule": {
      "quote": "The greediest player takes the First Player card (as everyone knows, the greediest person is the one with the most coins in his pockets!).",
      "paraphrase": "The greediest player begins."
    },
    "game": {
      "name": "The Bloody Inn",
      "url": "https://www.boardgamegeek.com/boardgame/180593/bloody-inn"
    }
  },
  {
    "rule": {
      "quote": "The player who is best at keeping their word will be the starting player.",
      "paraphrase": "The player who is best at keeping their word begins."
    },
    "game": {
      "name": "Ponzi Scheme",
      "url": "https://www.boardgamegeek.com/boardgame/180899/ponzi-scheme"
    }
  },
  {
    "rule": {
      "quote": "The person who washed their hands most recently goes first and play then proceeds clockwise.",
      "paraphrase": "The player who washed their hands most recently begins."
    },
    "game": {
      "name": "Plague Inc.: The Board Game",
      "url": "https://www.boardgamegeek.com/boardgame/195162/plague-inc-board-game"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently had a “déjà vu” becomes First Player: place his Banner on the designated space next to the World Council Action spaces.",
      "paraphrase": "The player who most recently had a “déjà vu” begins."
    },
    "game": {
      "name": "Anachrony",
      "url": "https://www.boardgamegeek.com/boardgame/185343/anachrony"
    }
  },
  {
    "rule": {
      "quote": "The first player is the player who most recently robbed a bank.",
      "paraphrase": "The player who most recently robbed a bank begins."
    },
    "game": {
      "name": "Escape Plan",
      "url": "https://www.boardgamegeek.com/boardgame/142379/escape-plan"
    }
  },
  {
    "rule": {
      "quote": "The player with the worst luck takes the first turn.",
      "paraphrase": "The player with the worst luck begins."
    },
    "game": {
      "name": "Karmaka",
      "url": "https://www.boardgamegeek.com/boardgame/172552/karmaka"
    }
  },
  {
    "rule": {
      "quote": "Finally, the player with the highest value of actual cash on their person will be the first Sheriff.",
      "paraphrase": "The player with the highest value of actual cash on their person begins."
    },
    "game": {
      "name": "Sheriff of Nottingham",
      "url": "https://www.boardgamegeek.com/boardgame/157969/sheriff-nottingham"
    }
  },
  {
    "rule": {
      "quote": "The player with the freshest breath takes the starting player token and begins the game.",
      "paraphrase": "The player with the freshest breath begins."
    },
    "game": {
      "name": "Mint Delivery",
      "url": "https://www.boardgamegeek.com/boardgame/230251/mint-delivery"
    }
  },
  {
    "rule": {
      "quote": "The first player is the player who strikes the most chivalrous pose, he takes the Excalibur pawn and starts the game.",
      "paraphrase": "The player who strikes the most chivalrous pose begins."
    },
    "game": {
      "name": "Medieval Academy",
      "url": "https://www.boardgamegeek.com/boardgame/154386/medieval-academy"
    }
  },
  {
    "rule": {
      "quote": "Whoever was last in Iceland begins. If nobody has been to Iceland, the player who most urgently wants to go to Iceland begins.",
      "paraphrase": "The player who last went to Iceland begins."
    },
    "game": {
      "name": "Hekla",
      "url": "https://boardgamegeek.com/boardgame/3210/hekla"
    }
  },
  {
    "rule": {
      "quote": "Give the Gemstone to the player who has the most interesting thing in their pocket.",
      "paraphrase": "The player who has the most interesting thing in their pocket begins."
    },
    "game": {
      "name": "The Nacho Incident",
      "url": "https://boardgamegeek.com/boardgame/19048/nacho-incident"
    }
  },
  {
    "rule": {
      "quote": "The player wearing the most wool goes first, […]",
      "note": "From the 'quick start' document posted on BGG.",
      "paraphrase": "The player wearing the most wool begins."
    },
    "game": {
      "name": "Mother Sheep",
      "url": "https://boardgamegeek.com/boardgame/23598/mother-sheep"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently petted an animal starts first.",
      "paraphrase": "The player who most recently petted an animal begins."
    },
    "game": {
      "name": "Bye-Bye Black Sheep",
      "url": "https://boardgamegeek.com/boardgame/231027/bye-bye-black-sheep"
    }
  },
  {
    "rule": {
      "quote": "The player who was the last to visit a real castle goes first.",
      "paraphrase": "The player who last visited castle begins."
    },
    "game": {
      "name": "Château Roquefort ",
      "url": "https://boardgamegeek.com/boardgame/28089/chateau-roquefort"
    }
  },
  {
    "rule": {
      "quote": "The most goblin-like player starts the game with the first Bomb and takes the first turn.",
      "paraphrase": "The most goblin-like player begins."
    },
    "game": {
      "name": "Big Badaboom",
      "url": "https://boardgamegeek.com/boardgame/130227/big-badaboom"
    }
  },
  {
    "rule": {
      "quote": "The first player is the person who most recently finished reading a novel.",
      "paraphrase": "The player who most recently finished reading a novel begins."
    },
    "game": {
      "name": "Paperback",
      "url": "https://boardgamegeek.com/boardgame/141572/paperback"
    }
  },
  {
    "rule": {
      "quote": "The player who has seen the “The Godfather” the most times is the starting player. (The tiebreaker is Godfather II. After that, or alternately for the entire process, draw casino cards with the highest number going first.",
      "paraphrase": "The player who has seen the “The Godfather” the most times begins."
    },
    "game": {
      "name": "Gangster",
      "url": "https://boardgamegeek.com/boardgame/25749/gangster"
    }
  },
  {
    "rule": {
      "quote": "The game is started by the player whose name sounds more archaic.",
      "note": "Unofficial english translation on BGG.",
      "paraphrase": "The player whose name sounds more archaic begins."
    },
    "game": {
      "name": "Burgen Land",
      "url": "https://boardgamegeek.com/boardgame/34001/burgen-land"
    }
  },
  {
    "rule": {
      "quote": "The player with the longest eye-teeth begins: play continues in clockwise order.",
      "paraphrase": "The player with the longest eyeteeth begins."
    },
    "game": {
      "name": "Vampire",
      "url": "https://boardgamegeek.com/boardgame/497/vampire"
    }
  },
  {
    "rule": {
      "quote": "The owner of the game (who always deals and plays first) shuffles the Nuclear War deck and deals nine cards to each player. […] The owner begins by playing any one Secret card of his choice from his hand, face down onto the table, pausing to allow spies (see below) to steal it.",
      "paraphrase": "The player who owns the game begins."
    },
    "game": {
      "name": "Nuclear War",
      "url": "https://boardgamegeek.com/boardgame/713/nuclear-war"
    }
  },
  {
    "rule": {
      "quote": "The least wise player begins."
    },
    "game": {
      "name": "Die Sieben Weisen",
      "url": "https://boardgamegeek.com/boardgame/3231/die-sieben-weisen"
    }
  },
  {
    "rule": {
      "quote": "For the first turn, the player who was in school most recently takes the teacher’s pet marker; the teacher’s pet goes first and has certain responsibilities during the turn.",
      "paraphrase": "The player who was in school most recently begins."
    },
    "game": {
      "name": "Saigo no Kane",
      "url": "https://boardgamegeek.com/boardgame/32995/saigo-no-kane"
    }
  },
  {
    "rule": {
      "quote": "The player with most red hair has the first turn. He receives the active player marker.",
      "paraphrase": "The player with the most red hair begins."
    },
    "game": {
      "name": "Tàin",
      "url": "https://boardgamegeek.com/boardgame/32992/tain"
    }
  },
  {
    "rule": {
      "quote": "Whoever jumps highest may start and throw the red action dice.",
      "paraphrase": "The player who can jump highest begins."
    },
    "game": {
      "name": "Dancing Eggs",
      "url": "https://boardgamegeek.com/boardgame/8924/dancing-eggs"
    }
  },
  {
    "rule": {
      "quote": "First to go is the player who can make himself look most like a Monkey.",
      "paraphrase": "The player who makes the best monkey impression begins."
    },
    "game": {
      "name": "Coco Crazy",
      "url": "https://www.boardgamegeek.com/boardgame/2792/coco-crazy"
    }
  },
  {
    "rule": {
      "quote": "The player who last ate chocolate will go first.",
      "paraphrase": "The player who last ate chocolate begins."
    },
    "game": {
      "name": "Chocolatiers",
      "url": "https://www.boardgamegeek.com/boardgame/233354/chocolatiers"
    }
  },
  {
    "rule": {
      "quote": "The player who drank tea most recently will be the starting player (or you can choose randomly).",
      "paraphrase": "The player who drank tea most recently begins."
    },
    "game": {
      "name": "Ceylon",
      "url": "https://www.boardgamegeek.com/boardgame/248900/ceylon"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently pet a cat becomes the first player in the first round.",
      "paraphrase": "The player who most recently pet a cat begins."
    },
    "game": {
      "name": "Cat Café",
      "url": "https://www.boardgamegeek.com/boardgame/279135/cat-cafe"
    }
  },
  {
    "rule": {
      "quote": "The player who can say “ARRRR!” in the best pirate voice will be the first player.",
      "paraphrase": "The player who can say “ARRRR!” in the best pirate voice begins."
    },
    "game": {
      "name": "Buccaneer Bones",
      "url": "https://www.boardgamegeek.com/boardgame/147790/buccaneer-bones"
    }
  },
  {
    "rule": {
      "quote": "The most recent player to set something on fire goes first and play will proceed clockwise.",
      "paraphrase": "The most recent player to set something on fire begins."
    },
    "game": {
      "name": "The Brigade",
      "url": "https://www.boardgamegeek.com/boardgame/213503/brigade"
    }
  },
  {
    "rule": {
      "quote": "Give the Start Player token to the player who most recently finished a beer (root beer counts)",
      "note": "Same rules used in Brew Crafters: Travel Card Game",
      "paraphrase": "The player who most recently finished a beer (root beer counts) begins."
    },
    "game": {
      "name": "Brew Crafters",
      "url": "https://boardgamegeek.com/boardgame/139898/brew-crafters"
    }
  },
  {
    "rule": {
      "quote": "Cain plays first. If Cain is not in play, then the saddest person plays first.",
      "paraphrase": "The saddest player begins."
    },
    "game": {
      "name": "The Binding of Isaac: Four Souls",
      "url": "https://www.boardgamegeek.com/boardgame/255664/binding-isaac-four-souls"
    }
  },
  {
    "rule": {
      "quote": "The player who monkeys around the most begins."
    },
    "game": {
      "name": "Banana Bandido",
      "url": "https://www.boardgamegeek.com/boardgame/290462/banana-bandido"
    }
  },
  {
    "rule": {
      "quote": "The player who thinks he knows most about Australia goes first.",
      "paraphrase": "The player who thinks they knows most about Australia begins."
    },
    "game": {
      "name": "Australia",
      "url": "https://www.boardgamegeek.com/boardgame/15033/australia"
    }
  },
  {
    "rule": {
      "quote": "The player who has the most loose change in their pockets receives the 5 action markers and begins the game.",
      "paraphrase": "The player with the most loose change in their pockets begins."
    },
    "game": {
      "name": "Asante",
      "url": "https://www.boardgamegeek.com/boardgame/136056/asante"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently made an unreasonable demand will take the first turn, then play will proceed clockwise around the table.",
      "paraphrase": "The player who most recently made an unreasonable demand begins."
    },
    "game": {
      "name": "Aristocracy",
      "url": "https://www.boardgamegeek.com/boardgame/287247/aristocracy"
    }
  },
  {
    "rule": {
      "quote": "Starting Player: The player who proposed to play Roads & Boats starts laying the map.",
      "paraphrase": "The player who suggested the game begins."
    },
    "game": {
      "name": "Roads & Boats",
      "url": "https://www.boardgamegeek.com/boardgame/875/roads-boats"
    }
  },
  {
    "rule": {
      "quote": "The player to start is the member of the company with the highest rank in society (standard protocol applies: religious titles are always deemed greater than hereditary titles, and those higher than military titles; if of similar rank then compare subsidiary titles, number of estates or centuries that the title has been in the family; youth defers to age; when in doubt the highest military decoration takes seniority; and for the rest I refer you to the works of Messrs Debretts or Burkes).",
      "paraphrase": "The player with the highest rank in society begins."
    },
    "game": {
      "name": "The Extraordinary Adventures of Baron Munchausen",
      "url": "https://www.boardgamegeek.com/boardgame/2470/extraordinary-adventures-baron-munchausen"
    }
  },
  {
    "rule": {
      "quote": "Randomly determine the First Player (for instance, the player who is wearing the nicest jewelry).",
      "paraphrase": "The player who is wearing the nicest jewelry begins."
    },
    "game": {
      "name": "Queen's Necklace",
      "url": "https://www.boardgamegeek.com/boardgame/6068/queens-necklace"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently ate Belgian chocolate is the first player.",
      "paraphrase": "The player who most recently ate Belgian chocolate begins."
    },
    "game": {
      "name": "Bruges",
      "url": "https://boardgamegeek.com/boardgame/136888/bruges"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently had a wish come true is the starting player.",
      "paraphrase": "The player who most recently had a wish come true begins."
    },
    "game": {
      "name": "3 Wishes",
      "url": "https://boardgamegeek.com/boardgame/198836/3-wishes"
    }
  },
  {
    "rule": {
      "quote": "The last player to have flown is named player one.",
      "paraphrase": "The last player to have flown begins."
    },
    "game": {
      "name": "Asteroyds",
      "url": "https://boardgamegeek.com/boardgame/65200/asteroyds"
    }
  },
  {
    "rule": {
      "quote": "If no starting player can be agreed upon, the person whom most recently finished reading a fantasy novel should go first.",
      "note": "The second edition says: The person who most recently finished a good fantasy novel series should be first player. If no such person can be determined, the first player should be chosen randomly.",
      "paraphrase": "The player that most recently finished reading a fantasy novel begins."
    },
    "game": {
      "name": "The Lord of the Ice Garden",
      "url": "https://www.boardgamegeek.com/boardgame/157917/lord-ice-garden"
    }
  },
  {
    "rule": {
      "quote": "The player born closest to November 24st, 1967, may start, play proceeds clockwise from there.",
      "paraphrase": "The player born closest to November 24, 1967 begins."
    },
    "game": {
      "name": "Duck Dealer",
      "url": "https://www.boardgamegeek.com/boardgame/38553/duck-dealer"
    }
  },
  {
    "rule": {
      "quote": "The player with the longest whiskers goes first, or, in the case of a tie, whoever’s breath smells most like fish.",
      "paraphrase": "The player with the longest whiskers begins. If tied, choose whoever’s breath smells most like fish. "
    },
    "game": {
      "name": "Hot Tin Roof",
      "url": "https://boardgamegeek.com/boardgame/154499/hot-tin-roof"
    }
  },
  {
    "rule": {
      "quote": "The player who resembles an alien the most takes the First Player Marker.",
      "paraphrase": "The player who resembles an alien the most begins."
    },
    "game": {
      "name": "Andromeda",
      "url": "https://www.boardgamegeek.com/boardgame/167237/andromeda"
    }
  },
  {
    "rule": {
      "quote": "The players must choose a counselor to take the first turn. If no decision can be made, we suggest picking the player with the most tattoos.",
      "paraphrase": "The player with the most tattoos begins."
    },
    "game": {
      "name": "Camp Grizzly",
      "url": "https://www.boardgamegeek.com/boardgame/143096/camp-grizzly"
    }
  },
  {
    "rule": {
      "quote": "The player who can cackle the most like a vile and greedy medieval pardoner begins the game!",
      "paraphrase": "The player who can cackle the most like a vile and greedy medieval pardoner begins."
    },
    "game": {
      "name": "The Road to Canterbury",
      "url": "https://www.boardgamegeek.com/boardgame/96792/road-canterbury"
    }
  },
  {
    "rule": {
      "quote": "Play begins with the hungriest player. If there is a tie, play begins with the angriest player.",
      "paraphrase": "The hungriest player begins. If tied, play begins with the angriest player."
    },
    "game": {
      "name": "Guts of Glory",
      "url": "https://boardgamegeek.com/boardgame/124706/guts-glory"
    }
  },
  {
    "rule": {
      "quote": "The starting player is the person who most recently dug a hole.",
      "paraphrase": "The player who most recently dug a hole begins."
    },
    "game": {
      "name": "Super Motherload",
      "url": "https://www.boardgamegeek.com/boardgame/162286/super-motherload"
    }
  },
  {
    "rule": {
      "quote": "The last player to have flipped a table is the start player.",
      "paraphrase": "The last player to have flipped a table begins."
    },
    "game": {
      "name": "Flip City",
      "url": "https://www.boardgamegeek.com/boardgame/168679/flip-city"
    }
  },
  {
    "rule": {
      "quote": "The player with the largest hands can begin.",
      "paraphrase": "The player with the largest hands begins."
    },
    "game": {
      "name": "The Mole in the Hole",
      "url": "https://www.boardgamegeek.com/boardgame/321/mole-hole"
    }
  },
  {
    "rule": {
      "quote": "The player who was last in Hamburg will be the starting player. In case of a tie, or if none of the players have ever been to Hamburg, the player who most recently ate a hamburger will be starting player.",
      "paraphrase": "The player who was last in Hamburg begins. In case of a tie, choose the player who most recently ate a hamburger."
    },
    "game": {
      "name": "Merkator",
      "url": "https://www.boardgamegeek.com/boardgame/39684/merkator"
    }
  },
  {
    "rule": {
      "quote": "The first player (a role which does not influence the game) is the one who, from a common agreement, proclaims the most impactful provocation sentence.",
      "paraphrase": "The player with the best provocation begins."
    },
    "game": {
      "name": "Meeple War",
      "url": "https://www.boardgamegeek.com/boardgame/192120/meeple-war"
    }
  },
  {
    "rule": {
      "quote": "The start player is the person who used the least amount of fuel to get from their home to where the game is being played.",
      "paraphrase": "The player who used the least amount of fuel to get from their home to where the game is being played begins."
    },
    "game": {
      "name": "The Manhattan Project: Energy Empire",
      "url": "https://www.boardgamegeek.com/boardgame/176734/manhattan-project-energy-empire"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently visited an island is the first player and receive the “First Player” card.",
      "paraphrase": "The player who most recently visited an island begins."
    },
    "game": {
      "name": "Maka Bana",
      "url": "https://www.boardgamegeek.com/boardgame/8147/maka-bana"
    }
  },
  {
    "rule": {
      "quote": "The player who lives closest to the Leaning Tower of Pisa plays first.",
      "paraphrase": "The player who lives closest to the Leaning Tower of Pisa begins."
    },
    "game": {
      "name": "Lungarno",
      "url": "https://www.boardgamegeek.com/boardgame/35801/lungarno"
    }
  },
  {
    "rule": {
      "quote": "The last player to have defended a person or an honorable cause starts the game.",
      "paraphrase": "The last player to have defended a person or an honorable cause begins."
    },
    "game": {
      "name": "Last Bastion",
      "url": "https://www.boardgamegeek.com/boardgame/285984/last-bastion"
    }
  },
  {
    "rule": {
      "quote": "The player wearing the most jewelry runs the first auction.",
      "paraphrase": "The player wearing the most jewelry begins."
    },
    "game": {
      "name": "Das Kollier",
      "url": "https://www.boardgamegeek.com/boardgame/575/das-kollier"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently saw a Unicorn become the first player (and seek medical help as soon as possible).",
      "paraphrase": "The player who most recently saw a unicorn begins."
    },
    "game": {
      "name": "Kill The Unicorns",
      "url": "https://www.boardgamegeek.com/boardgame/204602/kill-unicorns"
    }
  },
  {
    "rule": {
      "quote": "The player who was most recently in the mountains takes the starting player marker.",
      "paraphrase": "The player who was most recently in the mountains begins."
    },
    "game": {
      "name": "K2",
      "url": "https://www.boardgamegeek.com/boardgame/73761/k2"
    }
  },
  {
    "rule": {
      "quote": "The first player is the one who carries the oldest item with him.",
      "paraphrase": "The player who carries the oldest item with them begins."
    },
    "game": {
      "name": "Jenseits von Theben",
      "url": "https://www.boardgamegeek.com/boardgame/13883/jenseits-von-theben"
    }
  },
  {
    "rule": {
      "quote": "The player who enjoys cold weather the most receives the First Player card.",
      "paraphrase": "The player who enjoys cold weather the most begins."
    },
    "game": {
      "name": "Inuit: The Snow Folk",
      "url": "https://www.boardgamegeek.com/boardgame/261009/inuit-snow-folk"
    }
  },
  {
    "rule": {
      "quote": "The last person to have taken a stroll in the forest is the start player and takes the Hiking Shoes.",
      "paraphrase": "The last player to have taken a stroll in the forest begins."
    },
    "game": {
      "name": "Indian Summer",
      "url": "https://www.boardgamegeek.com/boardgame/233678/indian-summer"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently been on a canal tour boat starts the game.",
      "paraphrase": "The player who was most recently on a canal tour boat begins."
    },
    "game": {
      "name": "Hotel Amsterdam",
      "url": "https://www.boardgamegeek.com/boardgame/42066/hotel-amsterdam"
    }
  },
  {
    "rule": {
      "quote": "The player with the shaggiest hair is the starting player and takes the fire marker.",
      "paraphrase": "The player with the shaggiest hair begins."
    },
    "game": {
      "name": "Honga",
      "url": "https://www.boardgamegeek.com/boardgame/255805/honga"
    }
  },
  {
    "rule": {
      "quote": "The player with the longest ears is the starting player; he writes his name on the top left of the score pad.",
      "paraphrase": "The player with the longest ears begins."
    },
    "game": {
      "name": "Hoppladi Hopplada!",
      "url": "https://www.boardgamegeek.com/boardgame/39088/hoppladi-hopplada"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently ate garlic takes the first turn.",
      "paraphrase": "The player who most recently ate garlic begins."
    },
    "game": {
      "name": "Horrified",
      "url": "https://www.boardgamegeek.com/boardgame/282524/horrified"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently cooked with or planted herbs is the starting player and takes the first turn.",
      "paraphrase": "The player who most recently cooked with or planted herbs begins."
    },
    "game": {
      "name": "Herbaceous",
      "url": "https://www.boardgamegeek.com/boardgame/195314/herbaceous"
    }
  },
  {
    "rule": {
      "quote": "Whoever suggested the game selects the starting player, and play proceeds clockwise.",
      "paraphrase": "The player who suggested the game begins."
    },
    "game": {
      "name": "GUBS: A Game of Wit and Luck",
      "url": "https://www.boardgamegeek.com/boardgame/31808/gubs-game-wit-and-luck"
    }
  },
  {
    "rule": {
      "quote": "Give the Lead Investigator to the player who was most recently shot (in a game or real life) to take the first turn!",
      "paraphrase": "The player who was most recently shot (in a game or real life) begins."
    },
    "game": {
      "name": "Good Cop Bad Cop",
      "url": "https://www.boardgamegeek.com/boardgame/153064/good-cop-bad-cop"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently was in South America or who rolls highest with 2 dice becomes the Start Player and receives the Start Player Token.",
      "paraphrase": "The player who was most recently South America begins."
    },
    "game": {
      "name": "El Gaucho",
      "url": "https://www.boardgamegeek.com/boardgame/162823/el-gaucho"
    }
  },
  {
    "rule": {
      "quote": "The first player to make a trumpet sound and announce herself as the starting player becomes the starting player.",
      "paraphrase": "The first player to make a trumpet sound and announce themselves as the starting player begins."
    },
    "game": {
      "name": "For Crown & Kingdom",
      "url": "https://www.boardgamegeek.com/boardgame/77011/crown-kingdom"
    }
  },
  {
    "rule": {
      "quote": "The player who last blew a kiss is the first player.",
      "paraphrase": "The player who last blew a kiss begins."
    },
    "game": {
      "name": "Fog of Love",
      "url": "https://www.boardgamegeek.com/boardgame/175324/fog-love"
    }
  },
  {
    "rule": {
      "quote": "The player with the hairiest head is First Player and gets a Victory Point.",
      "paraphrase": "The player with the hairiest head begins."
    },
    "game": {
      "name": "Fire & Axe: A Viking Saga",
      "url": "https://www.boardgamegeek.com/boardgame/12495/fire-axe-viking-saga"
    }
  },
  {
    "rule": {
      "quote": "The player who was most recently underground will start as the Adventurer.",
      "paraphrase": "The player who was most recently underground begins."
    },
    "game": {
      "name": "Dungeon Roll",
      "url": "https://www.boardgamegeek.com/boardgame/138788/dungeon-roll"
    }
  },
  {
    "rule": {
      "quote": "Give the First Player marker to the player who slept the longest last night.",
      "paraphrase": "The player who slept the longest last night begins."
    },
    "game": {
      "name": "Dreamwell",
      "url": "https://www.boardgamegeek.com/boardgame/180761/dreamwell"
    }
  },
  {
    "rule": {
      "quote": "The player who has been in the ocean or gone swimming most recently takes the first turn.",
      "paraphrase": "The player who has been in the ocean or gone swimming most recently begins."
    },
    "game": {
      "name": "Deep Sea Adventure",
      "url": "https://www.boardgamegeek.com/boardgame/169654/deep-sea-adventure"
    }
  },
  {
    "rule": {
      "quote": "The last player to have raised the dead becomes the first Active Player and they are given the Skull Draw Bag.",
      "paraphrase": "The last player to have raised the dead begins."
    },
    "game": {
      "name": "Dead Man's Cabal",
      "url": "https://www.boardgamegeek.com/boardgame/264321/dead-mans-cabal"
    }
  },
  {
    "rule": {
      "quote": "The player that woke up latest today is entitled to start.",
      "paraphrase": "The player that woke up latest today begins."
    },
    "game": {
      "name": "Dawn Under",
      "url": "https://www.boardgamegeek.com/boardgame/10814/dawn-under"
    }
  },
  {
    "rule": {
      "quote": "The player who looks most like a dragon goes first.",
      "paraphrase": "The player who looks most like a dragon begins."
    },
    "game": {
      "name": "D6 Dungeon",
      "url": "https://www.boardgamegeek.com/boardgame/204314/d6-dungeon"
    }
  },
  {
    "rule": {
      "quote": "Whoever was sick most recently receives the First Player Marker and 2 ATP.",
      "paraphrase": "The player who was sick most recently begins."
    },
    "game": {
      "name": "Cytosis: A Cell Biology Board Game",
      "url": "https://www.boardgamegeek.com/boardgame/202977/cytosis-cell-biology-board-game"
    }
  },
  {
    "rule": {
      "quote": "Find out which player has his alarm clock set the earliest for work. That unfortunate player will go first.",
      "paraphrase": "The player with their alarm clock set the earliest begins."
    },
    "game": {
      "name": "Counting Zzzzs",
      "url": "https://www.boardgamegeek.com/boardgame/7851/counting-zzzzs"
    }
  },
  {
    "rule": {
      "quote": "The last person to have weeded a garden is the starting player.",
      "paraphrase": "The last player to have weeded a garden begins."
    },
    "game": {
      "name": "Cottage Garden",
      "url": "https://www.boardgamegeek.com/boardgame/204027/cottage-garden"
    }
  },
  {
    "rule": {
      "quote": "The player whose last meal was the healthiest or least healthy as determined by all players will be first.",
      "paraphrase": "The player whose last meal was the healthiest begins."
    },
    "game": {
      "name": "Consumption: Food and Choices",
      "url": "https://www.boardgamegeek.com/boardgame/198517/consumption-food-and-choices"
    }
  }
]

"""
