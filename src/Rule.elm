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
      "quote": "Spillet startes av spilleren som ligner mest på en pirat.",
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
  }
]

"""
