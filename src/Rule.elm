module Rule exposing (Rule, getByIndex, getRandomIndex)

import Json.Decode exposing (Decoder)
import List.Nonempty as Nonempty exposing (Nonempty)
import Random exposing (Generator)
import Task


type alias Rule =
    { game : String
    , rule : String
    , url : String
    }


getRandomIndex : (Result String Int -> msg) -> Cmd msg
getRandomIndex msgWrapper =
    case listOfRules of
        Err error ->
            Task.attempt msgWrapper (Task.fail error)

        Ok rules ->
            Random.int 0 (Nonempty.length rules)
                |> Random.map Ok
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
      "quote": "The player with the largest cowboy boots (or other shoes) begins, the others follow in clockwise direction.",
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
      "name": "Zombie State: Diplomacy of the Dead",
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
      "paraphrase": "The player who last visited Iceland begins."
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
      "paraphrase": "The player with the longest whiskers begins. If tied, choose whoever’s breath smells most like fish."
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
      "paraphrase": "The player who was most recently in South America begins."
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
  },
  {
    "rule": {
      "quote": "The player with the shortest name takes the first turn.",
      "paraphrase": "The player with the shortest name begins."
    },
    "game": {
      "name": "Nanofictionary",
      "url": "https://www.boardgamegeek.com/boardgame/2895/nanofictionary"
    }
  },
  {
    "rule": {
      "quote": "The player who last visited Namibia, or who has ever been closest to Namibia, becomes the start player for the setup phase and places their train on the 1 space of the player track.",
      "paraphrase": "The player who last visited Namibia, or who has ever been closest to Namibia, begins."
    },
    "game": {
      "name": "Namibia",
      "url": "https://www.boardgamegeek.com/boardgame/55842/namibia"
    }
  },
  {
    "rule": {
      "quote": "The player with the biggest ears is the starting player.",
      "paraphrase": "The player with the biggest ears begins."
    },
    "game": {
      "name": "Mythe",
      "url": "https://www.boardgamegeek.com/boardgame/139629/mythe"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently traveled abroad begins or randomly determines a starting player who will take the Starting Player Token.",
      "paraphrase": "The player who has most recently traveled abroad begins."
    },
    "game": {
      "name": "Walking in Burano",
      "url": "https://www.boardgamegeek.com/boardgame/257769/walking-burano"
    }
  },
  {
    "rule": {
      "quote": "Choose a starting player, for example, the player who most recently drank mead from a cow horn.",
      "paraphrase": "The player who most recently drank mead from a cow horn begins."
    },
    "game": {
      "name": "Walhalla",
      "url": "https://www.boardgamegeek.com/boardgame/25605/walhalla"
    }
  },
  {
    "rule": {
      "quote": "The player who has traveled the closest to molten lava goes first (and the closer they’ve been to an active volcano, the more they get to brag).",
      "paraphrase": "The player who has traveled the closest to molten lava begins."
    },
    "game": {
      "name": "Volcano",
      "url": "https://www.boardgamegeek.com/boardgame/13084/volcano"
    }
  },
  {
    "rule": {
      "quote": "The Start Player Ship is given to the player who has pillaged and razed the most defenseless villages.",
      "paraphrase": "The player who has pillaged and razed the most defenseless villages begins."
    },
    "game": {
      "name": "Vikings",
      "url": "https://www.boardgamegeek.com/boardgame/27173/vikings"
    }
  },
  {
    "rule": {
      "quote": "Determine the first player. Whoever has the best evil laugh starts the game!",
      "paraphrase": "The player who has the best evil laugh begins."
    },
    "game": {
      "name": "Victorian Masterminds",
      "url": "https://www.boardgamegeek.com/boardgame/189453/victorian-masterminds"
    }
  },
  {
    "rule": {
      "quote": "Whoever is wearing the most colors is obviously the unicorniest, so they go first.",
      "paraphrase": "The player wearing the most colors begins."
    },
    "game": {
      "name": "Unstable Unicorns",
      "url": "https://www.boardgamegeek.com/boardgame/234190/unstable-unicorns"
    }
  },
  {
    "rule": {
      "quote": "The player that looks moste like Henry VIII or one of his wives is voted for as the start player.",
      "paraphrase": "The player that looks moste like Henry VIII, or one of his wives, begins."
    },
    "game": {
      "name": "Tudor",
      "url": "https://www.boardgamegeek.com/boardgame/219512/tudor"
    }
  },
  {
    "rule": {
      "quote": "Decide who starts the game by who has last opened a lock (in real life), even with a key!",
      "paraphrase": "The player who last opened a lock begins."
    },
    "game": {
      "name": "Triplock",
      "url": "https://www.boardgamegeek.com/boardgame/229791/triplock"
    }
  },
  {
    "rule": {
      "quote": "Give the dice to the player who most recently watched the Thunderbirds TV show. They will be the starting player.",
      "paraphrase": "The player who most recently watched the Thunderbirds TV show begins."
    },
    "game": {
      "name": "Thunderbirds",
      "url": "https://www.boardgamegeek.com/boardgame/160610/thunderbirds"
    }
  },
  {
    "rule": {
      "quote": "Start Player Marker: Give this to the player who most recently stole something.",
      "paraphrase": "The player who most recently stole something begins."
    },
    "game": {
      "name": "Thief's Market",
      "url": "https://www.boardgamegeek.com/boardgame/182351/thiefs-market"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently defeated a Worm in combat is the starting player.",
      "paraphrase": "The player who most recently defeated a worm in combat begins."
    },
    "game": {
      "name": "Terror Below",
      "url": "https://www.boardgamegeek.com/boardgame/270138/terror-below"
    }
  },
  {
    "rule": {
      "quote": "Determine who in your group is the most knowledgeable, and give the box of Terra cards to the player directly to his left.",
      "paraphrase": "The (player to the left of) the most knowledgeable player begins."
    },
    "game": {
      "name": "Terra",
      "url": "https://www.boardgamegeek.com/boardgame/153507/terra"
    }
  },
  {
    "rule": {
      "quote": "The player who last found a pot of gold is the starting player. If no-one has ever found a pot of gold, then the game ends immediately and everyone loses. Alternatively, the youngest player can start… The start player takes the Key card, puts it in front of him and is now called the 'key player'.",
      "note": "Translation from the german rules on BGG.",
      "paraphrase": "The player who last found a pot of gold begins."
    },
    "game": {
      "name": "Tempel des Schreckens",
      "url": "https://www.boardgamegeek.com/boardgame/206915/tempel-des-schreckens"
    }
  },
  {
    "rule": {
      "quote": "The last person to harvest a vegetable becomes the first player. Alternatively, randomly determine the first player using a method of your choosing.",
      "paraphrase": "The last player to harvest a vegetable begins."
    },
    "game": {
      "name": "Tawantinsuyu: The Inca Empire",
      "url": "https://www.boardgamegeek.com/boardgame/306481/tawantinsuyu-inca-empire"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently went on a journey is the starting player.",
      "paraphrase": "The player who most recently went on a journey begins."
    },
    "game": {
      "name": "Swamped",
      "url": "https://www.boardgamegeek.com/boardgame/173770/swamped"
    }
  },
  {
    "rule": {
      "quote": "The one who shout the loudest 'I will crush you all' begins the game.",
      "note": "Translated from the french rules.",
      "paraphrase": "The player who shout “I will crush you all!” the loudest begins."
    },
    "game": {
      "name": "Stratelite",
      "url": "https://www.boardgamegeek.com/boardgame/14181/stratelite"
    }
  },
  {
    "rule": {
      "quote": "The least punctual person is the starting player.",
      "paraphrase": "The least punctual player begins."
    },
    "game": {
      "name": "Steam Time",
      "url": "https://www.boardgamegeek.com/boardgame/180592/steam-time"
    }
  },
  {
    "rule": {
      "quote": "If one player has a character with more pips than any other, that player begins. Otherwise the player who has most recently watched an episode of Star Trek goes first.",
      "paraphrase": "The player who has most recently watched an episode of Star Trek begins."
    },
    "game": {
      "name": "Star Trek Chrono-Trek",
      "url": "https://www.boardgamegeek.com/boardgame/268276/star-trek-chrono-trek"
    }
  },
  {
    "rule": {
      "quote": "Give the Initiative Marker to the player who went stargazing most recently or choose who will play first.",
      "paraphrase": "The player who went stargazing most recently begins."
    },
    "game": {
      "name": "Space Race: The Card Game",
      "url": "https://www.boardgamegeek.com/boardgame/191177/space-race-card-game"
    }
  },
  {
    "rule": {
      "quote": "Give the First Player marker to the player who has been to space most recently. If no-one has ever been into space, then the player who most recently watched or read something about space receives the marker and begins the game.",
      "paraphrase": "The player who has been to space most recently begins. If tied, choose the player who most recently watched or read something about space."
    },
    "game": {
      "name": "Space Explorers",
      "url": "https://www.boardgamegeek.com/boardgame/235817/space-explorers"
    }
  },
  {
    "rule": {
      "quote": "The player who most has “their head in the clouds” is the first ”player.",
      "paraphrase": "The player who most has “their head in the clouds” begins."
    },
    "game": {
      "name": "Solenia",
      "url": "https://www.boardgamegeek.com/boardgame/254018/solenia"
    }
  },
  {
    "rule": {
      "quote": "The player seated nearest to a solid door-frame goes first, and play proceeds clock-wise around the table.",
      "paraphrase": "The player seated nearest to a solid door-frame begins."
    },
    "game": {
      "name": "Seismic",
      "url": "https://www.boardgamegeek.com/boardgame/22673/seismic"
    }
  },
  {
    "rule": {
      "quote": "The Foreman born closest to Samara is start player, the second-most close born is second player, and so on.",
      "paraphrase": "The player born closest to Samara begins."
    },
    "game": {
      "name": "Samara",
      "url": "https://www.boardgamegeek.com/boardgame/177490/samara"
    }
  },
  {
    "rule": {
      "quote": "Whoever waves the most quickly with the highest real bill or has brought a real welding torch to the game goes first.",
      "note": "Translated from the german rules.",
      "paraphrase": "The player who first waves with the highest denomination real bill (or has brought a real welding torch) begins."
    },
    "game": {
      "name": "Die Safeknacker",
      "url": "https://www.boardgamegeek.com/boardgame/3349/die-safeknacker"
    }
  },
  {
    "rule": {
      "quote": "The player who looks the oldest is the start-player.",
      "paraphrase": "The player who looks the oldest begins."
    },
    "game": {
      "name": "Ruhe in Frieden",
      "url": "https://www.boardgamegeek.com/boardgame/8185/ruhe-frieden"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently purchased tomatoes is named the start player and takes the start player card.",
      "paraphrase": "The player who most recently purchased tomatoes begins."
    },
    "game": {
      "name": "Reykholt",
      "url": "https://www.boardgamegeek.com/boardgame/241831/reykholt"
    }
  },
  {
    "rule": {
      "quote": "The last player who drank something with a particularly strange taste will be the first player.",
      "paraphrase": "The last player who drank something with a particularly strange taste begins."
    },
    "game": {
      "name": "Rebis",
      "url": "https://www.boardgamegeek.com/boardgame/301085/rebis"
    }
  },
  {
    "rule": {
      "quote": "Choose a start player according to your preferred method (the player who owns the most railroad games).",
      "paraphrase": "The player who owns the most railroad games begins."
    },
    "game": {
      "name": "Railroad Dice",
      "url": "https://www.boardgamegeek.com/boardgame/8192/railroad-dice"
    }
  },
  {
    "rule": {
      "quote": "Give the First Player marker to the poor soul who was most recently caught in the rain.",
      "paraphrase": "The player who was most recently caught in the rain begins."
    },
    "game": {
      "name": "Petrichor",
      "url": "https://www.boardgamegeek.com/boardgame/210274/petrichor"
    }
  },
  {
    "rule": {
      "quote": "Whoever went to the bathroom last is the starting player.",
      "paraphrase": "The player who last went to the bathroom begins."
    },
    "game": {
      "name": "Pecunia non olet",
      "url": "https://boardgamegeek.com/boardgame/209672/pecunia-non-olet-second-edition"
    }
  },
  {
    "rule": {
      "quote": "The player who was most recently sick goes first.",
      "note": "This rule has been changed in the newer version of the game.",
      "paraphrase": "The player who was most recently sick begins."
    },
    "game": {
      "name": "Pandemic",
      "url": "https://www.boardgamegeek.com/boardgame/30549/pandemic"
    }
  },
  {
    "rule": {
      "quote": "The last player to have visited or seen a picture of Italy is the start player.",
      "paraphrase": "The last player to have visited or seen a picture of Italy begins."
    },
    "game": {
      "name": "The Palaces of Carrara",
      "url": "https://www.boardgamegeek.com/boardgame/129948/palaces-carrara"
    }
  },
  {
    "rule": {
      "quote": "Whoever last ate with chopsticks, takes the start player marker.",
      "paraphrase": "The player who last ate with chopsticks begins."
    },
    "game": {
      "name": "Pagoda",
      "url": "https://www.boardgamegeek.com/boardgame/154003/pagoda"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently ate pie starts the game and play continues to the left.",
      "paraphrase": "The player who most recently ate pie begins."
    },
    "game": {
      "name": "Outfoxed!",
      "url": "https://www.boardgamegeek.com/boardgame/172931/outfoxed"
    }
  },
  {
    "rule": {
      "quote": "The most evolved player (or in case of doubt choose randomly) will be the first player.",
      "paraphrase": "The most evolved player begins."
    },
    "game": {
      "name": "On the Origin of Species",
      "url": "https://www.boardgamegeek.com/boardgame/276004/origin-species"
    }
  },
  {
    "rule": {
      "quote": "Choose who is the first active player (for instance who lives closer to Venice).",
      "paraphrase": "The player who lives closest to Venice begins."
    },
    "game": {
      "name": "Oltre Mare",
      "url": "https://www.boardgamegeek.com/boardgame/13551/oltre-mare"
    }
  },
  {
    "rule": {
      "quote": "If you most recently received a gift, go first.",
      "paraphrase": "The player who most recently received a gift begins."
    },
    "game": {
      "name": "No Thanks!",
      "url": "https://www.boardgamegeek.com/boardgame/12942/no-thanks"
    }
  },
  {
    "rule": {
      "quote": "The most silent player takes the dice and becomes the first player.",
      "paraphrase": "The most silent player begins."
    },
    "game": {
      "name": "Ninja Taisen",
      "url": "https://www.boardgamegeek.com/boardgame/162041/ninja-taisen"
    }
  },
  {
    "rule": {
      "quote": "The person with the least amount of hair goes first and play continues to the left.",
      "paraphrase": "The player with the least amount of hair begins."
    },
    "game": {
      "name": "Dweebies",
      "url": "https://boardgamegeek.com/boardgame/65814/dweebies"
    }
  },
  {
    "rule": {
      "quote": "The player who looks most like a mobster takes the Capo Ring and is the First player.",
      "note": "The revised edition has changed the wording to 'most like a gangster'.",
      "paraphrase": "The player who looks most like a mobster begins."
    },
    "game": {
      "name": "Nothing Personal",
      "url": "https://boardgamegeek.com/boardgame/120523/nothing-personal"
    }
  },
  {
    "rule": {
      "quote": "The first player is the person that most recently rode a train.",
      "paraphrase": "The player that most recently rode a train begins."
    },
    "game": {
      "name": "Skyline Express: Roll & Write",
      "url": "https://boardgamegeek.com/boardgame/299251/skyline-express-roll-write"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently visited a high-rise building is the start player.",
      "paraphrase": "The player who has most recently visited a high-rise building begins."
    },
    "game": {
      "name": "Skyline",
      "url": "https://boardgamegeek.com/boardgame/121423/skyline"
    }
  },
  {
    "rule": {
      "quote": "Whoever was the last to be in a city center becomes the starting player and receives the starting player figure.",
      "paraphrase": "The player who was last in a city center begins."
    },
    "game": {
      "name": "Forum Trajanum",
      "url": "https://boardgamegeek.com/boardgame/244049/forum-trajanum"
    }
  },
  {
    "rule": {
      "quote": "The most suspicious-looking player is the dealer in the first round.",
      "paraphrase": "The most suspicious-looking player begins."
    },
    "game": {
      "name": "Spyfall",
      "url": "https://boardgamegeek.com/boardgame/166384/spyfall"
    }
  },
  {
    "rule": {
      "quote": "The last person who drank rum is First Player. Give him the Parrot and the Hat tokens.",
      "paraphrase": "The last player who drank rum begins."
    },
    "game": {
      "name": "Sea of Clouds",
      "url": "https://boardgamegeek.com/boardgame/189052/sea-clouds"
    }
  },
  {
    "rule": {
      "quote": "The player who has the most cats in real life is the starting player and takes the first turn of the game.",
      "paraphrase": "The player who has the most cats begins."
    },
    "game": {
      "name": "Cat Lady",
      "url": "https://boardgamegeek.com/boardgame/228504/cat-lady"
    }
  },
  {
    "rule": {
      "quote": "Determine the first active player by asking who has most recently been abducted by the CIA or the KGB. Or choose the first active player randomly.",
      "paraphrase": "The player who has most recently been abducted by the CIA or the KGB beings."
    },
    "game": {
      "name": "Secrets",
      "url": "https://boardgamegeek.com/boardgame/200847/secrets"
    }
  },
  {
    "rule": {
      "quote": "Pick a player to go first. (Some sample criteria: furriest, most recent tantrum, most body parts, etc.)",
      "paraphrase": "The player who had the most recent tantrum begins."
    },
    "game": {
      "name": "Bears vs Babies",
      "url": "https://boardgamegeek.com/boardgame/211534/bears-vs-babies"
    }
  },
  {
    "rule": {
      "quote": "Pick a first player by deciding who loves bears the most.",
      "paraphrase": "The player who loves bears the most begins."
    },
    "game": {
      "name": "Ursa Miner",
      "url": "https://boardgamegeek.com/boardgame/226006/ursa-miner"
    }
  },
  {
    "rule": {
      "quote": "The person who has most recently been to Rio de Janeiro goes first. If this results in a tie, put the game away and never play it again. Or randomly pick a starting player.",
      "paraphrase": "The player who has most recently been to Rio de Janeiro begins."
    },
    "game": {
      "name": "Favelas",
      "url": "https://boardgamegeek.com/boardgame/230591/favelas"
    }
  },
  {
    "rule": {
      "quote": "The player who has visited the most continents is the start player. Give that player the first player dragon.",
      "paraphrase": "The player who has visited the most continents begins."
    },
    "game": {
      "name": "Helios",
      "url": "https://boardgamegeek.com/boardgame/154182/helios"
    }
  },
  {
    "rule": {
      "quote": "Whoever last visited Ireland starts the game. Failing that, the oldest player begins.",
      "note": "From unofficial translation on BoardGameGeek.",
      "paraphrase": "The player who last visited Ireland begins."
    },
    "game": {
      "name": "Keltis",
      "url": "https://boardgamegeek.com/boardgame/34585/keltis"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently visited a farm goes first!",
      "paraphrase": "The player who most recently visited a farm begins."
    },
    "game": {
      "name": "Farmageddon",
      "url": "https://boardgamegeek.com/boardgame/102897/farmageddon"
    }
  },
  {
    "rule": {
      "quote": "To begin, the player who can name the most of Jupiter’s moon takes the first turn. Players begin by declaring the number of moons they are able to name and then name them in any order.",
      "paraphrase": "The player who can name the most of Jupiter’s moons begins."
    },
    "game": {
      "name": "Exoplanets",
      "url": "https://boardgamegeek.com/boardgame/163976/exoplanets"
    }
  },
  {
    "rule": {
      "quote": "The shortest player is chosen to be the first player, that player gets the first player token.",
      "paraphrase": "The shortest player begins."
    },
    "game": {
      "name": "Myrmes",
      "url": "https://boardgamegeek.com/boardgame/126792/myrmes"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently used a wok starts the game.",
      "paraphrase": "The player who has most recently used a wok begins."
    },
    "game": {
      "name": "We Will Wok You",
      "url": "https://boardgamegeek.com/boardgame/121993/we-will-wok-you"
    }
  },
  {
    "rule": {
      "quote": "The player who does the best imitation of a woolly mammoth goes first.",
      "paraphrase": "The player who does the best imitation of a woolly mammoth begins."
    },
    "game": {
      "name": "Ooga Booga",
      "url": "https://boardgamegeek.com/boardgame/106753/ooga-booga"
    }
  },
  {
    "rule": {
      "quote": "The player with the best story about being stranded by their own accord goes first, or roll the number die and whoever rolls the highest goes first.",
      "paraphrase": "The player with the best story about being stranded by their own accord begins."
    },
    "game": {
      "name": "Lift Off! Get me off this Planet!",
      "url": "https://boardgamegeek.com/boardgame/161681/lift-get-me-planet"
    }
  },
  {
    "rule": {
      "quote": "The tallest player begins."
    },
    "game": {
      "name": "Campanile",
      "url": "https://boardgamegeek.com/boardgame/301/campanile"
    }
  },
  {
    "rule": {
      "quote": "The president leads the country each turn. Whoever has the most money on his or her person is the first president. (Yes, real money.)",
      "paraphrase": "The player with most money on his or her person begins."
    },
    "game": {
      "name": "Corporate America",
      "url": "https://boardgamegeek.com/boardgame/125943/corporate-america"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently visited London is the start player. Alternatively choose randomly.",
      "paraphrase": "The player who most recently visited London begins."
    },
    "game": {
      "name": "The Great Fire of London 1666",
      "url": "https://boardgamegeek.com/boardgame/41569/great-fire-london-1666"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently bought or sold something (in real life) gets to be the first player. The tiebreaker goes to the person who has known the owner of this game the longest.",
      "paraphrase": "The player who most recently bought or sold something (in real life) begins."
    },
    "game": {
      "name": "Mercante",
      "url": "https://boardgamegeek.com/boardgame/122890/mercante"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently visited a forest goes first.",
      "paraphrase": "The player who most recently visited a forest begins."
    },
    "game": {
      "name": "Lagoon: Land of Druids",
      "url": "https://boardgamegeek.com/boardgame/152053/lagoon-land-druids"
    }
  },
  {
    "rule": {
      "quote": "The player that has most recently committed a legal infraction, no matter how minor, is the start player and will go first.",
      "paraphrase": "The player that has most recently committed a legal infraction, no matter how minor, begins."
    },
    "game": {
      "name": "Grifters",
      "url": "https://boardgamegeek.com/boardgame/168054/grifters"
    }
  },
  {
    "rule": {
      "quote": "The player who looks the most like a zombie takes the First Player token.",
      "paraphrase": "The player who looks the most like a zombie begins."
    },
    "game": {
      "name": "City of Horror",
      "url": "https://boardgamegeek.com/boardgame/120217/city-horror"
    }
  },
  {
    "rule": {
      "quote": "Whoever has a squarer head goes first.",
      "paraphrase": "The player with the squarest head begins."
    },
    "game": {
      "name": "Cube Quest",
      "url": "https://boardgamegeek.com/boardgame/137330/cube-quest"
    }
  },
  {
    "rule": {
      "quote": "The player who last played a train game goes first; give them the Start Player figure. In the event of a tie, the last player who played a game set in Ancient Egypt goes first. If still tied, the player who last ate a piece of fruit goes first.",
      "paraphrase": "The player who last played a train game begins."
    },
    "game": {
      "name": "Cleopatra’s Caboose",
      "url": "https://boardgamegeek.com/boardgame/24795/cleopatras-caboose"
    }
  },
  {
    "rule": {
      "quote": "The player whose sign of the zodiac will next rule the sky – that is, whose birthday will come next – takes the first turn.",
      "paraphrase": "The player whose birthday is next begins."
    },
    "game": {
      "name": "The Stars are Right",
      "url": "https://boardgamegeek.com/boardgame/37696/stars-are-right"
    }
  },
  {
    "rule": {
      "quote": "The player who has ridden on a flying carpet most recently becomes start player.",
      "paraphrase": "The player who has ridden on a flying carpet most recently begins."
    },
    "game": {
      "name": "1001 Karawane",
      "url": "https://boardgamegeek.com/boardgame/30363/1001-karawane"
    }
  },
  {
    "rule": {
      "quote": "The player who most often daydreams begins the game.",
      "paraphrase": "The player who most often daydreams begins."
    },
    "game": {
      "name": "Pocket Rockets",
      "url": "https://boardgamegeek.com/boardgame/43264/pocket-rockets"
    }
  },
  {
    "rule": {
      "quote": "Choose a starting player by taking turns telling a love story – tragic or heart warming – from your personal experience (or, in a pinch, someone else’s).  If everyone agrees you told the most romantic or moving love story, you get to choose who goes first.",
      "paraphrase": "The player who can tell the most romantic or moving love story begins."
    },
    "game": {
      "name": "Genji",
      "url": "https://boardgamegeek.com/boardgame/33196/genji"
    }
  },
  {
    "rule": {
      "quote": "The shortest person starts to play.",
      "paraphrase": "The shortest player begins."
    },
    "game": {
      "name": "DIG",
      "url": "https://boardgamegeek.com/boardgame/222741/dig"
    }
  },
  {
    "rule": {
      "quote": "The player who last built something is the start player and receives the start player figure.",
      "paraphrase": "The player who last built something begins."
    },
    "game": {
      "name": "World Without End",
      "url": "https://boardgamegeek.com/boardgame/43528/world-without-end"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently visited a cave or cellar starts the game, and takes his first turn. ",
      "paraphrase": "The player who most recently visited a cave or cellar begins."
    },
    "game": {
      "name": "Small World Underground",
      "url": "https://www.boardgamegeek.com/boardgame/97786/small-world-underground"
    }
  },
  {
    "rule": {
      "quote": "Determine a first player with the method of your choice (for example, whoever best imitates a lion’s roar)",
      "paraphrase": "The player who best imitates a lion’s roar begins."
    },
    "game": {
      "name": "Zooloretto",
      "url": "https://boardgamegeek.com/boardgame/27588/zooloretto"
    }
  },
  {
    "rule": {
      "quote": "The player who was most recently on an island is the first player.",
      "paraphrase": "The player who was most recently on an island begins."
    },
    "game": {
      "name": "Valletta",
      "url": "https://boardgamegeek.com/boardgame/218920/valletta"
    }
  },
  {
    "rule": {
      "quote": "The last player to have seen a firefly is the starting player.",
      "paraphrase": "The last player to have seen a firefly begins."
    },
    "game": {
      "name": "Smile",
      "url": "https://boardgamegeek.com/boardgame/227893/smile"
    }
  },
  {
    "rule": {
      "quote": "The most organized player takes the score cards and puts the score marker on the 0 space. […] starting with the player with the 0 stone and proceeding clockwise.",
      "paraphrase": "The most organized player begins."
    },
    "game": {
      "name": "Narabi",
      "url": "https://boardgamegeek.com/boardgame/257836/narabi"
    }
  },
  {
    "rule": {
      "quote": "The last player to have visited a Chinatown (New York, Montréal, Paris, etc) receives the First Player card.",
      "paraphrase": "The last player to have visited a Chinatown (New York, Montréal, Paris, etc) begins."
    },
    "game": {
      "name": "Chinatown",
      "url": "https://boardgamegeek.com/boardgame/47/chinatown"
    }
  },
  {
    "rule": {
      "quote": "In the first round, the player who most recently visited a building under construction goes first.",
      "paraphrase": "The player who most recently visited a building under construction begins."
    },
    "game": {
      "name": "Blueprints",
      "url": "https://boardgamegeek.com/boardgame/140933/blueprints"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently watched a Back to the Future movie goes first. In the event of a tie, then the player who's seen the movies the greatest number of times goest first.",
      "paraphrase": "The player who has most recently watched a Back to the Future movie begins."
    },
    "game": {
      "name": "Back to the Future: The Card Game",
      "url": "https://www.boardgamegeek.com/boardgame/71676/back-future-card-game"
    }
  },
  {
    "rule": {
      "quote": "The player with the most mysterious look on his face starts the game;, the others follow clockwise.",
      "paraphrase": "The player with the most mysterious look on their face begins."
    },
    "game": {
      "name": "Die Jagd nach dem Gral",
      "url": "https://www.boardgamegeek.com/boardgame/31909/die-jagd-nach-dem-gral"
    }
  },
  {
    "rule": {
      "quote": "The player who can hold their breath the longest becomes the start player.",
      "paraphrase": "The player who can hold their breath the longest begins."
    },
    "game": {
      "name": "Aquarium",
      "url": "https://www.boardgamegeek.com/boardgame/36649/aquarium"
    }
  },
  {
    "rule": {
      "quote": "Choose a starting player. We recommend that the player who has most recently ridden on a train should be the first player.",
      "paraphrase": "The player who has most recently ridden on a train begins."
    },
    "game": {
      "name": "Station Master",
      "url": "https://www.boardgamegeek.com/boardgame/9615/station-master"
    }
  },
  {
    "rule": {
      "quote": "The player who has carried the heaviest item today is the starting player (stomachs do not count) and takes the first turn.",
      "paraphrase": "The player who has carried the heaviest item today (stomachs do not count) begins."
    },
    "game": {
      "name": "Antics!",
      "url": "https://www.boardgamegeek.com/boardgame/80642/antics"
    }
  },
  {
    "rule": {
      "quote": "The player who was last on a balcony goes first.",
      "paraphrase": "The player who was last on a balcony begins."
    },
    "game": {
      "name": "Council of Verona",
      "url": "https://boardgamegeek.com/boardgame/140863/council-verona"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently cleaned a window receives the starting player tile and places it next to his palace board.",
      "paraphrase": "The player who most recently cleaned a window begins."
    },
    "game": {
      "name": "Azul: Stained Glass of Sintra",
      "url": "https://boardgamegeek.com/boardgame/256226/azul-stained-glass-sintra"
    }
  },
  {
    "rule": {
      "quote": "The player who last captained a container ship starts the game.",
      "paraphrase": "The player who last captained a container ship begins."
    },
    "game": {
      "name": "Container: 10th Anniversary Jumbo Edition!",
      "url": "https://boardgamegeek.com/boardgame/229892/container-10th-anniversary-jumbo-edition"
    }
  },
  {
    "rule": {
      "quote": "The last player to see a songbird receives the first player card. ",
      "paraphrase": "The last player to see a songbird begins."
    },
    "game": {
      "name": "Piepmatz",
      "url": "https://boardgamegeek.com/boardgame/246200/piepmatz"
    }
  },
  {
    "rule": {
      "quote": "The last player to hike into the woods goes first and play continues to the left.",
      "paraphrase": "The last player to hike into the woods begins."
    },
    "game": {
      "name": "Dragonwood",
      "url": "https://boardgamegeek.com/boardgame/172933/dragonwood"
    }
  },
  {
    "rule": {
      "quote": "Give the Starting Player token to the player who ate bacon most recently.",
      "paraphrase": "The player who ate bacon most recently begins."
    },
    "game": {
      "name": "The Grimm Forest",
      "url": "https://boardgamegeek.com/boardgame/212402/grimm-forest"
    }
  },
  {
    "rule": {
      "quote": "The Staring Player is the player who most recently took out the Garbage.",
      "paraphrase": "The player who most recently took out the garbage begins."
    },
    "game": {
      "name": "Garbage Day!",
      "url": "https://boardgamegeek.com/boardgame/179259/garbage-day"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently painted is the first Artist.",
      "paraphrase": "The player who has most recently painted begins."
    },
    "game": {
      "name": "Pantone: The Game",
      "url": "https://boardgamegeek.com/boardgame/248584/pantone-game"
    }
  },
  {
    "rule": {
      "quote": "The player most recently in contact with Law Enforcement gets to pick their character first, then clockwise around the table. […] : The last player to choose a Character takes the First Player marker.",
      "paraphrase": "The player most recently in contact with Law Enforcement begins."
    },
    "game": {
      "name": "Police precinct",
      "url": "https://boardgamegeek.com/boardgame/118536/police-precinct"
    }
  },
  {
    "rule": {
      "quote": "Give the Dealer Token to the person who has most recently seen a horse, or choose a player randomly.",
      "paraphrase": "The player who has most recently seen a horse begins."
    },
    "game": {
      "name": "Tiny Epic Western",
      "url": "https://boardgamegeek.com/boardgame/180852/tiny-epic-western"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently swung a sword is given the active player token and starts the game, or determine the first player randomly.",
      "paraphrase": "The player who most recently swung a sword begins."
    },
    "game": {
      "name": "Tiny Epic Kingdoms",
      "url": "https://boardgamegeek.com/boardgame/148951/tiny-epic-kingdoms"
    }
  },
  {
    "rule": {
      "quote": "Give the First Player Token to the player who has most recently done a scavenger hunt, or determine as a group who gets the token. This player goes first.",
      "paraphrase": "The player who has most recently done a scavenger hunt begins."
    },
    "game": {
      "name": "Tiny Epic Quest",
      "url": "https://boardgamegeek.com/boardgame/201921/tiny-epic-quest"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently visited a mall goes first (or choose your own method to determine and remember the first player).",
      "paraphrase": "The player who most recently visited a mall begins."
    },
    "game": {
      "name": "Tiny Epic Zombies",
      "url": "https://boardgamegeek.com/boardgame/244536/tiny-epic-zombies"
    }
  },
  {
    "rule": {
      "quote": "The starting player is the person who most recently had a birthday. The queen loves birthdays.",
      "paraphrase": "The player who most recently had a birthday begins."
    },
    "game": {
      "name": "Courtier",
      "url": "https://boardgamegeek.com/boardgame/122891/courtier"
    }
  },
  {
    "rule": {
      "quote": "The player who went for a hike most recently is the start player and takes the Element bag.",
      "paraphrase": "The player who went for a hike most recently begins."
    },
    "game": {
      "name": "Ecos: First Continent",
      "url": "https://boardgamegeek.com/boardgame/279254/ecos-first-continent"
    }
  },
  {
    "rule": {
      "quote": "Whoever inherited the most awesome legacy takes the first turn.",
      "paraphrase": "The player who has inherited the most awesome legacy begins."
    },
    "game": {
      "name": "Lost Legacy: Second Chronicle",
      "url": "https://boardgamegeek.com/boardgame/173319/lost-legacy-second-chronicle-vorpal-sword-whitegol"
    }
  },
  {
    "rule": {
      "quote": "The player who was most recently barefoot on the beach goes first.",
      "paraphrase": "The player who was most recently barefoot on the beach begins."
    },
    "game": {
      "name": "Sand Castles",
      "url": "https://boardgamegeek.com/boardgame/174049/sandcastles"
    }
  },
  {
    "rule": {
      "quote": "The weirdest player begins the round by saying “Play Me!” Then, all players begin by rolling their dice at the same time.",
      "paraphrase": "The weirdest player begins."
    },
    "game": {
      "name": "Play Me: Alice in Wonderdice",
      "url": "https://boardgamegeek.com/boardgame/158851/play-me-alice-wonderdice"
    }
  },
  {
    "rule": {
      "quote": "The first player is the one who won the last game, or the one who makes the best dinosaur noise. ",
      "paraphrase": "The player who makes the best dinosaur noise begins."
    },
    "game": {
      "name": "Dino Hunt Dice",
      "url": "https://boardgamegeek.com/boardgame/125368/dino-hunt-dice"
    }
  },
  {
    "rule": {
      "quote": "Choose a start player. We recommend the player who has last read a real newspaper.",
      "paraphrase": "The player who has last read a newspaper begins."
    },
    "game": {
      "name": "Penny Press",
      "url": "https://boardgamegeek.com/boardgame/148205/penny-press"
    }
  },
  {
    "rule": {
      "quote": "After players pack their equipment, they determine the starting player: whoever was most recently in a cave is the starting player.",
      "paraphrase": "The player who was most recently in a cave begins."
    },
    "game": {
      "name": "The Cave",
      "url": "https://boardgamegeek.com/boardgame/129351/cave"
    }
  },
  {
    "rule": {
      "quote": "The Beardiest player begins.",
      "paraphrase": "The player with the best beard begins."
    },
    "game": {
      "name": "Tunhell",
      "url": "https://boardgamegeek.com/boardgame/179627/tunhell"
    }
  },
  {
    "rule": {
      "quote": "Choose a first player, for example the last person who have killed someone (a noisy neighbour, an annoying mother in law, or a really hard boss in a video game).",
      "paraphrase": "The last player to have killed someone (a noisy neighbour, an annoying mother in law, or a really hard boss in a video game) begins."
    },
    "game": {
      "name": "10' to Kill",
      "url": "https://boardgamegeek.com/boardgame/174476/10-kill"
    }
  },
  {
    "rule": {
      "quote": "The player with the darkest past is the boss.",
      "paraphrase": "The player with the darkest past begins."
    },
    "game": {
      "name": "Rob 'n Run",
      "url": "https://boardgamegeek.com/boardgame/234093/rob-n-run"
    }
  },
  {
    "rule": {
      "quote": "The player with the greenest thumb begins the game, and play proceeds clockwise.",
      "paraphrase": "The player with the greenest thumb begins."
    },
    "game": {
      "name": "Lotus",
      "url": "https://boardgamegeek.com/boardgame/198525/lotus"
    }
  },
  {
    "rule": {
      "quote": "The starting player is the one who most recently traveled to a place they had never visited before. Give this player the starting player marker. Play will proceed clockwise.",
      "paraphrase": "The player who most recently traveled to a place they had never visited before begins."
    },
    "game": {
      "name": "Lost Ruins of Arnak",
      "url": "https://boardgamegeek.com/boardgame/312484/lost-ruins-arnak"
    }
  },
  {
    "rule": {
      "quote": "The player who last packed a bag goes first by reading the first Story card out loud.",
      "paraphrase": "The player who last packed a bag begins."
    },
    "game": {
      "name": "Squire for Hire",
      "url": "https://boardgamegeek.com/boardgame/287780/squire-hire"
    }
  },
  {
    "rule": {
      "quote": "Give the First Player Card to the player who has lived in the same place the longest.",
      "paraphrase": "The player who has lived in the same place the longest begins."
    },
    "game": {
      "name": "Villagers",
      "url": "https://boardgamegeek.com/boardgame/241724/villagers"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently lit a candle goes first. When in doubt, the oldest player goes first.",
      "paraphrase": "The player who most recently lit a candle begins."
    },
    "game": {
      "name": "Disney Hocus Pocus: The Game",
      "url": "https://boardgamegeek.com/boardgame/303648/disney-hocus-pocus-game"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently visited a cemetery or graveyard goes first.",
      "paraphrase": "The player who most recently visited a cemetery or graveyard begins."
    },
    "game": {
      "name": "Skulls of Sedlec",
      "url": "https://boardgamegeek.com/boardgame/303553/skulls-sedlec"
    }
  },
  {
    "rule": {
      "quote": "The starting player is the player who last visited an animal park.",
      "paraphrase": "The player who last visited an animal park begins."
    },
    "game": {
      "name": "Bärenpark",
      "url": "https://boardgamegeek.com/boardgame/219513/barenpark"
    }
  },
  {
    "rule": {
      "quote": "The most daring player is designated as the first player.",
      "paraphrase": "The most daring player begins."
    },
    "game": {
      "name": "Carnegie",
      "url": "https://boardgamegeek.com/boardgame/310873/carnegie"
    }
  },
  {
    "rule": {
      "quote": "First off, you’ll need to determine a first player. I recommend choosing the person who has most recently raised a full legion of undead, but if no one is up to that simple task, feel free to choose randomly.",
      "paraphrase": "The player who most recently raised a full legion of undead begins."
    },
    "game": {
      "name": "Overboss",
      "url": "https://boardgamegeek.com/boardgame/310192/overboss-boss-monster-adventure"
    }
  },
  {
    "rule": {
      "quote": "The player who was last in a tavern places the beer mug in front of him as a Start Player marker.",
      "paraphrase": "The player who was last in a tavern begins."
    },
    "game": {
      "name": "The Taverns of Tiefenthal",
      "url": "https://boardgamegeek.com/boardgame/269207/taverns-tiefenthal"
    }
  },
  {
    "rule": {
      "quote": "Finally, determine a Start Player (the last player to use a needle and thread) and give them the Start Player token.",
      "paraphrase": "The last player to use a needle and thread begins."
    },
    "game": {
      "name": "Rococo: Deluxe Edition",
      "url": "https://boardgamegeek.com/boardgame/296100/rococo-deluxe-edition"
    }
  },
  {
    "rule": {
      "quote": "Whoever was sick most recently receives the First Player Marker […]",
      "paraphrase": "The player who was sick most recently begins."
    },
    "game": {
      "name": "Cytosis: A Cell Biology Board Game",
      "url": "https://boardgamegeek.com/boardgame/202977/cytosis-cell-biology-board-game"
    }
  },
  {
    "rule": {
      "quote": "The player who used a tool most recently receives the first player token.",
      "paraphrase": "The player who used a tool most recently begins."
    },
    "game": {
      "name": "Fantastic Factories",
      "url": "https://boardgamegeek.com/boardgame/216600/fantastic-factories"
    }
  },
  {
    "rule": {
      "quote": "The player with the most colorful clothing begins the game.",
      "paraphrase": "The player with the most colorful clothing begins."
    },
    "game": {
      "name": "Hanabi",
      "url": "https://boardgamegeek.com/boardgame/98778/hanabi"
    }
  },
  {
    "rule": {
      "quote": "Decide who goes first by any method of your choosing. We suggest whoever can name the most Disney villians in 30 seconds, but it’s up to you.",
      "paraphrase": "The player who can name the most Disney villains in 30 seconds begins."
    },
    "game": {
      "name": "Munchkin Disney",
      "url": "https://boardgamegeek.com/boardgame/312346/munchkin-disney"
    }
  },
  {
    "rule": {
      "quote": "Decide who goes first by any method of your choosing. We suggest whoever can sing the theme song to SpongeBob SquarePants the best, but it’s up to you.",
      "paraphrase": "The player can sing the theme song to SpongeBob SquarePants the best begins."
    },
    "game": {
      "name": "Munchkin: SpongeBob SquarePants",
      "url": "https://boardgamegeek.com/boardgame/340042/munchkin-spongebob-squarepants"
    }
  },
  {
    "rule": {
      "quote": "The  player  who  most  resembles  their  current  RPG character goes first. Or you could just roll the die.",
      "paraphrase": "The  player  who  most  resembles  their  current  RPG character begins."
    },
    "game": {
      "name": "Munchkins & Mazes",
      "url": "https://boardgamegeek.com/boardgame/323002/munchkins-mazes"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently ate at a restaurant goes first. In case of ties, argue about the quality of the respective restaurants until someone else gets bored and starts drawing cards.",
      "paraphrase": "The player who most recently ate at a restaurant begins."
    },
    "game": {
      "name": "Munchkin Crazy Cooks",
      "url": "https://boardgamegeek.com/boardgame/258610/munchkin-crazy-cooks"
    }
  },
  {
    "rule": {
      "quote": "The player who played a video game most recently gets the first-player token.",
      "paraphrase": "The player who played a video game most recently begins."
    },
    "game": {
      "name": "Megaland",
      "url": "https://boardgamegeek.com/boardgame/251293/megaland"
    }
  },
  {
    "rule": {
      "quote": "The last player to cook goes first.",
      "paraphrase": "The last player to cook something begins."
    },
    "game": {
      "name": "Potions Class",
      "url": "https://boardgamegeek.com/boardgame/260322/potions-class"
    }
  },
  {
    "rule": {
      "quote": "The strongest player starts the game. (The players can use any method they prefer in determining this!)",
      "paraphrase": "The strongest player begins."
    },
    "game": {
      "name": "Thermopyles",
      "url": "https://boardgamegeek.com/boardgame/141019/thermopyles"
    }
  },
  {
    "rule": {
      "quote": "The player who was most recently on a boat is the Staring Player and will take the first turn.",
      "paraphrase": "The player who was most recently on a boat begins."
    },
    "game": {
      "name": "Harbour",
      "url": "https://boardgamegeek.com/boardgame/155969/harbour"
    }
  },
  {
    "rule": {
      "quote": "The King has the first turn, if this is your first round the player who most recently dug a hole plays first.",
      "paraphrase": "The player who most recently dug a hole begins."
    },
    "game": {
      "name": "Kingless",
      "url": "https://boardgamegeek.com/boardgame/283591/kingless@"
    }
  },
  {
    "rule": {
      "quote": "The player who saw a Dragon most recently (the ones in this game do not count!) is the First Player and takes the First Player token.",
      "paraphrase": "The player who saw a Dragon most recently begins."
    },
    "game": {
      "name": "Dragon Castle",
      "url": "https://boardgamegeek.com/boardgame/232219/dragon-castle"
    }
  },
  {
    "rule": {
      "quote": "The player with the best sheep impression goes first.",
      "paraphrase": "The player with the best sheep impression begins. "
    },
    "game": {
      "name": "Sheep Boom Bah",
      "url": "https://boardgamegeek.com/boardgame/259310/sheep-boom-bah"
    }
  },
  {
    "rule": {
      "quote": "The person who opened the box is the starting player for the first round.",
      "paraphrase": "The player who opened the game box begins."
    },
    "game": {
      "name": "Goblins, Inc.",
      "url": "https://boardgamegeek.com/boardgame/110524/goblins-inc"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently paid for a good or service in cash claims the quest token and places it next to his or her shop.",
      "paraphrase": "The player who has most recently paid for a good or service in cash begins."
    },
    "game": {
      "name": "Bargain Quest",
      "url": "https://boardgamegeek.com/boardgame/223740/bargain-quest"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently planted a tree receives the First Player card.",
      "paraphrase": "The player who has most recently planted a tree begins."
    },
    "game": {
      "name": "Ginkgopolis",
      "url": "https://boardgamegeek.com/boardgame/128271/ginkgopolis"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently eaten berries is the start player.",
      "paraphrase": "The player who has most recently eaten berries begins."
    },
    "game": {
      "name": "Carcassonne: Hunters and Gatherers ",
      "url": "https://boardgamegeek.com/boardgame/4390/carcassonne-hunters-and-gatherers"
    }
  },
  {
    "rule": {
      "quote": "The last person who have trimmed a real plant is the first player. If nobody has the green thumb, determine the first player randomly.",
      "paraphrase": "The last player who have trimmed a real plant begins."
    },
    "game": {
      "name": "Topiary",
      "url": "https://boardgamegeek.com/boardgame/210900/topiary"
    }
  },
  {
    "rule": {
      "quote": "The player who most recently watered a plant is the first player.",
      "paraphrase": "The player who most recently watered a plant begins."
    },
    "game": {
      "name": "Canopy",
      "url": "https://boardgamegeek.com/boardgame/295607/canopy"
    }
  },
  {
    "rule": {
      "quote": "The last player to watch a Sci-Fi movie or TV series becomes the 1st player.",
      "paraphrase": "The last player to watch a Sci-Fi movie or TV series begins."
    },
    "game": {
      "name": "Ganymede",
      "url": "https://boardgamegeek.com/boardgame/248005/ganymede"
    }
  },
  {
    "rule": {
      "quote": "The tallest player receives the starting player card and will keep it until the end of the game.",
      "paraphrase": "The tallest player begins."
    },
    "game": {
      "name": "Treelings",
      "url": "https://boardgamegeek.com/boardgame/298017/treelings"
    }
  },
  {
    "rule": {
      "quote": "The player that was last stung by a bee receives the first player token and starts the game.",
      "paraphrase": "The player that was last stung by a bee begins."
    },
    "game": {
      "name": "Meadow",
      "url": "https://boardgamegeek.com/boardgame/314491/meadow"
    }
  },
  {
    "rule": {
      "quote": "Whoever most recently planted a tree goes first, and takes the Start Player token.",
      "paraphrase": "The player who most recently planted a tree begins."
    },
    "game": {
      "name": "CO₂: Second Chance",
      "url": "https://boardgamegeek.com/boardgame/214887/co-second-chance"
    }
  },
  {
    "rule": {
      "quote": "The first player is the person who most recently visited a market.",
      "paraphrase": "The player who most recently visited a market begins."
    },
    "game": {
      "name": "Mercado de Lisboa",
      "url": "https://boardgamegeek.com/boardgame/262477/mercado-de-lisboa"
    }
  },
  {
    "rule": {
      "quote": "Choose the player who has most recently visited a museum containing Egyptian artifacts as the start player. ",
      "paraphrase": "The player who has most recently visited a museum containing Egyptian artifacts begins. "
    },
    "game": {
      "name": "Tutankhamun",
      "url": "https://boardgamegeek.com/boardgame/286667/tutankhamun"
    }
  },
  {
    "rule": {
      "quote": "The player who has most recently traveled near Scotland is First Player.",
      "paraphrase": "The player who has most recently traveled near Scotland begins."
    },
    "game": {
      "name": "Schotten Totten",
      "url": "https://boardgamegeek.com/boardgame/372/schotten-totten"
    }
  },
  {
    "rule": {
      "quote": "The most splendid player is the start player, then play continues around in clockwise order.",
      "paraphrase": "The most splendid player begins."
    },
    "game": {
      "name": "Blue Moon City",
      "url": "https://boardgamegeek.com/boardgame/21882/blue-moon-city"
    }
  },
  {
    "rule": {
      "quote": "The player who woke up the earliest today should start the game.",
      "paraphrase": "The player who woke up the earliest today begins."
    },
    "game": {
      "name": "Dale of Merchants",
      "url": "https://boardgamegeek.com/boardgame/176165/dale-merchants"
    }
  },
  {
    "rule": {
      "quote": "The player who has visited Amsterdam most often starts (or the players can draw lots for the privilege).",
      "paraphrase": "The player who has visited Amsterdam most often begins."
    },
    "game": {
      "name": "Merchants of Amsterdam",
      "url": "https://boardgamegeek.com/boardgame/531/merchants-amsterdam"
    }
  },
  {
    "rule": {
      "quote": "The player who last ate fish gets the 5 dice and starts the game.",
      "paraphrase": "The player who last ate fish begins."
    },
    "game": {
      "name": "Sushizock im Gockelwok",
      "url": "https://boardgamegeek.com/boardgame/37400/sushizock-im-gockelwok"
    }
  },
  {
    "rule": {
      "quote": "The player whose birthday was the most recent will get the starting player card and start the game",
      "paraphrase": "The player whose birthday was the most recent begins."
    },
    "game": {
      "name": "Lost Cities: Rivals",
      "url": "https://boardgamegeek.com/boardgame/253398/lost-cities-rivals"
    }
  },
  {
    "rule": {
      "quote": "The player who was most recently on a ship becomes the start player.",
      "paraphrase": "The player who was most recently on a ship begins."
    },
    "game": {
      "name": "Merchants",
      "url": "https://boardgamegeek.com/boardgame/32114/merchants"
    }
  },
  {
    "rule": {
      "quote": "The player who has last seen a real penguin begins.",
      "paraphrase": "The player who has last seen a real penguin begins."
    },
    "game": {
      "name": "Penguin Party",
      "url": "https://boardgamegeek.com/boardgame/56933/penguin-party"
    }
  },
  {
    "rule": {
      "quote": "The hairiest player starts, then play goes in a clockwise direction.",
      "paraphrase": "The hairiest player begins."
    },
    "game": {
      "name": "Jäger und Sammler",
      "url": "https://boardgamegeek.com/boardgame/65568/jager-und-sammler"
    }
  },
  {
    "rule": {
      "quote": "The player who travelled most recently goes first.",
      "paraphrase": "The player who travelled most recently begins."
    },
    "game": {
      "name": "Chartae",
      "url": "https://boardgamegeek.com/boardgame/269257/chartae"
    }
  },
  {
    "rule": {
      "quote": "Then all players moo. The loudest moo’er will play first.",
      "paraphrase": "The player who “moo” the loudest begins."
    },
    "game": {
      "name": "Black Sheep",
      "url": "https://boardgamegeek.com/boardgame/36739/black-sheep"
    }
  },
  {
    "rule": {
      "quote": "Der goldigste Spieler beginnt.",
      "paraphrase": "The cutest player begins.",
      "note": "Translated from German."
    },
    "game": {
      "name": "GOLD",
      "url": "https://boardgamegeek.com/boardgame/306680/gold"
    }
  },
  {
    "rule": {
      "quote": "The oldest, and thus wisest, player begins the game, and play continues clockwise until the end of the game is triggered.",
      "paraphrase": "The oldest, and thus wisest, player begins."
    },
    "game": {
      "name": "Tajuto",
      "url": "https://boardgamegeek.com/boardgame/286830/tajuto"
    }
  },
  {
    "rule": {
      "quote": "Whoever most recently ate a cooked, green vegetable goes first and play continues to the left.",
      "paraphrase": "The player who most recently ate a cooked, green vegetable begins."
    },
    "game": {
      "name": "Abandon All Artichokes",
      "url": "https://boardgamegeek.com/boardgame/302260/abandon-all-artichokes"
    }
  },
  {
    "rule": {
      "quote": "The player to have most recently committed regicide goes first.",
      "paraphrase": "The player who most recently committed regicide begins."
    },
    "game": {
      "name": "Regicide",
      "url": "https://boardgamegeek.com/boardgame/307002/regicide"
    }
  }
]
"""
