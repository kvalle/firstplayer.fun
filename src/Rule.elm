module Rule exposing (Rule, allTheRules, ruleGenerator)

import Json.Decode exposing (Decoder)
import Random exposing (Generator)


type alias Rule =
    { game : String
    , rule : String
    , url : String
    }


ruleDecoder : Decoder Rule
ruleDecoder =
    Json.Decode.map3 Rule
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "rule" Json.Decode.string)
        (Json.Decode.field "url" Json.Decode.string)


ruleGenerator : Maybe (Generator Rule)
ruleGenerator =
    let
        lastRules =
            List.tail allTheRules |> Maybe.withDefault []

        makeGenerator firstRule =
            Random.uniform firstRule lastRules
    in
    List.head allTheRules
        |> Maybe.map makeGenerator


allTheRules : List Rule
allTheRules =
    Json.Decode.decodeString (Json.Decode.list ruleDecoder) rulesJson
        |> Result.withDefault []


rulesJson : String
rulesJson =
    """
[
  {
    "rule": "The starting player is the player who most recently constructed something (in real life).",
    "name": "Tiny Towns",
    "url": "https://boardgamegeek.com/boardgame/265736/tiny-towns"
  },
  {
    "rule": "The player who proves best able - by general consent - to do a polar bear impression becomes the Starting Player.",
    "name": "Ice Flow",
    "url": "https://boardgamegeek.com/boardgame/31133/ice-flow"
  },
  {
    "rule": "The player who last ate something coconut flavored starts the round and play continues clockwise.",
    "name": "Tiki Topple",
    "url": "https://boardgamegeek.com/boardgame/34373/tiki-topple"
  },
  {
    "rule": "The hairiest player begins a preparatory round, […]",
    "name": "Mammoth Hunters (aka, Eiszeit)",
    "url": "https://boardgamegeek.com/boardgame/5767/mammoth-hunters"
  },
  {
    "rule": "The person who last has been onboard of a ship starts, the other players follow in a clock-wise order.",
    "name": "Atlantic Star",
    "url": "https://boardgamegeek.com/boardgame/2570/atlantic-star"
  },
  {
    "rule": "The player with the worst medical history leads the first trick.",
    "name": "Quacksalbe",
    "url": "https://boardgamegeek.com/boardgame/819/quacksalbe"
  },
  {
    "rule": "The meanest player begins the game. ",
    "name": "Vegas",
    "url": "https://boardgamegeek.com/boardgame/1255/vegas"
  },
  {
    "rule": "Whoever can stay under water the longest goes first. If that cannot be determined now, the oldest player starts.",
    "name": "The Reef",
    "url": "https://boardgamegeek.com/boardgame/509/reef"
  },
  {
    "rule": "The player with the most colorful clothing begins.",
    "name": "Flowerpower",
    "url": "https://boardgamegeek.com/boardgame/1545/flowerpower"
  },
  {
    "rule": "The player with the largest cowboy boots (or other shoes) begins, the others follow in clockwise direction. ",
    "name": "Abilene",
    "url": "https://boardgamegeek.com/boardgame/906/abilene"
  },
  {
    "rule": "The player who can sing the sweetest high C begins.  ",
    "name": "Maestro",
    "url": "https://boardgamegeek.com/boardgame/569/maestro"
  },
  {
    "rule": "Whoever has last reached the peak of Mount Everest using nothing but blue and white checkered stilts carved from the wood of a Mammoth tree is declared the Starting Player. In case of a tie, the wisest player of the group begins the game. ",
    "name": "The Bridges of Shangri-La",
    "url": "https://boardgamegeek.com/boardgame/8190/bridges-shangri-la"
  },
  {
    "rule": "The last person to feed a live duck starts the game and playproceeds clockwise",
    "name": "Duck, Duck, Bruce",
    "url": "https://boardgamegeek.com/boardgame/2114/duck-duck-bruce"
  },
  {
    "rule": "The person with the longest hair goes first.",
    "name": "Aquarius",
    "url": "https://boardgamegeek.com/boardgame/814/aquarius"
  },
  {
    "rule": "Give the starting leader card to the leader (player) who had the least amount of sleep last night.",
    "name": "Zombie State: Diplomacy of the Dead ",
    "url": "https://boardgamegeek.com/boardgame/61484/zombie-state-diplomacy-dead"
  },
  {
    "rule": "The Player who has travelled to the farthest destination in the last year becomes the First Player and receives immediately the Bartolomeu Dias Tile and 2 Victory Points.",
    "name": "Vasco da Gama",
    "url": "https://boardgamegeek.com/boardgame/41002/vasco-da-gama"
  },
  {
    "rule": "The player that most recently visited a port in real life begins the game.",
    "name": "Cargo Noir",
    "url": "https://boardgamegeek.com/boardgame/90305/cargo-noir"
  },
  {
    "rule": "The player who has most recently been abducted by aliens begins.  In case of a tie, the player who has most recently visited a farm can go first.  If you still can't figure it out, pick someone to go first.",
    "name": "Pasture 51: They Came for our Angus",
    "url": "https://boardgamegeek.com/boardgame/152825/pasture-51-they-came-our-angus"
  },
  {
    "rule": "The starting player marker goes to the player who paid for something most recently.",
    "name": "Last Will",
    "url": "https://boardgamegeek.com/boardgame/97842/last-will"
  },
  {
    "rule": "To determine who goes first, all players will attempt to guess the current time. Whoever comes closest starts the game.",
    "name": "Chrononauts",
    "url": "https://boardgamegeek.com/boardgame/815/chrononauts"
  },
  {
    "rule": "The first player at the beginning of the game is the player who most recently had a great ideea (or random if you prefer).",
    "name": "Tempus",
    "url": "https://boardgamegeek.com/boardgame/17161/tempus"
  },
  {
    "rule": "The player who can tell the most honest fact his been through recives the \\"Sheriff\\" token.",
    "name": "Robin Hood",
    "url": "https://boardgamegeek.com/boardgame/104640/robin-hood"
  },
  {
    "rule": "The nicest player gets the Starting Player Token. (Or you can choose the starting player randomly.)",
    "name": "Dungeon Lords",
    "url": "https://boardgamegeek.com/boardgame/45315/dungeon-lords"
  },
  {
    "rule": "The player who most recently fed a pet gets the Starting Player Token. Of course, you can also choose the starting player randomly, arbitrarily, or by fiat. Just pick somebody.",
    "name": "Dungeon Petz",
    "url": "https://boardgamegeek.com/boardgame/97207/dungeon-petz"
  },
  {
    "rule": "The player with either (A) the best mustache, or (B) the longest hair goes First. If one player has a very nice mustache and the other player has the longest hair, settle the issue with an arm wrestling match.",
    "name": "The Great Heartland Hauling Co.",
    "url": "https://boardgamegeek.com/boardgame/111417/great-heartland-hauling-co"
  },
  {
    "rule": "The player who last visited an island goes first and play continues to the left. ",
    "name": "Forbidden Island",
    "url": "https://boardgamegeek.com/boardgame/65244/forbidden-island"
  },
  {
    "rule": "The thirstiest player goes first and play continues to the left.",
    "name": "Forbidden Desert",
    "url": "https://boardgamegeek.com/boardgame/136063/forbidden-desert"
  },
  {
    "rule": "The Mission Leader token is given to the hairiest player.",
    "name": "The Grizzled",
    "url": "https://boardgamegeek.com/boardgame/171668/grizzled"
  },
  {
    "rule": "Each player receives a game sheet and a pen. The smartest among them takes the six dice and starts the game.",
    "name": "That's Pretty Clever (aka Ganz schön clever)",
    "url": "https://boardgamegeek.com/boardgame/244522/s-pretty-clever"
  },
  {
    "rule": "The player who was the last to eat dates will get the starting player token and takes the first turn. If neither player has eaten dates, the blue player starts.",
    "name": "Targi",
    "url": "https://boardgamegeek.com/boardgame/118048/targi"
  },
  {
    "rule": "The player who most recently extracted DNA from a mosquito trapped in amber will be the first player. If no player has successfully accomplished that task, the player who most recently visited a theme park will be the first player.",
    "name": "Dinosaur Island",
    "url": "https://boardgamegeek.com/boardgame/221194/dinosaur-island"
  },
  {
    "rule": "The start player is the player who was most recently on board a ship.",
    "name": "Maracaibo",
    "url": "https://boardgamegeek.com/boardgame/276025/maracaibo"
  },
  {
    "rule": "The player who most recently visited Yokohama receives the Start Player Card and becomes the starting player. If this method does not work in determining a starting player, use any preferred method in choosing a starting player.",
    "name": "Yokohama",
    "url": "https://boardgamegeek.com/boardgame/196340/yokohama"
  },
  {
    "rule": "The player who has seen the film “The Martian” the most times is the first player.",
    "name": "On Mars",
    "url": "https://boardgamegeek.com/boardgame/184267/mars"
  },
  {
    "rule": "The last person to cook something goes first.",
    "name": "The Quacks of Quedlinburg",
    "url": "https://boardgamegeek.com/boardgame/244521/quacks-quedlinburg"
  },
  {
    "rule": "The player who last used a needle begins.",
    "name": "Patchwork",
    "url": "https://boardgamegeek.com/boardgame/163412/patchwork"
  },
  {
    "rule": "The player who visited Lisboa most recently is the starting player, and takes the Starting Player marker.",
    "name": "Lisboa",
    "url": "https://boardgamegeek.com/boardgame/161533/lisboa"
  },
  {
    "rule": "The sneakiest player gets to go first (or you may choose randomly).",
    "name": "Clank!: A Deck-Building Adventure",
    "url": "https://boardgamegeek.com/boardgame/201808/clank-deck-building-adventure"
  },
  {
    "rule": "The sneakiest player gets to go first (or you may choose randomly).",
    "name": "Clank! In! Space!: A Deck-Building Adventure",
    "url": "https://boardgamegeek.com/boardgame/233371/clank-space-deck-building-adventure"
  },
  {
    "rule": "The player who has most recently been to another city goes first. Give that player the First Player marker.",
    "name": "Lords of Waterdeep",
    "url": "https://boardgamegeek.com/boardgame/110327/lords-waterdeep"
  },
  {
    "rule": "The player who most recently embarked on a voyage around the world is the start player.",
    "name": "The Voyages of Marco Polo",
    "url": "https://boardgamegeek.com/boardgame/171623/voyages-marco-polo"
  },
  {
    "rule": "The most humble player goes first.",
    "name": "Everdell",
    "url": "https://boardgamegeek.com/boardgame/199792/everdell"
  },
  {
    "rule": "The Starting Player is the player who lives closest to water.",
    "name": "Le Havre",
    "url": "https://boardgamegeek.com/boardgame/35677/le-havre"
  },
  {
    "rule": "Give the Starting Player Marker to the player who has spent the least time on planet Terra, in the Sol system.",
    "name": "Eclipse",
    "url": "https://boardgamegeek.com/boardgame/72125/eclipse"
  },
  {
    "rule": "Give the Starting Player Marker to the player who most recently sacrificed something. In case of a tie, give the Starting Player Marker to the player who volunteers for the next sacrifice.",
    "name": "Tzolk'in: The Mayan Calendar",
    "url": "https://boardgamegeek.com/boardgame/126163/tzolk-mayan-calendar"
  },
  {
    "rule": "Give the First Player token to the player who was born furthest to the north.",
    "name": "Blood Rage",
    "url": "https://boardgamegeek.com/boardgame/170216/blood-rage"
  },
  {
    "rule": "The player whose ears are the most pointed starts the game.",
    "name": "Small World",
    "url": "https://boardgamegeek.com/boardgame/40692/small-world"
  },
  {
    "rule": "The player who has most recently earned glory in battle receives the First Player Marker. (Alternately, select a starting player at random.)",
    "name": "Champions of Midgard",
    "url": "https://boardgamegeek.com/boardgame/172287/champions-midgard"
  },
  {
    "rule": "Whoever sat down first at the table goes first.",
    "name": "The Quest for El Dorado",
    "url": "https://boardgamegeek.com/boardgame/217372/quest-el-dorado"
  },
  {
    "rule": "The player who has visited the most European countries in his lifetime begins the game.",
    "name": "Ticket to Ride: Europe",
    "url": "https://boardgamegeek.com/boardgame/14996/ticket-ride-europe"
  },
  {
    "rule": "The player who is the most experienced traveler goes first.",
    "name": "Ticket to Ride",
    "url": "https://boardgamegeek.com/boardgame/9209/ticket-ride"
  },
  {
    "rule": "The player who most recently rode a bike (youngest if tied).",
    "name": "Flamme Rouge",
    "url": "https://boardgamegeek.com/boardgame/199478/flamme-rouge"
  },
  {
    "rule": "Whoever was most recently on a date goes first (if tied, the youngest player wins the tie). ",
    "name": "Love Letter",
    "url": "https://boardgamegeek.com/boardgame/129622/love-letter"
  },
  {
    "rule": "The player who most recently visited Portugal takes the starting player marker",
    "name": "Azul",
    "url": "https://boardgamegeek.com/boardgame/230802/azul"
  },
  {
    "rule": "Randomly select a player to be the Start Player and give them the Dice Bag. One suggested method is whoever most recently visited a cathedral.",
    "name": "Sagrada",
    "url": "https://boardgamegeek.com/boardgame/199561/sagrada"
  },
  {
    "rule": "Whoever can balance on one foot like a flamingo for the longest time, starts the game.",
    "name": "Animal Upon Animal",
    "url": "https://boardgamegeek.com/boardgame/17329/animal-upon-animal"
  },
  {
    "rule": "The last player to have seen a circus act becomes the first player for the first act and takes the first player token",
    "name": "Meeple Circus",
    "url": "https://boardgamegeek.com/boardgame/193214/meeple-circus"
  },
  {
    "rule": "Give the starting player token to the player who was most recently in a laboratory.",
    "name": "Alchemists",
    "url": "https://boardgamegeek.com/boardgame/161970/alchemists"
  },
  {
    "rule": "The player with the huskiest voice is the starting player. The player with the next huskiest voice is second, and so on.  Wizened old gamers may decide order randomly.",
    "name": "Snow Tails",
    "url": "https://boardgamegeek.com/boardgame/38054/snow-tails"
  },
  {
    "rule": "A player starts, the one who has seen most recently a kung-fu movie. ",
    "name": "Ghost Stories",
    "url": "https://boardgamegeek.com/boardgame/37046/ghost-stories"
  },
  {
    "rule": "The player who last read a history book receives the start player card.",
    "name": "Troyes",
    "url": "https://boardgamegeek.com/boardgame/73439/troyes"
  },
  {
    "rule": "The player who most recently has dug a planting bed in their garden takes the Starting player token and becomes the Starting player.",
    "name": "Terra Mystica",
    "url": "https://boardgamegeek.com/boardgame/120677/terra-mystic"
  },
  {
    "name": "Kodama: The Tree Spirits",
    "rule": "The player wearing the most green starts the game as the first player.",
    "url": "https://boardgamegeek.com/boardgame/181810/kodama-tree-spirits"
  },
  {
    "name": "Archaeology: The New Expedition",
    "rule": "The first player is the last person to have put their feet in the sand.",
    "url": "https://boardgamegeek.com/boardgame/191300/archaeology-new-expedition"
  },
  {
    "name": "Arboretum",
    "rule": "The player who last watered a plant is the start player. ",
    "url": "https://boardgamegeek.com/boardgame/140934/arboretum"
  },
  {
    "name": "Mord im Arosa",
    "rule": "The player who was the last to have a knife in his hand throws both victims into the shaft of the hotel. After that, players in turn throw two each of their own clues, one after the other, into the shaft.",
    "url": "https://boardgamegeek.com/boardgame/80006/mord-im-arosa"
  },
  {
    "name": "Bunny Bunny Moose Moose",
    "rule": "In the first round, the narrator is the player who looks most like a moose. In case of a tie, choose the player who looks most like a rabbit. If players are still tied, choose the narrator randomly. Or just pick the narrator any way you like.",
    "url": "https://boardgamegeek.com/boardgame/59149/bunny-bunny-moose-moose"
  },
  {
    "name": "Pharaoh's Gulo Gulo",
    "rule": "The player who can best walk like a mummy may begin.",
    "url": "https://boardgamegeek.com/boardgame/175088/pharaohs-gulo-gulo"
  },
  {
    "name": "Reef Encounter",
    "rule": "The player who most recently went swimming now chooses the start player.",
    "url": "https://boardgamegeek.com/boardgame/12962/reef-encounter"
  },
  {
    "name": "Hamburgum",
    "rule": "The player who last visited a church becomes the start player.",
    "url": "https://boardgamegeek.com/boardgame/30381/hamburgum"
  },
  {
    "name": "Savannah Tails",
    "rule": "The player with the longest neck is the 1st player, the second longest is 2nd and so on.",
    "url": "https://boardgamegeek.com/boardgame/54507/savannah-tails"
  },
  {
    "name": "Android",
    "rule": "The player who has read the most science fiction books goes first. If the players can’t agree who this is, select one player at random to go first.",
    "url": "https://boardgamegeek.com/boardgame/39339/android"
  },
  {
    "name": "Shear Panic",
    "rule": "The starting player is the person who most recently had a haircut (was sheared!).",
    "url": "https://boardgamegeek.com/boardgame/18866/shear-panic"
  },
  {
    "name": "Cleopatra and the Society of Architects",
    "rule": "The player with the best Egyptian credentials (a nose as famous as Cleopatra's, a mummified Crocodile pet, or an extensive hieroglyphic library) starts the game. Otherwise, the youngest player goes first.",
    "url": "https://boardgamegeek.com/boardgame/22141/cleopatra-and-society-architects"
  },
  {
    "name": "Zombie Dice",
    "rule": "The first player is the one who won the last game, or the one who can say “Braaaaains!” with the most feeling.",
    "url": "https://boardgamegeek.com/boardgame/62871/zombie-dice"
  }
]

"""
