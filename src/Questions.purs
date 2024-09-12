module Questions where

type Question =
  { prompt :: String
  , answers :: Array String
  }

initialQuestions :: Array Question
initialQuestions =
  [ { prompt: "Which color do you prefer, in general?"
    , answers:
        [ "Red"
        , "Green"
        , "Blue"
        , "Orange"
        ]
    }
  , { prompt: "Have you or your family ever owned an animal?"
    , answers:
        [ "Yes"
        , "No"
        ]
    }
  , { prompt: "Waterfalls?"
    , answers:
        [ "I hate Waterfalls"
        , "I don't hate Waterfalls"
        ]
    }
  , { prompt: "Would you feel hesitant to throw around a basketball indoors?"
    , answers:
        [ "Yes"
        , "No"
        ]
    }
  , { prompt: "Which is better? Use your initial interpretation."
    , answers:
        [ "Snake"
        , "Coffee"
        , "Tree"
        ]
    }
  , { prompt: "How many syllables are in \"crazed\"?"
    , answers:
        [ "1"
        , "2"
        , "3"
        ]
    }
  , { prompt: "All else being equal, if the Earth had 2 moons, how would you feel about it?"
    , answers:
        [ "Yes"
        , "Neutral"
        , "No"
        ]
    }
  , { prompt: "In front of you is a bottomless pit. Truely bottomless. Do you jump in?"
    , answers:
        [ "Yes"
        , "Yes, conditional on that I have a way to escape any time I want"
        , "No"
        ]
    }
  , { prompt: "Which number is more lucky?"
    , answers:
        [ "0"
        , "1"
        , "2"
        , "three"
        , "4"
        ]
    }
  ]
