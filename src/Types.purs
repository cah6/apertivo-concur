module Types where

import Custom.Prelude

import Simple.JSON as JSON

type HappyHour =
  { city :: String
  , restaurant :: String
  , id :: String
  , link :: String
  , placeId :: String
  , latLng :: LatLng
  , schedule :: Array Schedule
  }

type Schedule =
  { days :: Array String
  , scheduleDescription :: String
  , time :: String
  }

type LatLng =
  { latitude :: Number
  , longitude :: Number
  }


staticHappyHours :: Array HappyHour
staticHappyHours = case JSON.readJSON exampleHappyHours of
  Left e -> []
  Right xs -> xs

exampleHappyHours :: String
exampleHappyHours = """
[
  {
    "latLng": {
      "latitude": 42.3599043,
      "longitude": -83.06867
    },
    "schedule": [
      {
        "days": [
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday"
        ],
        "time": "15:00-18:00",
        "scheduleDescription": "$5 combo of burger, duck fat fries, and PBR"
      }
    ],
    "restaurant": "Common Pub Detroit",
    "link": "https://detroit.eater.com/maps/best-new-detroit-happy-hour-map/common-pub-detroit",
    "city": "Detroit",
    "id": "09244c2e-0d87-4004-864b-a66971348d16",
    "placeId": "ChIJnU2kyqLSJIgRbDqpVFux0PM"
  },
  {
    "latLng": {
      "latitude": 42.33471100000001,
      "longitude": -83.04327699999999
    },
    "schedule": [
      {
        "days": [
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday"
        ],
        "time": "16:30-18:30",
        "scheduleDescription": "1/2 off beer (min $2.50) and wine (min $4), $5 plates"
      }
    ],
    "restaurant": "Firebird Tavern",
    "link": "https://www.firebirdtavern.com/menus/detroit-happy-hour/",
    "city": "Detroit",
    "id": "50b39ab5-89f2-442d-b1fc-bea650008ace",
    "placeId": "ChIJf4jamy4tO4gRlgZDMN-0m4A"
  },
  {
    "latLng": {
      "latitude": 42.3524391,
      "longitude": -83.0616
    },
    "schedule": [
      {
        "days": [
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday"
        ],
        "time": "22:00-23:30",
        "scheduleDescription": "$5 off burgers, 1/2 off fries and shareables, $1.50 off drafts, $3 wine and wells"
      },
      {
        "days": [
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday"
        ],
        "time": "15:00-18:00",
        "scheduleDescription": "$5 off burgers, 1/2 off fries and shareables, $1.50 off drafts, $3 wine and wells"
      }
    ],
    "restaurant": "HopCat",
    "link": "https://hopcat.com/detroit",
    "city": "Detroit",
    "id": "768640e8-79c0-414e-af0c-f56682c2ed22",
    "placeId": "ChIJMSuIlbnSJIgRbUFj__-VGdA"
  }
]
"""
