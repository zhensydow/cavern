data Request = RqUnknown

data Response = RpUnknown

play :: Scene -> World -> Player -> Request -> Response
play scene world player request = RpUnknown
