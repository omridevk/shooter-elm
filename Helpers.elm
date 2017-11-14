module Helpers exposing (angle)

angle x y = ((atan2 x -y) * (180 / pi))