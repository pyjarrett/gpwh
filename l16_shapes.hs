data Shape = Circle Double
    | Square Double
    | Rectangle Double Double

area (Circle radius) = pi * radius^2
area (Square side) = side^2
area (Rectangle width height) = width * height

perimeter (Circle radius) = 2 * pi * radius
perimeter (Square side) = 4 * side
perimeter (Rectangle width height) = 2 * width + 2 * height
