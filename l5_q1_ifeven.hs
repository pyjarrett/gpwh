ifEven f x = if even x
    then f x
    else x

inc x = x + 1
double x = 2 * x
square x = x * x
ifEvenInc x = ifEven inc x
ifEvenDouble x = ifEven double x
ifEvenSquare x = ifEven square x
