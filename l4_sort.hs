import Data.List

names = [("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Abraham", "Hook"),
    ("Stephen", "Morris")]

compareNames name1 name2 = if lastCompare /= EQ
                            then lastCompare
                            else firstCompare
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2
          lastCompare = compare lastName1 lastName2
          firstCompare = compare firstName1 firstName2

locations = ["PO Box 1234, San Francisco, CA, 94111",
    "PO Box 789, New York, NY, 10013",
    "PO Box 456, Reno, NV, 89523"]

addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location

sfOffice name = if lastName < "L"
           then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
           else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = (fst name) ++ " " ++ (snd name) 

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV, 89523"
    where nameText = snd name

dcOffice name = nameText ++ " - Washington, DC"
    where nameText = (fst name) ++ " " ++ (snd name) ++ " Esq."

getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

-- Continuation in Lesson 5

flipBinaryArgs fn = (\a b -> fn b a)
addressLetterV2 = flipBinaryArgs addressLetter
addressLetterNY = addressLetterV2 "ny"
