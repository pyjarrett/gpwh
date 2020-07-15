robot (name, attack, hp) = \message -> message (name, attack, hp)

{- helper functions -}
name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, h) = h

{- accessor functions -}
getName   aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp     aRobot = aRobot hp

{- mutation function -}
setName   aRobot n = aRobot (\(_, a, h) -> robot (n, a, h))
setAttack aRobot a = aRobot (\(n, _, h) -> robot (n, a, h))
setHp     aRobot h = aRobot (\(n, a, _) -> robot (n, a, h))

printRobot aRobot = aRobot (\(n, a, h) -> n
                    ++ "  Attack: " ++ (show a)
                    ++ "  Hp: " ++ (show h))

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
    where attack = if getHp aRobot > 0
                   then getAttack aRobot
                   else 0

killerRobot = robot ("IG-88", 25, 200)
gentleGiant = robot ("Mr. Friendly", 10, 300)
gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

allRobots = [killerRobot, gentleGiant]
getRobotHp robots = map getHp robots
