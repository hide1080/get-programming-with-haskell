robot (name, attack, hp) = \ message -> message (name, attack, hp)

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, h) = h

getName r = r name
getAttack r = r attack
getHP r = r hp

setName r newName = r (\ (n, a, h) -> robot (newName, a, h))
setAttack r newAttack = r (\ (n, a, h) -> robot (n, newAttack, h))
setHP r newHP = r (\ (n, a, h) -> robot (n, a, newHP))

killerRobot = robot ("Killer", 25, 200)
nicerRobot = setName killerRobot "Kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHP killerRobot 50

printRobot r =
  r (\ (n, a, h) -> n ++
                    " attack:" ++ (show a) ++
                    " hp:" ++ (show h))

damage r attackDamage =
  r (\ (n, a, h) -> robot (n, a, h - attackDamage))

fight attacker defender = damage defender attack
  where attack = if getHP attacker > 10
                 then getAttack attacker
                 else 0
gentleGiant = robot ("Mr. Friendly", 10, 300)

--10.3
fastRobot = robot ("speedy",15,40)
slowRobot = robot ("slowpoke",20,30)

-- fastRobotRound1 = fight slowRobot fastRobot
-- slowRobotRound1 = fight fastRobot slowRobot
-- fastRobotRound2 = fight slowRobotRound1 fastRobotRound1
-- slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
-- fastRobotRound3 = fight slowRobotRound2 fastRobotRound2
-- slowRobotRound3 = fight fastRobotRound2 slowRobotRound2

-- slowRobotRound1 = fight fastRobot slowRobot
-- fastRobotRound1 = fight slowRobotRound1 fastRobot
-- slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
-- fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
-- slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
-- fastRobotRound3 = fight slowRobotRound3 fastRobotRound2

fastRobotRound3 = fight slowRobotRound3 fastRobotRound2
fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
fastRobotRound1 = fight slowRobotRound1 fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
slowRobotRound1 = fight fastRobot slowRobot
