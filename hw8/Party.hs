module Party where

import Data.Monoid
import Data.Tree      
import Employee
 

-- | Adds Employee to the GuestList and update cached Fun score.
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = ef}) (GL lst gf) = GL (emp:lst) (ef+gf)

instance Monoid GuestList where
    mempty = GL [] 0 
    mappend (GL al af) (GL bl bf) = GL (al ++ bl) (af + bf)
moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ af) g2@(GL _ bf) = if af > bf then g1 else g2 

e1 = Emp "a" 10 
e2 = Emp "b" 20
list0 = GL [] 0 
list1 = glCons e1 list0 
list2 = glCons e2 list0

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f init (Node {rootLabel = rl,subForest = sf}) = f rl (map (treeFold f init) sf) 

nextLevel :: Employee -> [(GuestList,GuestList)] -> (GuestList,GuestList)
nextLevel boss bestLists = (maximums withBossL,maximums withoutBossL)
    where   withoutBossL = map fst bestLists
            withoutSubBoss = map snd bestLists
            withBossL = map (glCons boss) withoutSubBoss

maximums :: (Monoid a,Ord a) => [a] -> a 
maximums [] = mempty
maximums lst = maximum lst 

maxFun :: Tree Employee -> GuestList 
maxFun tree = uncurry max res 
    where res = treeFold nextLevel (mempty,mempty) tree 

formatGL :: GuestList -> String
formatGL (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
  where employees = map (\(Emp {empName = name}) -> name) lst            

computeOutput :: String -> String 
computeOutput = formatGL . maxFun . read  

main :: IO()
main = readFile "company.txt" >>= putStrLn . computeOutput