{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es =
    -- ns => nodes
    -- es => edges
    let nodesSet = S.fromList ns
        edgesSet = S.fromList es
    in (nodesSet, edgesSet)

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = fst

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = snd

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = 
    let allEdges = edges graph in
        S.map snd (S.filter (\(x, _) -> x == node) allEdges)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = 
    let allEdges = edges graph in
        S.map fst (S.filter (\(_, x) -> x == node) allEdges)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = 
    let allNodes = nodes graph 
        allEdges = edges graph
        newEdges = S.filter (\(x, y) -> x /= node && y /= node) allEdges
        newNodes = S.delete node allNodes
    in (newNodes, newEdges)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
createEdge edge searchedValue newValue
    | ((fst edge) == searchedValue) = (newValue, (snd edge))
    | otherwise = ((fst edge), newValue)

splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph = 
    let allNodes = nodes graph
        allEdges = edges graph
        -- muchiile care contin nodul old
        targetEdges = S.filter (\(x, y) -> x == old || y == old) allEdges
        -- muchiile care nu contin nodul old
        ntargetEdges = S.filter (\(x, y) -> x /= old && y /= old) allEdges
        -- setul de noduri ce trebuie introduse
        setNewNodes = (S.fromList news)

        -- Pentru fiecare nou nod, iterez in setul de muchii ce contin nodul old
        -- Folosind createEdge, generez noile perechi de muchii
        -- Liste aferente fiecare nou nod vor si impreunate in fold
        newEdges = S.foldl (\acc new -> S.union (S.map (\edge -> (createEdge edge old new)) targetEdges) acc)
                            S.empty
                            setNewNodes
                              
        finalEdges = S.union newEdges ntargetEdges
        finalNodes2 = S.union setNewNodes (S.delete old allNodes)
        in (finalNodes2, finalEdges)
{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}


checkTargets edge targetNodes newValue =
    let src = fst edge
        dst = snd edge
        check_fst = elem src targetNodes
        check_snd = elem dst targetNodes
    in  if (check_fst == True) then
            if (check_snd == True) then
                (newValue, newValue)
            else
                (newValue, dst)
        else if (check_snd == True) then
            (src, newValue)
        else
            (src, dst)

mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph =
    let allNodes = nodes graph
        allEdges = edges graph
        -- nodurile care respecta proprietatea
        targetNodes = S.filter (\nod -> (prop nod) == True) allNodes 
        -- nodurile care nu respecta proprietatea
        ntargetNodes = S.filter (\nod -> (prop nod) == False) allNodes
        -- elimin din graf nodurile care respecta proprietete si adaug nodul nod
        finalNodes = S.insert node ntargetNodes

        -- Pentru fiecare nod target => iterare cu foldl
        -- Iterez in lista de muchii => si elimin muchiile care contin targetNodes

        -- Pentru fiecare edge:  contine target => modific perechea 
        --                       nu contine target => nu modific perchea
        finalEdges = S.foldl (\acc edge-> S.insert (checkTargets edge targetNodes node) acc)
                            S.empty allEdges
     
        -- Corner case: daca nu exista niciun target
        in case (S.null targetNodes) of
            True -> (allNodes, allEdges)
            otherwise -> (finalNodes, finalEdges)
