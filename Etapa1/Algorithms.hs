module Algorithms where

import qualified Data.Set as S
import Data.Maybe (fromJust)
import StandardGraph
import qualified Data.List as L

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    *** TODO ***

    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.
    
    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}

search :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> [a]                  -- oldStructure
       -> S.Set a              -- lista de noduri vizitate
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f oldStructure visitedNodes node graph = 
    let neighbors = S.toList (outNeighbors node graph)
        -- Vecinii nevizitati ai nodului curent
        nneighbors = filter (\neigh -> (S.member neigh visitedNodes == False)) neighbors

        -- imbinarea intre lista elementelor deja aflate in structura si vecinii nevizitati
        newStructure = f oldStructure nneighbors in
    
    -- Nodul curent este deja vizitat => nu il adaug in parcurgere
    if (S.member node visitedNodes == True) then

        --  Am elemente in structura cu care sa continui
        if (null newStructure /= True) then
            [] ++ (search f newStructure visitedNodes (head newStructure) graph)
        else 
            []
    -- Nodul curent nu este vizitat => il adaug in parcurgere
    else
        if (null newStructure /= True) then
           [node] ++ (search f newStructure (S.insert node visitedNodes) (head newStructure) graph)
        else 
            [node]

{-
    *** TODO ***

    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}
bindBFS :: [a]->[a]->[a]
bindBFS list1 list2
    | null list1 = list2
    | otherwise = (tail list1) ++ list2

bfs :: Ord a => a -> Graph a -> [a]
bfs = search bindBFS [] S.empty


{-
    *** TODO ***

    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}
bindDFS :: [a]->[a]->[a]
bindDFS list1 list2
    | null list1 = list2
    | otherwise = list2 ++ (tail list1)

dfs :: Ord a => a -> Graph a -> [a]
dfs = search bindDFS [] S.empty

{-
    *** TODO ***

    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
-}
countIntermediate :: Ord a
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
countIntermediate from to graph =
    let bfsTraversal = (bfs from graph)
        dfsTraversal = (dfs from graph)
        bfsIndex = L.findIndex (to ==) bfsTraversal
        dfsIndex = L.findIndex (to ==) dfsTraversal
    in case bfsIndex of 
        Nothing   -> Nothing
        otherwise -> Just ((fromJust bfsIndex) - 1, (fromJust dfsIndex) - 1)
