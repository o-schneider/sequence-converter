module Sequence
  ( Sequence
  , renameSequence
  , contentsToSequences
  , sequenceToContent
  ) where

type Sequence = (String, [String])

contentsToSequences :: String -> [Sequence]
contentsToSequences contents =
  let (last, all) = foldl toSequence (("", []), []) (lines contents)
  in all ++ [last]

toSequence :: (Sequence, [Sequence]) -> String -> (Sequence, [Sequence])
toSequence (current, all) line =
  case line of
    (x:xs) ->
      if x == '>'
        then ((x : xs, []), all)
        else let (id, adn) = current
             in ((id, adn ++ [line]), all)
    _ -> (current, all ++ [current])

sequenceToContent :: Sequence -> String
sequenceToContent (id, adn) = id ++ "\n" ++ unlines adn ++ "\n"

renameSequence :: Sequence -> Sequence
renameSequence (identifier, adn) =
  let newId = renameLine identifier
  in (newId, adn)

renameLine :: String -> String
renameLine = fst . foldl getId ("", Yes) . head . words

getId :: (String, Continue) -> Char -> (String, Continue)
getId (id, continue) char =
  if continue == No || char `elem` ['a' .. 'z']
    then (id, No)
    else (id ++ [char], Yes)

data Continue
  = Yes
  | No
  deriving (Eq)
