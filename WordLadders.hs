-- Based on go-doublet by Nathan Smith
-- https://github.com/neocortical/go-doublet

-- Ported as my first Haskell program

import System.IO						-- For file loading
import System.CPUTime					-- So we can know how long things take
import Data.List						-- We want foldl'
import Data.Char						-- To get toLower and isSpace
import Data.Maybe						-- To make Maybe handling easier
import qualified Data.Set as Set		-- So we can make Sets
import qualified Data.Map as Map		-- We also need to make Maps

------------------ Getting the word dictionary ------------------

type WordDictionary = Set.Set String

-- Load the dictionary file, return a list of lower case trimmed words as strings
loadDictionaryWords :: IO [String]
loadDictionaryWords = do
	fileText <- readFile "dictionary.txt"
	return $ stripStrings $ lowercaseLines fileText		-- Why can't I use . instead of $?
	where
		stripStrings = map (takeWhile isAlpha)			-- Cut off the extra '\r's on the end of the strings
		lowercaseLines = lines . map toLower			-- Convert the input to lower case and then from lines

-- Creates our WordDictionary by processing the dictionary.txt file into a Set
loadDictionary :: IO WordDictionary
loadDictionary = do
	wordList <- loadDictionaryWords
	return $ Set.fromList wordList

------------------ Generating the word graph ------------------

type WordGraph = Map.Map String [String]

-- Given a String, a character, and a position, replace letter at the given position with the new character
-- If the new character and the old character are the same, we return nothing
replaceLetter :: String -> Char -> Int -> Maybe String
replaceLetter word char pos
	| char == oldChar	= Nothing										-- If we're not changing things, return Nothing
	| otherwise			= Just $ firstPart ++ [char] ++ secondPart		-- Put the new character in the old one's place
	where
		(firstPart, oldChar:secondPart) = splitAt pos word

-- Given a String and a WordDictionary, generate all words that are one character away
neighboringWords :: String -> WordDictionary -> [String]
neighboringWords word dict = 
	filter inDict $ catMaybes possibleWords								-- Remove maybes and things not in the dictionary
	where
		inDict = flip Set.member dict
		characterPositions = [0 .. length word - 1]		-- Character positions in the string
		allLetters = ['a' .. 'z']						-- Possible alternate characters
		possibleWords =									-- The generated possible words
			[replaceLetter word l p | p <- characterPositions, l <- allLetters]
			
-- Generate the word graph from a dictionary of words
createWordGraph :: WordDictionary -> WordGraph
createWordGraph dict = foldl' insertWord Map.empty $ Set.toList dict
	where
		insertWord map word = Map.insert word (neighboringWords word dict) map
		
------------------ The A* Implementation ------------------

data Node = Node {
					word :: String,
					currentCost :: Int,
					guessedCost :: Int,
					previous :: Maybe Node
				} deriving (Show)

-- Sum up the two costs on a node so we can order things the way we want
fullCost :: Node -> Int
fullCost a = currentCost a + guessedCost a

-- Make it so we can order Nodes
instance Ord Node where
	compare a b = compare (fullCost a) (fullCost b)

-- And to do that, we must be able to check equality on Nodes
instance Eq Node where
	(==) a b = (word a) == (word b)

-- Give a basic estimate of the 'distance' between two words based on characters that differ
roughDistance :: String -> String -> Int
roughDistance [] _		= 0								-- When string one is done, we're done
roughDistance (x:xs) (y:ys)
	| x == y	= roughDistance xs ys					-- If they're the same, distance doesn't change
	| otherwise	= 1 + roughDistance xs ys

-- Given a final node, reconstruct the words in the path
reconstructPath :: Node -> [String]
reconstructPath Node {word = w, previous = Just p}	= reconstructPath p ++ [w]
reconstructPath Node {word = w}						= [w]

-- The open set is just a list of Nodes we're still considering in priority order (lowest to highest cost)
type OpenSet = [Node]

-- Closed set is the words we've already checked, this will save us a little typing
type ClosedSet = Set.Set String

-- A simple bootstrap function for the A* implementation
-- It sets up the initial node for the search
findPath :: String -> String -> WordDictionary -> WordGraph -> Maybe [String]
findPath startWord endWord dict graph = aStar endWord openSet closedSet dict graph
	where
		ourNode = Node startWord 0 (roughDistance startWord endWord) Nothing
		openSet = [ourNode]
		closedSet = Set.empty

-- Update the open set with the given node, useful to let us fold new words in
-- If it's new, insert it. If it's a duplicate, replace the old node if the cost is lower
updateOpenSet :: String -> Int -> OpenSet -> Node -> OpenSet
updateOpenSet endWord curretnCost open node 
	| existingNode == Nothing								-- Never seen it, add it to the queue in the right place
			= insert node open
	| tentativeCost < currentCost (fromJust existingNode)	-- We cost less, swap us in
			= insert node withoutExisting
	| otherwise												-- We cost more, don't modify the open set
			= open
	where
		existingNode = find (node ==) open					-- Lowest scoring version of our word
		withoutExisting = filter (node /=) open				-- Queue without the word in question
		tentativeCost = currentCost node + 1				-- Since we move by one letter, cost just increments

-- Convert a list of words into a list of Nodes given the endWord, neighboring words, and the current node
neighboringNodes :: String -> [String] -> Node -> [Node]
neighboringNodes _ [] _ = []
neighboringNodes endWord (w:ws) node = newNode : neighboringNodes endWord ws node
	where
		newNode = Node w (currentCost node + 1) (roughDistance w $word node) $ Just node

-- The actual A* implementation we use, (naively) converted to be functional by me
-- q is a list of nodes in priority order to check, closed is a set of words we've already been through 
aStar :: String -> OpenSet -> ClosedSet -> WordDictionary -> WordGraph -> Maybe [String]
aStar endWord [] _ _ _	= Nothing														-- Couldn't find it, so nothing
aStar endWord q@(n @ Node {word = w, currentCost = c}:ns) closed dict graph				-- Our queue still has stuff in it
	| w == endWord			= Just $ reconstructPath n									-- Found the word, return the path
	| neighborWords == []	= aStar endWord ns updatedClosed dict graph					-- Recurse knowing this word was a dead end
	| otherwise				= aStar endWord withNeighbors updatedClosed dict graph		-- Add the neighbors to the priority queue
	where 
		neighborWords = filter (flip Set.notMember closed) $ neighboringWords w dict	-- Unchecked neighboring words
		neighborNodes = neighboringNodes endWord neighborWords n						-- Those words as nodes
		withNeighbors = foldl' (updateOpenSet endWord c) ns neighborNodes				-- Updated open set with new neighbors
		updatedClosed = Set.insert w closed												-- Closed set with the word we just checked

------------------ Functions dealing with user IO ------------------

-- Get two words, run with them if we get them
getTwoWords :: WordDictionary -> WordGraph -> IO ()
getTwoWords dict graph = do
	putStrLn "Enter two words: "												-- I can't get this to flush without putStrLn
	typedLine <- getLine														-- Read the input and separate it into words
	let ourWords = words typedLine
	case ourWords of
		[]			-> return ()												-- Program is done when they don't enter words
		(x:y:z)		-> runWithTwoWords dict graph x y							-- Try the words they gave us (ignore extra)
						>> getTwoWords dict graph
		otherwise	-> putStrLn "Please enter two words."						-- Make them try again
						>> getTwoWords dict graph
										 
-- Check that the two words are the same length and valid
runWithTwoWords :: WordDictionary -> WordGraph -> String -> String -> IO ()
runWithTwoWords dict graph one two | lenOne /= lenTwo	= putStrLn "Words must be the same length"
	where
		lenOne = length one
		lenTwo = length two
runWithTwoWords dict graph one two | wordOneBad			= putStrLn $ "Word '" ++ one ++ "' is not in the dictionary"											
	where
		wordOneBad = Set.notMember one dict
runWithTwoWords dict graph one two | wordTwoBad			= putStrLn $ "Word '" ++ two ++ "' is not in the dictionary"
	where
		wordTwoBad = Set.notMember two dict
runWithTwoWords dict graph one two | otherwise			= prettyPrintAnswer one two $ findPath one two dict graph

-- Show the results we found
prettyPrintAnswer :: String -> String -> Maybe [String] -> IO ()
prettyPrintAnswer one two Nothing		= putStrLn $ "Couldn't find a path between '" ++ one ++ "' and '" ++ two ++ "'."
prettyPrintAnswer one two (Just words)	= putStrLn $ intercalate " >> " words

main = do
	-- Load in the dictionary
	
	beforeDict <- getCPUTime
	dict <- loadDictionary
	let dictEntries = Set.size dict
	putStrLn $ "Loaded the dictionary of " ++ show dictEntries ++ " words"
	afterDict <- getCPUTime
	let dictDiff = (fromIntegral (afterDict - beforeDict)) / (10^9)
	putStrLn $ "Loaded the in " ++ show dictDiff ++ "ms"
	
	-- Now we'll setup the graph
	
	beforeGraph <- getCPUTime
	let graph = createWordGraph dict
	let graphEntries = Map.size graph
	putStrLn $ "Generated the graph for " ++ show graphEntries ++ " words"
	afterGraph <- getCPUTime
	let graphDiff = (fromIntegral (afterGraph - beforeGraph)) / (10^9)
	putStrLn $ "Generated in " ++ show graphDiff ++ "ms"
	
	-- Now we can do the main loop
	
	getTwoWords dict graph

