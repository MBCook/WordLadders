import System.IO						-- For file loading
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