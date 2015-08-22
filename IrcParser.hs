module IrcParser( IrcMessage(tags, prefix, command, args), parseMessage ) where

import Data.List

data IrcMessage = IrcMessage {
                        tags :: String,
                        prefix :: String,
                        command :: String,
                        args :: [String],
                        toParse :: [String]
                        } deriving Show


parseMessage :: String -> IrcMessage 
parseMessage [] = error "Emptry message!"
parseMessage xs =  let initialState = IrcMessage { 
                        tags = "",
                        prefix = "",
                        command = "",
                        args = [],
                        toParse = words xs
                        } in
        getArgs . getCommand . getPrefix . getTags $ initialState

isToParsePrefix :: String -> IrcMessage -> Bool
isToParsePrefix prefix state
        | null (toParse state) = False
        | otherwise = isPrefixOf prefix (head (toParse state))

getTags :: IrcMessage -> IrcMessage
getTags state
        | isToParsePrefix "@" state = state {
                                       tags = tail (head (toParse state)),
                                       toParse = tail (toParse state) }
        | otherwise = state

getPrefix :: IrcMessage -> IrcMessage 
getPrefix state
        | isToParsePrefix ":" state = state {
                                        prefix = tail (head (toParse state)),
                                        toParse = tail (toParse state) }
        | otherwise = state

getCommand :: IrcMessage -> IrcMessage
getCommand state
        | null (toParse state) = state
        | otherwise = state {
                        command = head (toParse state),
                        toParse = tail (toParse state) }

parseArgs :: [String] -> [String]
parseArgs [] = []
parseArgs xs = (takeWhile (\x -> not (isPrefixOf ":" x)) xs) ++ [(unwords (dropWhile (\x -> not (isPrefixOf ":" x)) xs))]

getArgs :: IrcMessage -> IrcMessage
getArgs state = state { args = parseArgs (toParse state),
                        toParse = [] }
