module IrcParser( IrcMessage(extPrefix, prefix, command, args), parseMessage ) where

import Data.List

data IrcMessage = IrcMessage {
                        extPrefix :: String,
                        prefix :: String,
                        command :: String,
                        args :: [String],
                        toParse :: [String]
                        } deriving Show


parseMessage :: String -> IrcMessage 
parseMessage [] = error "Emptry message!"
parseMessage xs =  let initialState = IrcMessage { 
                        extPrefix = "",
                        prefix = "",
                        command = "",
                        args = [],
                        toParse = words xs
                        } in
        getArgs . getCommand . getPrefix . getExtPrefix $ initialState

isToParsePrefix :: String -> IrcMessage -> Bool
isToParsePrefix prefix state
        | null (toParse state) = False
        | otherwise = isPrefixOf prefix (head (toParse state))

getExtPrefix :: IrcMessage -> IrcMessage
getExtPrefix state
        | isToParsePrefix "@" state = state {
                                       extPrefix = tail (head (toParse state)),
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
