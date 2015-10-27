{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RecordWildCards #-}
module Util.CHAPFile 
    ( CHAPIdent (..)
    , getChapSecrets
    , dropComments
    , saveChapSecrets
    ) where
        
import Data.Text as T
import Data.Text.Encoding as TE
import Data.Maybe
import Data.Conduit as C
import Data.Conduit.List as CL
import Data.Conduit.Binary as CB
import Data.ByteString.Char8 as BC8
import Data.Char
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.List as L
import Prelude 


data CHAPIdent = CHAPIdent
    { ciClient:: Text
    , ciServer:: Text
    , ciSecret:: Text
    -- maybe *
    , ciIP:: Maybe Text
    }
    deriving Show

getChapSecrets:: String-> IO [CHAPIdent]
getChapSecrets filename = do
    runResourceT $ CB.sourceFile filename $$ CB.lines =$= dropComments =$= getChapIdent =$ consume
    
dropComments = awaitForever $ \line -> do
    let dropped = BC8.dropWhile isSpace line
    if BC8.length dropped < 1 
        then return ()
        else do
            if BC8.head dropped == '#'
                then return ()
                else do
                    yield line
                    return ()

getChapIdent:: (Monad m, MonadIO m) => Conduit ByteString m CHAPIdent
getChapIdent = awaitForever $ \l-> do 
    let line = TE.decodeUtf8 l 
        filterC:: Monad m => m [Text]
        filterC = yield line $$ lineToColumnsC =$ consume
    filtered <- filterC
    case L.length filtered of
        some | some < 3 -> return ()
        3 -> do
            yield $! CHAPIdent (filtered !! 0) (filtered !! 1) (filtered !! 2) Nothing
        some | some > 2 -> do
            yield $! CHAPIdent (filtered !! 0) (filtered !! 1) (filtered !! 2) (Just (filtered !! 3))
        _ -> return ()


lineToColumnsC:: Monad m => Conduit Text m Text
lineToColumnsC = awaitForever $ \l-> do
    let line = T.dropWhile isSpace l
    if T.length line < 1 
        then return ()
        else do 
            case T.head line of
                '\"' -> do
                    let ret = T.takeWhile (/='\"') $! T.drop 1 $! line 
                        tailed = T.drop 1 $! T.dropWhile (/='\"') $! T.drop 1 $! line
                    yield $! ret
                    leftover tailed
                '\'' -> do
                    let ret = T.takeWhile (/='\'') $! T.drop 1 $! line
                        tailed = T.drop 1 $! T.dropWhile (/='\'') $! T.drop 1 $! line
                    yield $! ret
                    leftover $! tailed
                _ -> do
                    let ret = T.takeWhile ( not . isSpace ) $! T.dropWhile (isSpace) $! T.replace (T.singleton '\t') " " line
                        tailed =  T.drop (T.length ret) $! T.dropWhile (isSpace) $! T.replace (T.singleton '\t') " " line
                    if T.length ret < 1 
                       then return ()
                       else do
                           yield ret
                           leftover tailed

saveChapSecrets:: String-> [CHAPIdent] -> IO ()
saveChapSecrets filename  ident = do
    runResourceT $ sourceList ident  $$ identToBSC =$ sinkFile filename

identToBSC = awaitForever $ \CHAPIdent {..} -> do
    let ret = TE.encodeUtf8 $! line `T.append` "\n"
        ip = case ciIP of
                  Nothing-> []
                  Just some -> [ quoteOnSpace some]
        client = quoteOnSpace ciClient
        server = quoteOnSpace ciServer
        secret = quoteOnSpace ciSecret
        line = T.concat $! L.intersperse " " $! [ client, server, secret] ++ ip
    yield ret

    
quoteOnSpace:: Text-> Text
quoteOnSpace value | T.any isSpace value = "\"" `T.append` value `T.append` "\""
quoteOnSpace value | T.any (not . isPrint) value = "\"" `T.append` value `T.append` "\""
quoteOnSpace value = value
