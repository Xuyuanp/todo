module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Data.Time
import           System.Directory
import           System.Environment
import           System.IO.Error
import qualified System.IO.Strict     as SIO
import           Text.Printf          (printf)

main :: IO ()
main = getArgs >>= parseArgs >>= runTodo doIt

parseArgs :: [String] -> IO String
parseArgs [] = do
    t <- getCurrentTime
    parseArgs [formatTime defaultTimeLocale "%Y-%m-%d" t]
parseArgs (file:_) = do
    let fileName = "./.lists/" `mappend` file `mappend` ".txt"
    fileExist <- doesFileExist fileName
    unless fileExist $ appendFile fileName ""
    return fileName

type Todo = ReaderT FilePath IO

runTodo :: Todo a -> FilePath -> IO a
runTodo = runReaderT

data Item = Item { itemDone :: Bool, itemDue :: UTCTime, itemContent :: String } deriving (Show, Read, Eq)

showItem :: Int -> Item -> String
showItem index it = printf " %-5d | %-4s | %-12s | %s"
    index
    (if itemDone it then "*" else " ")
    (formatTime defaultTimeLocale "%Y-%m-%d" $ itemDue it)
    (itemContent it)

doIt :: Todo ()
doIt = do
    displayMenu
    exitFlag <- liftIO getLine >>= judgeOperation
    when (exitFlag /= Just "Exit") doIt

judgeOperation :: String -> Todo (Maybe String)
judgeOperation input = do
    fileName <- ask
    case input of
        "1" -> do
            addTodoFile
            return $ Just "Input."
        "2" -> do
            liftIO $ catch (runReaderT updateTodoFile fileName) handler
            return $ Just "Update."
        "3" -> do
            liftIO $ catch (runReaderT deleteTodoList fileName) handler
            return $ Just "Delete."
        "4" -> do
            liftIO $ catch (runReaderT completedStamp fileName) handler
            return $ Just "CompletedStamp"
        "0" -> return $ Just "Exit"
        _ -> do
            liftIO . putStrLn $ "Unknown command"
            return Nothing

borderLine :: String
borderLine = concat $ replicate 88 "-"

displayMenu :: Todo ()
displayMenu = do
    liftIO $ do
        putStrLn $ "\n" `mappend` printf " %-5s | %-4s | %-12s | %s" "Index" "Done" "Datetime" "Content"
        putStrLn borderLine
    list <- readTodoFile
    liftIO $ do
        printList $ zip [1..] list
        putStrLn borderLine
        putStrLn "1)Add 2)Update 3)Delete 4)Done 0)Exit\n\nPlease input operation:"
    where
        printList :: [(Int, Item)] -> IO ()
        printList [] = return ()
        printList ((index, item):ls) = do
            putStrLn $ showItem index item
            printList ls

readTodoFile :: Todo [Item]
readTodoFile = do
    fileName <- ask
    contents <- liftIO . SIO.readFile $ fileName
    return $ read <$> lines contents

addTodoFile :: Todo ()
addTodoFile = do
    fileName <- ask
    liftIO $ do
        t <- getCurrentTime
        putStrLn "Please input todo things:"
        todoItem <- getLine
        appendFile fileName $ show Item { itemDone = False
                                        , itemDue = t
                                        , itemContent = todoItem
                                        } `mappend` "\n"

withIndex :: (Int -> Item -> Todo ()) -> Todo ()
withIndex f = do
    input <- liftIO getLine
    when (trim input /= "") $ do
        list <- readTodoFile
        let index = (read input :: Int) - 1
        if index < length list then do
            let item = list !! index
            f index item
        else
            liftIO . putStrLn $ "Invalid index number."

trim :: String -> String
trim "" = ""
trim s = let lstrim = dropWhile (`elem` " \t") in reverse . lstrim . reverse . lstrim $ s

updateTodoFile :: Todo ()
updateTodoFile = do
    liftIO . putStrLn $ "Please input index of item to be edited:"
    withIndex $ \index item -> do
        list <- readTodoFile
        newList <- liftIO $ do
            putStrLn $ concat ["Item to be edited is: ", itemContent item, ". Please input new item:"]
            newCntnt <- getLine
            if newCntnt /= "" then do
                t <- getCurrentTime
                return $ concat [ take index list
                                , [ item { itemContent = newCntnt, itemDue = t } ]
                                , drop (index + 1) list
                                ]
            else
                return []
        saveList newList

deleteTodoList :: Todo ()
deleteTodoList = do
    liftIO . putStrLn $ "Please input the index of item to be deleted:"
    withIndex $ \index _ -> do
        list <- readTodoFile
        let newList = take index list `mappend` drop (index + 1) list
        saveList newList

saveList :: [Item] -> Todo ()
saveList [] = return ()
saveList items = do
    fileName <- ask
    liftIO . writeFile fileName . unlines $ show <$> items

completedStamp :: Todo ()
completedStamp = do
    liftIO . putStrLn $ "Please input the index of item has been done:"
    withIndex $ \index item -> do
        list <- readTodoFile
        t <- liftIO getCurrentTime
        let newList = concat [ take index list
                             , [ item { itemDone = True, itemDue = t } ]
                             , drop (index + 1) list
                             ]
        saveList newList

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "File not exist"
    | otherwise = ioError e
