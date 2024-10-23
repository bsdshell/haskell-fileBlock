-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-} 
-- import Turtle
-- echo "turtle"

-- import Data.Set   -- collide with Data.List 
import Control.Monad
import Data.Char
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Process
import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Data.IORef 
import Control.Monad (unless, when)
import Control.Concurrent 
import Text.RawString.QQ

import qualified Text.Regex.TDFA as TD
import AronModule 
import AronAlias

--import Data.Array

-- import Graphics.Rendering.OpenGL as GL 
-- import Graphics.Rendering.OpenGL.GLU.Matrix as GM  
-- import qualified Graphics.UI.GLFW as G
-- import Data.Set(Set) 
-- import qualified Data.Set as S 

--if (length argList) == 2 
--then case head argList of 
--    "svg" -> run cmd >> run "ls" >>= \x -> pp x 
--            where 
--                cmd = "pwd" 
--    "png" ->run cmd >> run ("ls " ++ fn) >>= \x -> pp x  
--            where 
--                cmd = "pwd" 
--    _     -> print "more arg" 
--else print "Need more arguments" 

--    takeFileName gives "file.ext"
--    takeDirectory gives "/directory"
--    takeExtension gives ".ext"
--    dropExtension gives "/directory/file"
--    takeBaseName gives "file"
--    "/directory" </> "file.ext".
--    "/directory/file" <.> "ext".
--    "/directory/file.txt" -<.> "ext".
-- |  end_fold ,}}}


p1 = "/Users/cat/myfile/bitbucket/testfile/test.tex"

-- zo - open
-- za - close

-- fileBlock "/x.x" "===" -drophead
-- fileBlock "/x.x" "===" -droplast
-- fileBlock "/x.x" "===" -append  "str"
-- fileBlock "/x.x" "===" -prepend "str"
-- fileBlock "/x.x" "===" -size         => 3

{-|
    === trim list of String
    @
    ["", "", "a", " ", " "] => ["a"]
    @
-}
--trimList::[String] -> [String]
--trimList lss = let f x =  dropWhile null $ map trim x 
--                   rev = reverse
--               in  (rev . f . rev . f) lss 

{-|
main = do
        let ls = ["", "", "", " ", " "]
        let lt = trimList ls 
        pre lt
-}
  
{--
data FileBlock =  DropHead
             | DropLast
             | DropIndex Int
             | PrintIndex Int
             | PrintHead
             | PrintLast
             | PrintAll
             | Append String
             | AppendList [String]
             | Prepend String
             | NumberOfBlock deriving (Show, Eq)

fileBlock :: FilePath -> String -> FileBlock -> IO()
fileBlock path de opt = do
  fs <- fileExist path >>= \x -> not x ? (createFile path >> rfl path) $ rfl path
  let fs' = rev $ dropWhile (\x -> (len . trim) x == 0) $ rev fs
  let separator = concat $ take 10 $ repeat de
  let delimiter = len fs' > 0 ? last fs' $ separator
  let ls = filter (\x -> len x > 0 ) $ splitBlock fs' separator
  -- IF the last block:[Str] of ls:[[Str]] is empty THEN remove it ELSE do nothing
  -- [[]]    => []
  -- [["a"]] => [["a"]]
  let ls' =  len ls == 0 ? ls $ let s = concat $ map trim $ last ls in (len s == 0 ? init ls $ ls) 

  case opt of
    DropHead -> do
      let ss = map (\x -> trimList x ++ [delimiter] ) ls'
      let s = len ss > 0 ? tail (map (trim . unlines) ss) $ [] 
      wfl path s
      pp "-drophead"
    DropLast -> do
      let ss = map (\x -> trimList x ++ [delimiter] ) ls'
      let s = init $ map (trim . unlines) ss 
      wfl path s
      pp "-droplast"
    DropIndex inx -> do
      let ss = map (\x -> trimList x ++ [delimiter] ) ls'
      let st = removeIndex inx $ map (trim . unlines) ss
      wfl path st
      pp "-dropindex n"
    PrintIndex inx -> do
      let ss = map trimList ls'
      let st = 0 <= inx && inx < len ss ? ss !! inx $ error ("ERROR: len =" ++ (sw . len) ss ++ " index=" ++ sw inx)
      putStr $ unlines st
      pp "-printindex n"
    PrintHead -> do
      let ss = map trimList ls'
      let st = len ss > 0 ? head ss $ error ("ERROR: len =" ++ (sw . len) ss)
      putStr $ unlines st
      pp "-printhead"
    PrintLast -> do
      let ss = map trimList ls'
      let st = len ss > 0 ? last ss $ error ("ERROR: len =" ++ (sw . len) ss)
      putStr $ unlines st
      pp "-printlast"
    PrintAll -> do
      let ss = map trimList ls'
      mapM_ (putStrLn . unlines) ss
      pp "-printall"

    Append str -> do
      -- Remove the first and last empty lines str contains them 
      -- ss = [String]
      let ss = let s = map trimList $ [lines str] in if len s == 0 then [] else head s
      -- Remove the str first if there is str in the list
      let lu = removeFirstList ss $ filter (not . null) $ map trimList ls' 
      -- let la = unique $ filter (not . null) $ map trimList $ ls' ++ [lines str] 
      let la = lu ++ [ss]
      let tt = map (trim . unlines) $ map (++[delimiter]) la 
      wfl path tt 
      pp "-append"

    AppendList ls -> do
      let la = unique $ map trimList $ ls' ++ [ls]
      let tt = map (trim . unlines) $ map (++[delimiter]) la 
      wfl path tt 
      pp "-appendlist"

    Prepend str -> do
      let ss = let s = map trimList $ [lines str] in if len s == 0 then [] else head s
      -- Remove the str first if there is str in the list
      let lu = removeFirstList ss $ filter (not . null) $ map trimList ls' 
      -- let la = unique $ filter (not . null) $ map trimList $ ls' ++ [lines str] 
      let la = [ss] ++ lu
      let tt = map (trim . unlines) $ map (++[delimiter]) la 
      wfl path tt 
      pp "-prepend"
    NumberOfBlock -> do
      putStr $ (sw . len) ls'
--}
  
main = do 
        argList <- getArgs
        let lena = len argList
        if lena == 3 || lena == 4 then do
            let path = head argList        -- "/tmp/x.x"
            let delimiter = concat $ take 10 $ repeat $ (head . tail) argList  -- "="
            let opt = trim $ argList !! 2 
            case opt of
              v | v == "-drophead" && lena == 3 -> do
                              fileBlock path delimiter DropHead
                              {--
                              let ss = map (\x -> trimList x ++ [delimiter] ) ls'
                              let s = len ss > 0 ? tail (map (trim . unlines) ss) $ [] 
                              wfl path s
                              --}

              v | v == "-droplast" && lena == 3 -> do
                              fileBlock path delimiter DropLast
                              {--                    
                              let ss = map (\x -> trimList x ++ [delimiter] ) ls'
                              let s = init $ map (trim . unlines) ss 
                              wfl path s
                              --}

              v | v == "-dropindex" && lena == 4 -> do
                              let inx = strToInt $ last argList
                              fileBlock path delimiter $ DropIndex inx
                              {--                    
                              let ss = map (\x -> trimList x ++ [delimiter] ) ls'
                              let inx = strToInt $ last argList
                              let st = removeIndex inx $ map (trim . unlines) ss
                              wfl path st
                              --}

              v | v == "-printindex" && lena == 4 -> do
                              let inx = strToInt $ last argList
                              fileBlock path delimiter $ PrintIndex inx
                              {--                    
                              let ss = map (\x -> trimList x) ls'
                              let st = 0 <= inx && inx < len ss ? ss !! inx $ error ("ERROR: len =" ++ (show . len) ss ++ " index=" ++ show inx)
                              putStr $ unlines st
                              --}


              v | v == "-printhead" && lena == 3 -> do
                              let inx = strToInt $ last argList
                              fileBlock path delimiter $ PrintHead
                              {--                    
                              let ss = map (\x -> trimList x) ls'
                              let inx = strToInt $ last argList
                              let st = len ss > 0 ? head ss $ error ("ERROR: len =" ++ (show . len) ss ++ " index=" ++ show inx)
                              putStr $ unlines st
                              --}

              v | v == "-printlast" && lena == 3 -> do
                              fileBlock path delimiter PrintLast
                              {--                    
                              let ss = map (\x -> trimList x) ls'
                              let inx = strToInt $ last argList
                              let st = len ss > 0 ? last ss $ error ("ERROR: len =" ++ (show . len) ss ++ " index=" ++ show inx)
                              putStr $ unlines st
                              --}

              v | v == "-printall" && lena == 3 -> do
                              fileBlock path delimiter PrintAll
                              {--                    
                              let ss = map (\x -> trimList x) ls'
                              mapM_ (putStrLn . unlines) ss
                              --}

              v | v == "-append" && lena == 4 -> do
                              let str = last argList 
                              fileBlock path delimiter $ Append str
                              {--                    
                              let str = last argList 
                              -- Remove the first and last empty lines str contains them 
                              -- ss = [String]
                              let ss = let s = map trimList $ [lines str] in if len s == 0 then [] else head s
                              -- Remove the str first if there is str in the list
                              let lu = removeFirstList ss $ filter (not . null) $ map trimList ls' 
                              -- let la = unique $ filter (not . null) $ map trimList $ ls' ++ [lines str] 
                              let la = lu ++ [ss]
                              let tt = map (trim . unlines) $ map (\x -> x ++ [delimiter] ) la 
                              wfl path tt
                              --}

              v | v == "-appendlist" && lena == 4 -> do
                              let str = last argList 
                              let hls = read str :: [String] 
                              fileBlock path delimiter $ AppendList hls
                              -- Fri 24 Feb 14:32:19 2023 
                              -- TODO: fixed the order of items after apply -apppendlist
                              -- SEE: -append and -prepend
                              {--                    
                              let la = unique $ map trimList $ ls' ++ [hls] 
                              let tt = map (trim . unlines) $ map (\x -> x ++ [delimiter] ) la 
                              wfl path tt
                              --}

              v | v == "-prepend" && lena == 4 -> do
                              let str = last argList
                              fileBlock path delimiter $ Prepend str
                              {--                    
                              let ss = let s = map trimList $ [lines str] in if len s == 0 then [] else head s
                              -- Remove the str first if there is str in the list
                              let lu = removeFirstList ss $ filter (not . null) $ map trimList ls' 
                              -- let la = unique $ filter (not . null) $ map trimList $ ls' ++ [lines str] 
                              let la = [ss] ++ lu
                              let tt = map (trim . unlines) $ map (\x -> x ++ [delimiter] ) la 
                              wfl path tt
                              --}

              v | v == "-size" && lena == 3 -> do
                              fileBlock path delimiter NumberOfBlock
                              -- putStr $ (show . len) ls'

              _           -> do 
                              pp "Invalid option"
        else do
            putStrLn $ [r| 
                          fileBlock "/tmp/x.x" "==" -printall                    => Print all block from file 
                          fileBlock "/tmp/x.x" "==" -printindex 1                => Print index 1 from file 
                          fileBlock "/tmp/x.x" "==" -printhead                   => Print all block from file 
                          fileBlock "/tmp/x.x" "==" -printlast                   => Print all block from file 
                          fileBlock "/tmp/x.x" "==" -drophead                    => Drop the first block from file 
                          fileBlock "/tmp/x.x" "==" -droplast                    => Drop the last block from file 
                          fileBlock "/tmp/x.x" "==" -dropindex                   => Delete index block from file 
                          fileBlock "/tmp/x.x" "==" -append  "str"               => Append str to file 
                          fileBlock "/tmp/x.x" "==" -prepend "str"               => Prepend str to file 
                          fileBlock "/tmp/x.x" "==" -appendlist "[\"a\", \"b\"]" => Append Haskell list to file 
                          fileBlock "/tmp/x.x" "==" -size                        => Print the numbers of blocks in file 

                       |] 
















