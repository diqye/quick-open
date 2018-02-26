module Main where
import Control.Monad(forM)
import System.FilePath.Posix((</>))
import qualified System.Directory as Dir
import System.Environment(getArgs)
import qualified System.Process as P
import qualified Data.List as L
import qualified Data.Char as C

-- 通过命令行 快速打开/切换Mac上的应用程序

applicationPaths = [ "/Applications" ]

configPaths :: IO [FilePath]
configPaths = do home <- Dir.getHomeDirectory
                 let configFile = home </> ".tconfig"
                 r <- Dir.doesFileExist configFile
                 if r then parseVal configFile else return applicationPaths
  where parseVal configFile = do content <- readFile configFile
                                 return $ (read content :: [FilePath])

type Name = String

getAppPaths :: [String] -> IO [(Name,FilePath)]
getAppPaths apaths = do paths <- forM apaths transPaths
                        return . concat $ paths
  where transPaths :: FilePath -> IO [(Name,FilePath)]
        transPaths path = do paths <- Dir.listDirectory path
                             return . map (toTuple path) $ paths
        toTuple path appName = (appName,path </> appName)

validatePaths :: [String] -> IO Bool
validatePaths paths = do res <- forM paths validate
                         return . all id $ res
  where validate :: String -> IO Bool
        validate path = do r <- Dir.doesDirectoryExist path
                           if r then return () else someMessage path
                           return r

someMessage :: String -> IO ()
someMessage path = do putStrLn "-----------------"
                      putStrLn $ "目录(" ++ path ++ ")不存在"
                      putStrLn "-----------------"

main :: IO ()
main = do applicationPaths <- configPaths
          r <- validatePaths applicationPaths
          if r then mainLogic applicationPaths else return ()    

mainLogic :: [FilePath] -> IO ()
mainLogic applicationPaths = do args <- getArgs
                                case args of [] -> openApp "" applicationPaths
                                             [(':':rest)] -> P.callCommand $ "open http://" ++ rest
                                             ["baidu"] -> P.callCommand "open https://www.baidu.com"
                                             ["baidu",word] -> P.callCommand $ "open https://www.baidu.com/s?wd=" ++ word
                                             [name] -> openApp name applicationPaths
                                             _ -> putStrLn "参数个数不对"
openApp :: String -> [FilePath] -> IO ()
openApp name applicationPaths = do allapp <- getAppPaths applicationPaths
                                   let papp = if name == "" then allapp else search' name allapp
                                   case papp of [] -> putStrLn  $ "找不到程序 " ++ name
                                                [(name,abspath)] -> P.callCommand $ "open \"" ++ abspath ++ "\""
                                                xs -> putStrLn $ "多个程序 " ++ show (map fst xs)
               
search' :: String -> [(Name,FilePath)] -> [(Name,FilePath)]
search' name  = filter (L.isInfixOf (toLowercase name) .toLowercase  . fst)
  where toLowercase = map C.toLower
