{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Console.CmdArgs as C
import Language.Haskell.Exts as E (parseFile
                             , Module(..)
                             , ModuleName(..)
                             , SrcSpanInfo(..)
                             , fromSrcInfo
                             , fromParseResult
                             , importModule)

import qualified Data.Text as T (pack, Text(..), unpack, intercalate, unlines)

import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HM

import qualified Data.List as List
import qualified Data.List.Unique as LU

import qualified Distribution.Package as P (unPackageName)
import qualified Distribution.ModuleName as M (fromString, components, ModuleName(..))

import Distribution.PackageDescription

import Distribution.Hackage.DB.Parsed (readTarball, parseVersionData, VersionData(..))
import Distribution.Hackage.DB (hackageTarball)


data Args = Args {
  input :: String
  } deriving (Show, Data, Typeable)


mainArgs = Args {
    input = def &= C.name "input" &= help "input file" &= typ "FILE"
    } &= help "find dependencies from source file"

getFileMods :: FilePath -> IO [M.ModuleName]
getFileMods input = do
  parse <- parseFile input
  case (fromParseResult parse) of
    E.Module _ _ _ impDecl _ -> do
      return $ LU.sortUniq $ [ M.fromString imp | y <- impDecl,
                               let E.ModuleName _ imp = importModule y ]

getHackageMods = do
    db <- hackageTarball >>= readTarball Nothing

    let new = HM.empty :: HM.HashMap T.Text [T.Text]
    let res = Map.foldlWithKey' (\new pkgName pkgVersionData
                                  ->
                                   (if pkgName /= "rerebase" &&
                                       pkgName /= "haskell2010" &&
                                       pkgName /= "fay-base"
                                    then
                                        buildRevDict
                                        (T.pack $ P.unPackageName pkgName)
                                        pkgVersionData
                                        new
                                    else
                                      new
                                   )
                               ) new db
    return res

buildRevDict :: T.Text
             -> Map.Map a VersionData
             -> HM.HashMap T.Text [T.Text]
             -> HM.HashMap T.Text [T.Text]
buildRevDict pkgName pkgVersions revDict = do
  case Map.null pkgVersions of
    True -> revDict
    False -> do
      let (_, lastPkgVersion) = Map.findMax $ pkgVersions
      case condLibrary $ (cabalFile lastPkgVersion) of
            Just lib -> (List.foldl' (\revDict mod ->
                                        HM.insertWith
                                       (++)
                                       (T.pack $ List.intercalate "." $ M.components mod)
                                       ([pkgName])
                                       revDict
                                     ) revDict (exposedModules $ condTreeData $ lib) )
            _ -> revDict


getMods :: Maybe (Map.Map a VersionData) -> [T.Text]
getMods pkgVersions =
  case pkgVersions of
    Just versions ->
      case Map.null versions of
        True -> []
        False -> do
          let (_, lastPkgVersion) = Map.findMax $ versions
          case condLibrary $ (cabalFile lastPkgVersion) of
            Just lib -> [ T.pack $ List.intercalate "." (M.components mod)
                        | mod <- exposedModules $ condTreeData $ lib ]
            _ -> []
    _ -> []


showPkg :: ([T.Text], T.Text) -> [Char]
showPkg ([], imp) = "-- not pkg found for import " ++  (T.unpack imp)
showPkg ([p], imp) = T.unpack p
showPkg ((p:ps), imp) = (T.unpack p) ++ " -- with " ++ show(List.length ps) ++ " more candidate(s) " ++ show ps

main :: IO ()
main = do
  cmds <- cmdArgs $ mainArgs
  fileMods <- getFileMods $ (input cmds)
  hMods <- getHackageMods


  let res = [ (pkgs, imp) | x <- fileMods
                          , let imp = T.pack $ List.intercalate "." (M.components x)
                          , let pkgs = (case HM.lookup imp hMods of 
                                              Nothing -> []
                                              Just y -> y
                                       )]
  mapM_ putStrLn $ (LU.sortUniq $ fmap showPkg res)
