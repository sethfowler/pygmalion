import Prelude hiding (catch)
import Control.Exception
import System.Directory
import System.IO.Error hiding (catch)
import System.Process
import Test.Framework (defaultMain, testGroup)
import Test.Golden (goldenVsFile)

import Pygmalion.Core

main:: IO ()
main = setCurrentDirectory "tests" >> defaultMain tests

tests =
  [
    testGroup "Compilation Database"
    [
      goldenVsFile' "NoCommands" runPygmake,
      goldenVsFile' "Simple"     runPygmake
    ]
  ]

goldenVsFile' name act = goldenVsFile name goldenFile file act'
  where goldenFile = name ++ "/" ++ name ++ ".golden"
        file       = name ++ "/compile_commands.json"
        act'       = do setCurrentDirectory name
                        act
                        setCurrentDirectory ".."

runPygmake :: IO ()
runPygmake = do
  removeFileIfExists dbFile
  removeFileIfExists compileCommandsFile
  waitForProcess =<< runCommand "../../dist/build/pygmake/pygmake clean"
  waitForProcess =<< runCommand "../../dist/build/pygmake/pygmake"
  waitForProcess =<< runCommand "../../dist/build/pygmake/pygmake clean"
  return ()

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = removeFile file `catch` exceptDoesNotExist
  where exceptDoesNotExist e | isDoesNotExistError e = return ()
                             | otherwise             = throwIO e
