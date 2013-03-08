import System.Directory
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
        act'       = act name

runPygmake :: String -> IO ()
runPygmake name = do
    sh $ "rm -f " ++ dbFile
    sh $ "rm -f " ++ compileCommandsFile
    sh $ "../../dist/build/pygmake/pygmake clean"
    sh $ "../../dist/build/pygmake/pygmake"
    sh $ "../../dist/build/pygmake/pygmake clean"
    return ()
  where sh cmd = waitForProcess =<< (runCommand $ "cd " ++ name ++ " && " ++ cmd)
