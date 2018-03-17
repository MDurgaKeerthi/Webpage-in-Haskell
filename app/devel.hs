{-# LANGUAGE PackageImports #-}
import "project1" Application (develMain)
import Prelude (IO)

main :: IO ()
main = develMain    -- handles yesod devel
