import Test.Tasty
import Test.Tasty.Golden as Gold
import Test.Tasty.Golden.Manage as Gold (defaultMain)

main = Gold.defaultMain goldenTests

goldenTests :: TestTree
goldenTests = testGroup "Golden" [ ]
