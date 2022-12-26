module Application.Shared.ApplicationError (ApplicationError(..)) where
import PiggyBalance.ValueObjects.PiggyBalanceError (PiggyBalanceError)

newtype ApplicationError 
  = PiggyBalanceError PiggyBalanceError
  deriving (Eq, Show)
