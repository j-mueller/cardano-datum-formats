module Cardano.Protocol.Liqwid.Loan (
    LoanDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Liqwid.Common (PubKeyHash, fromList)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data LoanDatum = LoanDatum
    { ldBorrower :: PubKeyHash
    , ldPrincipal :: Integer
    , ldAccruedInterest :: Integer
    , ldMinInterest :: Integer
    , ldInterestIndex :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary LoanDatum where
    arbitrary = LoanDatum <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance PTx.ToData LoanDatum where
    toBuiltinData LoanDatum{ldBorrower, ldPrincipal, ldAccruedInterest, ldMinInterest, ldInterestIndex} =
        PlutusTx.mkList
            [ PTx.toBuiltinData ldBorrower
            , PlutusTx.mkI ldPrincipal
            , PlutusTx.mkI ldAccruedInterest
            , PlutusTx.mkI ldMinInterest
            , PlutusTx.mkI ldInterestIndex
            ]

instance PTx.FromData LoanDatum where
    fromBuiltinData dt = do
        [borrower, D.getI -> Just ldPrincipal, D.getI -> Just ldAccruedInterest, D.getI -> Just ldMinInterest, D.getI -> Just ldInterestIndex] <- fromList dt
        ldBorrower <- PTx.fromBuiltinData borrower
        pure LoanDatum{ldBorrower, ldPrincipal, ldAccruedInterest, ldMinInterest, ldInterestIndex}
