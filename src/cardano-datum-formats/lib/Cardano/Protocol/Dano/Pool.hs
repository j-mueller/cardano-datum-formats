module Cardano.Protocol.Dano.Pool (
    TupleAsset (..), Price (..), PoolDatum (..), ProtocolConfig (..),
    activeReserves, virtualReserves, quoteExactIn,
) where

import Cardano.Asset (Asset (..))
import Cardano.Data qualified as D
import Control.Monad (guard)
import PlutusTx qualified as P
import PlutusTx.Builtins qualified as B
import Test.QuickCheck (Arbitrary (..))

-- Dano encodes asset pairs as lists, unlike constructor-encoded Asset.
newtype TupleAsset = TupleAsset { unTupleAsset :: Asset }
    deriving stock (Eq, Show)

data Price = Price { priceNumerator :: Integer, priceDenominator :: Integer }
    deriving stock (Eq, Show)

data PoolDatum = PoolDatum
    { tokenX :: TupleAsset
    , tokenY :: TupleAsset
    , lpFeeRate :: Integer
    , platformFeeX :: Integer
    , platformFeeY :: Integer
    , totalSwapFee :: Integer
    , sqrtLowerPrice :: Price
    , sqrtUpperPrice :: Price
    , minXChange :: Integer
    , minYChange :: Integer
    , circulatingLpToken :: Integer
    , lastWithdrawEpoch :: Integer
    }
    deriving stock (Eq, Show)

data ProtocolConfig = ProtocolConfig
    { platformFeeRate :: Integer
    , swapFee :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary TupleAsset where
    arbitrary = TupleAsset <$> arbitrary
instance Arbitrary Price where
    arbitrary = Price <$> arbitrary <*> arbitrary
instance Arbitrary PoolDatum where
    arbitrary = PoolDatum <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary ProtocolConfig where
    arbitrary = ProtocolConfig <$> arbitrary <*> arbitrary

instance P.ToData TupleAsset where
    toBuiltinData (TupleAsset Asset{aPolicyId, aTokenName}) = B.mkList [B.mkB aPolicyId, B.mkB aTokenName]
instance P.FromData TupleAsset where
    fromBuiltinData d = D.fromList d $ \case
        [D.getB -> Just policy, D.getB -> Just name] -> Just (TupleAsset $ Asset policy name)
        _ -> Nothing
instance P.ToData Price where
    toBuiltinData (Price n d) = B.mkConstr 0 [B.mkI n, B.mkI d]
instance P.FromData Price where
    fromBuiltinData d = D.withConstr d $ \case
        (0, [D.getI -> Just n, D.getI -> Just denominator]) -> Just (Price n denominator)
        _ -> Nothing
instance P.ToData PoolDatum where
    toBuiltinData PoolDatum{tokenX,tokenY,lpFeeRate,platformFeeX,platformFeeY,totalSwapFee,sqrtLowerPrice,sqrtUpperPrice,minXChange,minYChange,circulatingLpToken,lastWithdrawEpoch} =
        B.mkConstr 0 [P.toBuiltinData tokenX,P.toBuiltinData tokenY,B.mkI lpFeeRate,B.mkI platformFeeX,B.mkI platformFeeY,B.mkI totalSwapFee,P.toBuiltinData sqrtLowerPrice,P.toBuiltinData sqrtUpperPrice,B.mkI minXChange,B.mkI minYChange,B.mkI circulatingLpToken,B.mkI lastWithdrawEpoch]
instance P.FromData PoolDatum where
    fromBuiltinData d = D.withConstr d $ \case
        (0, [x,y,fee,fx,fy,total,lower,upper,mx,my,lp,epoch]) ->
            PoolDatum <$> P.fromBuiltinData x <*> P.fromBuiltinData y <*> D.getI fee <*> D.getI fx <*> D.getI fy <*> D.getI total <*> P.fromBuiltinData lower <*> P.fromBuiltinData upper <*> D.getI mx <*> D.getI my <*> D.getI lp <*> D.getI epoch
        _ -> Nothing
instance P.ToData ProtocolConfig where
    toBuiltinData (ProtocolConfig rate fee) = B.mkConstr 0 [B.mkI rate,B.mkI fee]
instance P.FromData ProtocolConfig where
    fromBuiltinData d = D.withConstr d $ \case
        (0, [D.getI -> Just rate,D.getI -> Just fee]) -> Just (ProtocolConfig rate fee)
        _ -> Nothing

-- Reserves exclude accrued fees and, for ADA, the pool's fixed deposit.
-- Rewards must be queried when executing an ADA pool after an epoch boundary.
activeReserves :: PoolDatum -> Integer -> Integer -> Integer -> Maybe (Integer, Integer)
activeReserves datum amountX amountY rewards = do
    guard (all (>= 0) [amountX,amountY,rewards,platformFeeX datum,platformFeeY datum,totalSwapFee datum])
    let adaX = unTupleAsset (tokenX datum) == Asset ("") ("")
        x = amountX - platformFeeX datum + rewards - if adaX then 3_000_000 + totalSwapFee datum else 0
        y = amountY - platformFeeY datum
    guard (x >= 0 && y >= 0 && x+y > 0)
    guard (adaX || rewards == 0)
    pure (x,y)

-- Integer operations match the protocol SDK's rational liquidity calculation.
virtualReserves :: Price -> Price -> Integer -> Integer -> Maybe (Integer, Integer)
virtualReserves (Price an ad) (Price bn bd) x y = do
    guard (all (> 0) [an,ad,bn,bd] && bn*ad > an*bd)
    guard (x >= 0 && y >= 0 && x+y > 0)
    let dd = ad*bd
        nn = an*bn
        discriminant = (y*dd-x*nn)^ (2 :: Int) + 4*x*y*ad*ad*bn*bn
        numerator = y*dd+x*nn+integerSqrt discriminant
        denominator = 2*(bn*ad-bd*an)
    pure (x+ceilDivide (numerator*bd) (denominator*bn),y+ceilDivide (numerator*an) (denominator*ad))

-- Returns output and the platform's input-token fee. Positive delta sells X.
quoteExactIn :: PoolDatum -> ProtocolConfig -> Integer -> Integer -> Integer -> Integer -> Maybe (Integer, Integer)
quoteExactIn datum config amountX amountY rewards delta = do
    guard (delta /= 0 && lpFeeRate datum >= 0 && lpFeeRate datum < 10_000)
    guard (platformFeeRate config >= 0 && platformFeeRate config <= 10_000 && swapFee config >= 0)
    guard (minXChange datum >= 0 && minYChange datum >= 0)
    (x,y) <- activeReserves datum amountX amountY rewards
    (vx,vy) <- virtualReserves (sqrtLowerPrice datum) (sqrtUpperPrice datum) x y
    let (vin,vout,realOut,minIn,minOut) = if delta > 0 then (vx,vy,y,minXChange datum,minYChange datum) else (vy,vx,x,minYChange datum,minXChange datum)
        input = abs delta
        retained = input*(10_000-lpFeeRate datum)
        output = vout*retained `div` (vin*10_000+retained)
        fee = (input*lpFeeRate datum `div` 10_000)*platformFeeRate config `div` 10_000
    guard (input >= minIn && output >= minOut && output > 0 && output <= realOut)
    pure (output,fee)

ceilDivide :: Integer -> Integer -> Integer
ceilDivide n d = (n+d-1) `div` d

integerSqrt :: Integer -> Integer
integerSqrt n
    | n < 2 = n
    | otherwise = go n ((n+1) `div` 2)
  where
    go previous next
        | next >= previous = previous
        | otherwise = go next ((next+n `div` next) `div` 2)
