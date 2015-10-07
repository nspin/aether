{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module CRC
    ( 
    -- , crc32c
    ) where

import Data.Bits
import Data.Word.N
import Numeric

import GHC.TypeLits

class CRC (n :: Nat) (r :: Nat) where
    step :: W r -> W r -> W n -> W r

instance CRC 0 r where
    step _ register _ = register

instance {-# OVERLAPPABLE #-} forall n n' r r'. ( KnownNat n
                                                , KnownNat n'
                                                , KnownNat r
                                                , KnownNat r'
                                                , KnownNat (2 ^ n)
                                                , KnownNat (2 ^ n')
                                                , KnownNat (2 ^ r)
                                                , KnownNat (2 ^ r')
                                                , n ~ (n' + 1)
                                                , n ~ (1 + n')
                                                , r ~ (r' + 1)
                                                , r ~ (1 + r')
                                                , CRC n' r
                                                ) => CRC n r where

-- instance forall n n' r r'. ( KnownNat n
--                            , KnownNat n'
--                            , KnownNat r
--                            , KnownNat r'
--                            , KnownNat (2 ^ n)
--                            , KnownNat (2 ^ n')
--                            , KnownNat (2 ^ r)
--                            , KnownNat (2 ^ r')
--                            , n ~ (n' + 1)
--                            , n ~ (1 + n')
--                            , r ~ (r' + 1)
--                            , r ~ (1 + r')
--                            , CRC n' r
--                            ) => CRC n r where

    step seed register msg = step (seed :: W r) (nextReg :: W r) (msgRest :: W n')

      where

        msgSig :: W 1
        msgRest :: W n'
        (msgSig, msgRest) = split msg

        regSig  :: W 1
        regRest :: W r'
        (regSig, regRest) = split register

        register' :: W r
        register' = regRest >+< msgSig

        nextReg :: W r
        nextReg = case regSig of 0 -> register'
                                 _ -> register' `xor` seed

-- crc32c :: forall n m o. o ~ (n + m) => W o -> W 32
crc32c :: CRC n 32 => W n -> W 32
crc32c = step crc32c_p 0

crc32c_p :: W 32
crc32c_p = 0x1EDC6F41

-- TESTS

testData :: W 72
testData =  (49 :: W 8)
        >+< (50 :: W 8)
        >+< (51 :: W 8)
        >+< (52 :: W 8)
        >+< (53 :: W 8)
        >+< (54 :: W 8)
        >+< (55 :: W 8)
        >+< (56 :: W 8)
        >+< (57 :: W 8)

goTest = showHex (crc32c testData) ""
