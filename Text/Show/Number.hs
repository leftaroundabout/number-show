-- |
-- Module      : Text.Show.Number
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE TemplateHaskell          #-}

module Text.Show.Number where

import Lens.Micro
import Lens.Micro.TH
import Control.Arrow

type Δ n = n

data NumShowComponents n = NumShowComponents {
      _scaleExponent :: Int
    , _scaledIntPart :: Int
    , _significantDecimals :: [Int]
    , _remainder :: n
    } deriving (Show)
makeLenses ''NumShowComponents

errorLtdShow :: RealFloat n => Δ n -> n -> ShowS
errorLtdShow δ = preShowNum δ 10 3 >>> asm
 where asm nsc = shows (nsc^.scaledIntPart)
                   . case nsc^.significantDecimals of
                       [] -> id
                       ds  -> ('.':) . flip (foldr shows) ds
                   . case nsc^.scaleExponent of
                       0 -> id
                       e -> ('e':) . shows e


preShowNum :: RealFloat n =>
           Δ n       -- ^ Uncertainty allowance
        -> Int       -- ^ Basis
        -> Int       -- ^ Minimum magnitude of exponent to warrant scientific notation
        -> n         -- ^ Number to show
        -> NumShowComponents n
preShowNum δ b emin 𝑥
  | δ<0        = preShowNum (-δ) b emin 𝑥
  | 𝑥<0        = preShowNum δ b emin (-𝑥) & scaledIntPart %~ negate
  | 𝑥>0        = NumShowComponents exponent intPart sigDigits (rmd * 𝑏^^exponent)
  | otherwise  = NumShowComponents 0 0 [] 𝑥
 where exponent = closeZero emin . max uncrtExp . floor $ logBase 𝑏 𝑥
       uncrtExp = floor $ logBase 𝑏 δ
       μ = 𝑏^^exponent
       fIntPart = floor $ 𝑥/μ
       (intPart, sigDigits) = case sigDigs of
           [hd] | uncrtExp >= exponent
                               -> (fIntPart + (2*hd)`div`b, [])
           (hd:hds) | hd >= b  -> (fIntPart+1, 0:hds)
           hds                 -> (fIntPart  ,   hds)
       (sigDigs, rmd) = go (exponent - uncrtExp - 1) (𝑥/μ - fromIntegral fIntPart)
        where go n 𝑟
                | n>0
                , r' <- floor 𝑟'
                , (sd', 𝑟'') <- go (n-1) (𝑟' - fromIntegral r')
                   = case sd' of
                      (sd₀:sds) | sd₀ >= b  -> (r'+1:0:sds, 𝑟''/𝑏)
                      _                     -> (r':sd', 𝑟''/𝑏)
                | r' <- round 𝑟'
                   = ([r'], 𝑟 - fromIntegral r'/𝑏)
               where 𝑟' = 𝑟*𝑏
       𝑏 = fromIntegral b

closeZero :: (Num a, Ord a) => a -> a -> a
closeZero c x | x >= c     = x
              | x <= (-c)  = x
              | otherwise  = 0
