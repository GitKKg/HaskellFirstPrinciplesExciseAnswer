-- | 
{-# LANGUAGE OverloadedStrings #-}
module Haskell.FirstPrinciples.MonadTransformers.MaybeTinUse where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Web.Scotty

param' :: Parsable a => Text -> MaybeT ActionM a 
param' k = MaybeT $
  rescue (Just <$> param k) (const (return Nothing))
-- param' :: Parsable a => Text -> ActionM (Maybe a)
-- param' k = rescue (Just <$> param k) (const (return Nothing))

type Reco = (Integer, Integer, Integer,Integer)

-- type locahost:3000/?1=1&2=2&3=3&4=4
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    reco <- runMaybeT $ do
      a <- param' "1"
      liftIO $ print a --not show in console,why? see comment about address type above
      liftIO $ print "next1" --not show in console
      liftIO $ print beam -- not show in console
      liftIO $ print "next2" -- not show in console
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift.lift) $ print b -- not show in console
      liftIO $ print "next3"
      -- return ((1,3,3,2) :: Reco) -- only pass when del all ref to a,b,c,d,why?
      return ((a,b,c,d):: Reco)
    liftIO $ print reco -- show in console
    liftIO $ print "next4" -- show in console
    liftIO $ print beam
    html $ mconcat ["<h1>Scotty,", beam," me up!</h1>"]



{-
main = scotty 3000 $ do
  get "/:word" $ do
    beam' <- param' "word"
    let beam = fromMaybe "" beam'
    i <- param' "2"
    liftIO $ print (i :: Maybe Integer)
    liftIO $ print beam'
    html $ mconcat ["<h1>Scotty,",beam,"me up!</h1>"]
-}
