{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Text.Template.Render (
  Template (..),
  Frag (..),
  render,
  renderA,
) where

import Control.Monad.StrictIdentity (StrictIdentity (..))
import Data.Foldable (Foldable (..))
import Data.Text.Builder.Linear (Builder)
import Data.Text.Template.Parse (
  Frag (..),
  Lit (builder),
  Template (..),
  Var,
 )

type ContextB = Var -> Builder
type ContextAB f = Var -> f Builder

render :: Template -> ContextB -> Builder
{-# INLINE render #-}
render template ctxFunc = runStrictIdentity_ $ renderA template (StrictIdentity . ctxFunc)

renderA :: (Applicative f) => Template -> ContextAB f -> f Builder
{-# INLINEABLE renderA #-}
renderA (Template frags) ctxFunc = foldMap' id <$> traverse renderFrag frags
 where
  renderFrag (LitF s) = pure s.builder
  renderFrag (VarF v) = ctxFunc v
