module TestMe
    ( -- * Basic Operations
      square
    , double
    ) where

-- | Simple function with inline code block: @square x@ squares a number
square :: Int -> Int
square x = x * x

{- | Multiline comment with multiple code blocks:
     Use @double x@ to multiply by 2
     Or use @triple x@ to multiply by 3

     Example:
     @
     let x = 5
     double x  -- returns 10
     triple x  -- returns 15
     @
-}
double :: Int -> Int
double x = 2 * x
