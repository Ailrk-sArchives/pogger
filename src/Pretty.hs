{-# LANGUAGE RecordWildCards #-}

module Pretty where

import AST
import Prettyprinter

instance Pretty PoggerVal where
  pretty (Atom str) = pretty str
  pretty (List xs) =
    mconcat $
      [pretty "'("] <> intersperse (pretty " ") (pretty <$> xs)
        <> [pretty ")"]
  pretty (DottedList [x] y) =
    pretty "'("
      <> pretty x
      <> pretty " . "
      <> pretty y
      <> pretty ")"
  pretty (DottedList xs y) = mconcat $ l (pretty <$> xs)
    where
      l as =
        [pretty "'("]
          <> as
          <> [pretty " . ", pretty y, pretty ")"]
  pretty (Number (Integer int)) = pretty int
  pretty (Number (Real float)) = pretty float
  pretty (Number (Rational de d)) =
    pretty
      ( case d of
          1 -> show de
          _ -> show de <> "/" <> show d
      )
  pretty (Number (Complex r i)) = pretty (show r <> "+" <> show i <> "i")
  pretty (Bool b) = if b then pretty "#t" else pretty "#f"
  pretty (Char c) = pretty $ "\\#" ++ [c]
  pretty (String s) = pretty s
  pretty (Fn PoggerPrimitiveFn {..}) = pretty "<primitives>"
  pretty (Fn PoggerFunc {..}) =
    pretty $
      "(lambda ("
        ++ unwords (show <$> params)
        ++ ( case varargs of
               Nothing -> ""
               Just arg -> " . " ++ arg
           )
        ++ ") ...)"

intersperse :: Doc ann0 -> [Doc ann1] -> [Doc ann]
intersperse = error "not implemented"

instance Show PoggerVal where
  show = show . pretty
