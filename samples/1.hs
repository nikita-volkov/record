module Templates where

import qualified Laika
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB


type TB = TextBuilder


-- * Layout
-------------------------

admin :: 
  (!
    title :: (TB, Maybe TB),
    content :: TB,
    nav :: Maybe Nav,
    userNav :: Maybe (~name :: TB~)
  !) ->
  TB
admin =
  $(Laika.file "templates/admin.html") .
  \x ->
    (!
      title = view [l|title|] x, 
      content = view [l|content|] x, 
      nav = maybe "" nav (view [l|nav|] x), 
      userNav = view [l|userNav|] x, 
      breadcrumb = Nothing :: Maybe [(~faIcon :: TB, name :: TB~)] 
    !)
