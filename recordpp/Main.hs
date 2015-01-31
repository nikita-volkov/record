module Main where

import BasePrelude
import Record.Types
import Record.Lens
import Language.Haskell.Exts


main =
  do
    string <- readFile "samples/1.hs"
    print $ parseModuleWithMode mode string
  where
    mode :: ParseMode
    mode =
      defaultParseMode {
        -- baseLanguage = UnknownLanguage "Haskell2010 + Anonymous Records"
        baseLanguage = 
          Haskell2010
        ,
        ignoreLanguagePragmas = 
          False
        ,
        ignoreLinePragmas =
          False
        ,
        extensions =
          map EnableExtension $
          [
            Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, 
            DeriveDataTypeable, DeriveFunctor, DeriveGeneric, EmptyDataDecls, 
            FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, 
            GeneralizedNewtypeDeriving, ImpredicativeTypes, LambdaCase, LiberalTypeSynonyms, 
            MultiParamTypeClasses, MultiWayIf, 
            OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, 
            RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, 
            TupleSections, TypeFamilies, TypeOperators
          ]
      }


