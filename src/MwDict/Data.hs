module MwDict.Data where

newtype EntryList =
  EntryList {getEntryList :: Either [Suggestion] [Entry]} deriving Show

data Entry = Entry { hws :: [Headword]
                   , fl  :: Maybe String
                   , ins :: [Inflection]
                   } deriving Show

newtype Suggestion = Suggestion {getSuggestion :: String} deriving Show

data Headword = Hw { hwText       :: String
                   , hwHightlight :: Bool
                   , hwHindex     :: Maybe Int
                   , hwSl         :: Maybe String
                   , hwPr         :: Maybe Pronunciation
                   } deriving Show

newtype Pronunciation = Pr {getPr :: [Text]} deriving Show

data Variant = Vr { vrA  :: String
                  , vrL  :: String
                  , vrPr :: Maybe Pronunciation
                  } deriving Show

data Inflection = In { inIl  :: Maybe String
                     , inIf  :: String
                     , inPrs :: [Pronunciation]
                     } deriving Show

data DirectionalCrossReference = Dx { dxText :: String
                                    , dxDxts :: [String]
                                    } deriving Show

data DefinitionField =
  Def { defSnote :: Maybe SupplementalInformationNote
      , defGram  :: Maybe GrammaticalElementAdjunct
      , defSl    :: [StatusLabel]
      , defSense :: [Sense]
      } deriving Show

newtype SupplementalInformationNote = Snote { getSnote :: [Text]
                                            } deriving Show

newtype GrammaticalElementAdjunct = Gram { getGram :: String
                                         } deriving Show

newtype StatusLabel = Sl {getSl :: String} deriving Show

data Sense = Sense { senseHeads :: [SenseHead]
                   } deriving Show

data SenseHead =
  SenseHead { senseSn    :: Maybe Int
            , senseBnote :: Maybe (Either BoldItalicNote PhrasalVerb)
            , sensePr    :: Maybe Pronunciation
            , senseVrs   :: [Variant]
            , senseLb    :: Maybe String
            , senseIns   :: [Inflection]
            , senseGram  :: Maybe GrammaticalElementAdjunct
            , senseSl    :: [StatusLabel]
            } deriving Show

newtype BoldItalicNote = Bnote {getBnote :: String} deriving Show

data PhrasalVerb = Phrasev { phrasevPva        :: String
                           , phrasevPvlAndPvas :: [(String, String)]
                           } deriving Show

data Text = Normal String
          | It     String
          | Phrase String
          | Vi     [Text]
          | Aq     [Text]
          deriving Show
