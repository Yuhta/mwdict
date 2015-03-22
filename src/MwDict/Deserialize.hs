{-# OPTIONS -XTupleSections -fno-warn-orphans #-}
module MwDict.Deserialize () where

import Control.Applicative ((<|>))
import MwDict.Data
import Text.XML.HXT.Arrow.Pickle.Xml
import Text.XML.HXT.Core

instance XmlPickler EntryList where
  xpickle = xpElem "entry_list"         $
            xpFilterAttr none           $
            xpWrapEither (wrap, unwrap) $
            xpPair xpickle xpickle
    where
      wrap (ss, []) = Right . EntryList $ Left  ss
      wrap ([], es) = Right . EntryList $ Right es
      wrap _        = Left "entry_list has both suggestions and entries"
      unwrap        = either (, []) ([], ) . getEntryList

instance XmlPickler Entry where
  xpickle = xpElem "entry"                      $
            xpFilterAttr none                   $
            xpFilterCont (neg $ nameIn ignored) $
            xpFilterCont (nameIn [ "hw", "ahw", "hsl", "pr", "altpr", "fl", "in"
                                 , "def"
                                 ]) $
            xpWrap (uncurry3 Entry, undefined)  $
            xpTriple
            xpHeadwords
            (xpOption $ xpElem "fl" xpText)
            xpIns
    where ignored = ["sound"]

instance XmlPickler Suggestion where
  xpickle = xpElem "suggestion" $
            xpWrap (Suggestion, getSuggestion) xpText

nameIn :: ArrowXml a => [String] -> a XmlTree XmlTree
nameIn xs = foldr1 (<+>) $ map hasName xs

xpHeadwords :: PU [Headword]
xpHeadwords = xpWrap (uncurry (:), undefined) $ xpPair
              (xpHeadword "hw") (xpList $ xpHeadword "ahw")

xpHeadword :: String -> PU Headword
xpHeadword tag = xpWrap (wrap, undefined) $ xp4Tuple
                 (xpElem tag $ xpHw)
                 (xpOption $ xpElem "hsl" xpText)
                 (xpOption $ xpPr "pr")
                 (xpOption $ xpPr "altpr")
  where wrap ((hw', highlight, hindex), hsl, pr, altpr) =
          Hw hw' highlight hindex hsl $ pr <|> altpr

xpHw :: PU (String, Bool, Maybe Int)
xpHw = xpTriple
       xpText
       (xpWrap (wrap, undefined) (xpAttrImplied "highlight" xpText))
       (xpAttrImplied "hindex" xpInt)
  where wrap = maybe False (== "yes")

xpPr :: String -> PU Pronunciation
xpPr tag = xpElem tag $ xpWrap (Pr, getPr) xpickle

xpIns :: PU [Inflection]
xpIns = xpWrap (concat, undefined) . xpList . xpElem "in" $ xpList xpIn

xpIn :: PU Inflection
xpIn = xpFilterCont (neg $ hasName "sound") $
       xpWrap (uncurry3 In, undefined)      $
       xpTriple
       (xpOption $ xpElem "il" xpText)
       (xpElem "if" xpText)
       xpPrList
  where xpPrList = xpList . xpWrapMaybe (uncurry (<|>), undefined) $
                   xpPair (xpOption $ xpPr "pr") (xpOption $ xpPr "altpr")

instance XmlPickler DirectionalCrossReference where
  xpickle = xpElem "dx" . xpWrap (uncurry Dx, undefined) $
            xpPair xpText (xpList $ xpElem "dxt" xpText)

instance XmlPickler SupplementalInformationNote where
  xpickle = xpElem "snote" $ xpWrap (Snote, getSnote) xpickle

instance XmlPickler Text where
  xpickle = xpAlt undefined [ wrap Normal xpText
                            , wrap It     $ xpElem "it"     xpText
                            , wrap Phrase $ xpElem "phrase" xpText
                            , wrap Vi     $ xpElem "vi"     xpickle
                            , wrap Aq     $ xpElem "aq"     xpickle
                            ]
    where wrap ctor = xpWrap (ctor, undefined)
