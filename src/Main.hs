module Main where

import MwDict.IO
import Text.XML.HXT.Core

main = runX $ loadXml True Learners "apple"
