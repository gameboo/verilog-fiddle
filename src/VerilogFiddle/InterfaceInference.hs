{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module VerilogFiddle.InterfaceInference (
  inferInterfaces
) where

import Data.Maybe
import Data.STRef
import Data.Foldable
import Text.Regex.TDFA
import Control.Monad.ST
import qualified Data.Map as M

import VerilogFiddle.Types

-- Regex helpers
----------------
type RegexRetType = (String, String, String, [String])
pattern RegexMatches subs <- (_, _, _, subs)

-- Generare QSYS tcl file
-------------------------

type DetectPort = VerilogPort -> Maybe VerilogPortWithIfc

detectClockPort :: DetectPort
detectClockPort p@VerilogPort{..} =
  if portName =~ "\\<(clk|CLK)(_(.*))?" then Just $ Clock p else Nothing

detectResetPort :: DetectPort
detectResetPort p@VerilogPort{..} =
  case portName =~ "\\<(rst|RST)(_(n|N))?(_(.*))?" :: RegexRetType of
    RegexMatches [_,_,"",_,_] -> Just $ Reset False p
    RegexMatches [_,_, _,_,_] -> Just $ Reset  True p
    _ -> Nothing

detectAXI4Port :: DetectPort
detectAXI4Port p@VerilogPort{..} =
  case portName =~ "\\<ax([ms])_((.+)_)*(.+)" :: RegexRetType of
    RegexMatches matches -> Just $ go matches
    _ -> Nothing
  where go ["m", _,    "", signm] = AXI4_M "axi4_m" signm p
        go ["s", _,    "", signm] = AXI4_S "axi4_s" signm p
        go ["m", _, ifcnm, signm] = AXI4_M ifcnm signm p
        go ["s", _, ifcnm, signm] = AXI4_S ifcnm signm p

detectLonePort :: DetectPort
detectLonePort p = Just $ LonePort p

detectPortIfcs :: [VerilogPort] -> [VerilogPortWithIfc]
detectPortIfcs = fmap (fromMaybe (error "port detection error") . detectIfc)
  where detectIfc p = asum [ detectClockPort p
                           , detectResetPort p
                           , detectAXI4Port p
                           , detectLonePort p ]

detectIfcs :: [VerilogPortWithIfc] -> M.Map String Ifc
detectIfcs ports = runST do
  currentClock <- newSTRef Nothing
  currentReset <- newSTRef Nothing
  go currentClock currentReset ports M.empty
  where go _ _ [] mp = return mp
        go clkRef rstRef (p:ps) mp = do
          clk <- readSTRef clkRef
          rst <- readSTRef rstRef
          (nm, ifc) <- case p of
            Clock vp -> do writeSTRef clkRef $ Just p
                           return (portName vp, newClkIfc)
            Reset _ vp -> do writeSTRef rstRef $ Just p
                             return (portName vp, newRstIfc)
            AXI4_M iNm _ _ ->
              return (iNm, fromMaybe (newAXI4Ifc clk rst) (M.lookup iNm mp))
            AXI4_S iNm _ _ ->
              return (iNm, fromMaybe (newAXI4Ifc clk rst) (M.lookup iNm mp))
            LonePort vp -> return (portName vp, newLoneIfc clk rst)
          go clkRef rstRef ps (M.insert nm ifc{ifcPorts = p : ifcPorts ifc} mp)
        newClkIfc = Ifc Nothing Nothing [] "clock"
        newRstIfc = Ifc Nothing Nothing [] "reset"
        newAXI4Ifc clk rst = Ifc clk rst [] "axi4"
        newLoneIfc clk rst = Ifc clk rst [] "lone"

inferInterfaces :: VerilogModule -> VerilogModuleWithIfc
inferInterfaces VerilogModule{..} = VerilogModuleWithIfc modName modIfcs
  where modIfcs = detectIfcs . detectPortIfcs $ modPorts
