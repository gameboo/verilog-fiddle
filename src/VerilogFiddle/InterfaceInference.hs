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
  if portName =~ "\\<(clk|CLK)(_(.*))?" then Just $ ClockPort p else Nothing

detectResetPort :: DetectPort
detectResetPort p@VerilogPort{..} =
  case portName =~ "\\<(rst|RST)(_(n|N))?(_(.*))?" :: RegexRetType of
    RegexMatches [_,_,"",_,_] -> Just $ ResetPort False p
    RegexMatches [_,_, _,_,_] -> Just $ ResetPort  True p
    _ -> Nothing

detectAXI4Port :: DetectPort
detectAXI4Port p@VerilogPort{..} =
  case portName =~ "\\<ax(l?)([ms])_((.+)_)*(.+)" :: RegexRetType of
    RegexMatches matches -> Just $ go matches
    _ -> Nothing
  where go ["", "m", _,    "", signm] = AXI4MPort "axi4_m" signm p
        go ["", "s", _,    "", signm] = AXI4SPort "axi4_s" signm p
        go ["", "m", _, ifcnm, signm] = AXI4MPort ifcnm signm p
        go ["", "s", _, ifcnm, signm] = AXI4SPort ifcnm signm p
        go ["l", "m", _,    "", signm] = AXI4LiteMPort "axi4_m" signm p
        go ["l", "s", _,    "", signm] = AXI4LiteSPort "axi4_s" signm p
        go ["l", "m", _, ifcnm, signm] = AXI4LiteMPort ifcnm signm p
        go ["l", "s", _, ifcnm, signm] = AXI4LiteSPort ifcnm signm p

detectConduitPort :: DetectPort
detectConduitPort p = Just $ ConduitPort p

detectPortIfcs :: [VerilogPort] -> [VerilogPortWithIfc]
detectPortIfcs = fmap (fromMaybe (error "port detection error") . detectIfc)
  where detectIfc p = asum [ detectClockPort p
                           , detectResetPort p
                           , detectAXI4Port p
                           , detectConduitPort p ]

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
            ClockPort vp -> do writeSTRef clkRef $ Just p
                               return (portName vp, newClkIfc)
            ResetPort _ vp -> do writeSTRef rstRef $ Just p
                                 return (portName vp, newRstIfc clk)
            AXI4MPort iNm _ _ ->
              return (iNm, fromMaybe (newAXI4Ifc clk rst) (M.lookup iNm mp))
            AXI4SPort iNm _ _ ->
              return (iNm, fromMaybe (newAXI4Ifc clk rst) (M.lookup iNm mp))
            AXI4LiteMPort iNm _ _ ->
              return (iNm, fromMaybe (newAXI4LiteIfc clk rst) (M.lookup iNm mp))
            AXI4LiteSPort iNm _ _ ->
              return (iNm, fromMaybe (newAXI4LiteIfc clk rst) (M.lookup iNm mp))
            ConduitPort vp -> return (portName vp, newConduitIfc clk rst)
          go clkRef rstRef ps (M.insert nm ifc{ifcPorts = p : ifcPorts ifc} mp)
        newClkIfc              = Ifc Nothing Nothing [] Clock
        newRstIfc      clk     = Ifc     clk Nothing [] Reset
        newAXI4Ifc     clk rst = Ifc     clk     rst [] AXI4
        newAXI4LiteIfc clk rst = Ifc     clk     rst [] AXI4Lite
        newConduitIfc  clk rst = Ifc     clk     rst [] Conduit

inferInterfaces :: VerilogModule -> VerilogModuleWithIfc
inferInterfaces VerilogModule{..} = VerilogModuleWithIfc modName modIfcs
  where modIfcs = detectIfcs . detectPortIfcs $ modPorts
