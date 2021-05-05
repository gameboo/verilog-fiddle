{-# LANGUAGE RecordWildCards #-}

module VerilogFiddle.PrettyQSYS (
  prettyQSYSTCL
) where

import Data.Map hiding (empty)
import Text.PrettyPrint

import VerilogFiddle.Types

comment doc = char '#' <+> doc

prettyVerilogModuleWithIfc :: VerilogModuleWithIfc -> Doc
prettyVerilogModuleWithIfc VerilogModuleWithIfc{..} =
  vcat $ modDefs : fileSetDefs : ifcsDefs
  where
    -- Module level definitions
    modDefs = vcat [ comment (text "module:" <+> text richModName)
                   , mProp "NAME" richModName
                   , mProp "DISPLAY_NAME" richModName ]
    -- File Sets definitions
    fileSetDefs = comment $ text "TODO: fileset"
    -- Sub-Interface level definitions
    ifcsDefs = ifcDefs <$> toList richModIfcs
    ifcDefs (iNm, Ifc{..}) =
      vcat $ [ comment (text "interface:" <+> text iNm)
             , iAdd iNm (show ifcType)
             , iProp iNm "ENABLED" "true"
             , case ifcClock of Just clk -> iAssocClk iNm clk
                                _ -> empty
             , case ifcReset of Just rst -> iAssocRst iNm rst
                                _ -> empty
             , case ifcType of
                 Reset -> iRstPolarity iNm $ head ifcPorts
                 _ -> empty ] ++ fmap (iIfcPort iNm) ifcPorts
    -- QSYS command helpers
    iAssocClk iNm clk@(ClockPort VerilogPort{..})
      | portDirection == In && portWidth == 1 =
        iProp iNm "associatedClock" portName
      | otherwise = error $ "broken clock: " ++ show clk
    iAssocRst iNm rst@(ResetPort _ VerilogPort{..})
      | portDirection == In && portWidth == 1 =
        iProp iNm "associatedReset" portName
      | otherwise = error $ "broken reset: " ++ show rst
    iRstPolarity iNm (ResetPort deAssrt _) =
      iProp iNm "synchronousEdges" $ if deAssrt then "DEASSERT" else "ASSERT"
    iIfcPort iNm (ClockPort VerilogPort{..}) =
      iPort iNm portName "clk" (show portDirection) portWidth
    iIfcPort iNm (ResetPort pol VerilogPort{..}) =
      iPort iNm portName ("reset" ++ if pol then "_n" else "")
                         (show portDirection) portWidth
    iIfcPort iNm (AXI4MPort _ sNm VerilogPort{..}) =
      iPort iNm portName sNm (show portDirection) portWidth
    iIfcPort iNm (AXI4SPort _ sNm VerilogPort{..}) =
      iPort iNm portName sNm (show portDirection) portWidth
    iIfcPort iNm (ConduitPort VerilogPort{..}) =
      iPort iNm portName portName (show portDirection) portWidth
    -- generic QSYS TCL command helpers
    mProp nm val = hsep [ text "set_module_property", text nm, text val ]
    iAdd nm tp = hsep [ text "add_interface", text nm, text tp, text "end" ]
    iProp iNm pNm val =
      hsep [ text "set_interface_property", text iNm, text pNm, text val ]
    iPort iNm pNm sNm dir w = hsep [ text "add_interface_port"
                                   , text iNm, text pNm, text sNm
                                   , text dir, integer w ]

prettyQSYSTCL :: VerilogModuleWithIfc -> String
prettyQSYSTCL = render . prettyVerilogModuleWithIfc
