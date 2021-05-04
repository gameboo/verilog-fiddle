{-# LANGUAGE RecordWildCards #-}

module VerilogFiddle.Types (
  VerilogPortDir (..)
, VerilogPort (..)
, VerilogModule (..)
, VerilogPortWithIfc (..)
, Ifc (..)
, VerilogModuleWithIfc (..)
) where

import Prelude hiding ((<>))
import Data.Map hiding (empty)
import Text.PrettyPrint

data VerilogPortDir = In | Out
instance Show VerilogPortDir where
  show In  = "Input"
  show Out = "Output"

data VerilogPort = VerilogPort {
  portName      :: String
, portDirection :: VerilogPortDir
, portWidth     :: Integer }
docVerilogPort :: VerilogPort -> Doc
docVerilogPort VerilogPort{..} =
  text portName <+>
  (braces . sep . punctuate comma)
    [ text "width:" <+> integer portWidth
    , text "dir:"   <+> text (show portDirection) ]
instance Show VerilogPort where show = render . docVerilogPort

data VerilogModule = VerilogModule {
  modName  :: String
, modPorts :: [VerilogPort] }
docVerilogModule :: VerilogModule -> Doc
docVerilogModule VerilogModule{..} =
  hang (text modName <> colon) 2 (sep $ fmap docVerilogPort modPorts)
instance Show VerilogModule where show = render . docVerilogModule

data VerilogPortWithIfc =
    Clock VerilogPort
  | Reset Bool VerilogPort
  | AXI4_M String String VerilogPort -- ifc name, axi4 standard sig name, full sig
  | AXI4_S String String VerilogPort -- ifc name, axi4 standard sig name, full sig
  | LonePort VerilogPort
docVerilogPortWithIfc :: VerilogPortWithIfc -> Doc
docVerilogPortWithIfc (Clock vp) =
  hsep [ text "Clock", text "--", docVerilogPort vp ]
docVerilogPortWithIfc (Reset activLo vp) =
  hsep [ text "Reset"
       , if activLo then text "[Active Low]" else empty
       , text "--", docVerilogPort vp ]
docVerilogPortWithIfc (AXI4_M _ sNm vp) =
  hsep [ text "AXI4 Master", text "-"
       , text sNm, text "--"
       , docVerilogPort vp ]
docVerilogPortWithIfc (AXI4_S _ sNm vp) =
  hsep [ text "AXI4 Slave", text "-"
       , text sNm, text "--"
       , docVerilogPort vp ]
docVerilogPortWithIfc (LonePort vp) =
  hsep [ text "<no interface>", text "--", docVerilogPort vp ]
instance Show VerilogPortWithIfc where show = render . docVerilogPortWithIfc

data Ifc = Ifc {
  ifcClock :: Maybe VerilogPortWithIfc
, ifcReset :: Maybe VerilogPortWithIfc
, ifcPorts :: [VerilogPortWithIfc]
, ifcType  :: String }
docIfc :: Ifc -> Doc
docIfc Ifc{..} =
  hang (text ifcType <+> text "(interface)") 2
       (vcat [ case ifcClock of
                 Just p -> text "associated clock:" <+> docVerilogPortWithIfc p
                 Nothing -> text "no associated clock"
             , case ifcReset of
                 Just p -> text "associated reset:" <+> docVerilogPortWithIfc p
                 Nothing -> text "no associated reset"
             , sep $ (text "ports:") : fmap docVerilogPortWithIfc ifcPorts ])
instance Show Ifc where show = render . docIfc

data VerilogModuleWithIfc = VerilogModuleWithIfc {
  richModName :: String
, richModIfcs :: Map String Ifc }
docVerilogModuleWithIfc :: VerilogModuleWithIfc -> Doc
docVerilogModuleWithIfc VerilogModuleWithIfc{..} =
  hang (hsep [text "--", text richModName, text "(module)", text "--"]) 2
       (vcat $ fmap prettyIfc (toList richModIfcs))
  where prettyIfc (nm, ifc) =
          text "*" <+> text nm <> colon <+> docIfc ifc
instance Show VerilogModuleWithIfc where show = render . docVerilogModuleWithIfc
