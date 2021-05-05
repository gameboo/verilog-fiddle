{-# LANGUAGE RecordWildCards #-}

module VerilogFiddle.Types (
  PortDir (..)
, IfcType (..)
, VerilogPort (..)
, VerilogModule (..)
, VerilogPortWithIfc (..)
, Ifc (..)
, VerilogModuleWithIfc (..)
) where

import Prelude hiding ((<>))
import Data.Map hiding (empty)
import Text.PrettyPrint

data PortDir = In | Out deriving Eq
instance Show PortDir where
  show In  = "Input"
  show Out = "Output"
data IfcType = Clock | Reset | AXI4 | Conduit deriving Eq
instance Show IfcType where
  show Clock = "clock"
  show Reset = "reset"
  show AXI4 = "axi4"
  show Conduit = "conduit"

data VerilogPort = VerilogPort {
  portName      :: String
, portDirection :: PortDir
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
    ClockPort VerilogPort
  | ResetPort Bool VerilogPort
  | AXI4MPort String String VerilogPort -- ifc name, axi4 standard sig name, full sig
  | AXI4SPort String String VerilogPort -- ifc name, axi4 standard sig name, full sig
  | ConduitPort VerilogPort
docVerilogPortWithIfc :: VerilogPortWithIfc -> Doc
docVerilogPortWithIfc (ClockPort vp) =
  hsep [ text "Clock", text "--", docVerilogPort vp ]
docVerilogPortWithIfc (ResetPort activLo vp) =
  hsep [ text "Reset"
       , if activLo then text "[Active Low]" else empty
       , text "--", docVerilogPort vp ]
docVerilogPortWithIfc (AXI4MPort _ sNm vp) =
  hsep [ text "AXI4 Master", text "-"
       , text sNm, text "--"
       , docVerilogPort vp ]
docVerilogPortWithIfc (AXI4SPort _ sNm vp) =
  hsep [ text "AXI4 Slave", text "-"
       , text sNm, text "--"
       , docVerilogPort vp ]
docVerilogPortWithIfc (ConduitPort vp) =
  hsep [ text "<no interface>", text "--", docVerilogPort vp ]
instance Show VerilogPortWithIfc where show = render . docVerilogPortWithIfc

data Ifc = Ifc {
  ifcClock :: Maybe VerilogPortWithIfc
, ifcReset :: Maybe VerilogPortWithIfc
, ifcPorts :: [VerilogPortWithIfc]
, ifcType  :: IfcType }
docIfc :: Ifc -> Doc
docIfc Ifc{..} =
  hang (text (show ifcType) <+> text "(interface)") 2
       (vcat [ case ifcClock of
                 Just p -> text "associated clock:" <+> docVerilogPortWithIfc p
                 Nothing -> text "no associated clock" , case ifcReset of
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
