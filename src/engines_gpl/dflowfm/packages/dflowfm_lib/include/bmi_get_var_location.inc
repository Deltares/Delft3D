!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2015-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

select case(var_name)
  case("DFM_COMM_DFMWORLD")
    location_name = ""
  case("iglobal_s")
    location_name = ""
  case("hwav")
    location_name = "face"
  case("twav")
    location_name = "face"
  case("uorb")
    location_name = "face"
  case("bodsed")
    location_name = "face"
  case("dpsed")
    location_name = "face"
  case("msed")
    location_name = "face"
  case("thlyr")
    location_name = "face"
  case("rsedeq")
    location_name = "face"
  case("sbcx")
    location_name = "face"
  case("sbcy")
    location_name = "face"
  case("sbwx")
    location_name = "face"
  case("sbwy")
    location_name = "face"
  case("sscx")
    location_name = "face"
  case("sscy")
    location_name = "face"
  case("sswx")
    location_name = "face"
  case("sswy")
    location_name = "face"
  case("taucr")
    location_name = "face"
  case("tetacr")
    location_name = "face"
  case("InterceptThickness")
    location_name = "face"
  case("InterceptHs")
    location_name = "face"
  case("PotEvap")
    location_name = "face"
  case("ActEvap")
    location_name = "face"
  case("infilt")
    location_name = "face"
  case("infiltcap0")
    location_name = "face"
  case("infiltcap")
    location_name = "face"
  case("HortonMinInfCap")
    location_name = "face"
  case("HortonMaxInfCap")
    location_name = "face"
  case("HortonDecreaseRate")
    location_name = "face"
  case("HortonRecoveryRate")
    location_name = "face"
  case("HortonState")
    location_name = "face"
  case("shx")
    location_name = ""
  case("shy")
    location_name = ""
  case("shi")
    location_name = ""
  case("zsp")
    location_name = ""
  case("zsp0")
    location_name = ""
  case("zspc")
    location_name = ""
  case("zspc0")
    location_name = ""
  case("v0ship")
    location_name = ""
  case("v1ship")
    location_name = ""
  case("qinship")
    location_name = ""
  case("vicushp")
    location_name = ""
  case("shL")
    location_name = ""
  case("shB")
    location_name = ""
  case("shd")
    location_name = ""
  case("stuw")
    location_name = ""
  case("fstuw")
    location_name = ""
  case("stuwmx")
    location_name = ""
  case("roer")
    location_name = ""
  case("froer")
    location_name = ""
  case("roermx")
    location_name = ""
  case("wx")
    location_name = "edge"
  case("wy")
    location_name = "edge"
  case("rain")
    location_name = "face"
  case("evap")
    location_name = "face"
  case("qext")
    location_name = "face"
  case("qextreal")
    location_name = "face"
  case("vextcum")
    location_name = "face"
  case("rainuni")
    location_name = ""
  case("numlatsg")
    location_name = ""
  case("qplat")
    location_name = ""
  case("qqlat")
    location_name = "face"
  case("balat")
    location_name = ""
  case("qplatCum")
    location_name = ""
  case("qplatCumPre")
    location_name = ""
  case("qplatAve")
    location_name = ""
  case("qLatReal")
    location_name = ""
  case("qLatRealCum")
    location_name = ""
  case("qLatRealCumPre")
    location_name = ""
  case("qLatRealAve")
    location_name = ""
  case("n1latsg")
    location_name = ""
  case("n2latsg")
    location_name = ""
  case("nnlat")
    location_name = ""
  case("kclat")
    location_name = "face"
  case("Qsunmap")
    location_name = "face"
  case("zbndz")
    location_name = "edge"
  case("zbndu")
    location_name = "edge"
  case("zbndq")
    location_name = "edge"
  case("turkin1")
    location_name = "edge"
  case("vicwws")
    location_name = "face"
  case("rnveg")
    location_name = "face"
  case("diaveg")
    location_name = "face"
  case("cfuveg")
    location_name = "face"
  case("alfaveg")
    location_name = "face"
  case("stemdens")
    location_name = "face"
  case("stemdiam")
    location_name = "face"
  case("stemheight")
    location_name = "face"
  case("Cdvegsp")
    location_name = "face"
  case("zws")
    location_name = ""
  case("kbot")
    location_name = "face"
  case("ktop")
    location_name = "face"
  case("Lbot")
    location_name = "edge"
  case("Ltop")
    location_name = "edge"
  case("s0")
    location_name = "face"
  case("s1")
    location_name = "face"
  case("s1max")
    location_name = "face"
  case("a0")
    location_name = "face"
  case("a1")
    location_name = "face"
  case("vol1")
    location_name = "face"
  case("vol0")
    location_name = "face"
  case("vol1_f")
    location_name = "face"
  case("hs")
    location_name = "face"
  case("ucx")
    location_name = "face"
  case("ucy")
    location_name = "face"
  case("ucz")
    location_name = "face"
  case("ucmag")
    location_name = "face"
  case("ucx_mor")
    location_name = "face"
  case("ucy_mor")
    location_name = "face"
  case("sa0")
    location_name = "face"
  case("sa1")
    location_name = "face"
  case("satop")
    location_name = "face"
  case("sabot")
    location_name = "face"
  case("tem0")
    location_name = "face"
  case("tem1")
    location_name = "face"
  case("u1")
    location_name = "edge"
  case("u_to_umain")
    location_name = "edge"
  case("q1")
    location_name = "edge"
  case("q1_main")
    location_name = "edge"
  case("au")
    location_name = "edge"
  case("au_nostrucs")
    location_name = "edge"
  case("hu")
    location_name = "edge"
  case("frcu")
    location_name = "edge"
  case("viusp")
    location_name = "edge"
  case("diusp")
    location_name = "edge"
  case("ndxi")
    location_name = ""
  case("ndx1db")
    location_name = ""
  case("kfs")
    location_name = ""
  case("bare")
    location_name = "face"
  case("ba_mor")
    location_name = "face"
  case("bl")
    location_name = "face"
  case("bl_min")
    location_name = "face"
  case("lnx1D")
    location_name = ""
  case("lnxi")
    location_name = ""
  case("lnx1Db")
    location_name = ""
  case("lnx")
    location_name = ""
  case("ln")
    location_name = ""
  case("LLkkk")
    location_name = ""
  case("lncn")
    location_name = ""
  case("kcu")
    location_name = ""
  case("iadv")
    location_name = "edge"
  case("dx")
    location_name = "edge"
  case("wu")
    location_name = "edge"
  case("wu_mor")
    location_name = "edge"
  case("wu1D2D")
    location_name = "edge"
  case("hh1D2D")
    location_name = "edge"
  case("bob")
    location_name = "edge"
  case("bob0")
    location_name = "edge"
  case("blup")
    location_name = "edge"
  case("xu")
    location_name = "edge"
  case("yu")
    location_name = "edge"
  case("vort")
    location_name = ""
  case("xzw")
    location_name = ""
  case("yzw")
    location_name = ""
  case("xk")
    location_name = ""
  case("yk")
    location_name = ""
  case("zk")
    location_name = ""
  case("kn")
    location_name = ""
  case("numk")
    location_name = ""
  case("zbnd1d2d1")
    location_name = ""
  case("zbnd1d2d0")
    location_name = ""
  case("zcrest1d2d")
    location_name = ""
  case("edgenumbers1d2d")
    location_name = ""
  case("kbnd1d2d")
    location_name = ""
  case("width_1d")
    location_name = ""
  case("qzeta_1d2d")
    location_name = ""
  case("qlat_1d2d")
    location_name = ""
  case("qtotal_1d2d")
    location_name = ""
  case("is_sumvalsnd")
    location_name = "face"
  case("is_maxvalsnd")
    location_name = "face"
  case("is_dtint")
    location_name = ""
  case("ndx2d")
    location_name = ""
  case("ndx")
    location_name = ""
  case("xz")
    location_name = "face"
  case("yz")
    location_name = "face"
  case("ba")
    location_name = "face"
  case("ndx1d")
    location_name = ""
  case default
    location_name = ""
end select
