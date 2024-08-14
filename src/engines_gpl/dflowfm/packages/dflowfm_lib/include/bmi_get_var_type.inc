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
    type_name = "int"
  case("iglobal_s")
    type_name = "int"
  case("hwav")
    type_name = "double"
  case("twav")
    type_name = "double"
  case("uorb")
    type_name = "double"
  case("bodsed")
    type_name = "double"
  case("dpsed")
    type_name = "double"
  case("msed")
    type_name = "double"
  case("thlyr")
    type_name = "double"
  case("rsedeq")
    type_name = "double"
  case("sbcx")
    type_name = "double"
  case("sbcy")
    type_name = "double"
  case("sbwx")
    type_name = "double"
  case("sbwy")
    type_name = "double"
  case("sscx")
    type_name = "double"
  case("sscy")
    type_name = "double"
  case("sswx")
    type_name = "double"
  case("sswy")
    type_name = "double"
  case("taucr")
    type_name = "double"
  case("tetacr")
    type_name = "double"
  case("InterceptThickness")
    type_name = "double"
  case("InterceptHs")
    type_name = "double"
  case("PotEvap")
    type_name = "double"
  case("ActEvap")
    type_name = "double"
  case("infilt")
    type_name = "double"
  case("infiltcap0")
    type_name = "double"
  case("infiltcap")
    type_name = "double"
  case("HortonMinInfCap")
    type_name = "double"
  case("HortonMaxInfCap")
    type_name = "double"
  case("HortonDecreaseRate")
    type_name = "double"
  case("HortonRecoveryRate")
    type_name = "double"
  case("HortonState")
    type_name = "int"
  case("shx")
    type_name = "double"
  case("shy")
    type_name = "double"
  case("shi")
    type_name = "double"
  case("zsp")
    type_name = "double"
  case("zsp0")
    type_name = "double"
  case("zspc")
    type_name = "double"
  case("zspc0")
    type_name = "double"
  case("v0ship")
    type_name = "double"
  case("v1ship")
    type_name = "double"
  case("qinship")
    type_name = "double"
  case("vicushp")
    type_name = "double"
  case("shL")
    type_name = "double"
  case("shB")
    type_name = "double"
  case("shd")
    type_name = "double"
  case("stuw")
    type_name = "double"
  case("fstuw")
    type_name = "double"
  case("stuwmx")
    type_name = "double"
  case("roer")
    type_name = "double"
  case("froer")
    type_name = "double"
  case("roermx")
    type_name = "double"
  case("wx")
    type_name = "double"
  case("wy")
    type_name = "double"
  case("rain")
    type_name = "double"
  case("evap")
    type_name = "double"
  case("qext")
    type_name = "double"
  case("qextreal")
    type_name = "double"
  case("vextcum")
    type_name = "double"
  case("rainuni")
    type_name = "double"
  case("numlatsg")
    type_name = "int"
  case("qplat")
    type_name = "double"
  case("qqlat")
    type_name = "double"
  case("balat")
    type_name = "double"
  case("qplatCum")
    type_name = "double"
  case("qplatCumPre")
    type_name = "double"
  case("qplatAve")
    type_name = "double"
  case("qLatReal")
    type_name = "double"
  case("qLatRealCum")
    type_name = "double"
  case("qLatRealCumPre")
    type_name = "double"
  case("qLatRealAve")
    type_name = "double"
  case("n1latsg")
    type_name = "int"
  case("n2latsg")
    type_name = "int"
  case("nnlat")
    type_name = "int"
  case("kclat")
    type_name = "int"
  case("Qsunmap")
    type_name = "double"
  case("zbndz")
    type_name = "double"
  case("zbndu")
    type_name = "double"
  case("zbndq")
    type_name = "double"
  case("turkin1")
    type_name = "double"
  case("vicwws")
    type_name = "double"
  case("rnveg")
    type_name = "double"
  case("diaveg")
    type_name = "double"
  case("cfuveg")
    type_name = "double"
  case("alfaveg")
    type_name = "double"
  case("stemdens")
    type_name = "double"
  case("stemdiam")
    type_name = "double"
  case("stemheight")
    type_name = "double"
  case("Cdvegsp")
    type_name = "double"
  case("zws")
    type_name = "double"
  case("kbot")
    type_name = "int"
  case("ktop")
    type_name = "int"
  case("Lbot")
    type_name = "int"
  case("Ltop")
    type_name = "int"
  case("s0")
    type_name = "double"
  case("s1")
    type_name = "double"
  case("s1max")
    type_name = "double"
  case("a0")
    type_name = "double"
  case("a1")
    type_name = "double"
  case("vol1")
    type_name = "double"
  case("vol0")
    type_name = "double"
  case("vol1_f")
    type_name = "double"
  case("hs")
    type_name = "double"
  case("ucx")
    type_name = "double"
  case("ucy")
    type_name = "double"
  case("ucz")
    type_name = "double"
  case("ucmag")
    type_name = "double"
  case("ucx_mor")
    type_name = "double"
  case("ucy_mor")
    type_name = "double"
  case("sa0")
    type_name = "double"
  case("sa1")
    type_name = "double"
  case("satop")
    type_name = "double"
  case("sabot")
    type_name = "double"
  case("tem0")
    type_name = "double"
  case("tem1")
    type_name = "double"
  case("u1")
    type_name = "double"
  case("u_to_umain")
    type_name = "double"
  case("q1")
    type_name = "double"
  case("q1_main")
    type_name = "double"
  case("au")
    type_name = "double"
  case("au_nostrucs")
    type_name = "double"
  case("hu")
    type_name = "double"
  case("frcu")
    type_name = "double"
  case("viusp")
    type_name = "double"
  case("diusp")
    type_name = "double"
  case("ndxi")
    type_name = "int"
  case("ndx1db")
    type_name = "int"
  case("kfs")
    type_name = "int"
  case("bare")
    type_name = "double"
  case("ba_mor")
    type_name = "double"
  case("bl")
    type_name = "double"
  case("bl_min")
    type_name = "double"
  case("lnx1D")
    type_name = "int"
  case("lnxi")
    type_name = "int"
  case("lnx1Db")
    type_name = "int"
  case("lnx")
    type_name = "int"
  case("ln")
    type_name = "int"
  case("LLkkk")
    type_name = "int"
  case("lncn")
    type_name = "int"
  case("kcu")
    type_name = "int"
  case("iadv")
    type_name = "int"
  case("dx")
    type_name = "double"
  case("wu")
    type_name = "double"
  case("wu_mor")
    type_name = "double"
  case("wu1D2D")
    type_name = "double"
  case("hh1D2D")
    type_name = "double"
  case("bob")
    type_name = "double"
  case("bob0")
    type_name = "double"
  case("blup")
    type_name = "double"
  case("xu")
    type_name = "double"
  case("yu")
    type_name = "double"
  case("vort")
    type_name = "double"
  case("xzw")
    type_name = "double"
  case("yzw")
    type_name = "double"
  case("xk")
    type_name = "double"
  case("yk")
    type_name = "double"
  case("zk")
    type_name = "double"
  case("kn")
    type_name = "int"
  case("numk")
    type_name = "int"
  case("zbnd1d2d1")
    type_name = "double"
  case("zbnd1d2d0")
    type_name = "double"
  case("zcrest1d2d")
    type_name = "double"
  case("edgenumbers1d2d")
    type_name = "int"
  case("kbnd1d2d")
    type_name = "int"
  case("width_1d")
    type_name = "double"
  case("qzeta_1d2d")
    type_name = "double"
  case("qlat_1d2d")
    type_name = "double"
  case("qtotal_1d2d")
    type_name = "double"
  case("is_sumvalsnd")
    type_name = "double"
  case("is_maxvalsnd")
    type_name = "double"
  case("is_dtint")
    type_name = "double"
  case("ndx2d")
    type_name = "int"
  case("ndx")
    type_name = "int"
  case("xz")
    type_name = "double"
  case("yz")
    type_name = "double"
  case("ba")
    type_name = "double"
  case("ndx1d")
    type_name = "int"
  case default
    type_name = ""
end select
