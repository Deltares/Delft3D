!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2015-2026.                                
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
    type_name = "float"
  case("twav")
    type_name = "float"
  case("twavcom")
    type_name = "float"
  case("uorb")
    type_name = "float"
  case("bodsed")
    type_name = "float"
  case("dpsed")
    type_name = "float"
  case("msed")
    type_name = "float"
  case("thlyr")
    type_name = "float"
  case("rsedeq")
    type_name = "float"
  case("sbcx")
    type_name = "float"
  case("sbcy")
    type_name = "float"
  case("sbwx")
    type_name = "float"
  case("sbwy")
    type_name = "float"
  case("sscx")
    type_name = "float"
  case("sscy")
    type_name = "float"
  case("sswx")
    type_name = "float"
  case("sswy")
    type_name = "float"
  case("taucr")
    type_name = "float"
  case("tetacr")
    type_name = "float"
  case("InterceptThickness")
    type_name = "float"
  case("InterceptHs")
    type_name = "float"
  case("PotEvap")
    type_name = "float"
  case("ActEvap")
    type_name = "float"
  case("infilt")
    type_name = "double"
  case("infiltcap")
    type_name = "double"
  case("horton_state")
    type_name = "int"
  case("shx")
    type_name = "float"
  case("shy")
    type_name = "float"
  case("shi")
    type_name = "float"
  case("zsp")
    type_name = "float"
  case("zsp0")
    type_name = "float"
  case("zspc")
    type_name = "float"
  case("zspc0")
    type_name = "float"
  case("v0ship")
    type_name = "float"
  case("v1ship")
    type_name = "float"
  case("qinship")
    type_name = "float"
  case("vicushp")
    type_name = "float"
  case("shL")
    type_name = "float"
  case("shB")
    type_name = "float"
  case("shd")
    type_name = "float"
  case("stuw")
    type_name = "float"
  case("fstuw")
    type_name = "float"
  case("stuwmx")
    type_name = "float"
  case("roer")
    type_name = "float"
  case("froer")
    type_name = "float"
  case("roermx")
    type_name = "float"
  case("wx")
    type_name = "float"
  case("wy")
    type_name = "float"
  case("rain")
    type_name = "float"
  case("evap")
    type_name = "float"
  case("qext")
    type_name = "float"
  case("qextreal")
    type_name = "float"
  case("vextcum")
    type_name = "float"
  case("rainuni")
    type_name = "float"
  case("numlatsg")
    type_name = "int"
  case("qplat")
    type_name = "float"
  case("qqlat")
    type_name = "float"
  case("balat")
    type_name = "float"
  case("qplatCum")
    type_name = "float"
  case("qplatCumPre")
    type_name = "float"
  case("qplatAve")
    type_name = "float"
  case("qLatReal")
    type_name = "float"
  case("qLatRealCum")
    type_name = "float"
  case("qLatRealCumPre")
    type_name = "float"
  case("qLatRealAve")
    type_name = "float"
  case("n1latsg")
    type_name = "int"
  case("n2latsg")
    type_name = "int"
  case("nnlat")
    type_name = "int"
  case("kclat")
    type_name = "int"
  case("qsunmap")
    type_name = "float"
  case("secchisp")
    type_name = "float"
  case("zbndz")
    type_name = "float"
  case("zbndu")
    type_name = "float"
  case("zbndq")
    type_name = "float"
  case("turkin1")
    type_name = "float"
  case("vicwws")
    type_name = "float"
  case("difwws")
    type_name = "float"
  case("rnveg")
    type_name = "float"
  case("diaveg")
    type_name = "float"
  case("cfuveg")
    type_name = "float"
  case("alfaveg")
    type_name = "float"
  case("stemdens")
    type_name = "float"
  case("stemdiam")
    type_name = "float"
  case("stemheight")
    type_name = "float"
  case("Cdvegsp")
    type_name = "float"
  case("zws")
    type_name = "float"
  case("kbot")
    type_name = "int"
  case("ktop")
    type_name = "int"
  case("Lbot")
    type_name = "int"
  case("Ltop")
    type_name = "int"
  case("s0")
    type_name = "float"
  case("s1")
    type_name = "float"
  case("s1max")
    type_name = "float"
  case("a0")
    type_name = "float"
  case("a1")
    type_name = "float"
  case("vol1")
    type_name = "float"
  case("vol0")
    type_name = "float"
  case("vol1_f")
    type_name = "float"
  case("hs")
    type_name = "float"
  case("ucx")
    type_name = "float"
  case("ucy")
    type_name = "float"
  case("ucz")
    type_name = "float"
  case("ucmag")
    type_name = "float"
  case("ucx_mor")
    type_name = "float"
  case("ucy_mor")
    type_name = "float"
  case("sa0")
    type_name = "float"
  case("sa1")
    type_name = "float"
  case("satop")
    type_name = "float"
  case("sabot")
    type_name = "float"
  case("tem0")
    type_name = "float"
  case("tem1")
    type_name = "float"
  case("u1")
    type_name = "float"
  case("u_to_umain")
    type_name = "float"
  case("q1")
    type_name = "float"
  case("q1_main")
    type_name = "float"
  case("au")
    type_name = "float"
  case("au_nostrucs")
    type_name = "float"
  case("hu")
    type_name = "float"
  case("frcu")
    type_name = "float"
  case("viusp")
    type_name = "float"
  case("diusp")
    type_name = "float"
  case("ndxi")
    type_name = "int"
  case("ndx1db")
    type_name = "int"
  case("kfs")
    type_name = "int"
  case("bare")
    type_name = "float"
  case("ba_mor")
    type_name = "float"
  case("bl")
    type_name = "float"
  case("bl_min")
    type_name = "float"
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
    type_name = "float"
  case("wu")
    type_name = "float"
  case("wu_mor")
    type_name = "float"
  case("wu1D2D")
    type_name = "float"
  case("hh1D2D")
    type_name = "float"
  case("bob")
    type_name = "float"
  case("bob0")
    type_name = "float"
  case("blup")
    type_name = "float"
  case("xu")
    type_name = "float"
  case("yu")
    type_name = "float"
  case("vort")
    type_name = "float"
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
    type_name = "float"
  case("zbnd1d2d0")
    type_name = "float"
  case("zcrest1d2d")
    type_name = "float"
  case("edgenumbers1d2d")
    type_name = "int"
  case("kbnd1d2d")
    type_name = "int"
  case("width_1d")
    type_name = "float"
  case("qzeta_1d2d")
    type_name = "float"
  case("qlat_1d2d")
    type_name = "float"
  case("qtotal_1d2d")
    type_name = "float"
  case("is_sumvalsnd")
    type_name = "float"
  case("is_maxvalsnd")
    type_name = "float"
  case("is_dtint")
    type_name = "float"
  case("ndx2d")
    type_name = "int"
  case("ndx")
    type_name = "int"
  case("xz")
    type_name = "float"
  case("yz")
    type_name = "float"
  case("ba")
    type_name = "float"
  case("ndx1d")
    type_name = "int"
  case default
    type_name = ""
end select
