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
     rank = 0
  case("iglobal_s")
     rank = 1
  case("hwav")
     rank = 1
  case("twav")
     rank = 1
  case("Uorb")
     rank = 1
  case("bodsed")
     rank = 2
  case("dpsed")
     rank = 1
  case("msed")
     rank = 3
  case("thlyr")
     rank = 2
  case("rsedeq")
     rank = 2
  case("sbcx")
     rank = 2
  case("sbcy")
     rank = 2
  case("sbwx")
     rank = 2
  case("sbwy")
     rank = 2
  case("sscx")
     rank = 2
  case("sscy")
     rank = 2
  case("sswx")
     rank = 2
  case("sswy")
     rank = 2
  case("taucr")
     rank = 1
  case("tetacr")
     rank = 1
  case("InterceptThickness")
     rank = 1
  case("InterceptHs")
     rank = 1
  case("PotEvap")
     rank = 1
  case("ActEvap")
     rank = 1
  case("infilt")
     rank = 1
  case("infiltcap0")
     rank = 1
  case("infiltcap")
     rank = 1
  case("HortonMinInfCap")
     rank = 1
  case("HortonMaxInfCap")
     rank = 1
  case("HortonDecreaseRate")
     rank = 1
  case("HortonRecoveryRate")
     rank = 1
  case("HortonState")
     rank = 1
  case("shx")
     rank = 1
  case("shy")
     rank = 1
  case("shi")
     rank = 1
  case("zsp")
     rank = 1
  case("zsp0")
     rank = 1
  case("zspc")
     rank = 1
  case("zspc0")
     rank = 1
  case("v0ship")
     rank = 1
  case("v1ship")
     rank = 1
  case("qinship")
     rank = 1
  case("vicushp")
     rank = 1
  case("shL")
     rank = 1
  case("shB")
     rank = 1
  case("shd")
     rank = 1
  case("stuw")
     rank = 1
  case("fstuw")
     rank = 1
  case("stuwmx")
     rank = 1
  case("roer")
     rank = 1
  case("froer")
     rank = 1
  case("roermx")
     rank = 1
  case("wx")
     rank = 1
  case("wy")
     rank = 1
  case("rain")
     rank = 1
  case("evap")
     rank = 1
  case("numlatsg")
     rank = 0
  case("qplat")
     rank = 1
  case("qqlat")
     rank = 1
  case("balat")
     rank = 1
  case("qplatCum")
     rank = 1
  case("qplatCumPre")
     rank = 1
  case("qplatAve")
     rank = 1
  case("qLatReal")
     rank = 1
  case("qLatRealCum")
     rank = 1
  case("qLatRealCumPre")
     rank = 1
  case("qLatRealAve")
     rank = 1
  case("n1latsg")
     rank = 1
  case("n2latsg")
     rank = 1
  case("nnlat")
     rank = 1
  case("kclat")
     rank = 1
  case("qext")
     rank = 1
  case("qextreal")
     rank = 1
  case("vextcum")
     rank = 1
  case("rainuni")
     rank = 0
  case("Qsunmap")
     rank = 1
  case("zbndz")
     rank = 1
  case("zbndu")
     rank = 1
  case("zbndq")
     rank = 1
  case("turkin1")
     rank = 1
  case("vicwws")
     rank = 1
  case("rnveg")
     rank = 1
  case("diaveg")
     rank = 1
  case("cfuveg")
     rank = 1
  case("alfaveg")
     rank = 1
  case("stemdens")
     rank = 1
  case("stemdiam")
     rank = 1
  case("stemheight")
     rank = 1
  case("Cdvegsp")
     rank = 1
  case("zws")
     rank = 1
  case("kbot")
     rank = 1
  case("ktop")
     rank = 1
  case("Lbot")
     rank = 1
  case("Ltop")
     rank = 1
  case("s0")
     rank = 1
  case("s1")
     rank = 1
  case("s1max")
     rank = 1
  case("a0")
     rank = 1
  case("a1")
     rank = 1
  case("vol1")
     rank = 1
  case("vol0")
     rank = 1
  case("vol1_f")
     rank = 1
  case("hs")
     rank = 1
  case("ucx")
     rank = 1
  case("ucy")
     rank = 1
  case("ucz")
     rank = 1
  case("ucmag")
     rank = 1
  case("ucx_mor")
     rank = 1
  case("ucy_mor")
     rank = 1
  case("sa0")
     rank = 1
  case("sa1")
     rank = 1
  case("satop")
     rank = 1
  case("sabot")
     rank = 1
  case("tem0")
     rank = 1
  case("tem1")
     rank = 1
  case("u1")
     rank = 1
  case("u_to_umain")
     rank = 1
  case("q1")
     rank = 1
  case("q1_main")
     rank = 1
  case("au")
     rank = 1
  case("au_nostrucs")
     rank = 1
  case("hu")
     rank = 1
  case("frcu")
     rank = 1
  case("viusp")
     rank = 1
  case("diusp")
     rank = 1
  case("taus")
     rank = 1
  case("ndxi")
     rank = 0
  case("ndx1db")
     rank = 0
  case("kfs")
     rank = 1
  case("bare")
     rank = 1
  case("ba_mor")
     rank = 1
  case("bl")
     rank = 1
  case("bl_min")
     rank = 1
  case("lnx1D")
     rank = 0
  case("lnxi")
     rank = 0
  case("lnx1Db")
     rank = 0
  case("lnx")
     rank = 0
  case("ln")
     rank = 2
  case("LLkkk")
     rank = 2
  case("lncn")
     rank = 2
  case("kcu")
     rank = 1
  case("iadv")
     rank = 1
  case("dx")
     rank = 1
  case("wu")
     rank = 1
  case("wu_mor")
     rank = 1
  case("wu1D2D")
     rank = 1
  case("hh1D2D")
     rank = 1
  case("bob")
     rank = 2
  case("bob0")
     rank = 2
  case("blup")
     rank = 1
  case("xu")
     rank = 1
  case("yu")
     rank = 1
  case("vort")
     rank = 1
  case("xzw")
     rank = 1
  case("yzw")
     rank = 1
  case("xk")
     rank = 1
  case("yk")
     rank = 1
  case("zk")
     rank = 1
  case("kn")
     rank = 2
  case("numk")
     rank = 0
  case("zbnd1d2d1")
     rank = 1
  case("zbnd1d2d0")
     rank = 1
  case("zcrest1d2d")
     rank = 1
  case("edgenumbers1d2d")
     rank = 1
  case("kbnd1d2d")
     rank = 2
  case("width_1d")
     rank = 1
  case("qzeta_1d2d")
     rank = 1
  case("qlat_1d2d")
     rank = 1
  case("qtotal_1d2d")
     rank = 1
  case("is_sumvalsnd")
     rank = 2
  case("is_maxvalsnd")
     rank = 2
  case("is_dtint")
     rank = 0
  case("ndx2d")
     rank = 0
  case("ndx")
     rank = 0
  case("xz")
     rank = 1
  case("yz")
     rank = 1
  case("ba")
     rank = 1
  case("ndx1d")
     rank = 0
  case default
     rank = 0
  end select
