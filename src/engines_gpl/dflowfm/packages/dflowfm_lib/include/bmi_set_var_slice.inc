!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2015-2025.                                
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
     ! Does not apply to scalar variables
     continue
  case("iglobal_s")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     iglobal_s(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("hwav")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     hwav(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("twav")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     twav(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("uorb")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     uorb(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr

  case("InterceptThickness")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     InterceptThickness(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("InterceptHs")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     InterceptHs(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("PotEvap")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     PotEvap(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ActEvap")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     ActEvap(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("infilt")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     infilt(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("infiltcap0")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     infiltcap0(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("infiltcap")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     infiltcap(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("HortonMinInfCap")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     HortonMinInfCap(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("HortonMaxInfCap")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     HortonMaxInfCap(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("HortonDecreaseRate")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     HortonDecreaseRate(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("HortonRecoveryRate")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     HortonRecoveryRate(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("HortonState")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     HortonState(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("shx")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     shx(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("shy")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     shy(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("shi")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     shi(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zsp")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zsp(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zsp0")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zsp0(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zspc")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zspc(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zspc0")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zspc0(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("v0ship")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     v0ship(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("v1ship")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     v1ship(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qinship")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qinship(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("vicushp")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     vicushp(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("shL")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     shL(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("shB")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     shB(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("shd")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     shd(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("stuw")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     stuw(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("fstuw")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     fstuw(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("stuwmx")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     stuwmx(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("roer")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     roer(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("froer")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     froer(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("roermx")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     roermx(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("wx")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     wx(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("wy")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     wy(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("rain")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     rain(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("evap")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     evap(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qext")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qext(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qextreal")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qextreal(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("vextcum")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     vextcum(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("rainuni")
     ! Does not apply to scalar variables
     continue
  case("numlatsg")
     ! Does not apply to scalar variables
     continue
  case("qplat")

     call c_f_pointer(xptr, x_2d_double_ptr, (/c_count(1), c_count(2)/))
     qplat(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_double_ptr
  case("qqlat")

     call c_f_pointer(xptr, x_3d_double_ptr, (/c_count(1), c_count(2), c_count(3)/))
     qqlat(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_double_ptr
  case("balat")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     balat(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qplatCum")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qplatCum(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qplatCumPre")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qplatCumPre(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qplatAve")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qplatAve(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qLatReal")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qLatReal(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qLatRealCum")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qLatRealCum(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qLatRealCumPre")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qLatRealCumPre(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qLatRealAve")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qLatRealAve(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("n1latsg")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     n1latsg(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("n2latsg")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     n2latsg(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("nnlat")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     nnlat(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("kclat")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     kclat(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("Qsunmap")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     Qsunmap(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zbndz")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zbndz(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zbndu")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zbndu(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zbndq")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zbndq(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("turkin1")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     turkin1(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("vicwws")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     vicwws(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("rnveg")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     rnveg(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("diaveg")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     diaveg(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("cfuveg")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     cfuveg(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("alfaveg")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     alfaveg(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("stemdens")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     stemdens(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("stemdiam")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     stemdiam(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("stemheight")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     stemheight(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("Cdvegsp")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     Cdvegsp(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zws")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zws(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("kbot")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     kbot(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("ktop")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     ktop(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("Lbot")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     Lbot(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("Ltop")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     Ltop(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("s0")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     s0(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("s1")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     s1(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("s1max")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     s1max(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("a0")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     a0(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("a1")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     a1(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("vol1")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     vol1(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("vol0")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     vol0(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("vol1_f")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     vol1_f(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("hs")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     hs(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ucx")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     ucx(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ucy")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     ucy(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ucz")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     ucz(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ucmag")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     ucmag(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ucx_mor")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     ucx_mor(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ucy_mor")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     ucy_mor(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("sa0")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     sa0(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("sa1")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     sa1(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("satop")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     satop(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("sabot")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     sabot(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("tem0")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     tem0(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("tem1")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     tem1(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("u1")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     u1(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("u_to_umain")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     u_to_umain(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("q1")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     q1(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("q1_main")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     q1_main(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("au")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     au(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("au_nostrucs")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     au_nostrucs(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("hu")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     hu(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("frcu")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     frcu(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("viusp")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     viusp(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("diusp")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     diusp(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ndxi")
     ! Does not apply to scalar variables
     continue
  case("ndx1db")
     ! Does not apply to scalar variables
     continue
  case("kfs")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     kfs(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("bare")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     bare(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ba_mor")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     ba_mor(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("bl")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     bl(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("bl_min")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     bl_min(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("lnx1D")
     ! Does not apply to scalar variables
     continue
  case("lnxi")
     ! Does not apply to scalar variables
     continue
  case("lnx1Db")
     ! Does not apply to scalar variables
     continue
  case("lnx")
     ! Does not apply to scalar variables
     continue
  case("ln")

     call c_f_pointer(xptr, x_2d_int_ptr, (/c_count(1), c_count(2)/))
     ln(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_int_ptr
  case("LLkkk")

     call c_f_pointer(xptr, x_2d_int_ptr, (/c_count(1), c_count(2)/))
     LLkkk(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_int_ptr
  case("lncn")

     call c_f_pointer(xptr, x_2d_int_ptr, (/c_count(1), c_count(2)/))
     lncn(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_int_ptr
  case("kcu")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     kcu(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("iadv")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     iadv(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("dx")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     dx(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("wu")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     wu(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("wu_mor")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     wu_mor(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("wu1D2D")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     wu1D2D(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("hh1D2D")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     hh1D2D(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("bob")

     call c_f_pointer(xptr, x_2d_double_ptr, (/c_count(1), c_count(2)/))
     bob(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_double_ptr
  case("bob0")

     call c_f_pointer(xptr, x_2d_double_ptr, (/c_count(1), c_count(2)/))
     bob0(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_double_ptr
  case("blup")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     blup(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("xu")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     xu(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("yu")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     yu(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("vort")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     vort(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("xzw")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     xzw(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("yzw")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     yzw(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("xk")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     xk(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("yk")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     yk(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zk")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zk(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("kn")

     call c_f_pointer(xptr, x_2d_int_ptr, (/c_count(1), c_count(2)/))
     kn(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_int_ptr
  case("numk")
     ! Does not apply to scalar variables
     continue
  case("zbnd1d2d1")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zbnd1d2d1(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zbnd1d2d0")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zbnd1d2d0(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("zcrest1d2d")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     zcrest1d2d(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("edgenumbers1d2d")

     call c_f_pointer(xptr, x_1d_int_ptr, (/c_count(1)/))
     edgenumbers1d2d(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_int_ptr
  case("kbnd1d2d")

     call c_f_pointer(xptr, x_2d_int_ptr, (/c_count(1), c_count(2)/))
     kbnd1d2d(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_int_ptr
  case("width_1d")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     width_1d(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qzeta_1d2d")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qzeta_1d2d(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qlat_1d2d")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qlat_1d2d(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("qtotal_1d2d")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     qtotal_1d2d(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("is_sumvalsnd")

     call c_f_pointer(xptr, x_2d_double_ptr, (/c_count(1), c_count(2)/))
     is_sumvalsnd(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_double_ptr
  case("is_maxvalsnd")

     call c_f_pointer(xptr, x_2d_double_ptr, (/c_count(1), c_count(2)/))
     is_maxvalsnd(c_start(1)+1:(c_start(1)+c_count(1)),c_start(2)+1:(c_start(2)+c_count(2))) = x_2d_double_ptr
  case("is_dtint")
     ! Does not apply to scalar variables
     continue
  case("ndx2d")
     ! Does not apply to scalar variables
     continue
  case("ndx")
     ! Does not apply to scalar variables
     continue
  case("xz")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     xz(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("yz")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     yz(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ba")

     call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
     ba(c_start(1)+1:(c_start(1)+c_count(1))) = x_1d_double_ptr
  case("ndx1d")
     ! Does not apply to scalar variables
     continue
  end select
