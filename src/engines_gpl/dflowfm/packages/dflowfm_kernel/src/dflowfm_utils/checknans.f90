!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
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

! 
! 

 subroutine checknans()
 use m_flowgeom
 use m_flow
 use m_reduce
 implicit none

 call newfil(mdump     , 'dump')

 call chknan(s0        , 's0       ', ndx)
 call chknan(s1        , 's1       ', ndx)
 call chknan(bbr       , 'bbr      ', ndx)
 call chknan(ccr       , 'ccr      ', ndx)
 call chknan(ddr       , 'ddr      ', ndx)
 call chknan(bb        , 'bb       ', ndx)
 call chknan(dd        , 'dd       ', ndx)
 call chknan(vol0      , 'vol0     ', ndx)
 call chknan(vol1      , 'vol1     ', ndx)
 call chknan(vol1_f    , 'vol1_f   ', ndx)
 call chknan(au        , 'au       ', ndx)
 call chknan(ba        , 'ba       ', ndx)
 call chknan(a1        , 'a1       ', ndx)
 call chknan(hu        , 'hu       ', ndx)
 call chknan(u0        , 'u0       ', ndx)
 call chknan(u1        , 'u1       ', ndx)

 call doclose(mdump)



 end subroutine checknans
