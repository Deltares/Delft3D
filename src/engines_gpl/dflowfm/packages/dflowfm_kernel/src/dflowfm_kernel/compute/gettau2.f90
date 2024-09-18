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

subroutine gettau2(n,taucurc,czc,ustw2,jawaveswartdelwaq_par)
   use m_flowgeom
   use m_flow
   use m_waves
   use m_get_cz
   !
   implicit none
   !
   ! Parameters
   integer         , intent(in   ) :: n                     !< Flow node number
   double precision, intent(  out) :: taucurc               !< Bed shear stress from current or current plus wave
   double precision, intent(  out) :: czc                   !< Chezy at flow node (taucurrent)
   double precision, intent(  out) :: ustw2                 !< Ustarwave Swart (if Jawaveswartdelwaq == 1)
   integer                         :: jawaveswartdelwaq_par !< Overwrite the global jawaveswartdelwaq
   !
   ! Local variables
   integer :: LL, nn                                                      !< Local link counters
   double precision ::  cf, cfn, cz, frcn, ar,  wa, ust, ust2, fw, z00    !< Local intermediate variables
   !
   ! Body
   ustw2 = 0d0
   czc   = 0d0
   cfn   = 0d0
   wa    = 0d0
   ust   = 0d0
   z00   = 0d0

   do nn = 1,nd(n)%lnx
      LL = abs( nd(n)%ln(nn) )
      frcn = frcu(LL)
      if (frcn > 0d0 .and. hu(LL) > 0d0) then
         call getcz(hu(LL), frcn, ifrcutp(LL), cz,LL)
         cf  = ag/(cz*cz)
         ar  = au(LL)*dx(LL)
         wa  = wa + ar       ! area  weigthed
         cfn = cfn + cf*ar
         if (jawaveswartdelwaq_par <= 1) then
            ust = ust + ustb(LL)*ar           ! ustb only exists for 3D, filled with 0 for kmx==0
         else
            ust = ust + taubxu(LL)*ar
         endif
         !z00 = z00 + ar*hu(LL)*exp(-1d0 - vonkar*cz/sag)   ! z0ucur, to avoid double counting
         z00 = z00 + ar*z0ucur(LL)   ! z0ucur, to avoid double counting
      endif
   enddo
   if (wa > 0d0) then
      cfn = cfn / wa
      ust = ust / wa
      z00 = z00 / wa
   endif
   z00 = max(z00,epsz0)
   !
   if (cfn > 0) then
      czc = sqrt(ag/cfn)
   endif
   !
   ust2 = 0d0
   if (kmx == 0) then
       ust2 = cfn*(ucx(n)*ucx(n) + ucy(n)*ucy(n))
   else
       ust2 = ust*ust
   endif
   !
   if (jawaveswartdelwaq_par == 0) then
      taucurc = rhomean*ust2
   else if (jawaveSwartDelwaq_par == 1) then
      if (twav(n) > 1d-2) then
         call Swart(twav(n), uorb(n), z00, fw, ustw2)
         ust2  = ust2 + ftauw*ustw2
      endif
      taucurc = rhomean*ust2
   else if (jawaveSwartDelwaq_par == 2) then
      taucurc = ust                              ! area averaged taubxu
   endif
end subroutine gettau2
