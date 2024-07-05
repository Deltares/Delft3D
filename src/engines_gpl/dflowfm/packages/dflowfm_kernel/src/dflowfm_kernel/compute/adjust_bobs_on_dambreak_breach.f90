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

 !> Calculate the links affected by the dam break and sets bobs accordingly
subroutine adjust_bobs_on_dambreak_breach(width, maxwidth, crl, startingLink, L1, L2, strucid)

   use m_flowgeom
   use fm_external_forcings_data
   use MessageHandling

   implicit none

   ! input
   double precision, intent(in) :: width            !< new width of breach [m]
   double precision, intent(in) :: maxwidth         !< width of dambreak structure, i.e. maximum breach width [m]
   double precision, intent(in) :: crl              !< breached crest level [m+REF]
   integer, intent(in)          :: startingLink     !< index of first link that breaches
   integer, intent(in)          :: L1               !< last flow link on the "left"
   integer, intent(in)          :: L2               !< last flow link on the "right"
   character(len=*), intent(in) :: strucid          !< name of the dambreak structure

   ! local variables
   integer                      :: k                !< index of the dambreak flow link (range L1 to L2)
   integer                      :: Lf               !< index of flow link
   double precision             :: hremainder       !< half of the remaining breach width [m]
   double precision             :: leftBreachWidth  !< width of the breach on the "left" [m]
   double precision             :: leftfrac         !< fraction of structure width on the "left" [-]
   double precision             :: leftside         !< total dambreak structure width on the "left" [m]
   double precision             :: remainder        !< remaining breach width [m]
   double precision             :: rightBreachWidth !< width of the breach on the "right" [m]
   double precision             :: rightside        !< total dambreak structure width on the "right" [m]

   ! process the breach at the starting link
   Lf = iabs(kdambreak(3,startingLink))
   if (Lf > 0 .and. width > 0d0) then
      ! some breach, set to breached crest level
      bob(1,Lf) = max(bob0(1, Lf), crl)
      bob(2,Lf) = max(bob0(2, Lf), crl)
      activeDambreakLinks(startingLink) = 1
   else
      ! no breach
   endif

   ! distribute remaining breach width
   if (width <= dambreakLinksEffectiveLength(startingLink)) then
      ! breach width still less than width of starting link
      dambreakLinksActualLength(startingLink) = max(width, 0d0)
      leftBreachWidth = 0d0
      rightBreachWidth = 0d0
   else
      ! breach width larger than width of starting link
      dambreakLinksActualLength(startingLink) = dambreakLinksEffectiveLength(startingLink)
      leftside  = sum(dambreakLinksEffectiveLength(L1:startingLink-1))
      rightside = sum(dambreakLinksEffectiveLength(startingLink+1:L2))
      remainder = width - dambreakLinksEffectiveLength(startingLink)
      if (dambreakWidening == DBW_SYMM) then
         ! original implementation which triggers a breach too wide error be
         hremainder = 0.5d0 * remainder
         leftBreachWidth = hremainder
         rightBreachWidth = hremainder
      elseif (dambreakWidening == DBW_PROP) then
         ! proportional
         leftfrac  = leftside / (leftside +  rightside)
         leftBreachWidth = leftfrac * remainder
         rightBreachWidth = (1.0d0 - leftfrac) * remainder
      elseif (dambreakWidening == DBW_SYMM_ASYMM) then
         ! first symmetric, then asymmetric
         hremainder = 0.5d0 * remainder
         if (hremainder < min(leftside,rightside)) then
            leftBreachWidth = hremainder
            rightBreachWidth = hremainder
         elseif (leftside <= rightside) then
            leftBreachWidth = leftside
            rightBreachWidth = remainder - leftside
         else
            rightBreachWidth = rightside
            leftBreachWidth = remainder - rightside
         endif
      endif
   endif

   ! process dam "left" of initial breach segment
   do k = startingLink - 1, L1, -1
      Lf = iabs(kdambreak(3,k))
      if (leftBreachWidth > 0d0) then
         ! some breach, set to breached crest level
         if (Lf > 0) then
            bob(1,Lf) = max(bob0(1, Lf), crl)
            bob(2,Lf) = max(bob0(2, Lf), crl)
         endif
         activeDambreakLinks(k) = 1
      else
         ! no breach
      endif
      if (leftBreachWidth >= dambreakLinksEffectiveLength(k)) then
         dambreakLinksActualLength(k) = dambreakLinksEffectiveLength(k)
         leftBreachWidth = leftBreachWidth - dambreakLinksEffectiveLength(k)
      else
         dambreakLinksActualLength(k) = leftBreachWidth
         leftBreachWidth = 0d0
      endif
   enddo

   ! process dam "right" of initial breach segment
   do k = startingLink + 1, L2
      Lf = iabs(kdambreak(3,k))
      if (rightBreachWidth > 0d0) then
         ! some breach, set to breached crest level
         if (Lf > 0) then
            bob(1,Lf) = max(bob0(1, Lf), crl)
            bob(2,Lf) = max(bob0(2, Lf), crl)
         endif
         activeDambreakLinks(k) = 1
      else
         ! no breach
      endif
      if (rightBreachWidth>=dambreakLinksEffectiveLength(k)) then
         dambreakLinksActualLength(k) = dambreakLinksEffectiveLength(k)
         rightBreachWidth = rightBreachWidth - dambreakLinksEffectiveLength(k)
      else
         dambreakLinksActualLength(k) = rightBreachWidth
         rightBreachWidth = 0d0
      endif
   enddo
   
   ! check for any unprocessed breach width
   if (leftBreachWidth > 1.0d-6 * maxwidth .or. rightBreachWidth > 1.0d-6 * maxwidth) then
      write (msgbuf, '(3a)' ) 'The breach  of dam ''', trim(strucid), ''' exceeds the actual dam width on at least one side of the breach point.'
      call SetMessage(LEVEL_WARN, msgbuf)
   endif

end subroutine adjust_bobs_on_dambreak_breach
