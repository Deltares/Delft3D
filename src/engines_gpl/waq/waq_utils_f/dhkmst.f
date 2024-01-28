!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_dhkmst
      use m_waq_precision


      implicit none

      contains


      subroutine dhkmst ( iknmrk , kenmrk , knmrki )

!     Deltares Software Centre

!>\file utility that sets a feature in the feature integer
!>
!>    The feature is an integer with at most 9 10-base features.
!>    This routine sets a feature. Routine evaluate_waq_attribute is the mirror
!>    routine that reads the feature.

!     Created : June     1994 by Jan van Beek as evaluate_waq_attribute

!     Modified: February 2011 by Leo Postma

      implicit none

!     Parameters          :

!     kind           function         name           description

      integer(kind=int_wp), intent(in   )  ::iknmrk       !< Index of the feature to be set
      integer(kind=int_wp), intent(inout)  ::kenmrk       !< Feature value to be modified
      integer(kind=int_wp), intent(in   )  ::knmrki       !< Value to update the feature with

!     Local               :

      integer(kind=int_wp) ::ikhlp           ! to store lower order part
      integer(kind=int_wp) ::ipower          ! to store higher powers of 10

      if     ( iknmrk .eq. 1 ) then
         kenmrk = (kenmrk/  10)*  10 + knmrki

      elseif ( iknmrk .eq. 2 ) then
         ikhlp  = mod(kenmrk, 10)
         kenmrk = (kenmrk/ 100)* 100 + knmrki* 10 + ikhlp

      elseif ( iknmrk .eq. 3 ) then
         ikhlp  = mod(kenmrk,100)
         kenmrk = (kenmrk/1000)*1000 + knmrki*100 + ikhlp

      elseif ( iknmrk .le. 0 .or. iknmrk .gt. 9 ) then
         kenmrk = -999

      else
         ipower = 10**iknmrk + 0.5
         ikhlp  = mod(kenmrk,ipower/10)
         kenmrk = (kenmrk/ipower)*ipower + knmrki*ipower/10 + ikhlp
      endif

      return
      end
      end module m_dhkmst
