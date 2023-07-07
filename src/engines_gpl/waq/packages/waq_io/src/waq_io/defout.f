!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      subroutine defout ( noutp  , nrvar  , iostrt , iostop , iostep ,
     &                    isrtou , igrdou )
!>\file
!>                   Sets default output behavior

!     Deltares Software Centre

!     CREATED: June 1993 by Jan van Beek

!     LOGICAL UNITNUMBERS : -

!     SUBROUTINES CALLED  : -

!     COMMON's            : SYSI  , Timer characteristics

      use timers       !   performance timers
      use output
      use m_sysi          ! Timer characteristics

      implicit none

!     Parameters         :

!     kind           function         name            Descriptipon

      integer  ( 4), intent(in   ) :: noutp         !< Number of output files
      integer  ( 4), intent(  out) :: nrvar (noutp) !< Number of extra output vars
      integer  ( 4), intent(  out) :: iostrt(noutp) !< Output start time (scu)
      integer  ( 4), intent(  out) :: iostop(noutp) !< Output stop time (scu)
      integer  ( 4), intent(  out) :: iostep(noutp) !< Output step time (scu)
      integer  ( 4), intent(  out) :: isrtou(noutp) !< Sort output indication
      integer  ( 4), intent(  out) :: igrdou(noutp) !< Output grid indication

!     Common declarations


!     Local declarations

      integer, parameter :: igseg= 1 , igmon= 2 , iggrd= 3 , igsub = 4
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "defout", ithndl )

!     set output system default action

!     MONitor file

      iostrt(1) = imstrt
      iostop(1) = imstop
      iostep(1) = imstep
      nrvar (1) = 0
      isrtou(1) = imo3
      igrdou(1) = igsub

!     GRID file

      iostrt(2) = idstrt
      iostop(2) = idstop
      iostep(2) = idstep
      nrvar (2) = 0
      isrtou(2) = idmp
      igrdou(2) = iggrd

!     HIStory file

      iostrt(3) = ihstrt
      iostop(3) = ihstop
      iostep(3) = ihstep
      nrvar (3) = 0
      isrtou(3) = ihi3
      igrdou(3) = igsub

!     MAP file

      iostrt(4) = idstrt
      iostop(4) = idstop
      iostep(4) = idstep
      nrvar (4) = 0
      isrtou(4) = imap
      igrdou(4) = igseg

!     BAL file

      if ( noutp .ge. 5 ) then
         iostrt(5) = imstrt
         iostop(5) = imstop
         iostep(5) = imstep
         nrvar (5) = 0
         if ( mod(intopt,64) .ge. 32 ) then
            isrtou(5) = iba3
         elseif ( mod(intopt,32) .ge. 16 ) then
            isrtou(5) = iba2
         else
            isrtou(5) = ibal
         endif
         igrdou(5) = igsub
      endif

!     NEFIS HIS file

      if ( noutp .ge. 6 ) then
         iostrt(6) = ihstrt
         iostop(6) = ihstop
         iostep(6) = ihstep
         nrvar (6) = 0
         isrtou(6) = 0
         igrdou(6) = igsub
      endif

!     NEFIS MAP file

      if ( noutp .ge. 7 ) then
         iostrt(7) = idstrt
         iostop(7) = idstop
         iostep(7) = idstep
         nrvar (7) = 0
         isrtou(7) = 0
         igrdou(7) = igseg
      endif

!     STATistical output MAP

      if ( noutp .ge. 8 ) then
         iostrt(8) = itstop
         iostop(8) = itstop
         iostep(8) = idt
         nrvar (8) = 0
         isrtou(8) = ima2
         igrdou(8) = igseg
      endif

!     STATistical output MON

      if ( noutp .ge. 9 ) then
         iostrt(9) = itstop
         iostop(9) = itstop
         iostep(9) = idt
         nrvar (9) = 0
         isrtou(9) = imo4
         igrdou(9) = igsub
      endif

      if (timon) call timstop( ithndl )
      return
      end
