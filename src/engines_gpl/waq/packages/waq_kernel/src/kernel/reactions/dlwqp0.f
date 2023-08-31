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
!!  stichting deltares
!!  p.o. box 177
!!  2600 mh delft, the netherlands
!!
!!  all indications and logos of, and references to registered trademarks
!!  of stichting deltares remain the property of stichting deltares. all
!!  rights reserved.
      module m_dlwqp0

      implicit none

      contains


      subroutine dlwqp0 ( conc   , amass  , deriv  , volume , idt    ,
     *                    nosys  , notot  , noseg  , lun     ,ivflag ,
     *                    surfac )

!     Deltares Software Centre

!>\File
!>           Sets an explicit time step from DERIV.

!     Created             :    April   1988 by Leo Postma

!     Modified            :    Nov     2012 by Jan van Beek
!                                           2D arrays, fortran 90 look and feel
!                                           conc of passive substances in mass/m2

!     Logical unitnumbers : lun     = number of monitoring file

!     Subroutines called  : none

      use timers
      implicit none

!     Parameters          :
!     type     kind  function         name                      description

      integer   (4), intent(in   ) :: nosys                   !< number of transported substances
      integer   (4), intent(in   ) :: notot                   !< total number of substances
      integer   (4), intent(in   ) :: noseg                   !< number of computational volumes
      real      (4), intent(inout) :: conc  (notot ,noseg)    !< concentrations per substance per volume
      real      (4), intent(inout) :: amass (notot ,noseg)    !< masses per substance per volume
      real      (4), intent(inout) :: deriv (notot ,noseg)    !< derivatives per substance per volume
      real      (4), intent(inout) :: volume(noseg )          !< volumes of the segments
      integer   (4), intent(in   ) :: idt                     !< integration time step size
      integer   (4), intent(in   ) :: lun                     !< unit number of the monitoring file
      integer   (4), intent(in   ) :: ivflag                  !< if 1 computational volumes
      real      (4), intent(in   ) :: surfac(noseg)           !< horizontal surface

      ! local declarations

      integer                      :: iseg                    !  segment loop counter
      integer                      :: i                       !  substance loop counter
      real                         :: v1                      !  segment volume
      real                         :: a                       !  segment mass
      integer, save                :: ivmess = 0              !  count messages on small volumes

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqp0", ithandl )

      ! loop accross the number of computational elements

      do iseg=1,noseg
         ! compute volumes if necessary

         if ( ivflag .eq. 1 ) volume(iseg) = amass(1,iseg) + idt*deriv(1,iseg)
         v1 = volume(iseg)
         if ( abs(v1).lt.1.0e-25 ) then
            if ( ivmess .lt. 25 ) then
               ivmess = ivmess + 1
               write ( lun, 1000 ) iseg  , v1
            elseif ( ivmess .eq. 25 ) then
               ivmess = ivmess + 1
               write ( lun, 1001 )
            endif
            volume (iseg) = 1.0
            v1            = 1.0
         endif

         !  active substances first

         do i=1,nosys
            a           = amass(i,iseg) + idt*deriv(i,iseg)
            amass(i,iseg) = a
            conc (i,iseg) = a / v1
            deriv(i,iseg) = 0.0
         enddo

         ! then the inactive substances

         do i=nosys+1,notot
            amass(i,iseg) = amass(i,iseg) + idt*deriv(i,iseg)
            conc (i,iseg) = amass(i,iseg) / max(tiny(1.0),surfac(iseg))
            deriv(i,iseg) = 0.0
         enddo

      enddo

      ! output formats

 1000 format ( 'volume of segment:', i7, ' is:',
     &          e15.6, ' 1.0 assumed.' )
 1001 format ('25 or more zero volumes , further messages surpressed')

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_dlwqp0
