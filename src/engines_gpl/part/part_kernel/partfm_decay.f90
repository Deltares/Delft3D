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

module m_partfm_decay
!
!  module declarations
!
contains
      subroutine partfm_decay ()
!
!    CALCULATES first order decay for each particle
!                 (per time step)
!

!     system administration : frank kleissen

!  data definition module(s)
!
      use partmem, only : iptime, subst, nosubs, mpart, wpart, dfact, decays, nopart
      use m_part_times, only : dts
      use fileinfo
      use timers           ! performance timer
!
      implicit none             ! force explicit typing

      double precision   :: ddfac                   ! control variable for smooth loading
!**   data assigments

      integer(4)         :: ithndl              ! handle to time this subroutine
      integer            :: ipart, isubnr
      double precision   :: tp, itdelt, deltt
      data       ithndl / 0 /
      if ( timon ) call timstrt( "partfm_decay", ithndl )

!**   initialisations
      do isubnr = 1, nosubs
         dfact(isubnr) = exp( -decays(isubnr) * dts / 86400.0 )
         write(lunit(2),'(a,a,14x,a,es15.7,a,es15.7,a)')  &
            '      Decay ',subst(isubnr),' : ',dfact(isubnr), &
            ' [coeff = ',decays(isubnr),']'
      enddo

      do ipart = 1, Nopart
          if(mpart(ipart).gt.0) then
             tp = real(iptime(ipart))  ! TODO: his known?
             itdelt = dts
             ddfac  = 2.0
             if ( tp .lt. 0.0 ) then           !   adaptations because of smooth loading
                tp     = 0.0
                itdelt = dts + iptime(ipart)
                ddfac  = itdelt/dts
             endif

             deltt = itdelt
    !**      calculate decay wpart for all substances, adapt for smooth loading with ddfac

             do isubnr = 1, nosubs
                if ( ddfac .gt.  1.5 ) then
                   wpart(isubnr, ipart) = wpart(isubnr, ipart) * dfact(isubnr)
                else
                   wpart(isubnr, ipart) = wpart(isubnr, ipart) *  &
                                                         dfact(isubnr)**ddfac
                endif
             enddo

    !**     end of particle loop

             iptime(ipart) = iptime(ipart) + dts
         endif
      end do

!**   end of subroutine

      if ( timon ) call timstop ( ithndl )
      return
      end subroutine partfm_decay

end module
