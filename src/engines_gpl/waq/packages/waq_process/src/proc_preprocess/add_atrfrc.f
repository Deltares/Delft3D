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

      subroutine add_atrfrc( lunrep, procesdef, sfracs)

      ! add attributes to processes from file

      use m_gkwini
      use m_getcom
      use m_dhucas
      use processet
      use timers       !   performance timers

      implicit none

      ! decalaration of arguments

      integer                   :: lunrep          ! report file
      type(procespropcoll)      :: procesdef       ! the process definition
      type(sfracsprop)          :: sfracs          ! substance fraction properties

      ! local declaration

      type(procesprop), pointer :: proc              ! single process
      integer                   :: nproc             ! number of processes
      integer                   :: iproc             ! loop counter processes
      logical                   :: lfound            ! command line argument found
      integer                   :: idummy            ! dummy
      real                      :: rdummy            ! dummy
      character(len=256)        :: patrfil           ! process attributes file
      integer                   :: lun_patr          ! unit number
      character(len=256)        :: type              ! sfrac_type from file
      integer                   :: ierr              ! ierr
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "add_atrfrc", ithndl )

      call getcom ( '-sfrac', 3    , lfound, idummy, rdummy, patrfil, ierr)
      if ( lfound ) then
         open(newunit=lun_patr,file=patrfil)

         ! loop over the processes

         nproc = procesdef%cursize
         do iproc = 1, nproc

            proc => procesdef%procesprops(iproc)
            call gkwini(lun_patr,proc%name,'sfrac_type',type)
            call dhucas(type,type,len(type))
            if ( type .eq. 'SPLITFLUX' ) then
               proc%sfrac_type = SFRAC_SPLITFLUX
            elseif ( type .eq. 'DUPLICATE' ) then
               proc%sfrac_type = SFRAC_DUPLICATE
            elseif ( type .eq. 'EXPAND' ) then
               proc%sfrac_type = SFRAC_EXPAND
            endif

         enddo

         close(lun_patr)
      endif

      if (timon) call timstop( ithndl )
      return
      end
