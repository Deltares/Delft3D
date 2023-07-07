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

      subroutine actrep( noalg   , noprot   , namprot, nampact, nopralg,
     +                   nampralg, constants)

!     Deltares Software Centre

!>/File
!>      replace active proto processes with actual processes

      use m_srstop
      use m_monsys
      use m_dhucas
      use m_dhslen
      use timers         !< performance timers
      use dlwq_data      !< data definitions
      use processet      !< use processet definitions
      implicit none

      ! decalaration of arguments

      integer  ( 4)    , intent(in   ) :: noalg             !< number of algae types
      integer  ( 4)    , intent(in   ) :: noprot            !< number of proto processes ono to one
      integer  ( 4)    , intent(in   ) :: nopralg           !< number of proto processes per algae type
      character(len=*) , intent(in   ) :: namprot(noprot)   !< name proto processes
      character(len=*) , intent(in   ) :: nampact(noprot)   !< name actual processes
      character(len=*) , intent(in   ) :: nampralg(noprot)  !< name proto processes per algae type
      type(t_dlwq_item), intent(inout) :: constants         !< delwaq constants list

      ! local decalaration

      character*10                     :: name1
      character*20                     :: name20
      character*80                     :: line
      integer                          :: nocons
      integer                          :: nocon2
      integer                          :: ico
      integer                          :: ipro
      integer                          :: ilen
      integer                          :: ialg
      integer                          :: ierr2
      integer(4)                       :: ithndl = 0

      if (timon) call timstrt( "actrep", ithndl )

      ! replace active proto processes with actual processes

      nocons = constants%no_item
      nocon2 = constants%no_item
      do ico = 1 , nocons
         call dhucas(constants%name(ico),name20,20)
         if ( name20(1:6) .eq. 'ACTIVE' ) then

            ! the one to one processes

            do ipro = 1 , noprot
               if ( namprot(ipro) .eq. name20(8:17) ) then
                  constants%name(ico)(8:) = nampact(ipro)
               endif
            enddo

            ! the processes that need to be expanded for every algal type

            do ipro = 1 , nopralg
               if ( nampralg(ipro) .eq. name20(8:17) ) then
                  name1 = nampralg(ipro)
                  call dhslen(name1,ilen)
                  do ialg = 1 , noalg
                     write(name1(ilen+1:),'(i2.2)') ialg
                     if ( ialg .eq. 1 ) then
                        constants%name(ico)(8:) = name1
                     else
                        nocon2 = nocon2 + 1
                        ierr2 = dlwq_resize(constants,nocon2)
                        if ( ierr2 .gt. 0 ) then
                           write(line,'(a,i10)') ' ERROR: actrep resize error constants size:',nocon2
                           call monsys(line,1)
                           call srstop(1)
                        endif
                        constants%no_item          = nocon2
                        constants%name(nocon2)     = constants%name(ico)
                        constants%name(nocon2)(8:) = name1
                     endif
                  enddo
               endif
            enddo
         endif
      enddo

      if (timon) call timstop( ithndl )
      return
      end
