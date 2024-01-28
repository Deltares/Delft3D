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
module m_write_version

implicit none

contains


      subroutine write_version(lun)

      use delpar_version_module
      use timers
!
      implicit none    ! force explicit typing
!
!     function              : echoes a header to screen
!
!     parameters            : none.
!
      character(len= 80) :: version_string
!
!     local scalars
!
      integer(kind=4) ::   lun

      character*120 idstr
      integer (4)   i

      character*75  startup_screen_text(7)
      
      data   startup_screen_text  / &
        '+-----------------------------------------------------------------------+', &
        '|                        D e l f t 3 D - P A R T                        |', &
        '|                                                                       |', &
        '| D-Particle Tracking     Water quality simulation in 2D/3D models      |', &
        '|                                                                       |', &
        '| Version xx.xxxx  xx-xx-xxxx                                           |', &
        '+-----------------------------------------------------------------------+'/
      
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "write_version", ithndl )
!
!     Get version string from file version_number.h.svn
!     (see also version_number project)
!
      call getfullversionstring_delpar(version_string)
!
      if (lun==0) then
!        scherm uitvoer
         do i = 1 , size(startup_screen_text)
            if ( startup_screen_text(i)(3:15) .eq. 'Version xx.xx' ) then
               write(startup_screen_text(i)(3:72),'(a)') version_string(1:70)
            end if
            write( * , * ) startup_screen_text(i)
         enddo
      else
!        print uitvoer
         write(lun,'(//13x,a)')   'PART - Particle tracking'
         write(lun,'(   6x,a)')   ' Water quality simulation in 2D/3D models      '
         write(lun,'(    a//)')   trim(version_string)
      end if
!
!     end of routine
!
      if ( timon ) call timstop ( ithndl )
      return
      end subroutine

end module m_write_version
