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

!     FUNCTION            : MAIN module for DELWAQ2 , dimensioning
!                           of the work array's.
!
!     SUBROUTINES CALLED  : DELWQ2, performs the simulation
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ACTION  INTEGER  1          INPUT   Action to be taken
!     ARGC    INTEGER  1          INPUT   Number of simulated command-line arguments
!     ARGV    INTEGER  1          INPUT   Simulated command-line arguments
!
!     ITOTA   INTEGER  1          INPUT   length of real workarray
!     ITOTI   INTEGER  1          INPUT   length of integer workarray
!     ITOTC   INTEGER  1          INPUT   length of character workarray
!
!
!      PARAMETER (ITOTA=0       ,ITOTI=0       ,ITOTC=0       )



subroutine dlwqmain(action, argc, argv, dlwqd)

      !DEC$ ATTRIBUTES DLLEXPORT::dlwqmain

      use delwaq2
      use delwaq2_data
      use dhcommand
      use m_actions
      use m_sysn
      use m_sysi

      implicit none

      integer, intent(in)                           :: action
      integer, intent(in)                           :: argc
      character(len=*), dimension(argc), intent(in) :: argv
      type(delwaq_data)                             :: dlwqd

      character(len=20)                             :: rundat

      logical                                       :: init        ! do not save!
      integer                                       :: lunrep

      integer, save                                 :: itota
      integer, save                                 :: itoti
      integer, save                                 :: itotc



      call delwaq2_main_init(dlwqd, itota, itoti, itotc, init, action, argc, argv)
      call delwq2(dlwqd%rbuf, dlwqd%ibuf, dlwqd%chbuf, itota, itoti, itotc, init, action, dlwqd)
      call delwaq2_main_finalise(action, lunrep, rundat)

end subroutine dlwqmain
