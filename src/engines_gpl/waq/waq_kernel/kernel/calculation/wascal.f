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
      module m_wascal

      implicit none

      contains


      subroutine wascal ( nowst , notot , nosys , noseg , syname,
     +                    conc  , itime , nowtyp, wastid, wstnam,
     +                    wsttyp, iwaste, iwtype, waste )
!
!     Deltares
!
!     CREATED:            : Jan van Beek
!
!     FUNCTION            : calls the user defined wasteload routines
!

      ! global declarations

      use m_srstop
      use m_monsys
      use delwaq_loads
      use delwaq_user_wasteloads
      use timers
      implicit none

      ! arguments declarations

      integer                             :: nowst
      integer                             :: notot
      integer                             :: nosys
      integer                             :: noseg
      character(len=20)                   :: syname(notot)
      real                                :: conc(notot,noseg)
      integer                             :: itime
      integer                             :: nowtyp
      character(len=20)                   :: wastid(nowst)
      character(len=40)                   :: wstnam(nowst)
      character(len=20)                   :: wsttyp(nowtyp)
      integer                             :: iwaste(nowst)
      integer                             :: iwtype(nowst)
      real                                :: waste(0:notot,nowst)

      ! local declarations

      integer, save       :: ifirst = 1
      integer             :: ierror
      integer             :: iwst
      integer             :: isys
      integer             :: lunrep
      character(len=256)  :: load_routine
      integer             :: ierr_alloc
      integer(4), save    :: ithandl = 0
      if ( timon ) call timstrt ( "wascal", ithandl )

      ! initialise dll

      call getmlu(lunrep)

      ! update the actual loads from the delwaq arrays to the wasteload structure

      if ( ifirst .eq. 1 ) then
         allocate(wasteloads(nowst), stat = ierr_alloc)
         if ( ierr_alloc .ne. 0 ) then
            write(lunrep,*) 'ERROR : allocating wasteloads structure'
            write(*,*) 'ERROR : allocating wasteloads structure'
            call srstop(1)
         endif
         do iwst = 1 , nowst
            allocate(wasteloads(iwst)%loads(notot+1))
            wasteloads(iwst)%id%id     = wastid(iwst)
            wasteloads(iwst)%id%name   = wstnam(iwst)
            wasteloads(iwst)%id%type   = wsttyp(iwtype(iwst))
            wasteloads(iwst)%loc%segnr = iwaste(iwst)
         enddo
      endif
      do iwst = 1 , nowst
         wasteloads(iwst)%flow = waste(0,iwst)
         do isys = 1 , notot
            wasteloads(iwst)%loads(isys) = waste(isys,iwst)
         enddo
      enddo

      ! call routine

      call delwaq_user_wasteload ( nowst , wasteloads, notot , nosys , noseg ,
     +                             itime , conc      , syname)

      ! updated wasteloads to old delwaq arrays

      do iwst = 1 , nowst
         waste(0,iwst) = wasteloads(iwst)%flow
         do isys = 1 , notot
            waste(isys,iwst) = wasteloads(iwst)%loads(isys)
         enddo
      enddo

      ifirst = 0

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_wascal
