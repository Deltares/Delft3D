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
      module m_outmap

      implicit none

      contains


      subroutine outmap ( iomap  , namfim , itime  , moname , noseg  ,
     &                    notot1 , conc1  , synam1 , notot2 , conc2  ,
     &                    synam2 , iknmrk , init   )

!     Deltares Software Centre

!     Function            : Writes map output

!     Created             : May       1993 by Jan van Beek
!     Modified            : October   2010 by Leo Postma
!                                          addition of feature array for drying and flooding
!                                          FORTRAN-90 look and feel

!     Files               : iomap = unit number of binary map output file

!     Routines called     : none

      use timers
      implicit none

!     Parameters          :

!     kind           function         name                    description

      integer   (4), intent(in   ) :: iomap                ! unit number output file
      character*(*), intent(in   ) :: namfim               ! name output file
      integer   (4), intent(in   ) :: itime                ! present time in clock units
      character(40), intent(in   ) :: moname(4)            ! model identification
      integer   (4), intent(in   ) :: noseg                ! number of computational volumes
      integer   (4), intent(in   ) :: notot1               ! number of variables in conc1
      real      (4), intent(in   ) :: conc1 (notot1,noseg) ! values
      character(20), intent(in   ) :: synam1(notot1)       ! names of variables in conc1
      integer   (4), intent(in   ) :: notot2               ! number of variables in conc2
      real      (4), intent(in   ) :: conc2 (notot2,noseg) ! values
      character(20), intent(in   ) :: synam2(notot2)       ! names of variables in conc2
      integer   (4), intent(in   ) :: iknmrk(noseg)        ! Feature array. Bit zero set means active.
      integer   (4), intent(inout) :: init                 ! Initialisation flag

      integer(4) iseg                   ! loop counter for segments
      integer(4) k                      ! loop counter for substances
      integer    ierr
      real   (4) amiss   /-999.0/       ! missing value indicator
      integer(4) ithandl /0/

      real(4), dimension(:,:), allocatable :: outconc

      if ( timon ) call timstrt ( "outmap", ithandl )

!     Initialize file

      if ( init .eq. 1 ) then
         init = 0
         write (iomap)  moname
         write (iomap)  notot1+notot2, noseg
         write (iomap)  synam1, synam2
      endif

!     Perform output:
!     Inactive segments should get missing values. For this, make
!     a copy of the concentrations.
!
!     Note: this may fail, if there is not enough memory, so provide
!     a slower alternative.

      allocate( outconc(notot1, noseg), source = conc1, stat = ierr )

      if ( ierr == 0 ) then
         do iseg = 1, noseg
            if ( .not. btest(iknmrk(iseg),0) ) then
               outconc(:,iseg) = amiss
            endif
         enddo

         write (iomap) itime, (outconc(:,iseg), conc2(:,iseg), iseg = 1,noseg)

         deallocate( outconc )
      else
         ! Slow alternative
         write (iomap) itime
         do iseg = 1, noseg
            if ( btest(iknmrk(iseg),0) ) then
               write (iomap) conc1(:,iseg), conc2(:,iseg)
            else
               write (iomap) ( amiss, k = 1, notot1), conc2(:,iseg)
            endif
         enddo
      endif

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_outmap
