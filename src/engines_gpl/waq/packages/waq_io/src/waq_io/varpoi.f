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

      subroutine varpoi ( notot  , nopa   , nosfun , syname , nocons ,
     &                    nofun  , coname , paname , funame , sfname ,
     &                    varnam , ivarip , lurep  )

!     Deltares Software Centre

!>\file
!>                sets pointers for output variables

!     Created:    December  1992 by Jan van Beek

!     Logical unitnumbers : lurep   - report file

      use m_zoek
      use timers       !   performance timers

      implicit none

!     kind           function         name                Descriptipon

      integer  ( 4), intent(in   ) :: notot             !< Total number of substances
      integer  ( 4), intent(in   ) :: nopa              !< Number of parameters
      integer  ( 4), intent(in   ) :: nosfun            !< Number of segment functions
      character(20), intent(in   ) :: syname(notot)     !< Names of systems
      integer  ( 4), intent(in   ) :: nocons            !< Number of constants used
      integer  ( 4), intent(in   ) :: nofun             !< Number of functions ( user )
      character(20), intent(in   ) :: coname(nocons)    !< Constant names
      character(20), intent(in   ) :: paname(nopa  )    !< Parameter names
      character(20), intent(in   ) :: funame(nofun )    !< Function names
      character(20), intent(in   ) :: sfname(nosfun)    !< Segment function names
      character(20), intent(in   ) :: varnam            !< Name of variable to be identified
      integer  ( 4), intent(  out) :: ivarip            !< Pointer in the SSA
      integer  ( 4), intent(in   ) :: lurep             !< Unit nr. report file

!     Local

      integer, parameter :: nopred = 6
      integer               indx           !  index in array of names
      character(20) predef(3)
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "varpoi", ithndl )

      predef(1) = 'volume'
      predef(2) = 'itime'
      predef(3) = 'idt'

!     determine how VAL is modelled

      call zoek ( varnam , 3    , predef , 20   , indx  )
      if ( indx .eq. 1 ) then
         write(lurep,*) '       ',varnam,'; Using DELWAQ VOLUME'
         ivarip = 1
         goto 800
      endif
      if ( indx .eq. 2 ) then
         write(lurep,*) '       ',varnam,'; Using DELWAQ ITIME'
         ivarip = 2
         goto 800
      endif
      if ( indx .eq. 3 ) then
         write(lurep,*) '       ',varnam,'; Using DELWAQ IDT'
         ivarip = 3
         goto 800
      endif

!     as model variable ?

      call zoek ( varnam , notot , syname , 20   , indx   )
      if ( indx .gt. 0 ) then
         write(lurep,*) '       ',varnam,'; Using substance nr',indx
         ivarip = nopred + nocons + nopa + nofun + nosfun + indx
         goto 800
      endif

!     as segment function ?

      call zoek ( varnam , nosfun, sfname , 20   , indx   )
      if ( indx .gt. 0 ) then
         write(lurep,*) '       ',varnam,'; Using segment function nr',indx
         ivarip = nopred + nocons + nopa + nofun + indx
         goto 800
      endif

!     as function ?

      call zoek ( varnam , nofun , funame , 20   , indx   )
      if ( indx .gt. 0 ) then
         write(lurep,*) '       ',varnam,'; Using function nr',indx
         ivarip = nopred + nocons + nopa + indx
         goto 800
      endif

!     as parameter ?

      call zoek ( varnam , nopa  , paname , 20   , indx   )
      if ( indx .gt. 0 ) then
         write(lurep,*) '       ',varnam,'; Using parameter nr',indx
         ivarip = nopred + nocons + indx
         goto 800
      endif

!     as constant ?

      call zoek ( varnam , nocons, coname , 20   , indx   )
      if ( indx .gt. 0 ) then
         write(lurep,*) '       ',varnam,'; Using constant nr',indx
         ivarip = nopred + indx
         goto 800
      endif

!     not found

      ivarip = -1

  800 continue

      if (timon) call timstop( ithndl )
      return
      end
