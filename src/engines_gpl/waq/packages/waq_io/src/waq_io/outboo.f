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

      subroutine outboo ( noutp  , nrvar  , igrdou , isrtou , noseg  ,
     &                    nodump , nx     , ny     , nrvart , nbufmx ,
     &                    ndmpar , notot  , ncbufm , noraai )
!>\file
!>                Sets the boot variables for OUTPUT system

!     Deltares Software Centre

!     CREATED: May -1993 by Jan van Beek

!     LOGICAL UNITNUMBERS : -

!     SUBROUTINES CALLED  : -

      use timers       !   performance timers
      use output

      implicit none

!     Parameters         :

!     kind           function         name            Descriptipon

      integer  ( 4), intent(in   ) :: noutp         !< Number of output files
      integer  ( 4), intent(in   ) :: nrvar (noutp) !< Number variables per output file
      integer  ( 4), intent(in   ) :: igrdou(noutp) !< Output grid indication
      integer  ( 4), intent(in   ) :: isrtou(noutp) !< Sort output indication
      integer  ( 4), intent(in   ) :: noseg         !< Number of computational cells
      integer  ( 4), intent(in   ) :: nodump        !< Number of monitoring points
      integer  ( 4), intent(in   ) :: nx            !< Length of dump grid
      integer  ( 4), intent(in   ) :: ny            !< Width of dump grid
      integer  ( 4), intent(  out) :: nrvart        !< Total number of output variables
      integer  ( 4), intent(  out) :: nbufmx        !< Length of output buffer needed
      integer  ( 4), intent(in   ) :: ndmpar        !< number of dump areas
      integer  ( 4), intent(in   ) :: notot         !< Number of substances
      integer  ( 4), intent(  out) :: ncbufm        !< Length of character buffer needed
      integer  ( 4), intent(in   ) :: noraai        !< Number of transects

!     Local

      integer, parameter :: igseg= 1 , igmon= 2 , iggrd= 3 , igsub= 4
      integer            :: nocel       !  size of the NEFIS cell
      integer            :: nbufou      !  help variable for length output buffer
      integer            :: ncbufo      !  help variable for length character buffer
      integer            :: iout        !  loop variable
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "outboo", ithndl )

!     Loop over the output files

      nrvart = 0
      nbufmx = 0
      do 100 iout = 1 , noutp
         nrvart = nrvart + nrvar(iout)

!        Grid

         select case ( igrdou(iout) )
            case ( igseg )
               nocel = noseg
            case ( igmon )
               nocel = nodump
            case ( iggrd )
               nocel = nx*ny
            case ( igsub )
               nocel = ndmpar
         end select

!        Calculate outputbuffer size for this file, for some (NEFIS,SUB)
!        also a character buffer size

         ncbufo = 0
         select case ( isrtou(iout) )

            case ( ihnf, imnf )
            !  NEFIS file, extra array with length NOCEL needed
            !  substance names and output names in char buffer.
               nbufou = nocel * ( nrvar(iout) + 1 )
               ncbufo = notot + nrvar(iout)

            case ( ihn2, imn2 )
            !  NEFIS file, extra array with length NOCEL needed
               nbufou = nocel * ( nrvar(iout) + 1 )

            case ( imo3 )
            !  On subarea's substances also in buffer, only the
            !  first half of the nrvar are real output vars.
            !  substance names and output names in char buffer.
               nbufou = nocel * ( notot + nrvar(iout)/2 )
               ncbufo = notot + nrvar(iout)/2

            case ( ihi3 )
            !  On subarea's substances also in buffer, only the
            !  first half of the nrvar are real output vars.
            !  substance names and output names in char buffer.
            !  also output for raaien
               nbufou = (nocel+noraai) * ( notot + nrvar(iout)/2 )
               ncbufo = notot + nrvar(iout)/2

            case ( ihn3 )
            !  NEFIS file, extra array with length NOCEL needed
            !  On subarea's substances also in buffer, only the
            !  first half of the nrvar are real output vars.
            !  substance names and output names in char buffer.
            !  also output for raaien
               nbufou = (nocel+noraai) * ( notot + nrvar(iout)/2 + 1 )
               ncbufo = notot + nrvar(iout)/2

            case ( imo4, ihi4 )
            !  On subarea's only the first half of the nrvar are
            !  real output vars.
               nbufou = nocel * ( nrvar(iout)/2 )

            case ( ihn4 )
            !  NEFIS file, extra array with length NOCEL needed
            !  On subarea's only the first half of the nrvar are
            !  real output vars.
               nbufou = nocel * ( nrvar(iout)/2 + 1 )

            case default
            !  Rest, normal
               nbufou = nocel * nrvar(iout)
         end select

!        Buffer is as big as the largest needed

         nbufmx = max ( nbufmx, nbufou )
         ncbufm = max ( ncbufm, ncbufo )

  100 continue

      if (timon) call timstop( ithndl )
      return
      end
