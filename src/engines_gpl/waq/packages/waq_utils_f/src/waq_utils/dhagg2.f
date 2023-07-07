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
      module m_dhagg2
      use m_srstop
      use m_monsys


      implicit none

      contains


      subroutine dhagg2 ( noseg1 , noseg2 , nototi , nototw , nototh ,
     &                    nototo , isysi  , isysw  , isysh  , isyso  ,
     &                    nsys   , ipgrid , iagtyp , arrinp , weight ,
     &                    arrhlp , arrout )

!     Deltares Software Centre

!>\File
!>                 Aggregates value to coarser grid

!     Created             : June 1998 by Jan van Beek

!     Subroutines called  : GETMLU, Get unit number report file
!                           SRSTOP, Stops execution with error

      implicit none

!     Arguments           :

!     Kind         Function         Name                    Description

      integer( 4), intent(in   ) :: noseg1                !< Number of segments on finer grid
      integer( 4), intent(in   ) :: noseg2                !< Number of segments on coarser grid
      integer( 4), intent(in   ) :: nototi                !< First dimension on finer grid
      integer( 4), intent(in   ) :: nototw                !< First dimension of weight on finer grid
      integer( 4), intent(in   ) :: nototh                !< First dimension on coarser help grid
      integer( 4), intent(in   ) :: nototo                !< First dimension on coarser output array
      integer( 4), intent(in   ) :: isysi                 !<
      integer( 4), intent(in   ) :: isysw                 !<
      integer( 4), intent(in   ) :: isysh                 !< Entry in help array to be used
      integer( 4), intent(in   ) :: isyso                 !< Offset in output
      integer( 4), intent(in   ) :: nsys                  !< Number of items to aggregate
      integer( 4), intent(in   ) :: ipgrid(noseg1)        !< Grid pointers to coarser grid
      integer( 4), intent(in   ) :: iagtyp                !< 1 = accum; 2 = average; 3 = weighted avg
      real   ( 4), intent(in   ) :: arrinp(nototi,noseg1) !< Array to be aggregated
      real   ( 4), intent(in   ) :: weight(nototw,noseg1) !< Weigth in averaging
      real   ( 4)                :: arrhlp(nototh,noseg2) !< Local help array
      real   ( 4), intent(  out) :: arrout(nototo,noseg2) !< Aggregated array

!     Local declarations

      integer( 4)  iseg1   !  Segment index finer grid
      integer( 4)  iseg2   !  Segment index coarser grid
      integer( 4)  lurep   !  Unit number report file
      integer( 4)  isys    !  Loop counter substances
      real   ( 4)  w       !  Help variable for weight
      real         abs

      select case ( iagtyp )

!        accumulate

         case ( 1 )
            arrout( isyso:isyso+nsys-1 , : ) = 0.0
            do iseg1 = 1 , noseg1
               iseg2 = ipgrid(iseg1)
               if ( iseg2 .le. 0 ) cycle
               do isys = 0 , nsys - 1
                  arrout(isyso+isys,iseg2) = arrout(isyso+isys,iseg2) +
     &                                       arrinp(isysi+isys,iseg1)
               enddo
            enddo

!        average

         case ( 2 )
            arrout( isyso:isyso+nsys-1 , : ) = 0.0
            arrhlp( isysh , : ) = 0.0
            do iseg1 = 1 , noseg1
               iseg2 = ipgrid(iseg1)
               if ( iseg2 .le. 0 ) cycle
               do isys = 0 , nsys - 1
                  arrout(isyso+isys,iseg2) = arrout(isyso+isys,iseg2) +
     &                                       arrinp(isysi+isys,iseg1)
               enddo
               arrhlp(isysh,iseg2) = arrhlp(isysh,iseg2) + 1.0
            enddo
            do iseg2 = 1 , noseg2
               w = arrhlp(isysh,iseg2)
               if ( abs(w) .gt. 1.e-20 ) then
                  do isys = 0 , nsys - 1
                     arrout(isyso+isys,iseg2) = arrout(isyso+isys,iseg2) / w
                  enddo
               else
                  do isys = 0 , nsys - 1
                     arrout(isyso+isys,iseg2) = 0.0
                  enddo
               endif
            enddo

!        weighted average

         case ( 3 )
            arrout( isyso:isyso+nsys-1 , : ) = 0.0
            arrhlp( isysh , : ) = 0.0
            do iseg1 = 1 , noseg1
               iseg2 = ipgrid(iseg1)
               if ( iseg2 .le. 0 ) cycle
               w = weight(isysw,iseg1)
               do isys = 0 , nsys - 1
                  arrout(isyso+isys,iseg2) = arrout(isyso+isys,iseg2) +
     &                                       arrinp(isysi+isys,iseg1)*w
               enddo
               arrhlp(isysh,iseg2) = arrhlp(isysh,iseg2) + w
            enddo
            do iseg2 = 1 , noseg2
               w = arrhlp(isysh,iseg2)
               if ( abs(w) .gt. 1.e-20 ) then
                  do isys = 0 , nsys - 1
                     arrout(isyso+isys,iseg2) = arrout(isyso+isys,iseg2) / w
                  enddo
               else
                  do isys = 0 , nsys - 1
                     arrout(isyso+isys,iseg2) = 0.0
                  enddo
               endif
            enddo

         case default
            call getmlu(lurep)
            write( lurep, 2000 ) iagtyp
            call srstop(1)

      end select

      return
 2000 format ( ' ERROR: undefind aggregation type in DHAGGR :',I8 )
      end
      end module m_dhagg2
