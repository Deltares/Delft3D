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
      module m_dhagg2
      use m_waq_precision
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

      integer(kind=int_wp), intent(in   )  ::noseg1                !< Number of segments on finer grid
      integer(kind=int_wp), intent(in   )  ::noseg2                !< Number of segments on coarser grid
      integer(kind=int_wp), intent(in   )  ::nototi                !< First dimension on finer grid
      integer(kind=int_wp), intent(in   )  ::nototw                !< First dimension of weight on finer grid
      integer(kind=int_wp), intent(in   )  ::nototh                !< First dimension on coarser help grid
      integer(kind=int_wp), intent(in   )  ::nototo                !< First dimension on coarser output array
      integer(kind=int_wp), intent(in   )  ::isysi                 !<
      integer(kind=int_wp), intent(in   )  ::isysw                 !<
      integer(kind=int_wp), intent(in   )  ::isysh                 !< Entry in help array to be used
      integer(kind=int_wp), intent(in   )  ::isyso                 !< Offset in output
      integer(kind=int_wp), intent(in   )  ::nsys                  !< Number of items to aggregate
      integer(kind=int_wp), intent(in   )  ::ipgrid(noseg1)        !< Grid pointers to coarser grid
      integer(kind=int_wp), intent(in   )  ::iagtyp                !< 1 = accum; 2 = average; 3 = weighted avg
      real(kind=real_wp), intent(in   )         ::arrinp(nototi,noseg1) !< Array to be aggregated
      real(kind=real_wp), intent(in   )  :: weight(nototw,noseg1) !< Weigth in averaging
      real(kind=real_wp)                 ::  arrhlp(nototh,noseg2) !< Local help array
      real(kind=real_wp), intent(  out)  ::arrout(nototo,noseg2) !< Aggregated array

!     Local declarations

      integer(kind=int_wp) ::iseg1   !  Segment index finer grid
      integer(kind=int_wp) ::iseg2   !  Segment index coarser grid
      integer(kind=int_wp) ::lurep   !  Unit number report file
      integer(kind=int_wp) ::isys    !  Loop counter substances
      real(kind=real_wp) ::w       !  Help variable for weight
      real(kind=real_wp) ::abs

      select case ( iagtyp )

!        accumulate

         case ( 1 )
            arrout( isyso:isyso+nsys-1 , : ) = 0.0
            do iseg1 = 1 , noseg1
               iseg2 = ipgrid(iseg1)
               if ( iseg2 <= 0 ) cycle
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
               if ( iseg2 <= 0 ) cycle
               do isys = 0 , nsys - 1
                  arrout(isyso+isys,iseg2) = arrout(isyso+isys,iseg2) +
     &                                       arrinp(isysi+isys,iseg1)
               enddo
               arrhlp(isysh,iseg2) = arrhlp(isysh,iseg2) + 1.0
            enddo
            do iseg2 = 1 , noseg2
               w = arrhlp(isysh,iseg2)
               if ( abs(w) > 1.e-20 ) then
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
               if ( iseg2 <= 0 ) cycle
               w = weight(isysw,iseg1)
               do isys = 0 , nsys - 1
                  arrout(isyso+isys,iseg2) = arrout(isyso+isys,iseg2) +
     &                                       arrinp(isysi+isys,iseg1)*w
               enddo
               arrhlp(isysh,iseg2) = arrhlp(isysh,iseg2) + w
            enddo
            do iseg2 = 1 , noseg2
               w = arrhlp(isysh,iseg2)
               if ( abs(w) > 1.e-20 ) then
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
