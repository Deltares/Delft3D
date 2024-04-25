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
      module m_srstop

      implicit none

      contains

      
      
      subroutine srstop ( iexit )

!     Deltares Software Centre

!>\file
!>              stops execution if possible with return value
!>
!>              system dependent routine, Intel Fortran

!     Created            : June  '91  BY  Jan van Beek

!     Logical units      : 13 file with return code

!     Parameters

!     kind           function         name                 description


      integer  ( 4), intent(in   ) :: iexit             !< return value
      integer  ( 4)                :: lunfil

      if ( iexit /= 0 ) then
         write (*,*) 'Stopped with error code :',iexit
      else
         write (*,*) 'Normal end'
      endif

      open  ( newunit=lunfil , file = 'delwaq.rtn' )
      write ( lunfil , * ) iexit
      close ( lunfil )



      select case ( iexit )
         case ( :0 )
            stop 0
         case (  1 )
            stop 1
         case default
            stop 255
      end select

      end
      end module m_srstop
