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

      module Output
!
!          module contains everything for specification of output variables
!          created 31 January 2003 by Leo Postma
!
!     contains the fiollowing derived types:
!          OutputPointer          ! a set of information with respect to one grid pointer
!          OutputPointerColl      ! a collection of these grid pointers
!
!     contains the following functions:
!
!     contains the following subroutine:
!
      integer, parameter :: NAME_SIZE      =  20                ! size of descriptive names
      integer, parameter :: STDN_SIZE      =  100               ! size of standard names
      integer, parameter :: UNIT_SIZE      =  40                ! size of units
      integer, parameter :: DESC_SIZE      =  60                ! size of long description

      integer, parameter :: IMON              =   1          ! type monitoring file
      integer, parameter :: IMO2              =   2          ! type of output file
      integer, parameter :: IDMP              =   3          ! type of output file
      integer, parameter :: IDM2              =   4          ! type of output file
      integer, parameter :: IHIS              =   5          ! type of output file
      integer, parameter :: IHI2              =   6          ! type of output file
      integer, parameter :: IMAP              =   7          ! type of output file
      integer, parameter :: IMA2              =   8          ! type of output file
      integer, parameter :: IBAL              =   9          ! type of output file
      integer, parameter :: IHNF              =  10          ! type of output file
      integer, parameter :: IHN2              =  11          ! type of output file
      integer, parameter :: IMNF              =  12          ! type of output file
      integer, parameter :: IMN2              =  13          ! type of output file
      integer, parameter :: IMO3              =  14          ! type of output file
      integer, parameter :: IMO4              =  15          ! type of output file
      integer, parameter :: IHI3              =  16          ! type of output file
      integer, parameter :: IHI4              =  17          ! type of output file
      integer, parameter :: IHN3              =  18          ! type of output file
      integer, parameter :: IHN4              =  19          ! type of output file
      integer, parameter :: IBA2              =  20          ! type of output file
      integer, parameter :: IBA3              =  21          ! type of output file
      integer, parameter :: IHNC              =  22          ! type of output file
      integer, parameter :: IHNC2             =  23          ! type of output file
      integer, parameter :: IMNC              =  24          ! type of output file
      integer, parameter :: IMNC2             =  25          ! type of output file
      integer, parameter :: IHNC3             =  26          ! type of output file
      integer, parameter :: IHNC4             =  27          ! type of output file
!
!          use NetCDF output and options
!
      logical               :: lncout
      integer, dimension(4) :: ncopt
!
!          this is the collection of the output pointers
!
      type OutputColl
         character(LEN=NAME_SIZE),pointer :: names(:)           ! names of variables
         character(LEN=STDN_SIZE),pointer :: stdnames(:)        ! standard names of variables
         character(LEN=UNIT_SIZE),pointer :: units(:)           ! units of variables
         character(LEN=DESC_SIZE),pointer :: descrs(:)          ! descriptions of variables
         integer, pointer                 :: pointers(:)        ! ponters in waq arrays
         integer                          :: cursize            ! filled up to this size
      end type OutputColl
!
      end module Output
