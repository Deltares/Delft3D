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

module dhcommand

    implicit none

    character(len=300), dimension(:), allocatable, save, private :: cmdargs

contains

subroutine dhstore_command( args )
!
!     DELTARES
!
!     CREATED             : april-29-2010 by A. Markus
!
!     FUNCTION            : Stores a set of (constructed) command-line
!                           arguments
!                           This routine is used in the library version
!                           of DELWAQ to simulate the standalone
!                           features
!
!     LOGICAL UNITNUMBERS : None
!
!     SUBROUTINES CALLED  : None
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ARG     CHAR*(*)      :     INPUT   Individual arguments
!
    character(len=*), dimension(:) :: args

    integer                        :: i
    integer                        :: count
    integer                        :: ierr
    integer                        :: lun
    logical                        :: opened
    logical                        :: exists
    character(len=256)             :: line

    count = 0
    inquire( file = 'delwaq.options', exist = exists )
    if ( exists ) then
        open( newunit=lun, file = 'delwaq.options' )
        do
            read( lun, '(a)', iostat = ierr ) line
            if ( ierr /= 0 ) then
                exit
            endif
            if ( line /= ' ' ) then
                count = count + 1
            endif
        enddo
        close( lun )
    endif

    if ( allocated(cmdargs) ) then
        deallocate( cmdargs )
    endif

    allocate( cmdargs(size(args)+count) )

    cmdargs(1:size(args)) = args

    if ( exists ) then
        count = size(args)
        open( newunit=lun, file = 'delwaq.options' )
        do
            read( lun, '(a)', iostat = ierr ) line
            if ( ierr /= 0 ) then
                exit
            endif
            if ( line /= ' ' ) then
                count = count + 1
                cmdargs(count) = line
            endif
        enddo
        close( lun )
    endif

end subroutine dhstore_command

function dhstored_arg( idx )
!
!     DELTARES
!
!     CREATED             : april-29-2010 by A. Markus
!
!     FUNCTION            : Returns a particular argument or an
!                           empty string if the index is out of range
!
!     LOGICAL UNITNUMBERS : None
!
!     SUBROUTINES CALLED  : None
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     IDX     INTEGER       :     INPUT   Index of the requested argument
!
    integer, intent(in) :: idx
    character(len=len(cmdargs(1))) :: dhstored_arg

    if ( idx < 0 .or. idx >= size(cmdargs) ) then
        dhstored_arg = ' '
    else
        dhstored_arg = cmdargs(idx+1)
    endif

end function dhstored_arg

integer function dhstored_number_args( )
!
!     DELTARES
!
!     CREATED             : april-29-2010 by A. Markus
!
!     FUNCTION            : Returns the number of stored arguments
!
!     LOGICAL UNITNUMBERS : None
!
!     SUBROUTINES CALLED  : None
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     IDX     INTEGER       :     INPUT   Index of the requested argument
!
    if ( allocated(cmdargs) ) then
        dhstored_number_args = size(cmdargs)
    else
        dhstored_number_args = 0
    endif

end function dhstored_number_args

end module dhcommand
