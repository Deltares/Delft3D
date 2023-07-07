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


!>\file
!>                    delwaq1_init: initializes timer and values
!     SUBROUTINES CALLED :
!                         *UNISET, reads input filename

subroutine delwaq1_init(argc, argv)
    use m_delwaq1_data
    use m_dhgarg

    implicit none

    integer, intent(in)                           :: argc
    character(len=*), dimension(argc), intent(in) :: argv

    !     Special system init

    call timini ( )                          ! initializes timer

    call dhstore_command( argv )

    narg = dhstored_number_args()            ! but timer is switched 'off' by default
    if ( narg .eq. 0 ) narg = iargc() + 1
    do ierr = 1, narg
        call dhgarg ( ierr, arg )
        if ( arg .eq. "timer" .or. arg .eq. "TIMER" ) then
          timon = .true.                     ! optionally switch it 'on'
          exit
        endif
    enddo
    if (timon) call timstrt( "delwaq1", ithndl )

    !        initialise values

    ierr   = 0
    iwar   = 0
    lunrep = lun(29)
    nolun  = nlun
    filtype = 0
    noitem = noitm
    noutp  = nooutp
    noinfo = 0
    nharms = 0
    niharm = 0
    nlines = 0
    npoins = 0
    newrsp = 0
    newisp = 0
    ivflag = 0
    itflag = 0
    ncbufm = 0
    novar  = 0
    noarr  = iasize + ijsize + icsize
    nufil  = 0
    do i=1, noitem
      nrftot(i) = 0
      nrharm(i) = 0
    end do
    StatProcesDef%maxsize = 0
    StatProcesDef%cursize = 0
    AllItems%maxsize = 0
    AllItems%cursize = 0
    GridPs%cursize=0
    GridPs%maxsize=0

    call uniset ( lun    , lchar , nolun , runid )


end subroutine delwaq1_init