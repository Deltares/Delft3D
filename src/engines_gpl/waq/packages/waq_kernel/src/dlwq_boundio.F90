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
module m_dlwq_boundio

implicit none

contains


subroutine dlwq_boundio( lunrep, notot , nosys , noseg , nobnd ,&
                         syname, bndid , ibpnt , conc  , bound ,&
                         runid )

    use m_srstop
    use m_getcom
    use m_get_filepath_and_pathlen
    use M_WQParComm
    use M_WQHBComm
    use timers
    use m_dhfext

  ! implicit none

    ! arguments
    integer                   :: lunrep             ! report file
    integer                   :: notot              ! number of substances
    integer                   :: nosys              ! number of active substances
    integer                   :: noseg              ! number of segments
    integer                   :: nobnd              ! number of boundaries
    character(len=20)         :: syname(nosys)      ! substance names
    character(len=20)         :: bndid(nobnd)       ! boundary id
    integer                   :: ibpnt(4,nobnd)     ! boundaruy administration
    real                      :: conc(notot,noseg)  ! concentrations
    real                      :: bound(nosys,nobnd) ! boundary concentrations
    character(len=255)        :: runid              ! runid

    ! constants

    ! locals
    type(TWQParComm), save            :: wqParComm     ! comm. info for curr. domain
    character(Len=WQParMaxLen),save   :: curDomName    ! current domain name
    character(Len=255)                :: ddconfig      ! configuration file for dd
    character(Len=255)                :: hbfile        ! file name for "hot" boundary input
    logical, save                     :: init = .TRUE.
    logical, save                     :: boundio_active
    logical, save                     :: hotbound_active
    character(len=255)                :: runpath       ! path in filename runid, to be removed
    integer                           :: pathlen       ! actual length of path
    character(len=255)                :: runext        ! extension in runid (.mon), to be removed
    integer                           :: extpos        ! position of extension
    integer                           :: extlen        ! length of extension
    type(TWQHBComm), save             :: wqHBComm      ! comm. info for hot boundary input
    integer                           :: idummy
    real                              :: rdummy
    integer                           :: ierr
    integer                           :: ibnd
    integer                           :: iseg

    integer(4) ithandl /0/
    if ( timon ) call timstrt ( "dlwq_boundio", ithandl )

    ! Init, lees configuratie, initialiseer IO

    if ( init ) then
        call get_filepath_and_pathlen(runid, runpath, pathlen)
        call dhfext(runid, runext, extpos, extlen)
        curDomName = runid(pathlen+1:extpos-1)
        call getcom ( '-d'  , 3 , boundio_active, idummy, rdummy, ddconfig, ierr)
        if ( boundio_active ) then
            write(lunrep,*)
            write(lunrep,*) 'found switch -d : domain decomposition for boundaries active'
            if ( ierr .ne. 0 ) then
                write(*,*) 'Error initialising domain decomposition for boundaries'
                write(lunrep,*) 'ERROR : reading configuration file for domain decomposition from command line'
                call srstop(1)
            endif
            write(lunrep,'(1x,2a)') 'configuration filename : ', ddconfig
            inquire(file = ddconfig, exist = boundio_active )
            if ( boundio_active ) then
                if ( .not. WQParInit(wqParComm, ddconfig, curDomName, nosys, nobnd, syname, bndid) ) then
                    write(*,*) 'Error initialising domain decomposition for boundaries'
                    write(lunrep,*) 'Error initialising domain decomposition for boundaries'
                    call srstop(1)
                endif
            else
                write(*,*) 'Error initialising domain decomposition for boundaries'
                write(lunrep,*) 'ERROR : configuration file for domain decomposition does not exist'
                call srstop(1)
            endif
        endif

    ! Extension to allow "hot" boundary update (relevant for SOBEK-RR)

        call getcom ( '-hb'  , 3 , hotbound_active, idummy, rdummy, hbfile, ierr)
        if ( hotbound_active ) then
            write(lunrep,*)
            write(lunrep,*) 'found switch -hb : hot boundary input from HIS file active'
            if ( ierr .ne. 0 ) then
                write(*,*) 'Error initialising hot boundary input '
                write(lunrep,*) 'ERROR : reading hot boundary input file from command line'
                call srstop(1)
            endif
            write(lunrep,'(1x,2a)') 'hot boundary input filename : ', hbfile
            inquire(file = hbfile, exist = hotbound_active )
            if ( hotbound_active ) then
                if ( .not. WQHBInit(wqHBComm, hbfile, nosys, nobnd, syname, bndid) ) then
                    write(*,*) 'Error initialising hot boundary input'
                    write(lunrep,*) 'Error initialising hot boundary input'
                    call srstop(1)
                endif
            else
                write(*,*) 'Error initialising hot boundary input'
                write(lunrep,*) 'ERROR : hot boundary file does not exist'
                call srstop(1)
            endif
        endif

        init = .false.
    endif

    if ( .not. boundio_active .and. .not.hotbound_active ) goto 9999 !return

boundio: &
    if ( boundio_active ) then

        ! set export boundaries for this domain equal to concentrations from associated segment

        do ibnd = 1 , nobnd
           iseg = ibpnt(3,ibnd)
           if ( iseg .gt. 0 ) then
              wqParComm % bndput(:,ibnd) = conc(1:nosys,iseg)
           else
              wqParComm % bndput(:,ibnd) = 0.0
           endif
        enddo

        ! set the boundaries from this domain in bndget, if not they would be overruled by the back copy

        wqParComm % bndget = bound

        ! put boundaries to other domains

        if ( .not. WQParPutValues(wqParComm) ) then
            write(*,*) 'Error putting boundIO'
            write(lunrep,*) 'Error putting boundIO'
            call srstop(1)
        endif

        ! get boundaries from other domains

        if ( .not. WQParGetValues(wqParComm) ) then
            write(*,*) 'Error getting boundIO'
            write(lunrep,*) 'Error getting boundIO'
            call srstop(1)
        endif

        ! copy boundaries from other domains to the boundaries of this domain

        bound = wqParComm % bndget

    endif boundio

hotbound: &
    if ( hotbound_active ) then

        ! copy present boundaries to local array

        wqHBComm % bndget = bound

        ! get boundaries from other domains

        if ( .not. WQHBGetValues(wqHBComm) ) then
            write(*,*) 'Error getting hot boundaries'
            write(lunrep,*) 'Error getting hot boundaries'
            call srstop(1)
        endif

        ! copy hot boundaries to the boundaries of this domain

        bound = wqHBComm % bndget

    endif hotbound

 9999 continue
    if ( timon ) call timstop ( ithandl )
    return

end subroutine

end module m_dlwq_boundio
