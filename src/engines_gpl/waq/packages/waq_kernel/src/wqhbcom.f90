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

module M_WQHBComm

    use m_monsys
    use waq_Dio_plt_rw

    implicit none

    !****************
    !* Data
    !****************

    !*
    !* Data Types
    !*
    !
    ! Constants
    !
    integer, parameter :: MaxLocLen = 20
    integer, parameter :: MaxParLen = 20

    !*
    !* Data for communication blocks
    !*
    !
    type TWQHBComm
        character(len=255)        :: filename
        integer                   :: nosys         ! #vars for this domain
        integer                   :: nobnd         ! #points in this domain
        character(len=MaxLocLen), pointer :: namsys(:)
        character(len=MaxParLen), pointer :: nambnd(:)
        real, pointer             :: bndget(:,:)
        real, pointer             :: valinp(:,:)
        integer                   :: numVars      ! #vars for comm. block
        integer                   :: numPoints    ! #points in comm. block
        character(len=MaxLocLen), pointer :: namPoints(:)
        character(len=MaxParLen), pointer :: namVars(:)
        integer, pointer          :: LocIndex(:)     ! location indices in other domain. domain
        integer, pointer          :: ParIndex(:)     ! parameter indices in other domain. domain
    end type TWQHBComm

contains



!*******************
!* Public functions
!*******************

!
! Create communication data for current domain
!
function WQHBInit(WQHBComm, HisFileName, nosys, nobnd, syname, bndid) result(success)

    ! return value
    logical         :: success        ! .true.: succes

    ! arguments
    type(TWQHBComm)::  WQHBComm      ! comm. info for curr. domain
    character(Len=*):: HisFileName   ! history file
    integer                   :: nosys              ! number of active substances
    integer                   :: noseg              ! number of segments
    integer                   :: nobnd              ! number of boundaries
    character(len=20)         :: syname(nosys)      ! substance names
    character(len=20)         :: bndid(nobnd)       ! boundary id

    ! locals

    integer            :: luhis
    integer            :: ivar
    integer            :: ipar                     ! loop counter/index  substances other domain
    integer            :: ipoint
    integer            :: ibnd                     ! loop counter/index  boundaries this domain
    integer            :: isys                     ! loop counter/index  substances this domain
    integer            :: lunrep                   ! unit number report file
    character*160      :: moname


    ! body:
    ! - map locations and variables

    call getmlu(lunrep)

!   Create mapping

    success = .false.
    WQHBComm % nosys         = nosys
    WQHBComm % nobnd         = nobnd
    allocate(WQHBComm % LocIndex(nobnd))
    allocate(WQHBComm % namsys(nosys))
    allocate(WQHBComm % ParIndex(nosys))
    allocate(WQHBComm % nambnd(nobnd))
    allocate(WQHBComm % bndget(nosys,nobnd))
    WQHBComm % namsys = syname
    WQHBComm % nambnd = bndid
    WQHBComm % filename = HisFileName

!   Read metadata from HIS file

    open (newunit = luhis,file=WQHBComm % filename,form='unformatted',access='stream')
    read (luhis) moname
    read (luhis) WQHBComm % numVars, WQHBComm % numPoints
    allocate(WQHBComm % namVars  (WQHBComm % numVars))
    allocate(WQHBComm % namPoints(WQHBComm % numPoints))
    do ivar = 1,WQHBComm % numVars
        read (luhis) WQHBComm % namVars(ivar)
    enddo

    do ipoint = 1,WQHBComm % numPoints
       read (luhis) ivar,WQHBComm % namPoints(ipoint)
       WQHBComm % namPoints(ipoint) = 'n'//WQHBComm % namPoints(ipoint)
    enddo
    close(luhis)

!   Set up boundary pointering over the domains

    write(lunrep,'(2A)') ' Hot boundary location matching for:',HisFileName
    WQHBComm % LocIndex = 0
    do ibnd = 1 , WQHBComm % nobnd
        do ipoint = 1 , WQHBComm % numPoints
            if ( StringsEqual(CaseInsens, WQHBComm % nambnd(ibnd), WQHBComm % namPoints(ipoint) ) ) then
                WQHBComm % LocIndex(ibnd) = ipoint
                exit
            endif
        enddo
        if ( WQHBComm % LocIndex(ibnd) .ne. 0 ) then
            write(lunrep,'(3A,i8)') ' for boundary ',WQHBComm % nambnd(ibnd)(1:20),' match found with number:',&
               WQHBComm % LocIndex(ibnd)
        else
            write(lunrep,'(3A)') ' for boundary ',WQHBComm % nambnd(ibnd)(1:20),' no match found'
        endif
    enddo

!   Set up substance pointering over the domains

    write(lunrep,'(/2A)') ' Hot boundary substance matching for:',HisFileName
    WQHBComm % ParIndex = 0
    do isys = 1 , WQHBComm % nosys
       do ipar = 1 , WQHBComm % numVars
          if ( StringsEqual(CaseInsens, WQHBComm % namsys(isys), WQHBComm % namVars(ipar) ) ) then
            WQHBComm % ParIndex(isys) = ipar
            exit
          endif
       enddo
       if ( WQHBComm % ParIndex(isys) .ne. 0 ) then
          write(lunrep,'(3A,i8)') ' for substance ',WQHBComm % namsys(isys)(1:20),' match found with number:',&
          WQHBComm % ParIndex(isys)
       else
          write(lunrep,'(3A)') ' for substance ',WQHBComm % namsys(isys)(1:20),' no match found'
       endif
    enddo

    allocate(WQHBComm % valinp(WQHBComm % numVars, WQHBComm % numPoints))

    success = .true.

end function WQHBInit

!
! Get values
!
function WQHBGetValues(WQHBComm) result(success)

    ! return value
    logical            :: success   ! .true.: succes

    ! arguments
    type(TWQHBComm)    :: WQHBComm ! comm. info for curr. domain

    ! locals
    integer            :: ibnd                     ! loop counter/index  boundaries this domain
    integer            :: ipar                     ! loop counter/index  substances other domain
    integer            :: isys                     ! loop counter/index  substances this domain
    integer            :: lunrep                   ! unit number report file
    integer            :: luhis
    integer            :: ivar
    integer            :: ipoint
    integer            :: itime
    character*160      :: moname
    character*20       :: c20


    ! Body:
    ! - read data and map to boundary matrix

    call getmlu(lunrep)

!   Create mapping

    success = .false.

!   Skip heading from HIS file

    open (newunit = luhis,file=WQHBComm % filename,form='unformatted',access='stream')
    read (luhis) moname
    read (luhis) ipoint,ivar
    read (luhis) ( c20, ivar = 1,WQHBComm % numVars )
    read (luhis) ( ivar, c20, ipoint = 1,WQHBComm % numPoints )

!   Read the actual data (time can be ignored)

    read (luhis) itime, WQHBComm % valinp
    close(luhis)

!   Map data

    do ibnd = 1 , WQHBComm % nobnd
        ipoint = WQHBComm % LocIndex(ibnd)
        if ( ipoint .gt. 0 ) then
           do isys = 1 , WQHBComm % nosys
               ipar = WQHBComm % ParIndex(isys)
               if ( ipar .gt. 0 ) then
                   WQHBComm % bndget(isys,ibnd) = WQHBComm % valinp(ipar,ipoint)
                   write(lunrep,'(5A,g12.4)') ' value for substance ',WQHBComm % namVars(ipar),' at location ',&
                               WQHBComm % namPoints(ipoint),' set to ',WQHBComm % valinp(ipar,ipoint)
               endif
           enddo
        endif
    enddo
    success = .true.

end function WQHBGetValues

end module M_WQHBComm
