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

module M_WQParComm

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
integer, parameter :: WQParMaxcommBlocks = 10  ! Max #comm. blocks per domain
integer, parameter :: WQParMaxLen       = 100 ! Max len of domain / comm.block


!
! Domain identification for current domain
!
!haracter(Len=WQParMaxLen) :: WQParCurrDomName = 'NotDefinedYet'


!
! #communication info per comm. block
!
type TWQParCommBlock

    logical                   :: putInited    ! block 'Put' initialized?
    logical                   :: getInited    ! block 'Get' initialized?

    integer                   :: numVars      ! #vars for comm. block
    integer                   :: numPoints    ! #points in comm. block
    integer, dimension(:), &
        pointer               :: LocIndex     ! location indices in other domain. domain
    integer, dimension(:), &
        pointer               :: ParIndex     ! parameter indices in other domain. domain

    character(Len=WQParMaxLen):: inName       ! name of incoming dataset
    character(Len=WQParMaxLen):: outName      ! name of outgoing dataset

    type(waq_DioPltType)          :: inDataset    ! DIO dataset for incoming values
    type(waq_DioPltType)          :: outDataset   ! DIO dataset for outgoing values

end type TWQParCommBlock


!*
!* Data for current domain
!*

!
! communication blocks
!
type TWQParComm
    character(Len=WQParMaxLen):: curDomName    ! current domain name
    integer                   :: nosys         ! #vars for this domain
    integer                   :: nobnd         ! #points in this domain
    character(len=DioMaxLocLen), pointer :: namsys(:)
    character(len=DioMaxParLen), pointer :: nambnd(:)
    real, pointer             :: bndput(:,:)
    real, pointer             :: bndget(:,:)
    type(TWQParCommBlock), &
        pointer, dimension(:) :: commBlock     ! array with comm. blocks
    integer                   :: numCommBlocks ! #comm. blocks
end type TWQParComm


!*
!* Declaration of private functions
!*

private :: WQParReadConfig
private :: WQParAddCommBlock


contains



!*******************
!* Public functions
!*******************

!
! Create communication data for current domain
!
function WQParInit(wqParComm, configFileName, curDomName, nosys, nobnd, syname, bndid) result(success)

    ! return value
    logical         :: success        ! .true.: succes

    ! arguments
    type(TWQParComm):: wqParComm      ! comm. info for curr. domain
    character(Len=*):: configFileName ! config file
    character(Len=*):: curDomName     ! current domain name
    integer                   :: nosys              ! number of active substances
    integer                   :: noseg              ! number of segments
    integer                   :: nobnd              ! number of boundaries
    character(len=20)         :: syname(nosys)      ! substance names
    character(len=20)         :: bndid(nobnd)       ! boundary id

    ! body:
    ! - store domain name
    ! - allocate max comm. blocks, set all to Uninitialized, init #blocks to 0
    ! - read config file for this domain

    wqParComm % curDomName = curDomName

    allocate(wqParComm % commBlock(WQParMaxcommBlocks))
    wqParComm % commBlock % putInited = .false.
    wqParComm % commBlock % getInited = .false.
    wqParComm % numCommBlocks = 0
    wqParComm % nosys         = nosys
    wqParComm % nobnd         = nobnd
    allocate(wqParComm % namsys(nosys))
    wqParComm % namsys = syname
    allocate(wqParComm % nambnd(nobnd))
    wqParComm % nambnd = bndid
    allocate(wqParComm % bndput(nosys,nobnd))
    allocate(wqParComm % bndget(nosys,nobnd))

    success = WQParReadConfig(wqParComm, configFileName, curDomName)

end function WQParInit


!
! Put values
!
function WQParPutValues(wqParComm) result(success)

    ! return value
    logical                :: success   ! .true.: succes

    ! arguments
    type(TWQParComm)       :: wqParComm ! comm. info for curr. domain

    ! locals
    integer                :: cb, v, p  ! loop counters (comm.block,var,point)
    type(TWQParCommBlock), &
        pointer            :: commBlock ! pointer to new commBlock

    success = .true.

    cb = 1
    do while ( cb <= wqParComm % numCommBlocks .and. success )

        commBlock => wqParComm % commBlock(cb)
        if ( .not. commBlock % putInited ) then
            commBlock % outDataset = waq_DioPltDefine(commBlock % outName, &
                                                  Dio_Var_Real,        &
                                                  wqParComm % namsys,  &
                                                  wqParComm % nambnd   )

            success = .true.
            if ( DioGetLastError() .ne. 0 ) then
               write(*,*) 'WQPar DIO: ', trim(DioGetLastErrorMsg())
               success = .false.
            endif
        endif
        if ( success ) then
            commBlock % putInited = .true.
            call waq_DioPltPut(commBlock % outDataset, wqParComm % bndput)
        endif
        cb = cb + 1
    enddo

end function WQParPutValues

!
! Get values
!
function WQParGetValues(wqParComm) result(success)

    ! return value
    logical                :: success   ! .true.: succes

    ! arguments
    type(TWQParComm)       :: wqParComm ! comm. info for curr. domain

    ! locals
    integer                :: cb                       ! loop counter comm.block
    integer                :: ipoint                   ! loop counter/index  boundaries other domain
    integer                :: ibnd                     ! loop counter/index  boundaries this domain
    integer                :: ipar                     ! loop counter/index  substances other domain
    integer                :: isys                     ! loop counter/index  substances this domain
    integer                :: lunrep                   ! unit number report file
    type(TWQParCommBlock), &
        pointer            :: commBlock                ! pointer to new commBlock
    character(len=DioMaxLocLen), pointer :: namloc(:)  ! boundary names in other domain
    character(len=DioMaxParLen), pointer :: nampar(:)  ! substance names in other domains
    real, dimension(:,:), &
        pointer               :: inValues     ! incoming values

    success = .true.
    call getmlu(lunrep)

    cb = 1
    do while ( cb <= wqParComm % numCommBlocks .and. success )

        commBlock => wqParComm % commBlock(cb)
        if ( .not. commBlock % getInited ) then
            commBlock % inDataset = waq_DioPltGetDataset(commBlock % inName )
            success = .false.
            if ( DioGetLastError() .ne. 0 ) then
                write(*,*) 'WQPar DIO: ', trim(DioGetLastErrorMsg())
            else
                success = .true.
            endif
            if ( success ) then

                ! set up boundary pointering over the domains

                write(lunrep,'(2A)') ' Domain decompostion boundary matching for:',commBlock % inName
                commBlock % numVars   = DioPltGetNPar(commBlock % inDataset)
                commBlock % numPoints = DioPltGetNLoc(commBlock % inDataset)
                allocate(commBlock % LocIndex(wqParComm % nobnd))
                commBlock % LocIndex = 0
                namloc => DioPltGetLocs(commBlock % inDataset)
                do ibnd = 1 , wqParComm % nobnd
                    do ipoint = 1 , commBlock % numPoints
                        if ( StringsEqual(CaseInsens, wqParComm % nambnd(ibnd), namloc(ipoint) ) ) then
                            commBlock % LocIndex(ibnd) = ipoint
                            exit
                        endif
                    enddo
                    if ( commBlock % LocIndex(ibnd) .ne. 0 ) then
                        write(lunrep,'(3A,i8)') ' for boundary ',wqParComm % nambnd(ibnd)(1:20),' match found with number:',&
                        commBlock % LocIndex(ibnd)
                    else
                        write(lunrep,'(3A)') ' for boundary ',wqParComm % nambnd(ibnd)(1:20),' no match found'
                    endif
                enddo

                ! set up substance pointering over the domains

                write(lunrep,'(2A)') ' Domain decompostion substance matching for:',commBlock % inName
                allocate(commBlock % ParIndex(wqParComm % nosys))
                commBlock % ParIndex = 0
                nampar => DioPltGetPars(commBlock % inDataset)
                do isys = 1 , wqParComm % nosys
                    do ipar = 1 , commBlock % numVars
                        if ( StringsEqual(CaseInsens, wqParComm % namsys(isys), nampar(ipar) ) ) then
                            commBlock % ParIndex(isys) = ipar
                            exit
                        endif
                    enddo
                    if ( commBlock % ParIndex(isys) .ne. 0 ) then
                        write(lunrep,'(3A,i8)') ' for substance ',wqParComm % namsys(isys)(1:20),' match found with number:',&
                        commBlock % ParIndex(isys)
                    else
                        write(lunrep,'(3A)') ' for substance ',wqParComm % namsys(isys)(1:20),' no match found'
                    endif
                enddo
            endif
        endif
        if ( success ) then
            commBlock % getInited = .true.
            if (waq_DioPltGet(commBlock % inDataset, inValues)) then
                do ibnd = 1 , wqParComm % nobnd
                    ipoint = commBlock % LocIndex(ibnd)
                    if ( ipoint .gt. 0 ) then
                        do isys = 1 , wqParComm % nosys
                            ipar = commBlock % ParIndex(isys)
                            if ( ipar .gt. 0 ) then
                                wqParComm % bndget(isys,ibnd) = inValues(ipar,ipoint)
                            endif
                        enddo
                    endif
                enddo
            endif
        endif
        cb = cb + 1
    enddo

end function WQParGetValues


subroutine WQParClose(wqParComm)

    ! arguments
    type(TWQParComm) :: wqParComm  ! comm. info for curr. domain

    ! locals
    integer          :: cb         ! comm.block loop counter

    ! body:
    ! - close datasets
    ! - free outgoing values
    ! - free comm.info blocks

    do cb = 1, wqParComm % numCommBlocks
        call DioPltDestroy(wqParComm % commBlock(cb) % inDataset )
        call DioPltDestroy(wqParComm % commBlock(cb) % outDataset)
    enddo
    deallocate(wqParComm % commBlock)

end subroutine WQParClose


!*******************
!* Private functions
!*******************


!
! Read configuration for current domain
!
function WQParReadConfig(wqParComm, configFileName, curDomName) result(success)

    ! return value
    logical          :: success ! .true.: succes

    ! arguments
    type(TWQParComm) :: wqParComm      ! comm. info for curr. domain
    character(Len=*) :: configFileName ! config file
    character(Len=*) :: curDomName     ! current domain name

    ! locals
    type(TWQParCommBlock), &
        pointer               :: commBlock    ! pointer to new commBlock
    integer                   :: idomain, indx! loop counters
    integer                   :: nodomain     ! number of domains
    integer                   :: iostat_config! iostat configuration file
    integer                   :: lunrep       ! unit number report file
    integer                   :: luncnf       ! unit number configuration file
    ! locals
    character(Len=WQParMaxLen):: neighborName ! name of neighbor domain
    character(Len=WQParMaxLen), allocatable :: domainname(:) ! domain names


    ! (FAKE) body:
    ! - find current domain ID in file
    ! - while comm.blocks in config file:
    !   . add new comm. block.
    !   . get neighbor name, determine DIO dataset names
    !   . initialize new comm. block. (#vars, #points, etc.)
    ! - define outgoing datasets
    ! - get incoming datasets


    success = .false.
    call getmlu(lunrep)
!   open(432,file=configFileName,shared,iostat=iostat_config)
    open(newunit=luncnf,file=configFileName,iostat=iostat_config)
    if ( iostat_config .eq. 0 ) then
        read(luncnf,*,iostat=iostat_config) nodomain
        write(lunrep,*) ' number of domains:',nodomain
        allocate (domainname(nodomain))
        do idomain = 1 , nodomain
            if ( iostat_config .eq. 0 ) then
                read(luncnf,'(A)',iostat=iostat_config) domainname(idomain)
                write(lunrep,*) ' domain number:',idomain,' name:',domainname(idomain)
            endif
        enddo
    endif
    close(luncnf)
    if ( iostat_config .eq. 0 ) then
       do idomain = 1, nodomain

           neighborName=domainname(idomain)

           if ( neighborName .ne. curDomName ) then

               commBlock => WQParAddCommBlock(wqParComm)

               commBlock % outName = trim(curDomName) // '_to_' // trim(neighborName)
               commBlock % inName  = trim(neighborName) // '_to_' // trim(curDomName)

               success = .true.

           endif

       end do
    else
       write(lunrep,*) 'error reading configuration file'
    endif

end function WQParReadConfig


!
! Add new comm. block
!
function WQParAddCommBlock(wqParComm) result(commBlock)

    ! return value
    type(TWQParCommBlock), &
        pointer             :: commBlock ! not NULL: pointer to new commBlock

    ! arguments
    type(TWQParComm)        :: wqParComm ! comm. info for curr. domain

    ! body
    ! - check of block can be added; if so, return pointer

    nullify(commBlock)
    if ( wqParComm % numCommBlocks < WQParMaxcommBlocks ) then
        wqParComm % numCommBlocks = wqParComm % numCommBlocks + 1
        commBlock => wqParComm % commBlock(wqParComm % numCommBlocks)
    endif

end function WQParAddCommBlock

end module M_WQParComm
