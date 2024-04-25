!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! module_restart: Triton Restart File
!!!
!!! (c) Deltares, dec 2002
!!!
!!! Stef.Hummel@deltares.nl / Ivo.Wenneker@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module M_TritonRestart

use Dio_2dfield_Rw
use Dio_const_Rw

implicit none

!*
!* Private variables
!*

type(DioStreamType), private  :: restartFile                ! Dio Handle to restart File
logical, private              :: restartFileOpen = .false.  ! restartFile open?

!*
!* Interfaces
!*

interface TritonRestartStore
    module procedure TritonRestartStoreScalar
    module procedure TritonRestartStore1D
    module procedure TritonRestartStore2D
end interface

interface TritonRestartLoad
    module procedure TritonRestartLoadScalar
    module procedure TritonRestartLoad1D
    module procedure TritonRestartLoad2D
end interface

contains


!********************
!* PUBLIC FUNCTIONS
!********************


!
! Open Restart File
!
function TritonRestartOpen(fileName, mode, timeStamp) result(retVal)

    ! return value
    logical                        :: retVal    ! .true.: success

    ! arguments
    character(Len=*), intent(IN)   :: fileName  ! Nefis file(s) (no extension)
    character(Len=*), intent(IN)   :: mode      ! Read/Write ('w' or 'r')
    double precision, intent(INOUT):: timeStamp ! 'w': IN:  time stamp to store
                                                ! 'r': OUT: time stamp in restart file

    ! locals
    type(DioConstType)             :: timeDS    ! dataset containing timestamp
    integer                        :: dioErr    ! DelftIO error mess. nr

    ! body:
    ! - Check if restart files exist
    ! - Open restart file and check for success
    ! - if Write: store timestamp
    ! - if read:  retreive timestamp

    retVal = .false.

    if ( TritonRestartExists( fileName, mode ) ) then

        restartFile = DioStreamCreate(dio_Nefis_stream, fileName, mode)
        if ( .not. DioStreamOpenedOK(restartFile) ) then
            call TritonRestartError( DioGetLastError(), DioGetLastErrorMsg() )
        else

            if ( mode == 'w' ) then

                ! Store timestep (define dataset on stream and put value)

                timeDS = DioConstDefine(restartFile, 'Time', Dio_Var_Double)
                dioErr = DioGetLastError()
                if ( dioErr .ne. 0 ) then
                    call TritonRestartError( dioErr, DioGetLastErrorMsg() )
                else
                    call DioConstPut(timeDS, timeStamp)
                    retVal          = .true.
                    restartFileOpen = .true.
                endif

            else if ( mode == 'r' ) then

                ! Retrieve timestep (get dataset from stream and get value)

                timeDS = DioConstGetDataset(restartFile, 'Time')
                dioErr = DioGetLastError()
                if ( dioErr .ne. 0 ) then
                    call TritonRestartError( dioErr, DioGetLastErrorMsg() )
                else
                    if (DioConstGet(timeDS, timeStamp)) then
                        retVal          = .true.
                        restartFileOpen = .true.
                    endif
                endif
            endif
        endif
    endif

end function TritonRestartOpen


!
! Store Scalar Dataset to restart File
!
subroutine TritonRestartStoreScalar(name, value)

    ! arguments
    character(Len=*), intent(IN) :: name     ! dataset (= variable) name
    double precision             :: value    ! value to store

    ! locals
    double precision, &
        dimension(1,1)           :: values2D ! 2D version of values

    ! body: transform to 2D and store

    values2D(1,1) = value
    call TritonRestartStore2D(name, values2D)

end subroutine TritonRestartStoreScalar


!
! Store 1d Dataset to restart File
!
subroutine TritonRestartStore1D(name, values)

    ! arguments
    character(Len=*), intent(IN) :: name     ! dataset (= variable) name
    double precision, &
        dimension(:), intent(IN) :: values   ! values in variable

    ! locals
    double precision, &
        dimension(:,:), pointer  :: values2D ! 2D version of values
    integer                      :: LB, UB   ! Lower/Upper bound of values
    integer                      :: i        ! counter (to avoid stack
                                             ! overflow and SUN errors)

    ! body: transform to 2D and store

    LB=Lbound(values,1) ; UB=Ubound(values,1)
    allocate(values2D(LB:UB,1))
    do i = LB, UB
        values2D(i,1) = values(i)
    enddo
    call TritonRestartStore2D(name, values2D)
    deallocate(values2D)

end subroutine TritonRestartStore1D


!
! Store 2d Dataset to restart File
!
subroutine TritonRestartStore2D(name, values)

    ! arguments
    character(Len=*),  intent(IN)  :: name    ! dataset (= variable) name
    double precision, &
        dimension(:,:), intent(IN) :: values  ! values in variable

    ! locals
    type(Dio2DFType)               :: dataset ! DelftIO dataset handle
    integer                        :: dioErr  ! DelftIO error number

    ! body: Define dataset <name> on restart file, and put its values

    if ( .not. restartFileOpen ) then
        call TritonRestartError(1, 'TritonRestartStore: restart File not open')
    else
        dataset = Dio2DFDefine(restartFile, name,           Dio_Var_Double, &
                                            size(values,1), size(values,2)    )
        dioErr = DioGetLastError()
        if ( dioErr .ne. 0 ) then
            call TritonRestartError( dioErr, DioGetLastErrorMsg() )
        else
            call Dio2DFPut(dataset, values)
            call Dio2DFDestroy(dataset)
        endif
    endif

end subroutine TritonRestartStore2D


!
! Load Scalar Dataset from restart File
!
function TritonRestartLoadScalar(name, value) result(retVal)

    ! return value
    logical                      :: retVal    ! .true.: success

    ! arguments
    character(Len=*), intent(IN) :: name      ! dataset (= variable) name
    double precision, intent(OUT):: value     ! value to load

    ! locals
    double precision, &
        dimension(1,1)           :: values2D  ! 2D version of values
    integer                      :: LB, UB    ! Lower/Upper bound of values
    integer                      :: i         ! counter (to avoid stack
                                              ! overflow and SUN errors)

    ! body: transform to 2D and load

    value = 0
    retVal = TritonRestartLoad2D(name, values2D)
    if (retVal) then
        value = values2D(1,1)
    endif

end function TritonRestartLoadScalar


!
! Load 1d Dataset from restart File
!
function TritonRestartLoad1D(name, values) result(retVal)

    ! return value
    logical                      :: retVal    ! .true.: success

    ! arguments
    character(Len=*), intent(IN) :: name      ! dataset (= variable) name
    double precision, &
        dimension(:), intent(OUT):: values    ! values to be delivered

    ! locals
    double precision, &
        dimension(:,:), pointer  :: values2D  ! 2D version of values
    integer                      :: LB, UB    ! Lower/Upper bound of values
    integer                      :: i         ! counter (to avoid stack
                                              ! overflow and SUN errors)

    ! body: transform to 2D and load

    LB=Lbound(values,1) ; UB=Ubound(values,1)
    allocate(values2D(LB:UB,1))
    retVal = TritonRestartLoad2D(name, values2D)
    if (retVal) then
        do i = LB, UB
            values(i) = values2D(i,1)
        enddo
    endif
    deallocate(values2D)

end function TritonRestartLoad1D


!
! Load 2d Dataset from restart File
!
function TritonRestartLoad2D(name, values) result(retVal)

    ! return value
    logical                         :: retVal    ! .true.: success

    ! arguments
    character(Len=*),  intent(IN)   :: name      ! dataset (= variable) name
    double precision, &
        dimension(:,:), intent(OUT) :: values    ! values to be delivered

    ! locals
    type(Dio2DFType)                :: dataset   ! DelftIO dataset handle
    double precision, pointer, &
                      dimension(:,:):: dioValues ! values in DelftIO dataset
    integer                         :: dioErr    ! DelftIO error number

    ! body: Get dataset <name> from restart file, and get its values

    retVal = .false.
    if ( .not. restartFileOpen ) then
        call TritonRestartError(2, 'TritonRestartLoad: restart File not open')
    else
        dataset = Dio2DFGetDataset(restartFile, name)
        dioErr = DioGetLastError()
        if ( dioErr .ne. 0 ) then
            call TritonRestartError( dioErr, DioGetLastErrorMsg() )
        else
            if (.not. Dio2DFGet(dataset, dioValues) ) then
                call TritonRestartError(DioGetLastError(), DioGetLastErrorMsg() )
            else
                ! Got values. Check if dimensions are the same as expected dimensions
                if ( size(dioValues,1) .ne. size(values,1) .or. &
                     size(dioValues,2) .ne. size(values,2)        ) then
                    call TritonRestartError(2, 'TritonRestartLoad: Incompatible sizes')
                else
                    values = dioValues
                    retVal = .true.
                endif
            endif
            call Dio2DFDestroy(dataset)
        endif
    endif

end function TritonRestartLoad2D


!
! Close the restart file
!
subroutine TritonRestartClose()

    ! body: close restart file if it was opened
    if ( restartFileOpen ) then
        call DioStreamClose(restartFile)
        restartFileOpen = .false.
    endif

end subroutine TritonRestartClose


!********************
!* PRIVATE FUNCTIONS
!********************

subroutine TritonRestartError(errNr, errMsg)

    ! arguments
    integer                      :: errNr   ! Error Nr.
    character(Len=*), intent(IN) :: errMsg  ! Error Text
    write(*, '(A,I4,A,A)') 'RESTART ERR ', errNr, ': ', errMsg

end subroutine TritonRestartError


function TritonRestartExists(fileName, mode) result(retVal)

    ! return value
    logical                        :: retVal        ! .true.: success

    ! arguments
    character(Len=*), intent(IN)   :: fileName      ! Nefis file(s) (no ext.)
    character(Len=*), intent(IN)   :: mode          ! Read/Write ('w' or 'r')

    ! locals
    character(LEN=DioMaxStreamLen) :: defFileName,& ! Filenames (Def/Dat part)
                                      datFileName
    logical                        :: defExists,&   ! Def/Dat part exists?
                                      datExists
    integer                        :: tempLun       ! temp lun to remove file
    integer                        :: errDef,errDat ! result when opening file

    ! body:
    ! - check if restart files exist.
    ! If so:
    ! - return OK if they were expected
    ! - remove them if they were not expected

    retVal = .false.

    defFileName = trim(fileName) // '.def'
    datFileName = trim(fileName) // '.dat'
    inquire(file=defFileName, exist=defExists)
    inquire(file=datFileName, exist=datExists)

    if ( mode == 'r' ) then
        if ( defExists .and. datExists ) then
            retVal = .true.
        endif
    else if ( mode == 'w' ) then
        errDef = 0 ; errDat = 0
        if ( defExists ) then
            open (newunit=tempLun, file=defFileName, iostat=errDef)
            if (errDef == 0) close(tempLun, status='delete', iostat=errDef)
        endif
        if ( datExists ) then
            open (newunit=tempLun, file=datFileName, iostat=errDat)
            if (errDat == 0) close(tempLun, status='delete', iostat=errDat)
        endif
        if (errDef == 0 .and. errDat == 0) then
            retVal = .true.
        endif
    endif

end function TritonRestartExists


end module M_TritonRestart


