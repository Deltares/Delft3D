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
module Dio_Shm_Tst

!
! modules
!

use Dio_Plt_Rw
use Dio_2DField_Rw

!
! constants
!

! Time steps

integer, parameter :: NTIMES = 5000  ! # time steps

! # datasets

integer, parameter :: nPltSets = 3 ! # PLT datasets
integer, parameter :: n2DFSets = 4 ! # 2dField datasets

integer, parameter :: pltType  = 1 ! # PLT dataset type
integer, parameter :: f2dType  = 2 ! # PLT dataset type

! Max sizes PLT datasets (num pars, num locs)

integer, parameter :: PLT_PAR_MAX = 50  ! max #pars in PLT sets
integer, parameter :: PLT_LOC_MAX = 300 ! max #locsin PLT sets


!
! data structures
!

! Data structure for values in datasets per variable type.
! Data structure is for allocating info and values per dataset.
! Only one of the values-pointers will be used, others are zero.
! 

type dsData
    character(len=DioMaxDsNameLen) :: name
    integer                        :: dsType
    integer                        :: varType
    integer                        :: numM, numN
    real*4           , pointer , dimension(:,:) :: rValues ! real 
    double precision , pointer , dimension(:,:) :: dValues ! double
    integer          , pointer , dimension(:,:) :: iValues ! integer
    character(Len=DioMaxParLen), pointer, dimension(:) :: pars
    character(Len=DioMaxLocLen), pointer, dimension(:) :: locs
end type dsData


!
! interfaces
!

interface diffInValues
    module procedure diffInValuesR
    module procedure diffInValuesD
    module procedure diffInValuesI
end interface


contains


!
! Set sizes of datasets that are exchanged
!

subroutine InitPltData(pltData)

    type(dsData), dimension(nPltSets) :: pltData
    integer                           :: i

    ! #pars/locs in PLT sets, variable type for each set

    pltData % dsType     = pltType

    pltData % name       = ' '
    pltData % numM       = 0
    pltData % numN       = 0
    pltData % varType    = Dio_Var_Unknown

    pltData(1) % name    = 'PLT-A'
    pltData(1) % numM    = 25
    pltData(1) % numN    = 12
    pltData(1) % varType = Dio_Plt_Integer

    pltData(2) % name    = 'PLT-B'
    pltData(2) % numM    = 7
    pltData(2) % numN    = 15
    pltData(2) % varType = Dio_Plt_Double

    pltData(3) % name    = 'PLT-C'
    pltData(3) % numM    = 2 ! 15
    pltData(3) % numN    = 3 ! 200
    pltData(3) % varType = Dio_Plt_Real

    do i = 1, nPltSets
        call InitData( pltData(i) )
    enddo

end subroutine InitPltData


subroutine Init2DFData(f2dData)

    type(dsData), dimension(n2DFSets) :: f2dData
    integer                           :: i

    ! numM, numN in 2DField sets, variable type for each set

    f2dData % dsType     = f2dType

    f2dData % name       = ' '
    f2dData % numM       = 0
    f2dData % numN       = 0
    f2dData % varType    = Dio_Var_Unknown

    f2dData(1) % name    = '2DF-1'
    f2dData(1) % numM    = 20
    f2dData(1) % numN    = 36
    f2dData(1) % varType = Dio_Plt_Integer

    f2dData(2) % name    = '2DF-2'
    f2dData(2) % numM    = 4
    f2dData(2) % numN    = 100
    f2dData(2) % varType = Dio_Plt_Double

    f2dData(3) % name    = '2DF-3'
    f2dData(3) % numM    = 12
    f2dData(3) % numN    = 63
    f2dData(3) % varType = Dio_Plt_Real

    f2dData(4) % name    = '2DF-4'
    f2dData(4) % numM    = 3
    f2dData(4) % numN    = 2
    f2dData(4) % varType = Dio_Plt_Real

    do i = 1, n2DFSets
        call InitData( f2dData(i) )
    enddo

end subroutine Init2DFData


subroutine InitData(dsDat)

    type(dsData) :: dsDat     ! dataset data
    integer      :: par,loc   ! par. / loc. counters for PLT

    nullify(dsDat % rValues)
    nullify(dsDat % dValues)
    nullify(dsDat % iValues)

    select case ( dsDat % varType )
        case (Dio_Plt_Real)
            allocate(dsDat % rValues(dsDat % numM,dsDat % numN))
        case (Dio_Plt_Double)
            allocate(dsDat % dValues(dsDat % numM,dsDat % numN))
        case (Dio_Plt_Integer)
            allocate(dsDat % iValues(dsDat % numM,dsDat % numN))
        case (Dio_Plt_Unknown)
            write(*,*) 'Not All Datasets Defined in module Dio_Shm_tst'
            stop
    end select

    if ( dsDat % dsType .eq. pltType ) then
        allocate(dsDat % pars(dsDat % numM))
        do par = 1, dsDat % numM
            write(dsDat % pars(par),'(A,I0)') trim(dsDat % Name) // ' par ', par
        enddo
        allocate(dsDat % locs(dsDat % numN))
        do loc = 1, dsDat % numN
            write(dsDat % locs(loc),'(A,I0)') trim(dsDat % Name) // ' loc ', loc
        enddo
    endif

end subroutine InitData


subroutine DestroyData(dsDat)

    type(dsData) :: dsDat

    select case ( dsDat % varType )
        case (Dio_Plt_Real)
            deallocate(dsDat % rValues)
        case (Dio_Plt_Double)
            deallocate(dsDat % dValues)
        case (Dio_Plt_Integer)
            deallocate(dsDat % iValues)
    end select

    if ( dsDat % dsType .eq. pltType ) then
        if (associated(dsDat % pars)) deallocate(dsDat % pars)
        if (associated(dsDat % locs)) deallocate(dsDat % locs)
    endif
end subroutine DestroyData


!
! Initialize array of values
!

subroutine initValues(dsDat)
    type(dsData) :: dsDat  ! dataset to be initialized
    integer      :: i,j     ! counters
    do i = 1, dsDat % numM
        do j = 1, dsDat % numN
            select case ( dsDat % varType )
                case (Dio_Plt_Real)
                    dsDat % rValues(i,j) = .1 * j + .01 * i
                case (Dio_Plt_Double)
                    dsDat % dValues(i,j) = .1D+00 * j + .01D+00 * i
                case (Dio_Plt_Integer)
                    dsDat % iValues(i,j) = 10 * j + i
            end select
        enddo
    enddo
end subroutine initValues


subroutine incrementValues(dsDat)
    type(dsData) :: dsDat  ! dataset to be initialized
    select case ( dsDat % varType )
        case (Dio_Plt_Real)
            dsDat % rValues = dsDat % rValues + 0.01
        case (Dio_Plt_Double)
            dsDat % dValues = dsDat % dValues + 0.01D+00
        case (Dio_Plt_Integer)
            dsDat % iValues = dsDat % iValues + 1
    end select
end subroutine incrementValues


function diffInValuesR(dsDat, reals, epsilon) result(retVal)

    logical              :: retVal     ! return value
    type(dsData)         :: dsDat      ! ds expected values
    real, dimension(:,:) :: reals      ! received data
    double precision     :: epsilon    ! diff accuracy

    double precision, pointer, &
          dimension(:,:) :: diffValues ! diff values in dataset

    allocate(diffValues(dsDat % numM, dsDat % numN))
    diffValues = reals - dsDat % rValues
    retVal = checkDiffInValues(diffValues , epsilon)
    deallocate(diffValues)

end function diffInValuesR


function diffInValuesD(dsDat, doubles, epsilon) result(retVal)

    logical                          :: retVal     ! return value
    type(dsData)                     :: dsDat      ! ds expected values
    double precision, dimension(:,:) :: doubles    ! received data
    double precision                 :: epsilon    ! diff accuracy

    double precision, pointer, &
          dimension(:,:) :: diffValues             ! diff values in dataset


    allocate(diffValues(dsDat % numM, dsDat % numN))
    diffValues = doubles - dsDat % dValues
    retVal = checkDiffInValues(diffValues , epsilon)
    deallocate(diffValues)

end function diffInValuesD


function diffInValuesI(dsDat, ints, epsilon) result(retVal)

    logical                 :: retVal     ! return value
    type(dsData)            :: dsDat      ! ds expected values
    integer, dimension(:,:) :: ints       ! received data
    double precision        :: epsilon    ! diff accuracy

    double precision, pointer, &
          dimension(:,:) :: diffValues             ! diff values in dataset


    allocate(diffValues(dsDat % numM, dsDat % numN))
    diffValues = ints - dsDat % iValues
    retVal = checkDiffInValues(diffValues , epsilon)
    deallocate(diffValues)

end function diffInValuesI


function checkDiffInValues(diffValues , epsilon) result(retVal)

    logical :: retVal  ! return value

    double precision, dimension(:,:) :: diffValues ! diff values in dataset
    double precision                 :: epsilon    ! diff accuracy

    integer :: i,j     ! counters

    retVal = .false.
    do i = 1,size(diffValues,1)
        do j = 1,size(diffValues,2)
            if( diffValues(i,j) >  epsilon .or. &
                diffValues(i,j) <  (-epsilon)     ) then
                    retVal = .true.
            endif
        enddo
    enddo

end function checkDiffInValues


end module Dio_Shm_Tst

