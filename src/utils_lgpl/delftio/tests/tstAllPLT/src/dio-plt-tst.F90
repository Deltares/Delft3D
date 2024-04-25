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
module Dio_Plt_Tst

    use Dio_Plt_Rw

    integer, parameter :: ra = 1 ! dataset/stream reals/ASCII
    integer, parameter :: da = 2 ! dataset/stream doubles/ASCII
    integer, parameter :: ia = 3 ! dataset/stream ints/ASCII
    integer, parameter :: rb = 4 ! dataset/stream reals/Binary
    integer, parameter :: db = 5 ! dataset/stream doubles/Binary
    integer, parameter :: ib = 6 ! dataset/stream ints/Binary

    integer, parameter :: nSets = 6 ! #datasets/streams

    integer, parameter :: NPARS  = 5   ! #parameters
    integer, parameter :: NLOCS  = 3   ! #locations
#if (defined(WIN32))
    integer, parameter :: NTIMES = 100 ! #timesteps
#else
    integer, parameter :: NTIMES = 10  ! shorten tests on Unix
#endif

contains


subroutine initValues(rValues, dValues, iValues)
    real*4,          dimension(:,:)    :: rValues ! real values in dataset
    double precision,dimension(:,:)    :: dValues ! double values in dataset
    integer,         dimension(:,:)    :: iValues ! integer values in dataset
    integer                            :: i,j     ! counters
    do i = 1,size(rValues,1)
        do j = 1,size(rValues,2)
            rValues(i,j) = .1 * j + .01 * i
            dValues(i,j) = .1D+00 * j + .01D+00 * i
            iValues(i,j) = 10 * j + i
        enddo
    enddo
end subroutine initValues


subroutine incrementValues(rValues, dValues, iValues)
    real*4,          dimension(:,:)    :: rValues ! real values in dataset
    double precision,dimension(:,:)    :: dValues ! double values in dataset
    integer,         dimension(:,:)    :: iValues ! integer values in dataset

    rValues = rValues + 0.01
    dValues = dValues + 0.01D+00
    iValues = iValues + 1

end subroutine incrementValues


function diffInValues(diffValues, epsilon) result(retVal)

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
end function diffInValues


end module Dio_Plt_Tst


