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
program test_nefis_restart_put

    use Dio_2dfield_Rw
    use Dio_const_Rw

    implicit none

    integer, parameter :: numM1 = 5
    integer, parameter :: numN1 = 3

    integer, parameter :: numM2 = 26
    integer, parameter :: numN2 = 50

    integer, parameter :: numM1Dble = 11
    integer, parameter :: numN1Dble = 13

    integer, parameter :: numM2Dble = 1
    integer, parameter :: numN2Dble = 21

    integer, parameter :: numM1Int = 2
    integer, parameter :: numN1Int = 1

    integer, parameter :: numM2Int = 18
    integer, parameter :: numN2Int = 18

    real,             dimension(numM1,numN1)        :: realValues1
    real,             dimension(numM2,numN2)        :: realValues2

    double precision, dimension(numM1Dble,numN1Dble):: dbleValues1
    double precision, dimension(numM2Dble,numN2Dble):: dbleValues2

    integer,          dimension(numM1Int,numN1Int)  :: intValues1
    integer,          dimension(numM2Int,numN2Int)  :: intValues2

    type(DioStreamType) :: outStream
    type(Dio2DFType)    :: realSet1, realSet2
    type(Dio2DFType)    :: dbleSet1, dbleSet2
    type(Dio2DFType)    :: intSet1 , intSet2

    type(DioConstType)  :: curTimeSet

    integer :: i,j
    integer :: errNr
    character(Len=DioMaxErrMsgLen) :: errMsg

    double precision :: currentTimeStep

    ! init values

    do i = 1,size(dbleValues1,1)
        do j = 1,size(dbleValues1,2)
            dbleValues1(i,j) = 1000.0D+00 + 1.0D+00 * j + 10.0D+00 * i
        enddo
    enddo
    do i = 1,size(dbleValues2,1)
        do j = 1,size(dbleValues2,2)
            dbleValues2(i,j) = 2000.0D+00 + 1.0D+00 * j + 10.0D+00 * i
        enddo
    enddo
    do i = 1,size(realValues1,1)
        do j = 1,size(realValues1,2)
            realValues1(i,j) = 3000.0 + 1.0 * j + 10.0 * i
        enddo
    enddo
    do i = 1,size(realValues2,1)
        do j = 1,size(realValues2,2)
            realValues2(i,j) = 4000.0 + 1.0 * j + 10.0 * i
        enddo
    enddo
    do i = 1,size(intValues1,1)
        do j = 1,size(intValues1,2)
            intValues1(i,j) = 5000 + 1 * j + 10 * i
        enddo
    enddo
    do i = 1,size(intValues2,1)
        do j = 1,size(intValues2,2)
            intValues2(i,j) = 6000 + 1 * j + 10 * i
        enddo
    enddo

!   Initialize dio library
    call DioInit

!   Open data stream

    outStream = DioStreamCreateSynched(dio_Nefis_stream, 'TESTRestart', 'w', .false.)
    if ( .not. DioStreamOpenedOK(outStream ) ) then
        errNr  = DioGetLastError()
        errMsg = DioGetLastErrorMsg()
        write(*, *) 'DIOTESTERROR', errNr, ': ', trim(errMsg)
    else
        write (*, *) 'OUT Stream Created'

    !   Create OUT data set

        realSet1 = Dio2DFDefine(outStream, 'R1', Dio_Var_Real,   numM1,     numN1    )
        realSet2 = Dio2DFDefine(outStream, 'R2', Dio_Var_Real,   numM2,     numN2    )

        dbleSet1 = Dio2DFDefine(outStream, 'D1', Dio_Var_Double, numM1Dble, numN1Dble)
        dbleSet2 = Dio2DFDefine(outStream, 'D2', Dio_Var_Double, numM2Dble, numN2Dble)

        intSet1  = Dio2DFDefine(outStream, 'I1', Dio_Var_Integer,numM1Int,  numN1Int )
        intSet2  = Dio2DFDefine(outStream, 'I2', Dio_Var_Integer,numM2Int,  numN2Int )

        write (*, *) 'OUT Datasets Created'

    !   Put OUT data for each timestep

        call Dio2DFPut (realSet1,realvalues1)
        call Dio2DFPut (dbleSet1,dbleValues1)

    !   Store current timestep

        curTimeSet = DioConstDefine(outStream, 'Time', Dio_Var_Double)

        currentTimeStep = DioDsTimeString2Julian('2002-03-04 23:55:44')
        call DioConstPut(curTimeSet, currentTimeStep)

    !   close data stream

        call DioStreamClose(outStream)

        write (*, *) 'Have closed OUT stream'
        ! write (*, *) 'Have closed OUT stream, GIVE ENTER'
        ! read (*, *)

    endif

end

