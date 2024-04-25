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

program test_nefis_restart_get

    use Dio_2dfield_Rw
    use Dio_const_Rw

    implicit none

    integer:: numM1
    integer:: numN1

    integer:: numM2
    integer:: numN2

    integer:: numM1Dble
    integer:: numN1Dble

    integer:: numM2Dble
    integer:: numN2Dble

    integer:: numM1Int
    integer:: numN1Int

    integer:: numM2Int
    integer:: numN2Int

    real,             dimension(:,:), pointer :: realValues1
    real,             dimension(:,:), pointer :: realValues2

    double precision, dimension(:,:), pointer :: dbleValues1
    double precision, dimension(:,:), pointer :: dbleValues2

    integer,          dimension(:,:), pointer :: intValues1
    integer,          dimension(:,:), pointer :: intValues2

    type(DioStreamType) :: inStream
    type(Dio2DFType)    :: realSet1, realSet2
    type(Dio2DFType)    :: dbleSet1, dbleSet2
    type(Dio2DFType)    :: intSet1 , intSet2

    type(DioConstType)  :: curTimeSet

    integer :: i, dioError

    integer :: errNr
    character(Len=DioMaxErrMsgLen) :: errMsg
    character(Len=DioMaxStreamLen) :: resFileName = 'TESTNefisRestart-res.txt'
    integer :: resLun

    double precision :: currentTimeStep


    open(newunit=resLun,file=resFileName)

!   Initialize dio library
    call DioInit

!   Open data stream

    inStream = DioStreamCreateSynched(dio_Nefis_stream, 'TESTRestart', 'r', .false.)
    if ( .not. DioStreamOpenedOK(inStream) ) then
        errNr = DioGetLastError()
        errMsg = DioGetLastErrorMsg()
        write(resLun, *) 'DIOTESTERROR', errNr, ': ', trim(errMsg)
    else
        write (resLun, *) 'IN Stream Created'

    !   Create IN data set

        realSet1 = Dio2DFGetDataset(inStream, 'R1')
        dioError = DioGetLastError()
        if ( dioError .ne. 0 ) then
            write(resLun, *) 'DIOTESTERROR:', dioError, ' ', trim(DioGetLastErrorMsg())
        else
            if (Dio2DFGet (realSet1,realvalues1) ) then
                do i = 1,size(realValues1,1)
                    write(resLun, *) realValues1(i,:)
                enddo
            else
                write(resLun, *) 'DIOTESTERROR:', DioGetLastError(),&
                                         ' ', trim(DioGetLastErrorMsg())
            endif
        endif

        realSet2 = Dio2DFGetDataset(inStream, 'R2')
        dioError = DioGetLastError()
        if ( dioError .ne. 0 ) then
            write(resLun, *) 'DIOTESTERROR:', dioError, ' ', trim(DioGetLastErrorMsg())
        else
            if (Dio2DFGet (realSet2,realvalues2) ) then
                do i = 1,size(realValues2,1)
                    write(resLun, *) realValues2(i,:)
                enddo
            else
                write(resLun, *) 'DIOTESTERROR:', DioGetLastError(),&
                                         ' ', trim(DioGetLastErrorMsg())
            endif
        endif

        dbleSet1 = Dio2DFGetDataset(inStream, 'D1')
        dioError = DioGetLastError()
        if ( dioError .ne. 0 ) then
            write(resLun, *) 'DIOTESTERROR:', dioError, ' ', trim(DioGetLastErrorMsg())
        endif

        dbleSet2 = Dio2DFGetDataset(inStream, 'D2')
        if ( dioError .ne. 0 ) then
            write(resLun, *) 'DIOTESTERROR:', dioError, ' ', trim(DioGetLastErrorMsg())
        endif

        intSet1  = Dio2DFGetDataset(inStream, 'I1')
        if ( dioError .ne. 0 ) then
            write(resLun, *) 'DIOTESTERROR:', dioError, ' ', trim(DioGetLastErrorMsg())
        endif

        intSet2  = Dio2DFGetDataset(inStream, 'I2')
        if ( dioError .ne. 0 ) then
            write(resLun, *) 'DIOTESTERROR:', dioError, ' ', trim(DioGetLastErrorMsg())
        endif

        write (resLun, *) 'IN Datasets Retrieved'

    !   get current timestep
        curTimeSet = DioConstGetDataset(inStream, 'Time')

        dioError = DioGetLastError()
        if ( dioError .ne. 0 ) then
            write(resLun, *) 'DIOTESTERROR:', dioError, ' ', trim(DioGetLastErrorMsg())
        else
            if ( DioConstGet(curTimeSet, currentTimeStep) ) then
                write(resLun, *) 'TIME STAMP: ', currentTimeStep, &
                        '(', DioDsJulian2DioTime(currentTimeStep), ')'
            else
                write(resLun, *) 'DIOTESTERROR:', DioGetLastError(),&
                                         ' ', trim(DioGetLastErrorMsg())
            endif
        endif

    !   close data stream

        call DioStreamClose(inStream)

    endif

    write (resLun, *) 'Have closed IN stream'

    close(resLun)


end

