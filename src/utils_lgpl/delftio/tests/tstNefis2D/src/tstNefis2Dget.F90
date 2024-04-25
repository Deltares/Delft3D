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
program test_get_dio_f90

    use Dio_2dfield_Rw

    real,             dimension(:,:), pointer :: realValues
    double precision, dimension(:,:), pointer :: dbleValues

    type(DioStreamType) :: inStream
    type(Dio2DFType)  :: inSet
    type(Dio2DFType)  :: inSetDble

    integer :: i,resLun

!   Initialize dio library
    call DioInit

!   Open file for storing results

    open(newunit=resLun,file='TESTNefis2D-res.txt')

!   Open data stream

    write (*, *) 'Opening IN Stream'
    inStream = DioStreamCreateSynched(dio_Nefis_stream, 'TESTNefis', 'r', .false.)
    write (*, *) 'IN Streams Created'

!   Get OUT data set

    inSet = Dio2DFGetDataset(inStream, 'R1')
    inSetDble = Dio2DFGetDataset(inStream, 'DP1')
    write (resLun, *) 'Num M inSet ',       Dio2DFGetNumM(inSet)
    write (resLun, *) 'Num N inSet ',       Dio2DFGetNumN(inSet)
    write (resLun, *) 'Num M inSetDble ', Dio2DFGetNumM(inSetDble)
    write (resLun, *) 'Num N inSetDble ', Dio2DFGetNumN(inSetDble)
    write (*, *) 'IN Datasets Initialized'

!   Get Values

    do i = 1, 200
        if ( Dio2DFGet (inSet, realValues) ) then
            if ( i .eq. 1 .or. mod(i,10) .eq. 0 ) then
                write (resLun, *) 'Got inSet reals for Step: ', i
                write(resLun,*) realValues
            endif
        else
            write (resLun, *) 'Did NOT get inSet reals for Step: ', i
        endif
        if ( Dio2DFGet (inSetDble, dbleValues) ) then
            if ( i .eq. 1 .or. mod(i,10) .eq. 0 ) then
                write (resLun, *) 'Got inSetDble doubles for Step: ', i
                write(resLun,*) dbleValues
            endif
        else
            write (resLun, *) 'Did NOT get inSetDble doubles for Step: ', i
        endif
    end do
    
!   Close data stream

    call DioStreamClose(inStream)

!   Close file for with results
    close(resLun)

    write (*, *) 'Got Everything'
    ! write (*, *) 'Got Everything, GIVE ENTER'
    ! read(*,*)
end

