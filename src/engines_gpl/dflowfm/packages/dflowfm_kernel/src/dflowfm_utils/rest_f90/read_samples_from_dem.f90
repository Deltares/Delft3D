!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

      subroutine read_samples_from_dem(filnam, jadoorladen)
    use dem
    use m_missing
    use m_samples
    implicit none
    character(len=*), intent(in) :: filnam
    integer, intent(in) :: jadoorladen
    integer :: ndraw
    COMMON /DRAWTHIS/ ndraw(50)

    integer :: i, j, is, istep
    type(DEMInfo) :: dem_info
    integer, allocatable :: arr(:,:)
    double precision, allocatable :: xarr(:,:), yarr(:,:)
    character(len=10) :: TEX


    call savesam()
    if (jadoorladen == 0) then
        ns = 0
    end if

    call read_dem_file(trim(filnam),dem_info, xarr, yarr, arr)
    if (dem_info%rows <= 0 .or. dem_info%cols <= 0) then
        call message('No samples read from file ', filnam, ' ')
        return
    end if

    call increasesam(ns + dem_info%rows*dem_info%cols)

    WRITE(TEX,'(I10)') dem_info%rows*dem_info%cols
    CALL READYY('Filtering '//TRIM(TEX)//' Samples Points',0d0)

    istep = int(dem_info%rows/100d0)
    do i=1,dem_info%rows
        do j=1,dem_info%cols
            if (arr(i,j) == NODATA) then
                continue
            else
                ns = ns + 1
                xs(ns) = xarr(i,j)
                ys(ns) = yarr(i,j)
                zs(ns) = dble(arr(i,j))
            end if
        end do
        IF (MOD(i,istep) .EQ. 0) THEN
            CALL READYY(' ',MIN( 1d0,dble(i)/dem_info%rows) )
        ENDIF
    end do
    deallocate(xarr, yarr, arr)
    CALL READYY(' ',-1d0)

    IF (NS .GT. 100000) NDRAW(32) = 7 ! Squares (faster than circles)
    IF (NS .GT. 500000) NDRAW(32) = 3 ! Small dots (fastest)

    WRITE(TEX,'(I10)') NS
    CALL READYY('Sorting '//TRIM(TEX)//' Samples Points',0d0)
    IF (NS .GT. 1) THEN
      CALL TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
      call get_samples_boundingbox()
      IPSTAT = IPSTAT_OK
    END IF
    CALL READYY(' ',-1d0)
   end subroutine read_samples_from_dem
