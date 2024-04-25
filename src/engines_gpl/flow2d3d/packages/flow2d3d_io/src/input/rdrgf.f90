!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
module m_rdrgf

private

!
! functions and subroutines
!
public rdrgf

contains

!> Read the grid point coordinates from a curvi-linear grd-file.
subroutine rdrgf(filrgf    ,lundia    ,error     ,nmax      ,mmax      , &
               & xcor      ,ycor      ,sferic    )
    use precision
    use string_module, only: remove_leading_spaces
    use system_utils, only: exifil
    ! 
    implicit none 
    ! 
! 
! Subroutine arguments
! 
    integer                                         :: lundia !< unit number for diagnostic output
    integer                           , intent(in)  :: mmax   !< number of grid points in the M-dir. specified in the mdf file  
    integer                           , intent(in)  :: nmax   !< number of grid points in the N-dir. specified in the mdf file  
    logical                                         :: error  !< flag=TRUE if an error is encountered 
    logical                           , intent(out) :: sferic !< flag=TRUE if grid in spherical coordinates
    real(fp)    , dimension(nmax,mmax)              :: xcor   !< x-coordinates of the grid points
    real(fp)    , dimension(nmax,mmax)              :: ycor   !< y-coordinates of the grid points
    character(*)                                    :: filrgf !< name of the grid file
! 
! Local variables 
! 
    integer                               :: ilen     !< length of grid file name
    integer                               :: lunrgf   !< unit number for the grid file
    integer                               :: m        !< loop index
    integer                               :: mc       !< number of grid points in the M-dir. specified in the grid file  
    integer                               :: n        !< loop index
    integer                               :: nc       !< number of grid points in the N-dir. specified in the grid file  
    integer                               :: pos      !< index of substring in another string
    logical                               :: kw_found !< flag indicating whether a keyword has been found
    real(fp)                              :: xymiss   !< missing value as read from file 
    character(256)                        :: rec      !< character var. containing one line
    character(10)                         :: dum      !< place holder for characters read from file that are not used/checked 
    character(2048)                       :: msg      !< character var. for longer error message
! 
!! executable statements ------------------------------------------------------- 
! 
    ! initialize local parameters 
    nc     = 0 
    mc     = 0 
    sferic = .false.
    error  = .false.

    ! check file existence 
    call remove_leading_spaces(filrgf    ,ilen      ) 
    error = .not.exifil(filrgf, lundia) 
    if (error) then
        call prterr(lundia, 'G004', 'grid file ' // filrgf)
        return
    endif

    open (newunit=lunrgf, file = filrgf(:ilen), form = 'formatted', status = 'old') 
    ! 
    ! Read file, check for end of file or error in file: 
    ! - The first line always contains comments 
    !   sferic is true when the first line contains the keyword Spherical 
    ! - Skip comment lines (starting with a '*'), while trying to read the 
    !   following keywords: 'Coordinate System' 
    !                       'Missing Value' 
    !   If 'Coordinate System' is present, it overrules the sferic-specification 
    !   in the first line! 
    ! - The next line contains the dimensions mc and nc 
    !   Parameter npart may also be on this line, but it is neglected 
    ! - Read the next line containing three zero's 
    !   xori, yori and alfori are not used anymore 
    ! - Read x coordinates 
    ! - Read y coordinates 
    ! 
    read (lunrgf, '(a)', end=7777, err=8888) rec 
    if (index(rec, 'Spherical')>=1 .or. index(rec, 'SPHERICAL')>=1) then 
       sferic = .true. 
    endif
    
    kw_found = .true.
    do while (kw_found)
       read(lunrgf,'(a)', end=7777, err=8888) rec 
       if (rec(1:1) == '*') cycle
       kw_found = .false. 
       ! 
       pos = index(rec,'Coordinate System') 
       if (pos >= 1) then 
          kw_found = .true. 
          if (index(rec(pos+1:),'spherical') >= 1 .or. & 
            & index(rec(pos+1:),'Spherical') >= 1 .or. & 
            & index(rec(pos+1:),'SPHERICAL') >= 1       ) then 
             sferic = .true. 
          else 
             sferic = .false. 
          endif 
       endif 
       ! 
       pos = index(rec,'Missing Value') 
       if (pos >= 1) then 
          kw_found = .true. 
          pos      = index(rec,'=') + 1 
          read(rec(pos:), *, err=8888) xymiss 
       endif 
    enddo
    ! 
    read(rec,*,err=8888)  mc,nc 
    if (mc /= mmax-1 .or. nc /= nmax-1) then
       error = .true.
       write(msg,'(A,I0,A,I0,3A,I0,A,I0,A)') 'Grid size (',mc,',',nc,') in file "',trim(filrgf), &
           & '" does not match the expected size (',mmax-1,',',nmax-1, &
           & ') based on the dimensions specified in the mdf-file'
       call prterr(lundia, 'P004', trim(msg)) 
       close (lunrgf) 
       return 
    endif

    ! nc > 9999 causes problem in reading format since the string "ETA=" and number will
    ! connect to one string whereas two strings are expected (see the two "dum" arguments
    ! in the read statements below)
    if (nc > 9999) then 
       error = .true. 
       call prterr(lundia, 'P004', 'in grid file: nmax larger than 9999 is not allowed') 
       close (lunrgf) 
       return 
    endif 
    
    ! read three zero's 
    read(lunrgf, '(a)', end=7777, err=8888) rec 

    ! unformatted read; the number of digits of xcor may vary 
    !  ETA=    N ...values...
    do n = 1, nc 
        read(lunrgf, *, end=7777, err=8888) dum, dum, (xcor(n,m), m=1,mc) 
        do m = 1, mc 
            if (isnan(xcor(n,m))) then
                error = .true.
                call prterr(lundia, 'G004', 'Grid file contains x-coordinate equal to NaN')
                close (lunrgf)
                return
            endif
        enddo 
    enddo 

    ! unformatted read; the number of digits of ycor may vary 
    do n = 1, nc 
        read(lunrgf, *, end=7777, err=8888) dum, dum, (ycor(n,m), m=1,mc) 
        do m = 1, mc 
            if (isnan(ycor(n,m))) then
                error = .true.
                call prterr(lundia, 'G004', 'Grid file contains y-coordinate equal to NaN')
                close (lunrgf)
                return
            endif
        enddo 
    enddo 
    close (lunrgf)
    return
    
    ! end of file while reading
 7777 continue
    error = .true. 
    call prterr(lundia, 'G006', 'grid file ' // filrgf)
    close (lunrgf)
    return
    
    ! error while reading
 8888 continue
    error = .true. 
    call prterr(lundia, 'G007', 'grid file ' // filrgf)
    close (lunrgf)
    return
end subroutine rdrgf 

end module m_rdrgf