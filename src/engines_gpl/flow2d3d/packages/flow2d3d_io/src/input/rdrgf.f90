subroutine rdrgf(filrgf    ,lundia    ,error     ,nmax      ,mmax      , &
               & xcor      ,ycor      ,sferic    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
!  
!  
!!--description----------------------------------------------------------------- 
! 
!    Function: Reads the coordinates of the depth points from the 
!              grid file 
! Method used: 
! 
!!--pseudo code and references-------------------------------------------------- 
! NONE 
!!--declarations---------------------------------------------------------------- 
    use precision
    use globaldata
    use string_module, only: remove_leading_spaces
    use dfparall
    use system_utils, only: exifil
    ! 
    implicit none 
    ! 
    type(globdat),target :: gdp 
    ! 
    ! The following list of pointer parameters is used to point inside the gdp structure 
    ! 
    integer, pointer :: mfg 
    integer, pointer :: mlg 
    integer, pointer :: nfg 
    integer, pointer :: nlg 
    integer, pointer :: mmaxgl 
    integer, pointer :: nmaxgl 
! 
! Global variables 
! 
    integer                                                                                                             :: lundia !  Description and declaration in inout.igs 
    integer                                                                                               , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90 
    integer                                                                                               , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90 
    logical                                                                                                             :: error  !!  Flag=TRUE if an error is encountered 
    logical                                                                                               , intent(out) :: sferic !  Description and declaration in tricom.igs 
    real(fp)    , dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)              :: xcor   !  Description and declaration in esm_alloc_real.f90 
    real(fp)    , dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)              :: ycor   !  Description and declaration in esm_alloc_real.f90 
    character(*)                                                                                                        :: filrgf !!  File name for the curvi-linear grid file (telmcrgf.xxx) 
! 
! Local variables 
! 
    integer                               :: isfer
    integer, dimension(3)                 :: ival
    integer                               :: i        ! Complete integer array  
    integer                               :: ilen     ! Help var.  
    integer                               :: lenrec   ! Help var. 
    integer                               :: j        ! Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1  
    integer                               :: lunrgf   ! Unit nr. for the curvi-linear grid file (telmcrgf.xxx)  
    integer                               :: mc       ! Number of grid points in the M-dir. specified in the curvi linear grid file  
    integer                               :: nc       ! Number of grid points in the N-dir. specified in the curvi linear grid file  
    integer                    , external :: newlun 
    integer                               :: pos 
    logical                               :: kw_found 
    real(fp)                              :: xymiss 
    real(fp), dimension(:,:), allocatable :: xtmp     ! temporary array containing xcor of entire domain 
    real(fp), dimension(:,:), allocatable :: ytmp     ! temporary array containing ycor of entire domain 
    character(256)                        :: errmsg   ! Character var. containing the errormessage to be written to file. The message depends on the error.  
    character(256)                        :: rec 
    character(4)                          :: errornr  ! Number of the errormessage which will be printed in case of error 
    character(10)                         :: dum 
! 
!! executable statements ------------------------------------------------------- 
! 
    mfg    => gdp%gdparall%mfg 
    mlg    => gdp%gdparall%mlg 
    nfg    => gdp%gdparall%nfg 
    nlg    => gdp%gdparall%nlg 
    nmaxgl => gdp%gdparall%nmaxgl 
    mmaxgl => gdp%gdparall%mmaxgl
    !
    ! initialize local parameters 
    ! 
    nc     = 0 
    mc     = 0 
    errornr = 'G004' 
    errmsg = 'grid file ' // filrgf 
    sferic = .false. 
    ! 
    ! check file existence 
    ! 
    call remove_leading_spaces(filrgf    ,ilen      ) 
    error = .not.exifil(filrgf, lundia) 
    if (error) goto 9999 
    ! 
    ! the master opens and reads the grid file 
    ! 
    if ( inode /= master ) goto 50 
    ! 
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
    read (lunrgf, '(a)', end = 7777, err = 8888) rec 
    if (index(rec, 'Spherical')>=1 .or. index(rec, 'SPHERICAL')>=1) then 
       sferic = .true. 
    endif 
10  continue 
       kw_found = .false. 
       read(lunrgf,'(a)',end = 7777,err=8888) rec 
       if (rec(1:1) == '*') goto 10 
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
          read(rec(pos:),*,err=8888) xymiss 
       endif 
    if (kw_found) goto 10 
    ! 
    if (sferic) then 
       write (lundia, *) 
       write (lundia, '(a)') 'Coordinate System: Spherical' 
       write (lundia, *) 
    endif 
    read(rec,*,err=8888)  mc,nc 
    ! 
    ! nc > 9999 can not be handled by Delft3D-FLOW 
    ! causes problem in reading format  
    ! file must be in netcdf format in this case 
    ! 
    if (nc > 9999) then 
       error = .true. 
       call prterr(lundia, 'P004', 'in grid file: Nmax larger than 9999 is not allowed ' // & 
                        & 'supply grid in Netcdf format') 
       close (lunrgf) 
       return 
    endif 
    ! 
    ! allocate temporary arrays to store coordinates read from grid file 
    ! 
    allocate (xtmp(nmaxgl,mmaxgl)) 
    allocate (ytmp(nmaxgl,mmaxgl)) 
    xtmp = 0. 
    ytmp = 0. 
    ! 
    ! read three zero's 
    ! 
    read(lunrgf, '(a)', end = 7777, err=8888) rec 
    ! 
    ! read XD 
    ! read unformatted: The number of digits of xcor may vary 
    ! 
    do j = 1, nc 
        read (lunrgf, *, end = 7777, err = 8888) dum, dum, (xtmp(j,i), i=1,mc) 
        do i = 1, mc 
            if (isnan(xtmp(j,i))) goto 6666 
        enddo 
    enddo 
    ! 
    ! read YD 
    ! read unformatted: The number of digits of ycor may vary 
    ! 
    do j = 1, nc 
       read (lunrgf, *, end = 7777, err = 8888) dum, dum, (ytmp(j, i), i=1,mc) 
       do i = 1, mc 
            if (isnan(ytmp(j,i))) goto 6666 
       enddo 
    enddo 
    close (lunrgf) 
    ! 
    if (sferic) then 
       isfer = 1 
    else 
       isfer = 0 
    endif 
    ival(1) = isfer 
    ival(2) = mc 
    ival(3) = nc 
 50 continue 
    ! 
    ! scatter integer array to all nodes and determine sferic and dimensions 
    ! 
    call dfbroadc_gdp ( ival, 3, dfint, gdp ) 
    !   isfer = ival(1) 
    mc    = ival(2) 
    nc    = ival(3) 
    ! 
    sferic = (ival(1) == 1) 
    ! 
    if (sferic) then 
       write (lundia, *) 
       write (lundia, '(a)') 'Coordinate System: Spherical' 
       write (lundia, *) 
    endif 
    ! 
    ! allocate temporary arrays to store coordinates (at all nodes except master) 
    ! 
    if ( inode /= master ) then 
       allocate (xtmp(nmaxgl,mmaxgl)) 
       allocate (ytmp(nmaxgl,mmaxgl)) 
       xtmp = 0. 
       ytmp = 0. 
    endif 
    ! 
    ! scatter arrays xtmp and ytmp to all nodes 
    ! 
    call dfbroadc_gdp ( xtmp, nmaxgl*mmaxgl, dfloat, gdp ) 
    call dfbroadc_gdp ( ytmp, nmaxgl*mmaxgl, dfloat, gdp ) 
    ! 
    ! put copies of parts of xcor, ycor for each subdomain 
    ! 
    call dfsync ( gdp ) 
    do j = mfg, mlg 
       do i = nfg, nlg 
          xcor(i-nfg+1,j-mfg+1) = xtmp(i,j) 
          ycor(i-nfg+1,j-mfg+1) = ytmp(i,j) 
       enddo 
    enddo 
    deallocate(xtmp,ytmp) 
    goto 9999 
    ! 
    ! test for reading error: label 7777 end of file 
    !                                8888 error while reading 
    ! 
 6666 continue 
    error = .true. 
    errornr = 'P004' 
    errmsg = 'Grid file contains NaN' 
    goto 9999 
 7777 continue 
    error = .true. 
    errornr = 'G006' 
    goto 9999 
 8888 continue 
    error = .true. 
    errornr = 'G007' 
    ! 
 9999 continue 
    if (error) call prterr(lundia, errornr, errmsg)
end subroutine rdrgf 
