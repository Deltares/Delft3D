subroutine hybfil(lundia    ,error     ,filrgh    ,fmttmp    ,nmax      , &
                & mmax      ,nmaxus    ,cfurou    ,cfvrou    ,gdp       )
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
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: Reads the bed stress coefficient arrays CFUROU
!              [array (r(ja(10)))] and CFVROU [array (r(ja(11)))]
!              from the attribute file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, pointer               :: mfg
    integer, pointer               :: mlg
    integer, pointer               :: nfg
    integer, pointer               :: nlg
    integer, pointer               :: mmaxgl
    integer, pointer               :: nmaxgl
!
! Global variables
!
    integer                                                                       :: lundia !  Description and declaration in inout.igs
    integer                                                         , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                         , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3), intent(out) :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3), intent(out) :: cfvrou !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                  :: filrgh !!  Name of the relevant file
    character(11)                                                   , intent(in)  :: fmttmp !!  Help var. for the attribute file formats (eg. the grid file)
!
! Local variables
!
    integer                               :: iocond   ! IO status for reading 
    integer                               :: lfile    ! Help var. specifying the length of character variables for file names 
    integer                               :: luntmp   ! Help var. for a unit number of an attribute file 
    integer                               :: m        ! Help (loop) var. for M-index 
    integer                               :: n        ! Help (loop) var. for N-index 
    integer, external                     :: newlun
    real(fp), dimension(:,:), allocatable :: ctmp     ! temporary array containing roughness of entire domain
!
!! executable statements -------------------------------------------------------
!
    mfg    => gdp%gdparall%mfg
    nfg    => gdp%gdparall%nfg
    mlg    => gdp%gdparall%mlg
    nlg    => gdp%gdparall%nlg
    mmaxgl => gdp%gdparall%mmaxgl
    nmaxgl => gdp%gdparall%nmaxgl
    !
    ! test file existence and if so read
    !
    lfile = len(filrgh)
    !
    if (exifil(filrgh, lundia)) then
       if (inode==master) then
          open (newunit=luntmp, file = filrgh(1:lfile), form = fmttmp, status = 'old')
       endif
       !
       ! allocate temporary array to store roughness of entire domain read from file
       !
       allocate (ctmp(nmaxgl,mmaxgl))
       !
       !
       ! unformatted file
       ! read record with bottom roughnesses
       ! for each row one record
       ! NOTE: nmaxus and mmax equal nmaxgl and mmaxgl, respectively (for entire domain)
       !       in case of parallel runs. Moreover, cfurou is associated with subdomain and
       !       hence, cfurou for entire domain is stored in temporary array ctmp
       !
       if (fmttmp(1:2)=='un') then
          do n = 1, nmaxgl
             if (inode==master) read (luntmp, iostat = iocond) (ctmp(n, m), m = 1, mmaxgl)
             call dfbroadc_gdp(iocond, 1, dfint, gdp)
             if (iocond/=0) then
                if (iocond<0) then
                   call prterr(lundia    ,'G006'    ,filrgh(1:lfile)      )
                else
                   call prterr(lundia    ,'G007'    ,filrgh(1:lfile)      )
                endif
                error = .true.
                goto 200
             endif
          enddo
          !
          ! send temporary array to other nodes
          !
          call dfbroadc_gdp(ctmp, mmaxgl*nmaxgl, dfloat, gdp)
          !
          ! put copies of parts of cfurou for each subdomain
          !
          do m = mfg, mlg
             do n = nfg, nlg
                cfurou(n-nfg+1,m-mfg+1,1) = ctmp(n,m)
             enddo
          enddo
          !
          do n = 1, nmaxgl
             if (inode==master) read (luntmp, iostat = iocond) (ctmp(n, m), m = 1, mmaxgl)
             call dfbroadc_gdp(iocond, 1, dfint, gdp)
             if (iocond/=0) then
                if (iocond<0) then
                   call prterr(lundia    ,'G006'    ,filrgh(1:lfile)      )
                else
                   call prterr(lundia    ,'G007'    ,filrgh(1:lfile)      )
                endif
                error = .true.
                exit
             endif
          enddo
          !
          ! send temporary array to other nodes
          !
          call dfbroadc_gdp(ctmp, mmaxgl*nmaxgl, dfloat, gdp)
          !
          ! put copies of parts of cfvrou for each subdomain
          !
          do m = mfg, mlg
             do n = nfg, nlg
                cfvrou(n-nfg+1,m-mfg+1,1) = ctmp(n,m)
             enddo
          enddo
       !
       ! stop reading file
       !
       else
          !
          ! freeformatted file, skip lines starting with a '*'
          !
          if (.not.parll .or. (parll .and. inode == master)) call skipstarlines(luntmp )
          !
          ! read record with bottom roughnesses
          ! for each row one record
          ! NOTE: nmaxus and mmax equal nmaxgl and mmaxgl, respectively (for entire domain)
          !       in case of parallel runs. Moreover, cfurou is associated with subdomain and
          !       hence, cfurou for entire domain is stored in temporary array ctmp
          !
          do n = 1, nmaxgl
             if (inode==master) read (luntmp, *, iostat = iocond) (ctmp(n, m), m = 1, mmaxgl)
             call dfbroadc_gdp(iocond, 1, dfint, gdp)
             if (iocond/=0) then
                if (iocond<0) then
                   call prterr(lundia    ,'G006'    ,filrgh(1:lfile)      )
                !
                else
                   call prterr(lundia    ,'G007'    ,filrgh(1:lfile)      )
                !
                endif
                error = .true.
                goto 200
             endif
          enddo
          !
          ! send temporary array to other nodes
          !
          call dfbroadc_gdp(ctmp, mmaxgl*nmaxgl, dfloat, gdp)
          !
          ! put copies of parts of cfurou for each subdomain
          !
          do m = mfg, mlg
             do n = nfg, nlg
                cfurou(n-nfg+1,m-mfg+1,1) = ctmp(n,m)
             enddo
          enddo
          !
          do n = 1, nmaxgl
             if (inode==master) read (luntmp, *, iostat = iocond) (ctmp(n, m), m = 1, mmaxgl)
             call dfbroadc_gdp(iocond, 1, dfint, gdp)
             if (iocond/=0) then
                if (iocond<0) then
                   call prterr(lundia    ,'G006'    ,filrgh(1:lfile)      )
                else
                   call prterr(lundia    ,'G007'    ,filrgh(1:lfile)      )
                endif
                error = .true.
                exit
             endif
          enddo
          !
          ! send temporary array to other nodes
          !
          call dfbroadc_gdp(ctmp, mmaxgl*nmaxgl, dfloat, gdp)
          !
          ! put copies of parts of cfvrou for each subdomain
          !
          do m = mfg, mlg
             do n = nfg, nlg
                cfvrou(n-nfg+1,m-mfg+1,1) = ctmp(n,m)
             enddo
          enddo
       !
       ! stop reading file
       !
       endif
       !
       ! close file
       !
  200  continue
       if (inode==master) close (luntmp)
       deallocate(ctmp)
    !
    ! test file existence <NO>
    !
    else
       error = .true.
    endif
end subroutine hybfil
