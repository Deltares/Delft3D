subroutine edyfil(lundia    ,error     ,filedy    ,fmttmp    ,nmax      , &
                & mmax      ,nmaxus    ,kmax      ,lstsci    ,vicuv     , &
                & dicuv     ,gdp       )
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
!    Function: Reads the eddy viscosity arrays VICUV and eddy
!              diffusity DICUV if LMAC > 0 from the attribute
!              file
! Method used:
!
!    Master reads, unformatted or formatted, arrays VICUV and DICUV for all grid points,
!    then broadcast values to points in each partition.
!    Finally check on NaN's.
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
    integer, pointer                      :: mfg 
    integer, pointer                      :: mlg 
    integer, pointer                      :: nfg 
    integer, pointer                      :: nlg 
    integer, pointer                      :: mmaxgl 
    integer, pointer                      :: nmaxgl 
!
! Global variables
!
    integer                                                                      , intent(in)  :: kmax   !!  Number of layers in the z-dir.
    integer                                                                      , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: lundia !  Description and declaration in inout.igs
    integer                                                                      , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                                      , intent(out) :: error  !  Flag=TRUE if an error is encountered
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 2) , intent(out) :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 2) , intent(out) :: vicuv  !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                 , intent(in)  :: filedy !  Name of the relevant file
    character(11)                                                                , intent(in)  :: fmttmp !  Help var. for the attribute file formats (eg. the grid file)
!
! Local variables
!
    integer           :: iocond  ! IO status for reading
    integer           :: istat   ! IO status for allocate
    integer           :: kbg     ! denotes the k-index of vicuv/dicuv containing the background values
    integer           :: lfile   ! Help var. specifying the length of character variables for file names 
    integer           :: luntmp  ! Help var. for a unit number of an attribute file 
    integer           :: m       ! Help (loop) var. for M-index 
    integer           :: n       ! Help (loop) var. for N-index 
    integer, external :: newlun
    character(300)    :: message
    real(fp), dimension(:,:), allocatable :: tmp   ! Temporary array containing dicuv/vicuv of entire domain 
!
!! executable statements -------------------------------------------------------
!
    !
    mfg    => gdp%gdparall%mfg 
    mlg    => gdp%gdparall%mlg 
    nfg    => gdp%gdparall%nfg 
    nlg    => gdp%gdparall%nlg 
    mmaxgl => gdp%gdparall%mmaxgl 
    nmaxgl => gdp%gdparall%nmaxgl
    !
    kbg = kmax + 1
    !
    ! Test file existence and if so read
    !
    lfile = len(filedy)
    !
    if (exifil(filedy, lundia)) then
       ! 
       ! allocate temporary array to store data of entire domain read from file 
       ! 
       allocate(tmp(nmaxgl,mmaxgl), stat = istat)
       istat = abs(istat)
       call dfreduce_gdp( istat, 1, dfint, dfmax, gdp )
       if (istat /= 0) then
          call prterr(lundia, 'G020', 'vicuv/dicuv')
          error = .true.
          return
       endif
       !
       if (inode == master) then
          open (newunit=luntmp, file = filedy(1:lfile), form = fmttmp, status = 'old')
       endif
       !
       ! Read records with horizontal eddy-viscosity, for each row one record
       !
       if (inode == master) then
          if (fmttmp(1:2) == 'un') then
             read (luntmp, iostat = iocond) ((tmp(n, m), m = 1, mmaxgl), n = 1, nmaxgl)
          else
             !
             ! Freeformatted file, skip lines starting with a '*' before reading actual data
             !
             call skipstarlines(luntmp)
             read (luntmp, *, iostat = iocond) ((tmp(n, m), m = 1, mmaxgl), n = 1, nmaxgl)
          endif
       endif
       call dfbroadc_gdp(iocond, 1, dfint, gdp)
       if (iocond /= 0) then
          if (iocond < 0) then
             call prterr(lundia, 'G006', filedy(1:lfile))
          else
             call prterr(lundia, 'G007', filedy(1:lfile))
          endif
          error = .true.
          goto 200
       endif
       call dfbroadc_gdp(tmp, mmaxgl*nmaxgl, dfloat, gdp)
       do m = mfg, mlg 
          do n = nfg, nlg 
             vicuv(n-nfg+1,m-mfg+1,kbg) = tmp(n,m) 
          enddo 
       enddo
       !
       ! Read records with horizontal eddy-diffusity if (lstsci > 0), for each row one record
       !
       if (lstsci > 0) then
          if (inode == master) then
             if (fmttmp(1:2) == 'un') then
                read (luntmp, iostat = iocond) ((tmp(n, m), m = 1, mmaxgl), n = 1, nmaxgl)
             else
                read (luntmp, *, iostat = iocond) ((tmp(n, m), m = 1, mmaxgl), n = 1, nmaxgl)
             endif
          endif
          call dfbroadc_gdp(iocond, 1, dfint, gdp)
          if (iocond /= 0) then
             if (iocond < 0) then
                call prterr(lundia, 'G006', filedy(1:lfile))
             else
                call prterr(lundia, 'G007', filedy(1:lfile))
             endif
             error = .true.
          endif
          call dfbroadc_gdp(tmp, mmaxgl*nmaxgl, dfloat, gdp)
          do m = mfg, mlg 
             do n = nfg, nlg 
                dicuv(n-nfg+1,m-mfg+1,kbg) = tmp(n,m)
             enddo
          enddo
       endif
       !
       ! Stop reading file
       !
       ! If a NaN is read -> error
       !
       do m = 1, mmax
          do n = 1, nmaxus
             if ( isnan(vicuv(n, m, kbg)) ) then
                write(message,'(2a)') 'NaN found in horizontal eddy-viscosity in file ',filedy
                call prterr(lundia, 'P004', message)
                !
                error = .true.
                goto 200
             endif
          enddo
       enddo
       do m = 1, mmax
          do n = 1, nmaxus
             if ( isnan(dicuv(n, m, kbg)) ) then
                write(message,'(2a)') 'NaN found in horizontal eddy-diffusity in file ',filedy
                call prterr(lundia, 'P004', message)
                !
                error = .true.
                goto 200
             endif
          enddo
       enddo
       !
       ! Close file and deallocate
       !
  200  continue
       deallocate(tmp)
       if (inode == master) then
          close (luntmp)
       endif
       !
    else
       !
       ! File does not exist
       !
       error = .true.
    endif
end subroutine edyfil
