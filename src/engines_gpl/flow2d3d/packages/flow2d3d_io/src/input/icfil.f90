subroutine icfil(lundia    ,error     ,filic     ,fmttmp    ,mmax      , &
               & nmax      ,nmaxus    ,kmax      ,lstsci    ,s1        , &
               & u1        ,v1        ,r1        ,gdp       )
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
!    Function: Reads initial field conditions record from an
!              (single precision) unformatted or formatted file
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
    integer, pointer  :: mfg
    integer, pointer  :: mlg
    integer, pointer  :: nfg
    integer, pointer  :: nlg
    integer, pointer  :: mmaxgl
    integer, pointer  :: nmaxgl
!
! Global variables
!
    integer                                                                         , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                         , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                         , intent(in)  :: lundia !  Description and declaration in inout.igs
    integer                                                                         , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                       :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                         , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                                         , intent(out) :: error  !  Flag=TRUE if an error is encountered
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(out) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(out) :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(out) :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci) , intent(out) :: r1     !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                                  :: filic  !  File name of initial condition file
    character(11)                                                                   , intent(in)  :: fmttmp !  File format of initial condition file
!
! Local variables
!
    integer                                     :: iocond       ! IO status for reading 
    integer                                     :: k            ! Help var. 
    integer                                     :: l            ! Help var. 
    integer                                     :: lfile        ! Help var. specifying the length of character variables for files 
    integer                                     :: luntmp       ! Unit number file 
    integer                                     :: m            ! Help var. 
    integer                                     :: n            ! Help var. 
    integer, external                           :: newlun
    real(sp), allocatable, dimension(:,:,:,:)   :: sbuff        !  Single precision buffer to read from file
    logical                                     :: test
    character(300)                              :: message
!
!
!! executable statements -------------------------------------------------------
!
    mfg         => gdp%gdparall%mfg
    mlg         => gdp%gdparall%mlg
    nfg         => gdp%gdparall%nfg
    nlg         => gdp%gdparall%nlg
    mmaxgl      => gdp%gdparall%mmaxgl
    nmaxgl      => gdp%gdparall%nmaxgl
    !
    ! Test file existence and if so read
    !
    lfile = len(filic)
    !
    if (exifil(filic, lundia)) then
       if (inode == master) then
          open (newunit=luntmp, file = filic(1:lfile), form = fmttmp, status = 'old')
       endif
       !
       ! Unformatted file
       !
       !allocate(sbuff(nmaxus, mmax, 0:kmax, max(1, lstsci)))
       allocate(sbuff(nmaxgl, mmaxgl, 1:kmax, max(1, lstsci)))
       !
       if (fmttmp(:2) == 'un') then
          !
          ! Read non-uniform initial conditions per nmaxus mmax values in s1 array
          !
          !read (luntmp, iostat = iocond) ((sbuff(n, m, 1, 1), m = 1, mmax), n = 1, nmaxus)
          if (inode == master) then
             read (luntmp, iostat = iocond) ((sbuff(n, m, 1, 1), m = 1, mmaxgl), n = 1, nmaxgl)
          endif
          call dfbroadc_gdp(iocond, 1, dfint, gdp)
          if (iocond /= 0) then
             if (iocond < 0) then
                call prterr(lundia, 'G006', filic(:lfile))
             else
                call prterr(lundia, 'G007', filic(:lfile))
             endif
             error = .true.
             goto 200
          endif
          !s1(1:nmaxus,1:mmax) = sbuff(1:nmaxus,1:mmax,1,1)
          call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*kmax*max(1,lstsci), dfreal, gdp)
          do m = mfg, mlg
             do n = nfg, nlg
                s1(n-nfg+1,m-mfg+1) = sbuff(n,m,1,1)
             enddo
          enddo
          !
          ! Per layer k: nmaxus mmax values in u1 array
          !
          do k = 1, kmax
             if (inode == master) read (luntmp, iostat = iocond) ((sbuff(n, m, k, 1), m = 1, mmaxgl), n = 1, nmaxgl)
             call dfbroadc_gdp(iocond, 1, dfint, gdp)
             if (iocond /= 0) then
                if (iocond < 0) then
                   call prterr(lundia, 'G006', filic(:lfile))
                else
                   call prterr(lundia, 'G007', filic(:lfile))
                endif
                error = .true.
                goto 200
             endif
          enddo
          !u1(1:nmaxus,1:mmax,1:kmax) = sbuff(1:nmaxus,1:mmax,1:kmax,1) 
          call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*kmax*max(1,lstsci), dfreal, gdp)
          do m = mfg, mlg
             do n = nfg, nlg
                u1(n-nfg+1,m-mfg+1,1:kmax) = sbuff(n,m,1:kmax,1)
             enddo
          enddo
          !
          ! Per layer k: nmaxus mmax values in v1 array
          !
          do k = 1, kmax
             if (inode == master) read (luntmp, iostat = iocond) ((sbuff(n, m, k, 1), m = 1, mmaxgl), n = 1, nmaxgl)
             call dfbroadc_gdp(iocond, 1, dfint, gdp)
             if (iocond /= 0) then
                if (iocond < 0) then
                   call prterr(lundia, 'G006', filic(:lfile))
                else
                   call prterr(lundia, 'G007', filic(:lfile))
                endif
                error = .true.
                goto 200
             endif
          enddo
          !v1(1:nmaxus,1:mmax,1:kmax) = sbuff(1:nmaxus,1:mmax,1:kmax,1) 
          call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*kmax*max(1,lstsci), dfreal, gdp)
          do m = mfg, mlg
             do n = nfg, nlg
                v1(n-nfg+1,m-mfg+1,1:kmax) = sbuff(n,m,1:kmax,1)
             enddo
          enddo
          !
          ! Per constituent l: kmax nmaxus mmax values in r1 array
          ! Only salinity, temperature, real constituents and secondary flow; no turbulence
          !
          if (lstsci > 0) then
             do l = 1, lstsci
                do k = 1, kmax
                   if (inode == master) read (luntmp, iostat = iocond) &
                      & ((sbuff(n, m, k, l), m = 1, mmaxgl), n = 1, nmaxgl)
                   call dfbroadc_gdp(iocond, 1, dfint, gdp)
                   if (iocond /= 0) then
                      if (iocond < 0) then
                         call prterr(lundia, 'G006', filic(:lfile))
                      else
                         call prterr(lundia, 'G007', filic(:lfile))
                      endif
                      error = .true.
                      goto 200
                   endif
                enddo
             enddo
             !r1(1:nmaxus,1:mmax,1:kmax,1:lstsci) = sbuff(1:nmaxus,1:mmax,1:kmax,1:lstsci) 
             call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*kmax*max(1,lstsci), dfreal, gdp)
             do m = mfg, mlg
                do n = nfg, nlg
                   r1(n-nfg+1,m-mfg+1,1:kmax,1:lstsci) = sbuff(n,m,1:kmax,1:lstsci)
                enddo
             enddo
          endif
          !
          ! Stop reading file
          !
       else
          !
          ! Freeformatted file, skip lines starting with a '*'
          !
          if (inode == master) call skipstarlines(luntmp    )
          !
          ! Read non-uniform initial conditions per nmaxus mmax values in s1 array
          !
          if (inode == master) read (luntmp, *, iostat = iocond) ((sbuff(n, m, 1, 1), m = 1, mmaxgl), n = 1, nmaxgl)
          call dfbroadc_gdp(iocond, 1, dfint, gdp)
          if (iocond /= 0) then
             if (iocond < 0) then
                call prterr(lundia, 'G006', filic(:lfile))
             else
                call prterr(lundia, 'G007', filic(:lfile))
             endif
             error = .true.
             goto 200
          endif
          call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*kmax*max(1,lstsci), dfreal, gdp)
          do m = mfg, mlg
             do n = nfg, nlg
                s1(n-nfg+1,m-mfg+1) = sbuff(n,m,1,1)
             enddo
          enddo
          !
          ! Per layer k: nmaxus mmax values in u1 array
          !
          do k = 1, kmax
             if (inode == master) read (luntmp, *, iostat = iocond) ((sbuff(n, m, k, 1), m = 1, mmaxgl), n = 1, nmaxgl)
             call dfbroadc_gdp(iocond, 1, dfint, gdp)
             if (iocond /= 0) then
                if (iocond < 0) then
                   call prterr(lundia, 'G006', filic(:lfile))
                else
                   call prterr(lundia, 'G007', filic(:lfile))
                endif
                error = .true.
                goto 200
             endif
          enddo
          call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*kmax*max(1,lstsci), dfreal, gdp)
          do m = mfg, mlg
             do n = nfg, nlg
                u1(n-nfg+1,m-mfg+1,1:kmax) = sbuff(n,m,1:kmax,1)
             enddo
          enddo
          !
          ! Per layer k: nmaxus mmax values in v1 array
          !
          do k = 1, kmax
             if (inode == master) read (luntmp, *, iostat = iocond) ((sbuff(n, m, k, 1), m = 1, mmaxgl), n = 1, nmaxgl)
             call dfbroadc_gdp(iocond, 1, dfint, gdp)
             if (iocond /= 0) then
                if (iocond < 0) then
                   call prterr(lundia, 'G006', filic(:lfile))
                else
                   call prterr(lundia, 'G007', filic(:lfile))
                endif
                error = .true.
                goto 200
             endif
          enddo
          call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*kmax*max(1,lstsci), dfreal, gdp)
          do m = mfg, mlg
             do n = nfg, nlg
                v1(n-nfg+1,m-mfg+1,1:kmax) = sbuff(n,m,1:kmax,1)
             enddo
          enddo
          !
          ! Per constituent l: kmax nmaxus mmax values in r1 array
          ! Only Salinity, Temperature, real constituents and secondary flow; no turbulence
          !
          if (lstsci>0) then
             do l = 1, lstsci
                do k = 1, kmax
                   if (inode == master) read (luntmp, *, iostat = iocond) ((sbuff(n, m, k, l), m = 1, mmaxgl), n = 1, nmaxgl)
                   call dfbroadc_gdp(iocond, 1, dfint, gdp)
                   if (iocond /= 0) then
                      if (iocond < 0) then
                         call prterr(lundia, 'G006', filic(:lfile))
                      else
                         call prterr(lundia, 'G007', filic(:lfile))
                      endif
                      error = .true.
                      goto 200
                   endif
                enddo
             enddo
             call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*kmax*max(1,lstsci), dfreal, gdp)
             do m = mfg, mlg
                do n = nfg, nlg
                   r1(n-nfg+1,m-mfg+1,1:kmax,1:lstsci) = sbuff(n,m,1:kmax,1:lstsci)
                enddo
             enddo
          endif
          !
          ! Stop reading file
          !
       endif      
       !
       ! If a NaN is read -> error
       !
       do m = 1, mmax
          do n = 1, nmaxus
             if ( isnan(s1(n,m)) ) then
                write(message,'(2a)') 'NaN found in water level in file ',filic(:lfile)
                call prterr(lundia, 'P004', message)
                !
                error = .true.
                goto 200
             endif
          enddo
       enddo
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                if ( isnan(u1(n, m, k)) ) then
                   write(message,'(2a)') 'NaN found in U-velocity in file ',filic(:lfile)
                   call prterr(lundia, 'P004', message)
                   !
                   error = .true.
                   goto 200
                endif
             enddo
          enddo
       enddo
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                if ( isnan(v1(n, m, k)) ) then
                   write(message,'(2a)') 'NaN found in V-velocity in file ',filic(:lfile)
                   call prterr(lundia, 'P004', message)
                   !
                   error = .true.
                   goto 200
                endif
             enddo
          enddo
       enddo
       do l = 1, lstsci
          do k = 1, kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   if ( isnan(r1(n, m, k, l)) ) then
                      write(message,'(2a)') 'NaN found in constituent in file ',filic(:lfile)
                      call prterr(lundia, 'P004', message)
                      !
                      error = .true.
                      goto 200
                   endif
                enddo
             enddo
          enddo
       enddo
       !
       ! Close file
       !
  200  continue
       deallocate(sbuff)
       !
       if (inode == master) close (luntmp)
    else
       !
       ! File does not exist
       !
       error = .true.
    endif
end subroutine icfil
