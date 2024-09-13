subroutine dfwrirst(lundia    ,runid     ,itrstc    ,nmaxus    ,mmax      , &
                & nmax      ,kmax      ,lstsci    ,ltur      ,s1        , &
                & u1        ,v1        ,r1        ,rtur1     ,umnldf    , &
                & vmnldf    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: dfwrirst.F90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfwrirst.F90 $
!!--description-----------------------------------------------------------------
!
!    Function: This routine writes the relevant output arrays to
!              the (single precision) restart file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)              , pointer :: dt
    real(fp)              , pointer :: tunit
    real(fp)              , pointer :: timsec
    integer               , pointer :: julday
    integer               , pointer :: ifirst
!
! Global variables
!
    integer, intent(in)            :: itrstc
                                   !!  Current time counter for the re-
                                   !!  start file. Start writing after
                                   !!  first interval is passed. Last time
                                   !!  will always be written to file for
                                   !!  ITRSTI > 0
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: ltur !  Description and declaration in esm_alloc_int.f90
    integer         :: lundia !  Description and declaration in inout.igs
    integer, intent(in)            :: mmax !  Description and declaration in esm_alloc_int.f90
    integer         :: nmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: nmaxus !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: s1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: umnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: vmnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(in) :: rtur1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in) :: u1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in) :: v1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(in) :: r1 !  Description and declaration in esm_alloc_real.f90
    character(*)    :: runid
                                   !!  Run identification code for the cur-
                                   !!  rent simulation (used to determine
                                   !!  the names of the in- /output files
                                   !!  used by the system)
    integer                              , pointer :: mfg
    integer                              , pointer :: mlg
    integer                              , pointer :: nfg
    integer                              , pointer :: nlg
    integer                              , pointer :: nmaxgl
    integer                              , pointer :: mmaxgl
    integer       , dimension(:,:)       , pointer :: iarrc
    integer       , dimension(:)         , pointer :: mf
    integer       , dimension(:)         , pointer :: ml
    integer       , dimension(:)         , pointer :: nf
    integer       , dimension(:)         , pointer :: nl
    integer                              , pointer :: lenlo
    integer                              , pointer :: lengl
!
!
! Local variables
!
    integer                        :: idate                ! Absolute date related to ITDATE and TIMSEC
    integer                        :: itime                ! Absolute time related to ITDATE and TIMSEC
    integer                        :: maxdim4              ! maximum size in 4'th dimension 
    integer                        :: lrid                 ! Actual length of var. RUNID
    integer                        :: lend                 ! Actual length of filrst
    integer                        :: lunrst               ! Unit number for the restart file, used only whne simulation sets the initial condition from this file
    integer                        :: newlun
    logical                        :: ex                   ! Help logical var. to determine whether the file currently beeing checked exist

    integer                               :: indx   ! array index
    integer                               :: jndx   ! array index
    integer                               :: kndx   ! array index
    integer                               :: ip     ! node number
    integer                               :: istart ! start pointer for each subdomain in collected array
    integer                               :: k      ! loop counter
    integer                               :: l      ! loop counter
    integer                               :: nohalo_length  ! length of field containing collected data
                                                            ! without halo points
    integer                               :: m      ! loop counter
    integer                               :: msiz   ! size of present subdomain in x-direction
    integer                               :: n      ! loop counter
    integer                               :: nsiz   ! size of present subdomain in y-direction

    real(fp)  , dimension(:), allocatable :: rdum    ! global array gathered from all nodes
    real(fp),    dimension(:), allocatable  :: rbuff1
    real(fp),    dimension(:,:), allocatable  :: rbuff2
    real(fp),    dimension(:,:), allocatable  :: rbuffg
    real(fp),    dimension(:,:,:), allocatable  :: rbuff3
    real(fp),    dimension(:,:,:,:), allocatable  :: rbuff4

    character(256)                 :: filrst ! Char. var. containing the restart file name
!
!! executable statements -------------------------------------------------------
!
    ifirst  => gdp%gdwrirst%ifirst
    timsec  => gdp%gdinttim%timsec
    julday  => gdp%gdinttim%julday
    dt      => gdp%gdexttim%dt
    tunit   => gdp%gdexttim%tunit
    mfg     => gdp%gdparall%mfg
    mlg     => gdp%gdparall%mlg
    nfg     => gdp%gdparall%nfg
    nlg     => gdp%gdparall%nlg
    mmaxgl  => gdp%gdparall%mmaxgl
    nmaxgl  => gdp%gdparall%nmaxgl
    iarrc   => gdp%gdparall%iarrc
    mf      => gdp%gdparall%mf
    ml      => gdp%gdparall%ml
    nf      => gdp%gdparall%nf
    nl      => gdp%gdparall%nl
    lenlo   => gdp%gdparall%ngridlo
    lengl   => gdp%gdparall%ngridgl
    !
    if (inode == master) then
       !
       !    - calculates global length without halo points
       !    - gather data in single array rdum (accounting for
       !      halo cells)
       !    - recasts array rdum in a compressed array
       !      rbuff1 in order to write to file with implied
       !      do loops consistent with reading of rstfil.f90
       !      and sequential mode
       !
       ! allocate data arrays for collection data
       !
       maxdim4 = max(1, lstsci, ltur)
       allocate(rdum  (lengl*(kmax+1)*maxdim4))
       nohalo_length = 0
       do ip = 0, nproc-1
          nohalo_length = nohalo_length + (nl(ip)-nf(ip)+1)*(ml(ip)-mf(ip)+1)
       enddo
       allocate( rbuff1(nohalo_length) )
    endif
    !
    !
    ! print data of hydrodynamics in global domain
    ! open restart file by the master node
    !
    if ( inode == master ) then
       !
       ! initialisation local parameters
       !
       filrst = ' '
       timsec = real(itrstc,fp)*dt*tunit
       !
       ! Get absolute date and time
       !
       call timdat(julday    ,timsec    ,idate    ,itime     )
       !
       ! get file name and test file existence
       !
       call remove_leading_spaces(runid     ,lrid      )
       !
       filrst(:8 + lrid) = 'tri-rst.' // runid(:lrid)
       lend = 8 + lrid + 16
       write (filrst(8 + lrid + 1:lend), '(a1,i8.8,a1,i6.6)') '.', idate, '.', itime
       !
       ! Define new unit number
       !
       lunrst = newlun(gdp)
       !
       ! Test existence of restart file
       !
       inquire (file = filrst(1:lend), exist = ex)
       if (ex) then
          !
          ! First entry write warning
          !
          if (ifirst==1) then
             call prterr(lundia    ,'S014'    ,filrst(:lend))
          endif
          open (lunrst, file = filrst(:lend))
          close (lunrst, status = 'delete')
       endif
       !
       ! New file => open file
       !
       open (lunrst, file = filrst(:lend), form = 'unformatted',          &
            & status = 'new')
    endif
    !
    ! write restart values S1, U1, V1, R1 and RTUR1
    !
    ! gather data of each data array and then write to restart file
    !
    ! gather s1
    !
    call dfsync(gdp)
    !
    ! first transpose local arrays for consistency with
    ! order of writing in rstfil
    ! transposition is done depending on the direction
    ! of cutting set in dfpartit.f90 (idir)
    ! THIS REORDERING MIGHT BE INCORRECT IF IDIR IS CHANGED
    ! AND LEAD TO WRONGLY TRANSPOSED ARRAY UNUSABLE FOR RESTART
    !
    if (idir == 2) then
       allocate(rbuff2(1:gdp%d%mmax+4, 1:gdp%d%nmax))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff2(m,n) = s1(n,m-2)
          enddo
       enddo
    else
       allocate(rbuff2(1:gdp%d%nmax, 1:gdp%d%mmax+4))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff2(n,m) = s1(n,m-2)
          enddo
       enddo
    endif
    call dfgather_lowlevel ( rdum, lengl, rbuff2, lenlo, dfloat, gdp )
    call dfsync(gdp)
    deallocate(rbuff2)
    !
    ! write s1
    !
    if (inode == master) then
       !
       istart = 0
       jndx   = 1
       kndx   = 1
       !
       do ip = 0, nproc-1
          if (idir == 2) jndx = kndx
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                if (idir == 1) indx = istart + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                if (idir == 2) indx = istart + (n - iarrc(3,ip))*msiz + (m - iarrc(1,ip)) + 1
                rbuff1(jndx) = rdum(indx)
                jndx = jndx + 1
             enddo
             if (idir == 2) jndx = jndx + mmaxgl - (ml(ip)-mf(ip)+1)
          enddo
          !
          istart = istart + msiz*nsiz
          if (idir == 2) kndx = kndx + (ml(ip)-mf(ip)+1)
          !
       enddo
       !
       write (lunrst) (real(rbuff1(jndx) ,sp), jndx = 1, nohalo_length)
       !
    endif
    !
    ! gather u1
    !
    if (idir == 2) then
       allocate(rbuff3(1:gdp%d%mmax+4, 1:gdp%d%nmax, 1:kmax))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff3(m,n,1:kmax) = u1(n,m-2,1:kmax)
          enddo
       enddo
    else
       allocate(rbuff3(1:gdp%d%nmax, 1:gdp%d%mmax+4, 1:kmax))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff3(n,m,1:kmax) = u1(n,m-2,1:kmax)
          enddo
       enddo
    endif
    call dfgather_lowlevel ( rdum, lengl*kmax, rbuff3, lenlo*kmax, dfloat, gdp )
    call dfsync(gdp)
    deallocate(rbuff3)
    !
    ! write u1
    !
    if (inode == master) then
       !
       istart = 0
       !
       do k = 1, kmax
          istart = 0
          jndx = 1
          kndx = 1
          do ip = 0, nproc-1
             if (idir == 2) jndx = kndx
             !
             msiz = iarrc(2,ip)-iarrc(1,ip)+1
             nsiz = iarrc(4,ip)-iarrc(3,ip)+1
             if (mod(nsiz,2)==0) nsiz = nsiz + 1
             istart = istart + (k-1)*msiz*nsiz
             !
             do n = nf(ip), nl(ip)
                do m = mf(ip), ml(ip)
                   if (idir == 1) indx = istart + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   if (idir == 2) indx = istart + (n - iarrc(3,ip))*msiz + (m - iarrc(1,ip)) + 1
                   rbuff1(jndx) = rdum(indx)
                   jndx = jndx + 1
                enddo
                if (idir == 2) jndx = jndx + mmaxgl - (ml(ip)-mf(ip)+1)
             enddo
             istart = istart + msiz*nsiz*(kmax-k+1)
             if (idir == 2) kndx = kndx + (ml(ip)-mf(ip)+1)
          enddo
          !
          write (lunrst) (real(rbuff1(jndx) ,sp), jndx = 1, nohalo_length)
          !
       enddo
       !
    endif
    !
    ! gather v1
    !
    if (idir == 2) then
    allocate(rbuff3(1:gdp%d%mmax+4, 1:gdp%d%nmax, 1:kmax))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff3(m,n,1:kmax) = v1(n,m-2,1:kmax)
          enddo
       enddo
    else
       allocate(rbuff3(1:gdp%d%nmax, 1:gdp%d%mmax+4, 1:kmax))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff3(n,m,1:kmax) = v1(n,m-2,1:kmax)
          enddo
       enddo
    endif
    call dfgather_lowlevel ( rdum, lengl*kmax, rbuff3, lenlo*kmax, dfloat, gdp )
    call dfsync(gdp)
    deallocate(rbuff3)
    !
    ! write v1
    !
    if (inode == master) then
       !
       istart = 0
       !
       do k = 1, kmax
          istart = 0
          jndx = 1
          kndx = 1
          do ip = 0, nproc-1
             if (idir == 2) jndx = kndx
             !
             msiz = iarrc(2,ip)-iarrc(1,ip)+1
             nsiz = iarrc(4,ip)-iarrc(3,ip)+1
             if (mod(nsiz,2)==0) nsiz = nsiz + 1
             istart = istart + (k-1)*msiz*nsiz
             !
             do n = nf(ip), nl(ip)
                do m = mf(ip), ml(ip)
                   if (idir == 1) indx = istart + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   if (idir == 2) indx = istart + (n - iarrc(3,ip))*msiz + (m - iarrc(1,ip)) + 1
                   rbuff1(jndx) = rdum(indx)
                   jndx = jndx + 1
                enddo
                if (idir == 2) jndx = jndx + mmaxgl - (ml(ip)-mf(ip)+1)
             enddo
             istart = istart + msiz*nsiz*(kmax-k+1)
             if (idir == 2) kndx = kndx + (ml(ip)-mf(ip)+1)
          enddo
          !
          write (lunrst) (real(rbuff1(jndx) ,sp), jndx = 1, nohalo_length)
          !
       enddo
       !
    endif
    !
    ! gather r1
    !
    if (idir == 2) then
       allocate(rbuff4(1:gdp%d%mmax+4, 1:gdp%d%nmax, 1:kmax, 1:lstsci))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff4(m, n, 1:kmax, 1:lstsci) = r1(n, m-2, 1:kmax, 1:lstsci)
          enddo
       enddo
    else
       allocate(rbuff4(1:gdp%d%nmax, 1:gdp%d%mmax+4, 1:kmax, 1:lstsci))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff4(n, m, 1:kmax, 1:lstsci) = r1(n, m-2, 1:kmax, 1:lstsci)
          enddo
       enddo
    endif
    call dfgather_lowlevel ( rdum, lengl*kmax*lstsci, rbuff4, lenlo*kmax*lstsci, dfloat, gdp )
    call dfsync(gdp)
    deallocate(rbuff4)
    !
    !
    ! write r1
    !
    if (inode == master) then
       !
       istart = 0
       !
       do l = 1, lstsci
          do k = 1, kmax
             jndx = 1
             kndx = 1
             istart = 0
             do ip = 0, nproc-1
                if (idir == 2) jndx = kndx
                !
                msiz = iarrc(2,ip)-iarrc(1,ip)+1
                nsiz = iarrc(4,ip)-iarrc(3,ip)+1
                if (mod(nsiz,2)==0) nsiz = nsiz + 1
                istart = istart + (k-1)*msiz*nsiz + (l-1)*msiz*nsiz*kmax
                !
                do n = nf(ip), nl(ip)
                   do m = mf(ip), ml(ip)
                      if (idir == 1) indx = istart + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                      if (idir == 2) indx = istart + (n - iarrc(3,ip))*msiz + (m - iarrc(1,ip)) + 1
                      rbuff1(jndx) = rdum(indx)
                      jndx = jndx + 1
                   enddo
                   if (idir == 2) jndx = jndx + mmaxgl - (ml(ip)-mf(ip)+1)
                enddo
                istart = istart + msiz*nsiz*(kmax-k+1) + (lstsci-l)*msiz*nsiz*kmax
                if (idir == 2) kndx = kndx + (ml(ip)-mf(ip)+1)
             enddo
             !
             write (lunrst) (real(rbuff1(jndx) ,sp), jndx = 1, nohalo_length)
             !
          enddo
       enddo
       !
    endif
    !
    ! gather rtur1
    !
    if (ltur > 0) then
       if (idir == 2) then
          allocate(rbuff4(1:gdp%d%mmax+4, 1:gdp%d%nmax, 1:kmax+1, 1:ltur))
          do n=1,gdp%d%nmax
             do m=1,gdp%d%mmax+4
                rbuff4(m, n, 1:kmax+1, 1:ltur) = rtur1(n, m-2, 0:kmax, 1:ltur)
             enddo
          enddo
       else
          allocate(rbuff4(1:gdp%d%nmax, 1:gdp%d%mmax+4, 1:kmax+1, 1:ltur))
          do n=1,gdp%d%nmax
             do m=1,gdp%d%mmax+4
                rbuff4(n, m, 1:kmax+1, 1:ltur) = rtur1(n, m-2, 0:kmax, 1:ltur)
             enddo
          enddo
       endif
       call dfgather_lowlevel ( rdum, lengl*(kmax+1)*ltur, rbuff4, lenlo*(kmax+1)*ltur, dfloat, gdp )
       call dfsync(gdp)
       deallocate(rbuff4)
    endif
    !
    if (inode == master) then
       !
       istart = 0
       !
       do l = 1, ltur
          do k = 1, kmax+1
             kndx = 1
             jndx = 1
             istart = 0
             do ip = 0, nproc-1
                if (idir == 2) jndx = kndx
                !
                msiz = iarrc(2,ip)-iarrc(1,ip)+1
                nsiz = iarrc(4,ip)-iarrc(3,ip)+1
                if (mod(nsiz,2)==0) nsiz = nsiz + 1
                istart = istart + (k-1)*msiz*nsiz + (l-1)*msiz*nsiz*(kmax+1)
                !
                do n = nf(ip), nl(ip)
                   do m = mf(ip), ml(ip)
                      if (idir == 1) indx = istart + (m-iarrc(1,ip))*nsiz + n-iarrc(3,ip)+1
                      if (idir == 2) indx = istart + (n-iarrc(3,ip))*msiz + m-iarrc(1,ip)+1
                      rbuff1(jndx) = rdum(indx)
                      jndx = jndx + 1
                   enddo
                   if (idir == 2) jndx = jndx + mmaxgl - (ml(ip)-mf(ip)+1)
                enddo
                istart = istart + msiz*nsiz*(kmax+1-k+1) + (ltur-l)*msiz*nsiz*(kmax+1)
                if (idir == 2) kndx = kndx + (ml(ip)-mf(ip)+1)
             enddo
             write (lunrst) (real(rbuff1(jndx) ,sp), jndx = 1, nohalo_length)
          enddo
       enddo
    endif
    !
    ! write filtered velocities to restart file to allow for
    ! restarts using subgrid viscosity model
    !
    !
    ! gather umnldf
    !
    if (idir == 2) then
       allocate(rbuff2(1:gdp%d%mmax+4, 1:gdp%d%nmax))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff2(m,n) = umnldf(n,m-2)
          enddo
       enddo
    else
       allocate(rbuff2(1:gdp%d%nmax, 1:gdp%d%mmax+4))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff2(n,m) = umnldf(n,m-2)
          enddo
       enddo
    endif
    call dfgather_lowlevel ( rdum, lengl, rbuff2, lenlo, dfloat, gdp )
    call dfsync(gdp)
    deallocate(rbuff2)
    !
    ! write umnldf
    !
    if (inode == master) then
       !
       istart = 0
       jndx   = 1
       kndx   = 1
       !
       do ip = 0, nproc-1
          if (idir == 2) jndx = kndx
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                if (idir == 1) indx = istart + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                if (idir == 2) indx = istart + (n - iarrc(3,ip))*msiz + (m - iarrc(1,ip)) + 1
                rbuff1(jndx) = rdum(indx)
                jndx = jndx + 1
             enddo
             if (idir == 2) jndx = jndx + mmaxgl - (ml(ip)-mf(ip)+1)
          enddo
          !
          istart = istart + msiz*nsiz
          if (idir == 2) kndx = kndx + (ml(ip)-mf(ip)+1)
          !
       enddo
       !
       write (lunrst) (real(rbuff1(jndx) ,sp), jndx = 1, nohalo_length)
       !
    endif
    !
    ! gather vmnldf
    !
    if (idir == 2) then
       allocate(rbuff2(1:gdp%d%mmax+4, 1:gdp%d%nmax))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff2(m,n) = vmnldf(n,m-2)
          enddo
       enddo
    else
       allocate(rbuff2(1:gdp%d%nmax, 1:gdp%d%mmax+4))
       do n=1,gdp%d%nmax
          do m=1,gdp%d%mmax+4
             rbuff2(n,m) = vmnldf(n,m-2)
          enddo
       enddo
    endif
    call dfgather_lowlevel ( rdum, lengl, rbuff2, lenlo, dfloat, gdp )
    call dfsync(gdp)
    deallocate(rbuff2)
    !
    ! write vmnldf
    !
    if (inode == master) then
       !
       istart = 0
       jndx   = 1
       kndx   = 1
       !
       do ip = 0, nproc-1
          if (idir == 2) jndx = kndx
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                if (idir == 1) indx = istart + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                if (idir == 2) indx = istart + (n - iarrc(3,ip))*msiz + (m - iarrc(1,ip)) + 1
                rbuff1(jndx) = rdum(indx)
                jndx = jndx + 1
             enddo
             if (idir == 2) jndx = jndx + mmaxgl - (ml(ip)-mf(ip)+1)
          enddo
          !
          istart = istart + msiz*nsiz
          if (idir == 2) kndx = kndx + (ml(ip)-mf(ip)+1)
          !
       enddo
       !
       write (lunrst) (real(rbuff1(jndx) ,sp), jndx = 1, nohalo_length)
       !
    endif
    !
    ! synchronize
    !
    call dfsync(gdp)
    !
    if (inode == master) then
       !
       ! Close unit
       !
       close (lunrst)
       !
       ! Redefine entry number
       !
       ifirst = 0
       !
       ! deallocate global array of hydrodynamics
       !
       deallocate( rdum )
       deallocate( rbuff1 )
    endif
end subroutine dfwrirst
