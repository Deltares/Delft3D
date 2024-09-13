subroutine rstfil(lundia    ,error     ,restid    ,lturi     ,mmax      , &
                & nmaxus    ,kmax      ,lstsci    ,ltur      ,w1        , &
                & s1        ,u1        ,v1        ,r1        ,rtur1     , &
                & umnldf    ,vmnldf    ,kfu       ,kfv       , &
                & dp        ,namcon    ,coninit   ,gdp       )
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
!  $Id: rstfil.f90 5834 2016-02-11 14:39:48Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/input/rstfil.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Reads initial field condition records from an
!              unformatted (single precision) restart file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use nan_check_module
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: tstart
    integer                              , pointer :: julday
    integer                              , pointer :: mfg
    integer                              , pointer :: mlg
    integer                              , pointer :: nfg
    integer                              , pointer :: nlg
    integer                              , pointer :: mmaxgl
    integer                              , pointer :: nmaxgl
    integer       , dimension(:)         , pointer :: mf
    integer       , dimension(:)         , pointer :: ml
    integer       , dimension(:)         , pointer :: nf
    integer       , dimension(:)         , pointer :: nl
!
! Global variables
!
    integer                                                                    , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(out) :: lturi  !  Description and declaration in tricom.igs
    integer                                                                                  :: lundia !  Description and declaration in inout.igs
    integer                                                                    , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(lstsci)                                                              :: coninit ! Flag=1 if constituent is initialized, all 0 upon entry
    logical                                                                                  :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: dp     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: umnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: vmnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(out) :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(out) :: w1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(out) :: r1     !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                             :: restid !!  Run identification of the restart file. If RESTID = non-blank then current simulation will use this file for setting the initial conditions
    character(20), dimension(lstsci + ltur)                                    , intent(in)  :: namcon !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                              :: idate
    integer                                              :: iocond  ! IO status for reading 
    integer                                              :: itime
    integer                                              :: ipos    ! index to a position in a string
    integer                                              :: k       ! Help var. 
    integer                                              :: l       ! Help var. 
    integer                                              :: luntmp  ! Unit number file 
    integer                                              :: m       ! Help var. 
    integer                                              :: n       ! Help var. 
    integer                                              :: newlun
    logical                                              :: ex
    logical                                              :: ex_nfs
    real(sp)      , dimension(:,:,:,:)     , allocatable :: sbuff   !!  Single precision buffer to read from file
    character(16)                                        :: datetime
    character(300)                                       :: filtmp  ! File name restart file 300 = 256 + a bit
    character(256)                                       :: filpath ! Path specification of restid
    integer                                              :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    julday          => gdp%gdinttim%julday
    tstart          => gdp%gdexttim%tstart
    !
    mfg             => gdp%gdparall%mfg
    mlg             => gdp%gdparall%mlg
    nfg             => gdp%gdparall%nfg
    nlg             => gdp%gdparall%nlg
    mmaxgl          => gdp%gdparall%mmaxgl
    nmaxgl          => gdp%gdparall%nmaxgl
    mf              => gdp%gdparall%mf
    ml              => gdp%gdparall%ml
    nf              => gdp%gdparall%nf
    nl              => gdp%gdparall%nl
    !
    error = .false.
    nm_pos = 1
    !
    ! test file existence, first 'tri-rst.<restid>.idate.itime'
    !
    write(lundia, '(a)') '*** Start of restart messages'
    !
    call timdat(julday, tstart*60, idate, itime)
    write (datetime,'(a1,i8.8,a1,i6.6)') '.', idate, '.', itime
    ipos = max(index(trim(restid), "/",.true.),index(trim(restid), "\",.true.))
    if (ipos > 0) then
       filpath = restid(1:ipos)
       restid  = restid(ipos+1:)
    else
      filpath = ""
    endif
    write (filtmp, '(4a,a1,i8.8,a1,i6.6)') trim(filpath), 'tri-rst.', trim(restid), trim(datetime)
    inquire (file = trim(filtmp), exist = ex)
    ex_nfs = .false.
    if (.not.ex) then
       !
       ! test file existence, second try 'tri-rst.<restid>'
       !
       write (filtmp, '(3a)') trim(filpath), 'tri-rst.', trim(restid)
       inquire (file = trim(filtmp), exist = ex)
       if (.not.ex) then
          !
          ! None of these two files exists
          ! Check new option: it may be a reference to a TRIM file.
          ! Use restid, because flow_nefis_restart will put it's value in gdp%gdrestart%restid
          !
          write (restid, '(2a)') trim(filpath), trim(restid)
          call flow_nefis_restart(lundia    ,error     ,restid    ,lturi     ,mmax      , &
                                & nmaxus    ,kmax      ,lstsci    ,ltur      ,w1        , &
                                & s1        ,u1        ,v1        ,r1        ,rtur1     , &
                                & umnldf    ,vmnldf    ,kfu       ,kfv       , &
                                & dp        ,ex_nfs    ,namcon    ,coninit   ,gdp       )
          if (error .and. .not.ex_nfs) then
             call prterr(lundia    ,'G004'    , &
             & trim(filtmp) // trim(datetime) // ', ' // trim(filtmp) // ' and ' // trim(restid) // '.dat/.def')
          endif
       endif
    endif
    if (.not.error .and. .not.ex_nfs) then
       !
       ! restart file found
       !
       write(lundia, '(a)') 'Restarting from ' // trim(filtmp)
       !
       ! Allocate temporary single precision array for the ENTIRE domain
       !
       allocate(sbuff(nmaxgl, mmaxgl, 0:kmax, max(1, lstsci, ltur)))
       !
       ! the restart file is opened and read by the master
       !
       if (inode == master) then
          luntmp = newlun(gdp)
          open (luntmp, file = trim(filtmp), form = 'unformatted',              &
               & status = 'old')
       endif
       !
       ! read restart values and distributed via a broadcast to the slaves
       ! per nmaxus mmax values in s1 array
       ! NOTE: nmaxus and mmax equal nmaxgl and mmaxgl, respectively (for entire domain)
       !       in case of parallel runs. Moreover, buffer is associated with entire domain
       !
       if (inode == master) then
          read (luntmp, iostat = iocond) ((sbuff(n, m, 1, 1), m = 1, mmaxgl), n = 1, nmaxgl)
       endif
       call dfbroadc_gdp(iocond, 1, dfint, gdp)
       if (iocond /= 0) then
          if (iocond < 0) then
             call prterr(lundia, 'G006', trim(filtmp))
          else
             call prterr(lundia, 'G007', trim(filtmp))
          endif
          error = .true.
          goto 200
       endif
       !
       ! send buffer to other nodes
       !
       call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*(kmax+1)*max(1,lstsci,ltur), dfreal, gdp)
       if (.not. nan_check(sbuff(:,:,1,1), 's1 (restart-file)', lundia)) call d3stop(1, gdp)
       !
       ! put copies of parts of s1 for each subdomain
       !
       if (parll) then
          do m = mf(inode-1), ml(inode-1)
             do n = nf(inode-1), nl(inode-1)
                s1(n-nfg+1,m-mfg+1) = sbuff(n,m,1,1)
             enddo
          enddo
       else
          s1(1:nmaxus,1:mmax) = sbuff(1:nmaxus,1:mmax,1,1)
       endif
       call dfexchg ( s1, 1, 1, dfloat, nm_pos, gdp )
       !
       ! per layer k: nmaxus mmax values in u1 array
       !
       do k = 1, kmax
          if (inode == master) then
             read (luntmp, iostat = iocond) ((sbuff(n, m, k, 1), m = 1, mmaxgl), n = 1,nmaxgl)
          endif
          call dfbroadc_gdp(iocond, 1, dfint, gdp)
          if (iocond /= 0) then
             if (iocond < 0) then
                call prterr(lundia, 'G006', trim(filtmp))
             else
                call prterr(lundia, 'G007', trim(filtmp))
             endif
             error = .true.
             goto 200
          endif
       enddo
       !
       ! send buffer to other nodes
       !
       call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*(kmax+1)*max(1,lstsci,ltur), dfreal, gdp)
       if (.not. nan_check(sbuff(:,:,1:kmax,1), 'u1 (restart-file)', lundia)) call d3stop(1, gdp)
       !
       ! put copies of parts of u1 for each subdomain
       !
       if (parll) then
          do m = mf(inode-1), ml(inode-1)
             do n = nf(inode-1), nl(inode-1)
                u1(n-nfg+1,m-mfg+1,1:kmax) = sbuff(n,m,1:kmax,1)
             enddo
          enddo
       else
          u1(1:nmaxus,1:mmax,1:kmax) = sbuff(1:nmaxus,1:mmax,1:kmax,1)
       endif
       call dfexchg ( u1, 1, kmax, dfloat, nm_pos, gdp )
       !
       ! per layer k: nmaxus mmax values in v1 array
       !
       do k = 1, kmax
          if (inode == master) then
             read (luntmp, iostat = iocond) ((sbuff(n, m, k, 1), m = 1, mmaxgl), n = 1, nmaxgl)
          endif
          call dfbroadc_gdp(iocond, 1, dfint, gdp)
          if (iocond /= 0) then
             if (iocond < 0) then
                call prterr(lundia, 'G006', trim(filtmp))
             else
                call prterr(lundia, 'G007', trim(filtmp))
             endif
             error = .true.
             goto 200
          endif
       enddo
       !
       ! send buffer to other nodes
       !
       call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*(kmax+1)*max(1,lstsci,ltur), dfreal, gdp)
       if (.not. nan_check(sbuff(:,:,1:kmax,1), 'v1 (restart-file)', lundia)) call d3stop(1, gdp)
       !
       ! put copies of parts of v1 for each subdomain
       !
       if (parll) then
          do m = mf(inode-1), ml(inode-1)
             do n = nf(inode-1), nl(inode-1)
                v1(n-nfg+1,m-mfg+1,1:kmax) = sbuff(n,m,1:kmax,1)
             enddo
          enddo
       else
          v1(1:nmaxus,1:mmax,1:kmax) = sbuff(1:nmaxus,1:mmax,1:kmax,1)
       endif
       call dfexchg ( v1, 1, kmax, dfloat, nm_pos, gdp )
       !
       ! per constituent l: kmax nmaxus mmax values in r1 array
       ! only Salinity, Temperature, real constituents and secondary
       ! flow; no turbulence
       !
       if (lstsci > 0) then
          do l = 1, lstsci
             coninit(l) = 1
             do k = 1, kmax
                if (inode==master) then
                   read (luntmp, iostat = iocond) ((sbuff(n, m, k, l), m = 1, mmaxgl), n = 1, nmaxgl)
                endif
                call dfbroadc_gdp(iocond, 1, dfint, gdp)
                if (iocond /= 0) then
                   if (iocond < 0) then
                      call prterr(lundia, 'G006', trim(filtmp))
                   else
                      call prterr(lundia, 'G007', trim(filtmp))
                   endif
                   error = .true.
                   goto 200
                endif
             enddo
          enddo
          !
          ! send buffer to other nodes
          !
          call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*(kmax+1)*max(1,lstsci,ltur), dfreal, gdp)
          if (.not. nan_check(sbuff(:,:,1:kmax,1:lstsci), 'r1 (restart-file)', lundia)) call d3stop(1, gdp)
          !
          ! put copies of parts of r1 for each subdomain
          !
          if (parll) then
             do m = mf(inode-1), ml(inode-1)
                do n = nf(inode-1), nl(inode-1)
                   r1(n-nfg+1,m-mfg+1,1:kmax,1:lstsci) = sbuff(n,m,1:kmax,1:lstsci)
                enddo
             enddo
             do l = 1, lstsci
                call dfexchg ( r1(:,:,:,l), 1, kmax, dfloat, nm_pos, gdp )
             enddo
          else
             r1(1:nmaxus,1:mmax,1:kmax,1:lstsci) = sbuff(1:nmaxus,1:mmax,1:kmax,1:lstsci)
          endif
       endif
       !
       ! Per turbulence l: 0:kmax nmaxus mmax values in rtur1 array
       ! If no turbulence arrays on restart file then rtur1 will be
       ! initialized in INITUR
       ! If only K on restart file EPS will be calculated in INITUR
       !
       if (ltur>0) then
          lturi = 0
          do l = 1, ltur
             do k = 0, kmax
                if (inode==master) then
                   read (luntmp, iostat = iocond) ((sbuff(n, m, k, l), m = 1, mmaxgl) , n = 1, nmaxgl)
                endif
                call dfbroadc_gdp(iocond, 1, dfint, gdp)
                if (iocond /= 0) then
                   if (iocond < 0) then
                      lturi = ltur
                      if (l==2) lturi = -ltur
                   else
                      call prterr(lundia, 'G007', trim(filtmp))
                      error = .true.
                   endif
                   goto 200
                endif
             enddo
          enddo
          !
          ! send buffer to other nodes
          !
          call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*(kmax+1)*max(1,lstsci,ltur), dfreal, gdp)
          if (.not. nan_check(sbuff(:,:,:,1:ltur), 'rtur1 (restart-file)', lundia)) call d3stop(1, gdp)
          !
          ! put copies of parts of rtur1 for each subdomain
          !
          if (parll) then
             do m = mf(inode-1), ml(inode-1)
                do n = nf(inode-1), nl(inode-1)
                   rtur1(n-nfg+1,m-mfg+1,0:kmax,1:ltur) = sbuff(n,m,0:kmax,1:ltur)
                enddo
             enddo
             do l = 1, ltur
                call dfexchg ( rtur1(:,:,:,l), 0, kmax, dfloat, nm_pos, gdp )
             enddo
          else
             rtur1(1:nmaxus,1:mmax,0:kmax,1:ltur) = sbuff(1:nmaxus,1:mmax,0:kmax,1:ltur)
          endif
       endif
       !
       ! read filtered velocity components to allow restarts
       ! using subgrid viscosity model
       !
       if (inode == master) then
          read (luntmp, iostat = iocond, end = 200) ((sbuff(n, m, 1, 1), m = 1, mmaxgl), n = 1, nmaxgl)
       endif
       !
       ! send buffer to other nodes
       !
       call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*(kmax+1)*max(1,lstsci,ltur), dfreal, gdp)
       if (.not. nan_check(sbuff(:,:,1,1), 'umnldf (restart-file)', lundia)) call d3stop(1, gdp)
       !
       ! put copies of parts of umnldf for each subdomain
       !
       if (parll) then
          do m = mf(inode-1), ml(inode-1)
             do n = nf(inode-1), nl(inode-1)
                umnldf(n-nfg+1,m-mfg+1) = sbuff(n,m,1,1)
             enddo
          enddo
       else
          umnldf(1:nmaxus,1:mmax) = sbuff(1:nmaxus,1:mmax,1,1)
       endif
       call dfexchg ( umnldf, 1, 1, dfloat, nm_pos, gdp )
       !
       if (inode == master) then
          read (luntmp, iostat = iocond) ((sbuff(n, m, 1, 1), m = 1, mmaxgl), n = 1, nmaxgl)
       endif
       !
       ! send buffer to other nodes
       !
       call dfbroadc_gdp(sbuff, mmaxgl*nmaxgl*(kmax+1)*max(1,lstsci,ltur), dfreal, gdp)
       if (.not. nan_check(sbuff(:,:,1,1), 'vmnldf (restart-file)', lundia)) call d3stop(1, gdp)
       !
       ! put copies of parts of vmnldf for each subdomain
       !
       if (parll) then
          do m = mf(inode-1), ml(inode-1)
             do n = nf(inode-1), nl(inode-1)
                vmnldf(n-nfg+1,m-mfg+1) = sbuff(n,m,1,1)
             enddo
          enddo
       else
          vmnldf(1:nmaxus,1:mmax) = sbuff(1:nmaxus,1:mmax,1,1)
       endif
       call dfexchg ( vmnldf, 1, 1, dfloat, nm_pos, gdp )
       !
       ! stop reading file
       !
       !
       ! close file
       !
  200  continue
       if (inode == master) close (luntmp)
       deallocate(sbuff)
    endif
    write (lundia, '(a)') '*** End   of restart messages'
    write (lundia, *)
end subroutine rstfil
