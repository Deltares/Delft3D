subroutine rdbcg(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & itlfsm    ,tlfsmo    ,dt        ,tunit     ,nto       , &
               & lstsc     ,bndneu    ,cstbnd    ,nambnd    ,typbnd    , &
               & rettim    ,ntoq      ,thetqh    ,thetqt    ,restid    , &
               & filic     ,paver     ,pcorr     ,tstart    ,tstop     , &
               & mxdnto    ,gdp       )
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
!    Function: - Reads TLFSMO-rec. from the MD-file
!              - Tests whether TLFSMO is a multiple of DT
!              - Reads RETTIM (TH.HARLEMAN) from the MD-file
!                only when lstsc > 0
!              - Test wether RETTIM is not negative
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use dfparall, only: parll
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: lsal
    integer , pointer :: ltem
    integer , pointer :: itis
    logical , pointer :: use_zavg_for_qtot
    integer, dimension(:), pointer :: bct_order
!
! Global variables
!
    integer                                    :: itlfsm !  Description and declaration in inttim.igs
    integer                      , intent(in)  :: lstsc  !  Description and declaration in dimens.igs
    integer                                    :: lundia !  Description and declaration in inout.igs
    integer                                    :: lunmd  !  Description and declaration in inout.igs
    integer                      , intent(in)  :: mxdnto !  Total number of open boundaries across all partitions of a parallel simulation
    integer                                    :: nrrec  !  Pointer to the record number in the MD-file
    integer                      , intent(in)  :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                      , intent(in)  :: ntoq   !  Description and declaration in dimens.igs
    logical                                    :: bndneu !  Description and declaration in numeco.igs
    logical                                    :: cstbnd !  Description and declaration in numeco.igs
    logical                      , intent(out) :: error  !  Flag=TRUE if an error is encountered
    logical                                    :: pcorr  !  Flag=TRUE when using pressure correction on boundaries using PAVER
    real(fp)                                   :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp)                                   :: paver  !  Description and declaration in numeco.igs
    real(fp)                                   :: thetqh !  Description and declaration in numeco.igs
    real(fp)                                   :: thetqt !  Description and declaration in numeco.igs
    real(fp)                                   :: tlfsmo !  Timespan for smoothing (in minutes)
    real(fp)                     , intent(in)  :: tstart !  Flow start time in minutes
    real(fp)                     , intent(in)  :: tstop  !  Flow stop time in minutes
    real(fp)                     , intent(in)  :: tunit  !  Description and declaration in exttim.igs
    real(fp), dimension(nto, lstsc, 2)         :: rettim !  Description and declaration in esm_alloc_real.f90
    character(*)                               :: filic  !  File name of initial condition file
    character(*)                               :: mdfrec !  Standard rec. length in MD-file (300)
    character(*)                               :: restid !  Run identification of the restart file
    character(1) , dimension(nto), intent(in)  :: typbnd !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto), intent(in)  :: nambnd !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                :: i       ! Local open boundary index
    integer                :: intor
    integer                :: it      ! Help integer var. for time par.
    integer                :: j       ! Global open boundary index
    integer                :: l       ! Loop parameter for constituents
    integer                :: lenc
    integer                :: lkw     ! Length of keyword 
    integer                :: n       ! Help var. loop 
    integer                :: nlook   ! Help var.: nr. of data to look for in the MD-file 
    integer                :: ntrec   ! Help. var to keep track of NRREC 
    logical                :: defaul
    logical                :: dtn
    logical                :: found   ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                :: lerror  ! Flag=TRUE if a local error is encountered 
    logical                :: newkw   ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                :: nodef   ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                :: present ! Logical determining whether a keyword was found in the MDF-file using prop_get
    real(fp)               :: rdef    ! Help var. containing default va- lue(s) for real variable 
    real(sp)               :: rhelp   ! Help var. for reading paver from MDF-file 
    real(fp)               :: t
    real(fp)               :: smofrac ! Fraction of smoothing time over total simulation time (0-100.0).
    real(fp), dimension(1) :: rval    ! Help array (real) where the data, recently read from the MD-file, are stored temporarily
    real(fp), allocatable, dimension(:, :, :) :: tmp_rettim ! Local array for the RETTIM values of all boundaries (for parallel computations).
    character(1)           :: cdef    ! Default value for character string
    character(1)           :: chulp   ! Help string to read character string
    character(300)         :: errmsg  ! Help text for error messages
    character(6)           :: keyw    ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(256)         :: message ! Help text for messages 
!
!! executable statements -------------------------------------------------------
!
    lsal  => gdp%d%lsal
    ltem  => gdp%d%ltem
    itis  => gdp%gdrdpara%itis
    use_zavg_for_qtot => gdp%gdnumeco%use_zavg_for_qtot
    bct_order => gdp%gdbcdat%bct_order
    !
    lerror  = .false.
    newkw   = .true.
    defaul  = .true.
    nodef   = .false.
    found   = .false.
    rdef    = 0.0_fp
    smofrac = 0.0_fp
    !
    ! initialize parameters that are to be read
    !
    bndneu = .false.
    cstbnd = .false.
    tlfsmo = 0.0_fp
    itlfsm = 0
    !
    ! locate 'Tlfsmo' record for smoothing time
    ! default value allowed (:= rdef)
    !
    keyw  = 'Tlfsmo'
    ntrec = nrrec
    nlook = 1
    call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       tlfsmo = rdef
    else
       tlfsmo = rval(1)
    endif
    !
    ! test time constistency
    ! tlfsmo <> n * dt not allowed
    ! tlfsmo > total simulation time is not allowed
    !
    itlfsm = nint(tlfsmo/dt)
    if (dtn(itlfsm, tlfsmo, dt)) then
       call prterr(lundia    ,'U044'    ,'Smoothing time'     )
       error = .true.
    endif
    if (itlfsm > 0) then
       smofrac = 100.0_fp * tlfsmo/(tstop-tstart)
       if (smofrac > 100.0_fp) then
          call prterr(lundia, 'P004' ,'Smoothing time is bigger than simulation time' )
          call d3stop(1         ,gdp       )
       elseif (smofrac > 25.0_fp) then
          message = ' '
          write (message, '(a,f7.1,a)') 'Smoothing time is ', smofrac, '% of simulation time'
          call prterr(lundia, 'Z013', trim(message))
       endif
       if (restid /= ' ') then
          call prterr(lundia ,'Z013' ,'Using Smoothing time and restart file' )
       endif
       if (filic /= ' ') then
          call prterr(lundia ,'Z013' , &
                    & 'Using Smoothing time and initial condition file')
       endif
    endif
    !
    ! version 2_40; keywords Retti_surface, Retti_bottom
    !
    ! locate 'Rettis' record for constituent return time at the surface
    ! (only if lstsc > 0) possible keyword not in md-file
    !
    if (lstsc > 0) then
       allocate(tmp_rettim(mxdnto, lstsc, 2))
       tmp_rettim = 0.0_fp
       !
       ! Old parameter Rettim: use it for both the bottom and the surface layer
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Rettim', tmp_rettim(:,1,1), mxdnto)
       do n = 1, mxdnto
          do l = 2, lstsc
             tmp_rettim(n,:,:) = tmp_rettim(n,1,1)
          enddo
       enddo
       !
       ! Constituent independent input: if present, overwrite current values
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Rettis', tmp_rettim(:,1,1), mxdnto)
       call prop_get(gdp%mdfile_ptr, '*', 'Rettib', tmp_rettim(:,1,2), mxdnto)
       !
       ! Copy to all constituents
       !
       do n =  1, mxdnto
          do l = 2, lstsc
             tmp_rettim(n,l,:) = tmp_rettim(n,1,:)
          enddo
       enddo
       !
       ! Constituent dependent input: if present, overwrite current values
       !
       do l = 1, lstsc
          if (l == lsal) then
             call prop_get(gdp%mdfile_ptr, '*', 'RetsS', tmp_rettim(:,l,1), mxdnto)
             call prop_get(gdp%mdfile_ptr, '*', 'RetbS', tmp_rettim(:,l,2), mxdnto)
          elseif (l == ltem) then
             call prop_get(gdp%mdfile_ptr, '*', 'RetsT', tmp_rettim(:,l,1), mxdnto)
             call prop_get(gdp%mdfile_ptr, '*', 'RetbT', tmp_rettim(:,l,2), mxdnto)
          else
             write(keyw,'(a,i2.2)') "Rets", l-max(ltem,lsal)
             call prop_get(gdp%mdfile_ptr, '*', keyw, tmp_rettim(:,l,1), mxdnto)
             write(keyw,'(a,i2.2)') "Retb", l-max(ltem,lsal)
             call prop_get(gdp%mdfile_ptr, '*', keyw, tmp_rettim(:,l,2), mxdnto)
          endif
       enddo
       if (parll) then
          do i = 1, nto
             j = bct_order(i)
             rettim(i,:,:) = tmp_rettim(j,:,:)
          enddo
       else
          rettim = tmp_rettim
       endif
       deallocate(tmp_rettim)
       do n = 1, nto
          do l = 1, lstsc
          !
          ! test value of rettim not negative
          !
          if (comparereal(rettim(n,l,1),0.0_fp) == -1) then
             write (errmsg, '(a,i0,a)') 'Thatcher-Harleman return time delay at surface of open boundary nr. ', n, ' < 0.:'
             call prterr(lundia, 'U190', errmsg)
             write (lundia, '(a,i0,a)') '            In the boundary point, constituent nr.', l, ':'
             write (lundia, '(a)')      '            The value will be reflected from the inner point.'
          endif
          if (comparereal(rettim(n,l,2),0.0_fp) == -1) then
             write (errmsg, '(a,i0,a)')  'Thatcher-Harleman return time delay at bed level of open boundary nr. ', n, ' < 0.:'
             call prterr(lundia, 'U190', errmsg)
             write (lundia, '(a,i0,a)')  '            In the boundary point, constituent nr.', l, ':'
             write (lundia, '(a,f12.3)') '            The surface value will be used: ', rettim(n,l,1)
             rettim(n,l,2) = rettim(n,l,1)
          endif
          !
          ! Define RETTIM in TUNIT units
          !
          rettim(n,l,1) = rettim(n,l,1) * tunit
          rettim(n,l,2) = rettim(n,l,2) * tunit
          enddo
       enddo
    endif
    !
    ! Relaxation parameter for QH boundaries; by default 0 (no relaxation)
    !
    thetqh = 0.0
    thetqt = 0.0
    if (ntoq > 0) then
       call prop_get(gdp%mdfile_ptr, '*', 'ThetQH', thetqh)
    endif
    call prop_get(gdp%mdfile_ptr, '*', 'ThetQT', thetqt)
    !
    ! Flag for discharge distribution at Qtot boundaries; by default now based on average waterlevel
    !
    use_zavg_for_qtot = .true.
    call prop_get(gdp%mdfile_ptr, '*', 'ZAvgQT', use_zavg_for_qtot)
    !
    ! read flag for CoaST BouNDary
    !
    keyw  = 'Cstbnd'
    ntrec = nrrec
    nlook = 1
    lkw   = 6
    lenc  = 1
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    !
    ! keyword in mdf file (FOUND) then read (default = N/n)
    !
    if (found) then
       cdef  = 'n'
       chulp = cdef
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       if (lerror) then
          error = .true.
          goto 9999
       else
          call small(chulp, 1)
          if (chulp == 'y') cstbnd = .true.
       endif
    endif
    !
    ! write diagnostic message flag for CoaST BouNDary = .true.
    !
    if (cstbnd) then
       write (lundia, '(a)') '*** Flag for Coast Boundary activated'
    endif
    !
    ! read flag for Neumann Boundary type
    !
    keyw  = 'Bndneu'
    ntrec = nrrec
    nlook = 1
    lkw   = 6
    lenc  = 1
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    !
    ! keyword in mdf file (FOUND) then read (default = N/n)
    !
    bndneu = .false.
    if (found) then
       cdef  = 'n'
       chulp = cdef
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       if (lerror) then
          error = .true.
          goto 9999
       else
          call small(chulp, 1)
          if (chulp == 'y') bndneu = .true.
       endif
    endif
    !
    ! write diagnostic message flag for BouNDary NEUman = .true.
    !
    if (bndneu) then
       intor = 0
       do n = 1, nto
          if (typbnd(n) == 'R') then
             intor = intor + 1
          endif
       enddo
       if (intor /= 0) then
          write (lundia, '(a)') '*** Flag for Neumann Boundary activated for: '
          do n = 1, nto
             if (typbnd(n) == 'R') then
                write (lundia, '(a)') nambnd(n)
             endif
          enddo
          call prterr(lundia ,'Z013' ,'Use boundary type ''N'' instead of the ''Bndneu'' keyword' )
       endif
    endif
    !
    ! Get average pressure paver, used for the pressure correction on boundaries from the MDF file
    ! Specified using keyword PavBnd (AVerage Pressure on BouNDaries) in [Pa]
    !
    pcorr = .false.
    paver = 0.0_fp
    call prop_get(gdp%mdfile_ptr, '*', 'PavBnd', paver, present)
    if (.not. present) then
       pcorr = .false.
    else
       message = ' '
       write(message,'(a,f14.3,a)') 'Specified pressure for boundary correction = ', paver, ' Pa'
       call prterr(lundia, 'G051', trim(message))
       pcorr = .true.
    endif
    !
 9999 continue
end subroutine rdbcg
