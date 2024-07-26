subroutine rstfil(lundia    ,error     ,restid    ,lturi     ,mmax      , &
                & nmaxus    ,kmax      ,lstsci    ,ltur      , &
                & s1        ,u1        ,v1        ,r1        ,rtur1     , &
                & umnldf    ,vmnldf    ,kfu       ,kfv       , &
                & dpd       ,namcon    ,coninit   ,gdp       )
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
    use rdarray, only: rdarray_nm, rdarray_nmk, rdarray_nmkl
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
    integer       , dimension(:,:)       , pointer :: iarrc
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
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: dpd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: umnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: vmnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(out) :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(out) :: r1     !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                             :: restid !!  Run identification of the restart file. If RESTID = non-blank then current simulation will use this file for setting the initial conditions
    character(20), dimension(lstsci + ltur)                                    , intent(in)  :: namcon !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                              :: ftype   ! File type FTYPE_UNFORM32 or FTYPE_UNFORM64
    integer                                              :: idate
    integer                                              :: idum    !< value used for initialization of integer arrays
    integer                                              :: ierror
    integer                                              :: iprec
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
    character(16)                                        :: datetime
    character(300)                                       :: restid0 ! File name restart file 300 = 256 + a bit
    character(300)                                       :: filtmp  ! File name restart file 300 = 256 + a bit
    character(256)                                       :: filpath ! Path specification of restid
    integer                                              :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    real(fp), dimension(:,:,:), pointer                  :: rst_rtur1
    real(fp)                                             :: rdum    !< value used for initialization of real arrays
!
!! executable statements -------------------------------------------------------
!
    julday              => gdp%gdinttim%julday
    tstart              => gdp%gdexttim%tstart
    !
    mfg                 => gdp%gdparall%mfg
    mlg                 => gdp%gdparall%mlg
    nfg                 => gdp%gdparall%nfg
    nlg                 => gdp%gdparall%nlg
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl
    iarrc               => gdp%gdparall%iarrc
    mf                  => gdp%gdparall%mf
    ml                  => gdp%gdparall%ml
    nf                  => gdp%gdparall%nf
    nl                  => gdp%gdparall%nl
    !
    error = .false.
    nm_pos = 1
    !
    ! Not all array entries are filled by the data from the restart file.
    ! This may cause non-relevant entries to have a random value.
    ! These values might be NaN ... which cause the nan_check of the whole
    ! array to unnecessarily raise the alarm. Initialize all array entries
    ! using an extreme number such that it's still clear if these array
    ! entries used.
    ! idum = -9999999
    ! rdum = -9999999.0_fp
    ! Using these values cause testcases to fail, see DELFT3D-37763
    ! Therefore the following values are used temporary inside the rdarray_nm calls
    !
    idum = 0
    rdum = 0.0_fp
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
          restid0 = restid
          write (restid, '(2a)') trim(filpath), trim(restid0)
          call restart_trim_flow(lundia    ,error     ,restid    ,lturi     ,mmax      , &
                               & nmaxus    ,kmax      ,lstsci    ,ltur      , &
                               & s1        ,u1        ,v1        ,r1        ,rtur1     , &
                               & umnldf    ,vmnldf    ,kfu       ,kfv       , &
                               & dpd       ,ex_nfs    ,namcon    ,coninit   ,rdum      , &
                               & idum      ,gdp       )
          if (error .and. .not.ex_nfs) then
             call prterr(lundia    ,'G004'    , &
                 & 'tri-rst.' // trim(restid0) // trim(datetime) // ', tri-rst.' // trim(restid0) // &
                 & ' and ' // trim(restid0) // '.dat/.def')
          endif
       endif
    endif
    if (.not.error .and. .not.ex_nfs) then
       !
       ! restart file found
       !
       write(lundia, '(a)') 'Restarting from ' // trim(filtmp)
       !
       ! the restart file is opened and read by the master
       !
       if (inode == master) then
          !
          ! Determine whether the restart file is single or double precision based on the record length
          ! This check is based on the assumption of a 4 bytes integer representing the record length
          ! (ifort default convention, i.e. not /assume:byterecl).
          !
          open (newunit=luntmp, file = trim(filtmp), form = 'unformatted',              &
               & access = 'direct', recl = 4, status = 'old')
          read (luntmp, rec=1) l
          close(luntmp)
          !
          iprec = l/nmaxgl/mmaxgl
          if (iprec==4) then
              ftype = FTYPE_UNFORM32
          elseif (iprec==8) then
              ftype = FTYPE_UNFORM64
          else
              ftype = FTYPE_UNKNOWN
          endif
          !
          open (newunit=luntmp, file = trim(filtmp), form = 'unformatted',              &
               & status = 'old')
       endif
       !
       call dfbroadc_gdp ( ftype, 1, dfint, gdp )
       if ( ftype == FTYPE_UNFORM32 ) then
           write(lundia, '(a)') 'This is a single precision unformatted restart file'
       elseif ( ftype == FTYPE_UNFORM64 ) then
           write(lundia, '(a)') 'This is a double precision unformatted restart file'
       else
           write(lundia, '(a)') '*** ERROR Unable to determine restart file type'
           ierror = 1
           goto 9999
       endif
       !
       call rdarray_nm(luntmp, filtmp, ftype, 'DUMMY', 0, &
                    & nf, nl, mf, ml, iarrc, gdp, &
                    & ierror, lundia, s1, 'DUMMY', rdum)
       if (ierror /= 0) goto 9999
       !
       call rdarray_nmk(luntmp, filtmp, ftype, 'DUMMY', 0, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & 1, kmax, ierror, lundia, u1, 'DUMMY', rdum)
       if (ierror /= 0) goto 9999
       !
       call rdarray_nmk(luntmp, filtmp, ftype, 'DUMMY', 0, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & 1, kmax, ierror, lundia, v1, 'DUMMY', rdum)
       if (ierror /= 0) goto 9999
       !
       ! per constituent l: kmax nmaxus mmax values in r1 array
       ! only Salinity, Temperature, real constituents and secondary
       ! flow; no turbulence
       !
       if (lstsci > 0) then
          call rdarray_nmkl(luntmp, filtmp, ftype, 'DUMMY', 0, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, lstsci, ierror, lundia, r1, 'DUMMY', rdum)
          if (ierror /= 0) goto 9999
          coninit = 1
       endif
       !
       ! Per turbulence l: 0:kmax nmaxus mmax values in rtur1 array
       !
       if (ltur>0) then
          lturi = 0
          allocate(rst_rtur1(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), stat = ierror)
          call rdarray_nmk(luntmp, filtmp, ftype, 'DUMMY', 0, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 0, kmax, ierror, lundia, rst_rtur1, 'DUMMY', rdum)
          if (ierror /= 0) then
              ! If no turbulence arrays on restart file then rtur1 will be
              ! initialized in INITUR
              lturi = ltur
              ierror = 0
          else
              lturi = 0
              do k = 0, kmax
                  do m = gdp%d%mlb, gdp%d%mub
                      do n = gdp%d%nlb, gdp%d%nub
                          rtur1(n,m,k,1) = rst_rtur1(n,m,k)
                      enddo
                  enddo
              enddo
          endif
          !
          if (ltur==2 .and. lturi==0) then
              call rdarray_nmk(luntmp, filtmp, ftype, 'DUMMY', 0, &
                            & nf, nl, mf, ml, iarrc, gdp, &
                            & 0, kmax, ierror, lundia, rst_rtur1, 'DUMMY', rdum)
              if (ierror /= 0) then
                  ! If only K on restart file EPS will be calculated in INITUR
                  lturi = -ltur
                  ierror = 0
              else
                  do k = 0, kmax
                     do m = gdp%d%mlb, gdp%d%mub
                        do n = gdp%d%nlb, gdp%d%nub
                           rtur1(n,m,k,2) = rst_rtur1(n,m,k)
                        enddo
                     enddo
                  enddo
              endif
          endif
          deallocate(rst_rtur1)
       endif
       !
       ! read filtered velocity components to allow restarts
       ! using subgrid viscosity model
       !
       call rdarray_nm(luntmp, filtmp, ftype, 'DUMMY', 0, &
                    & nf, nl, mf, ml, iarrc, gdp, &
                    & ierror, lundia, umnldf, 'DUMMY', rdum)
       if (ierror /= 0) then
           ierror = 0
           goto 9999
       endif
       !
       call rdarray_nm(luntmp, filtmp, ftype, 'DUMMY', 0, &
                    & nf, nl, mf, ml, iarrc, gdp, &
                    & ierror, lundia, vmnldf, 'DUMMY', rdum)
       if (ierror /= 0) goto 9999
       !
       ! close file
       !
 9999  continue
       if (inode == master) close (luntmp)
       !
       if (ierror == 0) then
           if ( .not. nan_check(s1    , 's1 (restart-file)'    , lundia, gdp%d%nlb, gdp%d%mlb) .or. &
              & .not. nan_check(u1    , 'u1 (restart-file)'    , lundia, gdp%d%nlb, gdp%d%mlb, 1) .or. &
              & .not. nan_check(v1    , 'v1 (restart-file)'    , lundia, gdp%d%nlb, gdp%d%mlb, 1) .or. &
              & .not. nan_check(r1    , 'r1 (restart-file)'    , lundia, gdp%d%nlb, gdp%d%mlb, 1, 1) .or. &
              & .not. nan_check(rtur1 , 'rtur1 (restart-file)' , lundia, gdp%d%nlb, gdp%d%mlb, 0, 1) .or. &
              & .not. nan_check(umnldf, 'umnldf (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb) .or. &
              & .not. nan_check(vmnldf, 'vmnldf (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb)      ) then
               ierror = 1
           endif
       endif
       if (ierror /= 0) error = .true.
    endif
    write (lundia, '(a)') '*** End   of restart messages'
    write (lundia, *)
end subroutine rstfil
