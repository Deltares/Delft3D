subroutine rdnum(lunmd     ,lundia    ,nrrec     ,mdfrec    , &
               & iter1     ,dryflc    ,drycrt    ,dco       ,ibaroc    , &
               & kmax      ,lstsci    ,icreep    ,trasol    ,momsol    , &
               & dgcuni    ,forfuv    ,forfww    ,ktemp     ,temint    , &
               & keva      ,evaint    ,old_corio ,dpsopt    ,dpuopt    , &
               & zmodel    ,gammax    ,fwfac     , &
               & nudge     ,nudvic    ,v2dwbl    ,ftauw     ,gdp       )
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
!    Function: - Reads numerical parameters ITER, DRYFLC and
!                BAROCP from the MDF file
!              - reads TEMINT if KTEMP <> 0, EVAINT if KEVA <> 0
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use string_module
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp), pointer :: ad_epsabs
    real(fp), pointer :: ad_epsrel
    real(fp), pointer :: depini
    integer , pointer :: ad_itrmax
    integer , pointer :: itis
    logical , pointer :: chz_k2d
    logical , pointer :: slplim
!
! Global variables
!
    integer      , intent(out) :: ibaroc    !  Description and declaration in numeco.igs
    integer      , intent(out) :: icreep    !  Description and declaration in tricom.igs
    integer                    :: iter1     !  Description and declaration in numeco.igs
    integer      , intent(in)  :: keva      !  Description and declaration in tricom.igs
    integer      , intent(in)  :: kmax      !  Description and declaration in esm_alloc_int.f90
    integer      , intent(in)  :: ktemp     !  Description and declaration in tricom.igs
    integer      , intent(in)  :: lstsci    !  Description and declaration in esm_alloc_int.f90
    integer                    :: lundia    !  Description and declaration in inout.igs
    integer                    :: lunmd     !  Description and declaration in inout.igs
    integer                    :: nrrec     !  Pointer to the record number in the MD-file
    integer                    :: nudge     !  Description and declaration in procs.igs
    logical      , intent(in)  :: zmodel    !  Description and declaration in procs.igs
    logical                    :: old_corio !  Description and declaration in numeco.igs
    logical                    :: v2dwbl    !  Description and declaration in numeco.igs
    real(fp)                   :: dco       !  Description and declaration in numeco.igs
    real(fp)                   :: dgcuni
    real(fp)                   :: drycrt    !  Description and declaration in numeco.igs
    real(fp)                   :: dryflc    !  Description and declaration in numeco.igs
    real(fp)                   :: fwfac     !  Description and declaration in numeco.igs
    real(fp)                   :: ftauw     !  Description and declaration in numeco.igs
    real(fp)                   :: gammax    !  Description and declaration in numeco.igs
    real(fp)                   :: nudvic    !  Description and declaration in numeco.igs
    character(*)               :: mdfrec    !  Standard rec. length in MD-file (300)
    character(1) , intent(out) :: evaint    !  Description and declaration in tricom.igs
    character(1)               :: forfuv    !  Description and declaration in tricom.igs
    character(1)               :: forfww    !  Description and declaration in tricom.igs
    character(1) , intent(out) :: temint    !  Description and declaration in tricom.igs
    character(13)              :: trasol    !  Description and declaration in tricom.igs
    character(6)               :: momsol
    character(8)               :: dpsopt    !  Description and declaration in numeco.igs
    character(8)               :: dpuopt
!
! Local variables
!
    integer                     :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                     :: lkw
    integer                     :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                     :: ntrec  ! Help. var to keep track of NRREC 
    logical                     :: defaul ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                     :: found
    logical                     :: lerror ! Flag=TRUE if an error is encountered
    logical                     :: lvalue
    logical                     :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    real(fp)                    :: rdef   ! Help var. containing default va- lue(s) for real variable 
    real(fp)     , dimension(1) :: rval   ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    character(13)               :: cdef   ! Default value when CHULP not found 
    character(13)               :: chulp  ! Help character variable 
    character(13), dimension(3) :: traopt ! Transport model options cyclic-method and van leer-2 
    character(6)                :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(200)              :: msg
!
    data traopt/'cyclic-method', 'van leer-2   ', 'iupw         '/
!
!! executable statements -------------------------------------------------------
!
    ad_itrmax   => gdp%gdnumeco%ad_itrmax
    ad_epsabs   => gdp%gdnumeco%ad_epsabs
    ad_epsrel   => gdp%gdnumeco%ad_epsrel
    depini      => gdp%gdnumeco%depini
    slplim      => gdp%gdnumeco%slplim
    itis        => gdp%gdrdpara%itis
    chz_k2d     => gdp%gdrivpro%chz_k2d
    !
    ! initialize local parameters
    !
    lerror = .false.
    newkw  = .true.
    defaul = .true.
    found  = .false.
    nlook  = 1
    !
    ! initialize parameters that are to be read
    ! 
    iter1  = 2
    ad_itrmax = 50
    ad_epsabs = 1.e-8_fp
    ad_epsrel = 0.5e-3_fp
    dryflc = 0.1_fp
    drycrt = -999.999_fp
    dco    = -999.999_fp
    dgcuni = 0.5_fp
    gammax = 1.0_fp
    fwfac  = 1.0_fp
    nudvic = -1.0_fp
    ftauw  = 1.0_fp
    !
    icreep = 999
    ibaroc = 1
    trasol = traopt(1)
    forfuv = 'Y'
    forfww = 'N'
    nudge  = 0
    !
    temint = 'Y'
    evaint = 'Y'
    !
    ! old_corio is intended to be false by default
    ! but that can only be done when the new corio method is tested
    ! until then, old_corio is true by default
    !
    old_corio = .true.
    slplim    = .false.
    v2dwbl    = .false.
    !
    ! locate and read 'Dpuopt' record for determining DPU procedure
    ! dpuopt initialised as ' ', to allow special checks on combinations  
    ! with other settings in ck_dpopt.
    !
    dpuopt = ' '
    lenc   = 8
    call prop_get(gdp%mdfile_ptr, '*', 'Dpuopt', dpuopt)
    if (dpuopt(1:lenc) /= ' ') then
       !
       ! Make sure dpuopt is in upper case
       !
       call str_upper(dpuopt, lenc)
    endif
    !
    ! locate and read 'Waqopt' option
    ! default = no ('N') which means MOMCOR = 'cyclic'
    ! WARNING: momsol must be read/set before Iter is read/set!
    !
    momsol = 'cyclic'
    keyw   = 'Waqopt'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 1
       cdef = 'N'
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp(:lenc)         ,cdef(:lenc)          ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
       else
          !
          ! set MOMSOL to WAQUA if CHULP = Y/y
          !
          if (chulp(:1)=='Y' .or. chulp(:1)=='y') then
             momsol = 'waqua '
             call prterr(lundia, 'G051', 'Waqua option enabled')
          endif
       endif
    endif
    !
    ! Solver for momentum equation
    ! options:
    !     Cyclic ( Default for sigma-model )
    !     Waqua
    !     Flood
    !     MDUE   ( Multi-Directional Upwind Explicit (default for z-model) )
    !     MDUI   ( Multi-Directional Upwind Implicit )
    !     IUPW   ( Implicit Upwind (first order accurate) ) 
    !     FINVOL ( Conservative Finite Volume approach )
    ! If parameter momsol == 'waqua ', before reading Momsol from md-file,
    ! then the waqua option has been enabled in the old fashioned way
    ! (Waqopt=#Yes#).
    ! The md-file may not contain both the keywords 'Waqopt' and 'Momsol'.
    !
    keyw  = 'Momsol'
    ntrec = nrrec
    lkw   = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       if (momsol == 'waqua ') then
          call prterr(lundia, 'P004', 'Usage of both "Momsol" and "Waqopt" is not allowed')
          call d3stop(1,gdp)
       else
          lenc = 6
          cdef = 'cyclic'
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chulp(:lenc)         ,cdef(:lenc)          ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
             if (zmodel) then
                momsol = 'mdue  '
             else
                momsol = 'cyclic'
             endif
             call prterr(lundia    ,'V078'    ,momsol   )
          else
             momsol = chulp(:lenc)
             call small(momsol,lenc)
             if (zmodel) then
                if (momsol == 'mdue  ') then
                   call prterr(lundia    ,'V079'    ,momsol    )
                elseif (momsol == 'mdui  ') then
                   call prterr(lundia    ,'V079'    ,momsol    )
                elseif (momsol == 'iupw  ') then
                   call prterr(lundia    ,'V079'    ,momsol    )
                elseif (momsol == 'flood ') then
                   call prterr(lundia    ,'V079'    ,momsol    )
                elseif (momsol == 'finvol') then
                   call prterr(lundia    ,'V079'    ,momsol    )
                else
                   msg = 'Using default momentum solver "Multi Directional Upwind Explicit"'
                   call prterr(lundia    ,'Z013'    ,trim(msg)   )
                   msg = momsol//' can not be used as momentum solver in z-model'
                   write (lundia,'(a,a)') '            ',trim(msg)
                   momsol = 'mdue  '
                endif
             else
                if (momsol == 'cyclic' .or. momsol == 'waqua ' .or. momsol == 'flood ') then
                   call prterr(lundia    ,'V079'    ,momsol    )
                else   
                   msg = 'Using default momentum solver "Cyclic"'
                   call prterr(lundia    ,'Z013'    ,trim(msg)   )
                   msg = momsol//' can not be used as momentum solver in sigma-model'
                   write (lundia,'(a,a)') '            ',trim(msg)
                   momsol = 'cyclic'
                endif
             endif
          endif
       endif
    else
       if (zmodel) then
          momsol = 'mdue  '
          call prterr(lundia    ,'V078'    ,momsol   )
       else
          if (momsol /= 'waqua ') then
             momsol = 'cyclic'
             call prterr(lundia    ,'V078'    ,momsol       )
          endif
       endif
    endif
    if (momsol == 'flood ') then
       !
       ! Set Iter1 to 1; can be overruled by specifying Iter.
       !
       iter1 = 1
       !
       ! locate 'Dgcuni' Criterion to determine at which points the
       ! 'flow accelerating zone' exists. In this case the water level
       ! downstream is influenced by the level upstream when flow is 
       ! super critical (see UPWHU)
       ! Default value of Dgcuni = 0.5 meter
       !
       keyw  = 'Dgcuni'
       ntrec = nrrec
       rdef  = dgcuni
       lkw   = 6
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       lerror = .false.
       !
       ! not found ?
       !
       if (found) then
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
             dgcuni = rdef
          else
             dgcuni = rval(1)
          endif
       endif
    endif
    !
    chz_k2d = .false.
    if (momsol == 'waqua') chz_k2d = .true.
    call prop_get(gdp%mdfile_ptr, '*', 'ChzK2d', chz_k2d)
    !
    ! 'Iter  ' restrict number of iterations
    ! WARNING: momsol is supposed to be already read/set
    !          then iter1 has a default value
    !
    call prop_get(gdp%mdfile_ptr, '*', 'Iter', iter1)
    !
    ! Numerical settings for iteration in advection-diffusion solver (DIFU)
    !
    call prop_get(gdp%mdfile_ptr, '*', 'ADmxIt', ad_itrmax)
    call prop_get(gdp%mdfile_ptr, '*', 'ADepAb', ad_epsabs)
    call prop_get(gdp%mdfile_ptr, '*', 'ADepRl', ad_epsrel)
    !
    ! Before DPSOPT existed, Delft3D-FLOW used the variable DRYFLP.
    ! This variable could be set to NO or MIN/MAX/MEAN. With the
    ! introduction of DPSOPT, however, DRYFLP has been redefined to
    ! be NO or YES. Since the value of DRYFLP was, however, used
    ! nowhere in the code. The variable DRYFLP has been removed
    ! (except as name for DPSOPT in the COM file). For backward
    ! compatibility we still read the Dryflp record first, before
    ! reading Dpsopt.
    !
    ! WARNING: Dryflp must be read before Dpsopt
    !
    dpsopt = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Dryflp', dpsopt)
    !
    ! locate and read 'Dpsopt' record for determining DPS procedure
    !
    chulp = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Dpsopt', chulp)
    !
    lenc = 8
    if (dpsopt(1:lenc) /= ' ' .and. chulp(1:lenc) /= ' ') then
       dpsopt = chulp(:lenc)
       msg = 'DRYFLP and DPSOPT both specified in MD-file. Using DPSOPT: '//dpsopt(1:lenc)
       call prterr(lundia, 'G051', trim(msg))
    elseif (chulp(1:lenc) /= ' ') then
       dpsopt = chulp(:lenc)
    elseif (dpsopt(1:lenc) /= ' ') then
       !
       ! Dpsopt given by Dryflp
       !
    else
       !
       ! Dryflp and Dpsopt both not specified. Dpsopt given a value in ck_dpopt.
       !
    endif
    if (dpsopt(1:lenc) /= ' ') then
       !
       ! Make sure dpsopt is in upper case
       !
       call str_upper(dpsopt, lenc)
    endif
    !
    ! 'Dryflc': threshold depth for drying flooding
    !
    call prop_get(gdp%mdfile_ptr,'*','Dryflc',dryflc)
    !
    ! 'Drycrt': threshold depth for drying
    !
    call prop_get(gdp%mdfile_ptr,'*','Drycrt',drycrt)
    !
    if (drycrt<0.0_fp) then
       !
       ! Drycrt not specified in mdf file, set to 0.5 * dryflc
       !
       drycrt = 0.5_fp*dryflc
       !
    else
       write(msg,'(a,e12.2)') "Drying criterium read: ", drycrt
       call prterr(lundia, 'G051', trim(msg))
       !
       ! Drycrt may not exceed dryflc
       !
       if (drycrt > dryflc) then
          drycrt = dryflc
          write(msg,'(a,e12.2)') "Drying criterium is reduced to flooding criterium: ", drycrt
          call prterr(lundia, 'U190', trim(msg))
       endif
    endif
    !
    ! 'DepIni': Initial water depth in all dry cells
    !
    depini = 0.1_fp * dryflc
    call prop_get(gdp%mdfile_ptr, '*', 'DepIni', depini)
    !
    ! locate 'Dco' Criterion under which HU/HV is spatially smoothed
    !              when a point is almost dry
    !
    keyw  = 'Dco   '
    ntrec = nrrec
    rdef  = dco
    lkw   = 3
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          dco = rdef
       else
          dco = rval(1)
       endif
    endif
    !
    ! locate 'Sigcor' Criterion for sigma coordinate correction
    !                 Anti Creep (N=0, Y=1)
    !
    keyw  = 'Sigcor'
    ntrec = nrrec
    lkw   = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    ! Default value is 'N' with means ICREEP=0
    !
    if (found) then
       lenc = 1
       cdef(:1) = 'N'
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp(:lenc)         ,cdef(:lenc)          ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          icreep = 0
       else
          icreep = 0
          if (chulp(:1)=='Y' .or. chulp(:1)=='y') icreep = 1
       endif
    endif
    !
    ! locate 'BarocP' Criterion for barocline pressure on open boundary
    !                 points (N=0, Y=1)
    !
    keyw  = 'BarocP'
    ntrec = nrrec
    lkw   = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    ! Default value is 'Y' with means IBAROC=1
    !
    if (found) then
       lenc = 1
       cdef(:1) = 'Y'
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp(:lenc)         ,cdef(:lenc)          ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          ibaroc = 1
       else
          ibaroc = 1
          if (chulp(:1)=='N' .or. chulp(:1)=='n') ibaroc = 0
       endif
    endif
    !
    ! locate 'Nudge'  Criterion for nudging on open boundary
    !                 points (N=0, Y=1)
    !
    lvalue = .false.
    call prop_get(gdp%mdfile_ptr,'*','Nudge',lvalue)
    if (lvalue) then
       nudge = 1
       msg = 'Nudging of constituents applied at open boundaries'
       call prterr(lundia, 'Z013', trim(msg))
    else
       nudge = 0
    endif
    !
    ! Solver for transport and Forester filters only if LSTSCI <> 0
    !
    if (lstsci>0) then
       !
       ! Locate 'Trasol' for transport solver
       ! Options:
       !     cyclic-method (=traopt(1)) default for sigma-model, can not be used for z-model
       !     van leer-2    (=traopt(2)) default for z-model
       !     iupw          (=traopt(3)) Implicit upwind option for z-model
       !
       keyw  = 'Trasol'
       ntrec = nrrec
       lkw   = 6
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       lerror = .false.
       !
       ! not found ?
       !
       if (found) then
          lenc = 13
          cdef = trasol
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chulp(:lenc)         ,cdef(:lenc)          ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
             if (zmodel) then
                trasol = traopt(2)
             else
                trasol = traopt(1)
             endif
             call prterr(lundia    ,'V049'    ,trasol   )
          else
             trasol = chulp(:lenc)
             call small(trasol    ,lenc      )
             if (zmodel) then
                if (trasol == traopt(2)) then
                   call prterr(lundia    ,'V349'    ,trasol    )
                elseif( trasol == traopt(3)) then
                   call prterr(lundia    ,'V349'    ,trasol    )
                else
                   msg = 'Using default transport solver "Van Leer-2 (explicit)"'
                   call prterr(lundia    ,'Z013'    ,trim(msg)       )
                   msg = trasol//' can not be used as transport solver in z-model'
                   write (lundia,'(a,a)') '            ',trim(msg)
                   trasol = traopt(2)
                endif
             else
                if (trasol == traopt(1) .or. trasol == traopt(2)) then
                   call prterr(lundia    ,'V349'    ,trasol    )
                else   
                   msg = 'Using default transport solver "Cyclic"'
                   call prterr(lundia    ,'Z013'    ,trim(msg)       )
                   msg = trasol//' can not be used as transport solver in sigma-model'
                   write (lundia,'(a,a)') '            ',trim(msg)
                   trasol = traopt(1)
                endif
             endif
          endif
       else
          if (zmodel) then
             trasol = traopt(2)
          else
             trasol = traopt(1)
          endif
          call prterr(lundia    ,'V049'    ,trasol   )
       endif
       !
       ! locate 'Forfuv' Criterion for Forester filter for UV
       ! Default = 'Y'
       !
       keyw  = 'Forfuv'
       ntrec = nrrec
       lkw   = 6
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       lerror = .false.
       !
       ! not found ?
       !
       if (found) then
          lenc = 1
          cdef = forfuv
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,chulp(:lenc)         ,cdef(:lenc)          ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
             forfuv = cdef(:1)
          else
             forfuv = cdef(:1)
             if (chulp(:1)=='N' .or. chulp(:1)=='n') forfuv = 'N'
          endif
       endif
       !
       ! Locate 'Forfww' Criterion for Forester filter for W,
       ! only if KMAX > 1 Default = 'N'
       !
       if (kmax>1) then
          keyw  = 'Forfww'
          ntrec = nrrec
          lkw   = 6
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          lerror = .false.
          !
          ! not found ?
          !
          if (found) then
             lenc = 1
             cdef = forfww
             call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,chulp(:lenc)         ,cdef(:lenc)          ,lenc      ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             !
             ! reading error?
             !
             if (lerror) then
                lerror = .false.
                forfww = cdef(:1)
             else
                forfww = cdef(:1)
                if (chulp(:1)=='Y' .or. chulp(:1)=='y') forfww = 'Y'
             endif
          endif
       endif
    endif
    !
    ! Locate 'Temint' interpolation method for the temperature
    ! only if ktemp > 0 (per defintion then temp = .true.)
    ! default value allowed, default
    !
    if (ktemp>0) then
       keyw  = 'Temint'
       ntrec = nrrec
       lenc  = 1
       nlook = 0
       cdef  = 'Y'
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp(:lenc)         ,cdef(:lenc)          ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          temint = cdef(:1)
       else
          temint = cdef(:1)
          if (chulp(:1)=='N' .or. chulp(:1)=='n') temint = 'N'
       endif
    endif
    !
    ! Locate 'Evaint' interpolation method for the rain/evaporation
    ! only if KEVA > 0, default value allowed, default
    !
    if (keva>0) then
       keyw     = 'Evaint'
       ntrec    = nrrec
       lenc     = 1
       nlook    = 0
       cdef(:1) = 'Y'
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          evaint = cdef(:1)
       else
          evaint = cdef(:1)
          if (chulp(:1)=='N' .or. chulp(:1)=='n') evaint = 'N'
       endif
    endif
    !
    ! Gammax
    !
    rdef = -999.0_fp
    call prop_get(gdp%mdfile_ptr, '*', 'Gammax', rdef)
    if (comparereal(rdef, -999.0_fp) /= 0) then
       gammax = rdef
       write (lundia,'(a,e12.2)') '*** MESSAGE Gammax specified by user to be ', gammax
    endif
    !
    ! Boundary viscosity for nudging
    !
    rdef = -999.0_fp
    call prop_get(gdp%mdfile_ptr, '*', 'Nudvic', rdef)
    if (comparereal(rdef, -999.0_fp) /= 0) then
       nudvic = rdef
       write (lundia,'(a,e12.2)') '*** MESSAGE Boundary viscosity specified by user to be ', nudvic
    endif
    !
    ! FWFac
    ! See also rdmor
    !
    call prop_get(gdp%mdfile_ptr, '*', 'FWFac' , fwfac)
    !
    ! FTauW
    !
    call prop_get(gdp%mdfile_ptr, '*', 'FTauW' , ftauw)
    !
    ! Flag to switch on slope limiter to avoid high velocities along steep slopes
    ! (used in cucnp/z_cucnp and uzd/z_uzd)
    !
    call prop_get(gdp%mdfile_ptr, '*', 'SlpLim', slplim)
    if (slplim) then
       write (msg,'(a)') 'Found Keyword SlpLim = #Y#: switching on slope limiter to avoid high velocities along steep slopes'
       call prterr(lundia, 'G051', trim(msg))
    endif
    !
    ! Ocorio (Old_Corio)
    ! Keyword to switch back to old computation of the tangential velocities used in the coriolis term
    !
    call prop_get(gdp%mdfile_ptr, '*', 'OCorio' , old_corio)
    if (.not.old_corio) then
       write (lundia,'(a)') '*** MESSAGE Using Coriolis formulation of Kleptsova-Stelling-Pietrzak.'
    endif
    !
    ! Flag to switch on using velocities above wave boundary layer to determine representative 2d velocity
    !
    call prop_get(gdp%mdfile_ptr, '*', 'V2DWBL', v2dwbl)
    if (v2dwbl) then
       write (msg,'(a)') 'Found Keyword V2DWBL = #Y#: using velocities above wave boundary layer to determine representative 2d velocity'
       call prterr(lundia, 'G051', trim(msg))
    endif
end subroutine rdnum
