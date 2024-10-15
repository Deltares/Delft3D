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
!-------------------------------------------------------------------------------
module m_rdsed
use m_depfil_stm

private

!
! functions and subroutines
!
public rdsed
public count_sed
public echosed

contains

subroutine rdsed(lundia    ,error     ,lsal      ,ltem      ,lsed      , &
               & lsedtot   ,lstsci    ,ltur      ,namcon    ,iopsus    , &
               & nmlb      ,nmub      ,filsed    ,sed_ptr   , &
               & sedpar    ,trapar    ,griddim   )
!!--description-----------------------------------------------------------------
!
! Read sediment parameters from an input file
! File type:
!    ASCII-file if file version below 02.00
!    INI  -file if file version is 02.00 or higher
!
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use string_module
    use MessageHandling, only: mess, LEVEL_ERROR
    use message_module
    use morphology_data_module
    use sediment_basics_module
    use flocculation, only: FLOC_NONE, FLOC_MANNING_DYER, FLOC_CHASSAGNE_SAFAR, FLOC_VERNEY_ETAL
    use system_utils, only:SHARED_LIB_PREFIX, SHARED_LIB_EXTENSION
    use grid_dimens_module, only: griddimtype
    !
    implicit none
    !
    ! The following list of pointer parameters is used to point inside sedpar and trapar
    !
    real(fp)                           , pointer :: csoil
    real(fp)                           , pointer :: mdcuni
    real(fp)                           , pointer :: kssilt
    real(fp)                           , pointer :: kssand
    real(fp)                           , pointer :: sc_cmf1
    real(fp)                           , pointer :: sc_cmf2
    real(fp)                           , pointer :: sc_flcf
    real(fp)                           , pointer :: d_micro
    real(fp)                           , pointer :: ustar_macro
    integer                            , pointer :: nmudfrac
    integer                            , pointer :: sc_mudfac
    logical          , dimension(:)    , pointer :: cmpupdfrac
    real(fp)         , dimension(:)    , pointer :: tpsnumber
    real(fp)         , dimension(:)    , pointer :: rhosol
    real(fp)         , dimension(:,:,:), pointer :: logseddia
    real(fp)         , dimension(:)    , pointer :: logsedsig
    real(fp)         , dimension(:)    , pointer :: sedd10
    real(fp)         , dimension(:)    , pointer :: sedd50
    real(fp)         , dimension(:)    , pointer :: sedd50fld
    real(fp)         , dimension(:)    , pointer :: seddm
    real(fp)         , dimension(:)    , pointer :: sedd90
    real(fp)         , dimension(:)    , pointer :: cdryb
    real(fp)         , dimension(:)    , pointer :: dstar
    real(fp)         , dimension(:)    , pointer :: taucr
    real(fp)         , dimension(:)    , pointer :: tetacr
    real(fp)         , dimension(:)    , pointer :: facdss
    real(fp)         , dimension(:)    , pointer :: sdbuni
    real(fp)         , dimension(:)    , pointer :: sedtrcfac
    real(fp)         , dimension(:)    , pointer :: thcmud
    real(fp)         , dimension(:)    , pointer :: tcguni
    real(fp)         , dimension(:)    , pointer :: mudcnt
    real(fp)         , dimension(:)    , pointer :: pmcrit
    integer          , dimension(:)    , pointer :: nseddia
    integer          , dimension(:)    , pointer :: sedtyp
    integer          , dimension(:)    , pointer :: tratyp
    character(10)    , dimension(:)    , pointer :: inisedunit
    character(20)    , dimension(:)    , pointer :: namsed
    character(256)   , dimension(:)    , pointer :: flsdbd
    character(256)   , dimension(:)    , pointer :: flstcg
    logical                            , pointer :: anymud
    logical                            , pointer :: bsskin
    character(256)                     , pointer :: flsdia
    character(256)                     , pointer :: flsmdc
    character(256)                     , pointer :: flspmc
    character(256)   , dimension(:)    , pointer :: dll_function_settle
    character(256)   , dimension(:)    , pointer :: dll_name_settle
    integer(pntrsize), dimension(:)    , pointer :: dll_handle_settle
    character(256)   , dimension(:)    , pointer :: dll_usrfil_settle
    integer          , dimension(:)    , pointer :: iform_settle
    real(fp)         , dimension(:,:)  , pointer :: par_settle
    integer          , dimension(:)    , pointer :: iform
    character(256)   , dimension(:)    , pointer :: flstrn
    !
    integer                            , pointer :: flocmod
    integer                            , pointer :: nflocpop
    integer                            , pointer :: nflocsizes
    integer          , dimension(:)    , pointer :: flocsize
    integer          , dimension(:,:)  , pointer :: floclist
    character(20)    , dimension(:)    , pointer :: namclay
    character(20)    , dimension(:)    , pointer :: namflocpop
!
! Arguments
!
    integer                                  , intent(in)  :: lsal    !<  Description and declaration in dimens.igs
    integer                                  , intent(in)  :: lsed    !<  Description and declaration in esm_alloc_int.f90
    integer                                  , intent(in)  :: lsedtot !<  Description and declaration in esm_alloc_int.f90
    integer                                  , intent(in)  :: lstsci  !<  Description and declaration in esm_alloc_int.f90
    integer                                  , intent(in)  :: ltem    !<  Description and declaration in dimens.igs
    integer                                  , intent(in)  :: ltur    !<  Description and declaration in esm_alloc_int.f90
    integer                                  , intent(in)  :: lundia  !<  Description and declaration in inout.igs
    logical                                  , intent(out) :: error   !<  Flag=TRUE if an error is encountered
    character(20) , dimension(lstsci + ltur) , intent(in)  :: namcon  !<  Description and declaration in esm_alloc_char.f90
    integer                                  , intent(out) :: iopsus
    integer                                  , intent(in)  :: nmlb
    integer                                  , intent(in)  :: nmub
    character(len=*)                         , intent(in)  :: filsed
    type(tree_data)                          , pointer     :: sed_ptr
    type(sedpar_type)                        , pointer     :: sedpar
    type(trapar_type)                        , pointer     :: trapar
    type(griddimtype)             , target   , intent(in)  :: griddim
!
! Local variables
!
    integer                     :: i
    integer                     :: isize
    integer                     :: istat
    integer(pntrsize)           :: istat_ptr
    integer                     :: j
    integer                     :: l
    integer                     :: lenc                !< Help var. (length of character var.) 
    integer                     :: luninp
    integer                     :: n                   !< Temporary storage for nseddia(l)
    integer                     :: nclayfrac           ! Number of clay fractions
    integer                     :: nm
    integer                     :: version
    integer(pntrsize), external :: open_shared_library
    real(fp)                    :: rmissval
    real(fp)                    :: seddxx              !< Temporary storage for sediment diameter
    real(fp)                    :: sedsg               !< Temporary storage for geometric standard deviation
    real(fp)                    :: tpsmud, tpssand
    logical                     :: ex
    logical                     :: success
    character(11)               :: fmttmp !< Format file ('formatted  ') 
    character(20)               :: sedname
    character(256)              :: filtrn
    character(256)              :: rec
    character(80)               :: parname
    character(20)               :: sc_type
    character(10)               :: versionstring
    character(6)                :: seddxxstring
    character(256)              :: errmsg
    character(256)              :: floc_str
    character(256)              :: settle_str
    type(tree_data), pointer    :: sedblock_ptr
!
!! executable statements -------------------------------------------------------
!
    csoil                => sedpar%csoil
    mdcuni               => sedpar%mdcuni
    kssilt               => sedpar%kssilt
    kssand               => sedpar%kssand
    sc_cmf1              => sedpar%sc_cmf1
    sc_cmf2              => sedpar%sc_cmf2
    sc_flcf              => sedpar%sc_flcf
    d_micro              => sedpar%d_micro
    ustar_macro          => sedpar%ustar_macro
    flocmod              => sedpar%flocmod
    nflocpop             => sedpar%nflocpop
    nflocsizes           => sedpar%nflocsizes
    nmudfrac             => sedpar%nmudfrac
    sc_mudfac            => sedpar%sc_mudfac
    flocsize             => sedpar%flocsize
    floclist             => sedpar%floclist
    cmpupdfrac           => sedpar%cmpupdfrac
    tpsnumber            => sedpar%tpsnumber
    rhosol               => sedpar%rhosol
    logseddia            => sedpar%logseddia
    logsedsig            => sedpar%logsedsig
    sedd10               => sedpar%sedd10
    sedd50               => sedpar%sedd50
    sedd50fld            => sedpar%sedd50fld
    seddm                => sedpar%seddm
    sedd90               => sedpar%sedd90
    cdryb                => sedpar%cdryb
    dstar                => sedpar%dstar
    taucr                => sedpar%taucr
    tetacr               => sedpar%tetacr
    sdbuni               => sedpar%sdbuni
    thcmud               => sedpar%thcmud
    mudcnt               => sedpar%mudcnt
    pmcrit               => sedpar%pmcrit
    nseddia              => sedpar%nseddia
    sedtyp               => sedpar%sedtyp
    tratyp               => sedpar%tratyp
    inisedunit           => sedpar%inisedunit
    namclay              => sedpar%namclay
    namsed               => sedpar%namsed
    flsdbd               => sedpar%flsdbd
    anymud               => sedpar%anymud
    bsskin               => sedpar%bsskin
    flsdia               => sedpar%flsdia
    flsmdc               => sedpar%flsmdc
    flspmc               => sedpar%flspmc
    dll_function_settle  => trapar%dll_function_settle
    dll_name_settle      => trapar%dll_name_settle
    dll_handle_settle    => trapar%dll_handle_settle
    dll_usrfil_settle    => trapar%dll_usrfil_settle
    iform_settle         => trapar%iform_settle
    par_settle           => trapar%par_settle
    iform                => trapar%iform
    flstrn               => trapar%flstrn
    !
    rmissval = -999.0_fp
    fmttmp   = 'formatted'
    !
    istat = 0
    if (.not. associated(sedpar%sedd50)) then
       !
       ! allocation of namsed, rhosol, sedtyp and tratyp have been allocated in count_sed routine
       !
       if (istat==0) allocate (sedpar%cmpupdfrac(                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%tpsnumber (                          lsedtot), stat = istat)
       !
       if (istat==0) allocate (sedpar%sedblock  (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%nseddia   (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%logseddia (2, 101,                   lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%logsedsig (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%sedd10    (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%sedd50    (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%seddm     (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%sedd90    (                          lsedtot), stat = istat)
       !
       if (istat==0) allocate (sedpar%cdryb     (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%dstar     (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%taucr     (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%tetacr    (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%sdbuni    (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%sedtrcfac (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%flsdbd    (                          lsedtot), stat = istat)
       if (istat==0) allocate (sedpar%inisedunit(                          lsedtot), stat = istat)
       !
       if (istat==0) allocate (sedpar%dss       (nmlb:nmub,max(1,lsed)), stat = istat)
       if (istat==0) allocate (sedpar%facdss    (                      max(1,lsed)), stat = istat)
       if (istat==0) allocate (sedpar%tcguni    (                      max(1,lsed)), stat = istat)
       !
       if (istat==0) allocate (sedpar%thcmud    (nmlb:nmub            ), stat = istat)
       if (istat==0) allocate (sedpar%flstcg    (                      max(1,lsed)), stat = istat)
       !
       if (istat==0) allocate (sedpar%mudcnt    (nmlb:nmub            ), stat = istat)
       if (istat==0) allocate (sedpar%pmcrit    (nmlb:nmub            ), stat = istat)
       if (istat==0) allocate (sedpar%sedd50fld (nmlb:nmub            ), stat = istat)
       !
       if (istat==0) allocate (sedpar%namclay   (          max(1,lsed)), stat = istat)
       if (istat==0) allocate (sedpar%flocsize  (          max(1,lsed)), stat = istat)
       !
       if (istat/=0) then
          errmsg = 'RDSED: memory alloc error'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       !
       ! update local pointers
       !
       cmpupdfrac    => sedpar%cmpupdfrac
       tpsnumber     => sedpar%tpsnumber
       !
       nseddia       => sedpar%nseddia
       logseddia     => sedpar%logseddia
       logsedsig     => sedpar%logsedsig
       sedd10        => sedpar%sedd10
       sedd50        => sedpar%sedd50
       seddm         => sedpar%seddm
       sedd90        => sedpar%sedd90
       !
       cdryb         => sedpar%cdryb
       dstar         => sedpar%dstar
       taucr         => sedpar%taucr
       tetacr        => sedpar%tetacr
       sdbuni        => sedpar%sdbuni
       sedtrcfac     => sedpar%sedtrcfac
       flsdbd        => sedpar%flsdbd
       inisedunit    => sedpar%inisedunit
       !
       facdss        => sedpar%facdss
       thcmud        => sedpar%thcmud
       !
       mudcnt        => sedpar%mudcnt
       pmcrit        => sedpar%pmcrit
       sedd50fld     => sedpar%sedd50fld
       tcguni        => sedpar%tcguni
       flstcg        => sedpar%flstcg
       !
       namclay       => sedpar%namclay
       flocsize      => sedpar%flocsize
       !
       ! end check on assocation of sedpar%sedd50
       !
    endif 
    !
    ! Initialization of the just allocated arrays
    !
    cmpupdfrac = .TRUE.
    do i = 1,lsedtot
       sedpar%sedblock(i)%node_name => null()
       if (sedtyp(i) <= sedpar%max_mud_sedtyp) then
           tpsnumber(i) = 0.7_fp
       else
           tpsnumber(i) = 1.0_fp
       endif
    enddo
    flsdbd              = ' '
    flsmdc              = ' '
    flspmc              = ' '
    flsdia              = ' '
    dll_function_settle = ' '
    dll_usrfil_settle   = ' '
    flstcg              = ' '
    flstrn              = ' '
    !
    nseddia      = 0        ! nseddia counts relevant data
    logseddia    = rmissval
    logsedsig    = rmissval
    sedd10       = rmissval
    sedd50       = rmissval
    seddm        = rmissval
    sedd90       = rmissval
    !
    sedtrcfac    = rmissval
    !
    dstar        = rmissval
    taucr        = rmissval
    tetacr       = rmissval
    inisedunit   = 'kg/m2'
    !
    thcmud       = rmissval
    !
    mudcnt       =  0.0
    pmcrit       = -1.0
    sedd50fld    = rmissval
    !
    tcguni       = 1.5
    !
    ! Initialization of local parameters/arrays
    !
    version      = 0
    error        = .false.
    lenc         = 4
    !       
    facdss       = rmissval
    !
    seddxxstring = 'SedDXX'
    !
    ! check for mud fractions
    !
    anymud       = .false.
    nclayfrac    = 0
    nmudfrac     = 0
    flocsize     = -999
    namclay      = ' '
    do l = 1, lsed
       if (sedtyp(l) == SEDTYP_CLAY) then
          nclayfrac = nclayfrac + 1
       endif
       if (sedtyp(l) <= sedpar%max_mud_sedtyp) then
          anymud   = .true.
          nmudfrac = nmudfrac + 1
       endif
    enddo
    !
    ! Check version number of sed input file
    !
    versionstring = ' '
    call prop_get(sed_ptr, 'SedimentFileInformation', 'FileVersion', versionstring)
    if (versionstring == '02.00' .or. versionstring == '03.00') then
       if (versionstring == '03.00') sedpar%version = 3.0_fp
       error  = .false.
       !
       csoil  = 1.0e4_fp
       call prop_get(sed_ptr, 'SedimentOverall', 'Cref', csoil)
       !
       tpsmud  = 0.7_fp
       tpssand = 1.0_fp
       call prop_get(sed_ptr, 'SedimentOverall', 'MudTPS', tpsmud) ! for back compatibility
       call prop_get(sed_ptr, 'SedimentOverall', 'SchmidtNumberMud' , tpsmud ) ! value for mud, for consistency with UNST-7622
       call prop_get(sed_ptr, 'SedimentOverall', 'SchmidtNumberSand', tpssand) ! value for sand
       do i = 1,lsed
          if (sedtyp(i) <= sedpar%max_mud_sedtyp) then
              tpsnumber(i) = tpsmud
          else
              tpsnumber(i) = tpssand
          endif
       enddo
       !
       iopsus = 0
       call prop_get(sed_ptr, 'SedimentOverall', 'IopSus', iopsus)
       !
       call prop_get(sed_ptr, 'SedimentOverall', 'MudCnt', flsmdc)
       !
       floc_str = 'none'
       call prop_get(sed_ptr, 'SedimentOverall', 'FlocModel', floc_str)
       call str_lower(floc_str)
       select case (floc_str)
       case ('none')
          flocmod = FLOC_NONE
          nflocsizes = 1
       case ('manning_dyer')
          flocmod = FLOC_MANNING_DYER
          nflocsizes = 2
       case ('chassagne_safar')
          flocmod = FLOC_CHASSAGNE_SAFAR
          nflocsizes = 2
       case ('verney_etal')
          flocmod = FLOC_VERNEY_ETAL
          nflocsizes = -999
       case default
           errmsg = 'Unknown flocculation model "'//trim(floc_str)//'" specified.'
           call write_error(errmsg, unit=lundia)
           error = .true.
           return
       end select
       !
       call prop_get(sed_ptr, 'SedimentOverall', 'NFlocSizes', nflocsizes)
       select case (flocmod)
       case (FLOC_MANNING_DYER, FLOC_CHASSAGNE_SAFAR)
          if (nflocsizes /= 1 .and. nflocsizes /= 2) then
             errmsg = 'NFlocSizes must be 1 or 2 for the selected flocculation model.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
       case (FLOC_VERNEY_ETAL)
          if (nflocsizes == -999) then
             errmsg = 'NFlocSizes must be specified when using the population balance model.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          elseif (nflocsizes < 1) then
             errmsg = 'Invalid value specified for NFlocSizes.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
       end select
       !
       if (flocmod /= FLOC_NONE) then
          nflocpop = nclayfrac / nflocsizes
          !
          if (istat==0) allocate (sedpar%namflocpop(nflocpop), stat = istat)
          namflocpop    => sedpar%namflocpop
          namflocpop = ' '
          !
          if (nflocpop * nflocsizes /= nclayfrac) then
             write(errmsg,'(a,i0,a,i0,a)') 'The number of clay fractions (',nclayfrac,') is not a multiple of the number of floc sizes (',nflocsizes,').'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          !
          call prop_get(sed_ptr, 'SedimentOverall', 'TFloc', sedpar%tfloc)
          sedpar%tbreakup = sedpar%tfloc
          call prop_get(sed_ptr, 'SedimentOverall', 'TBreakUp', sedpar%tbreakup)
          !
          call prop_get(sed_ptr, 'SedimentOverall', 'DiaMicro', d_micro)
          call prop_get(sed_ptr, 'SedimentOverall', 'UstarMacro', ustar_macro)
       endif
       !
       sedpar%flnrd(0) = ' '
       call prop_get(sed_ptr, 'SedimentOverall', 'NodeRelations', sedpar%flnrd(0))
       if (sedpar%flnrd(0) .ne. ' ') then
          call combinepaths(filsed, sedpar%flnrd(0))
       endif
       !
       !
       ! Intel 7.0 crashes on an inquire statement when file = ' '
       !
       if (flsmdc == ' ') then
          ex = .false.
       else
          call combinepaths(filsed, flsmdc)
          inquire (file = flsmdc, exist = ex)
       endif
       if (ex) then
          !
          ! Space varying data has been specified
          ! Use routine that also read the depth file to read the data
          !
          call depfil_stm(lundia    ,error     ,flsmdc    ,fmttmp    , &
                        & mudcnt    ,1         ,1         ,griddim   , errmsg)
          if (error) then
              call write_error(errmsg, unit=lundia)
              return
          endif
          do nm = 1, griddim%nmmax
             mudcnt(nm) = max(0.0_fp, min(mudcnt(nm), 1.0_fp))
          enddo
       else
          flsmdc = ' '
          mdcuni = 0.0_fp
          call prop_get(sed_ptr, 'SedimentOverall', 'MudCnt', mdcuni)
          !
          ! Uniform data has been specified
          !
          mudcnt = max(0.0_fp,min(mdcuni,1.0_fp))
       endif
       !
       if ( .not. associated(sed_ptr%child_nodes) ) then
          errmsg = 'Unable to read sediment information'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       !
       ! Sand-mud interaction parameters
       !
       call prop_get(sed_ptr, 'SedimentOverall', 'PmCrit', flspmc)
       !
       ! Intel 7.0 crashes on an inquire statement when file = ' '
       !
       if (flspmc == ' ') then
          ex = .false.
       else
          call combinepaths(filsed, flspmc)
          inquire (file = flspmc, exist = ex)
       endif
       if (ex) then
          !
          ! Space varying data has been specified
          ! Use routine that also read the depth file to read the data
          !
          call depfil_stm(lundia    ,error     ,flspmc    ,fmttmp    , &
                        & pmcrit    ,1         ,1         ,griddim   )
          if (error) return
          do nm = 1, griddim%nmmax
             pmcrit(nm) = min(pmcrit(nm), 1.0_fp)
          enddo
       else
          flspmc = ' '
          call prop_get(sed_ptr, 'SedimentOverall', 'PmCrit', pmcrit(1))
          !
          ! Explicit loop because of stack overflow
          !
          do nm = 1, griddim%nmmax
             pmcrit(nm) = min(pmcrit(1), 1.0_fp)
          enddo
       endif
       !
       ! Get bed shear skin stress parameters
       !
       bsskin = .false.
       call prop_get(sed_ptr, 'SedimentOverall', 'BsSkin', bsskin)
       if (bsskin) then
          call prop_get(sed_ptr, 'SedimentOverall', 'KsSilt', kssilt)
          call prop_get(sed_ptr, 'SedimentOverall', 'KsSand', kssand)
          !
          sc_type = 'thickness'
          call prop_get(sed_ptr, 'SedimentOverall', 'SC_mudfactor', sc_type)
          call str_lower(sc_type)
          select case (sc_type)
          case ('fraction')
             sc_mudfac = SC_MUDFRAC
          case ('thickness')
             sc_mudfac = SC_MUDTHC
          case default
             errmsg = 'Unknown option for sc_mudfactor. Expecting ''fraction'' or ''thickness''.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          end select
          !
          call prop_get(sed_ptr, 'SedimentOverall', 'SC_cmf1', sc_cmf1)
          call prop_get(sed_ptr, 'SedimentOverall', 'SC_cmf2', sc_cmf2)
          if (sc_mudfac == SC_MUDFRAC) then
             sc_cmf1 = max(0.0_fp , min(sc_cmf1, 1.0_fp))
             sc_cmf2 = max(sc_cmf1, min(sc_cmf2, 1.0_fp))
          else
             sc_cmf1 = max(0.0_fp , sc_cmf1)
             sc_cmf2 = max(sc_cmf1, sc_cmf2)
          endif
          !
          call prop_get(sed_ptr, 'SedimentOverall', 'CritFluffFactor', sc_flcf)
       endif
       !
       do l = 1, lsedtot
          !
          ! namsed pre-filled in count-sed routine
          !
          sedname = namsed(l)
          write(lundia,'(a,i2,2a)') 'Sediment fraction ', l, ': ', sedname
          !
          ! find associated block (note that name order and block order may not correspond since
          ! suspended fractions need to be associated with the lower indices and e.g. Delft3D-FLOW
          ! requires suspended fraction order to match order of constituents).
          !
          do i = 1, size(sed_ptr%child_nodes)
             !
             ! Does sed_ptr contain a child with name 'Sediment' (converted to lower case)?
             !
             sedblock_ptr => sed_ptr%child_nodes(i)%node_ptr
             parname = tree_get_name( sedblock_ptr )
             call str_lower(parname)
             if ( trim(parname) /= 'sediment') cycle
             !
             parname = ' '
             call prop_get(sedblock_ptr, '*', 'Name', parname)
             if (.not. strcmpi(parname, sedname)) cycle
             !
             ! sediment fraction found
             !
             sedpar%flnrd(l) = ' '
             call prop_get(sedblock_ptr, '*', 'NodeRelations', sedpar%flnrd(l))
             if (sedpar%flnrd(l) .ne. ' ') then
                call combinepaths(filsed, sedpar%flnrd(l))
             endif
             !
             exit
          enddo
          sedpar%sedblock(l) = sedblock_ptr
          !
          if (flocmod /= FLOC_NONE .and. sedtyp(l) == SEDTYP_CLAY) then
             if (nflocsizes == 1) then ! in case of a single floc size per clay population, set sensible defaults
                 namclay(l) = namsed(l)
                 flocsize(l) = 1
             endif
             call prop_get(sedblock_ptr, '*', 'ClayLabel', namclay(l))
             call prop_get(sedblock_ptr, '*', 'FlocSize' , flocsize(l))
             if (namclay(l) == ' ') then
                errmsg = 'The ClayLabel string should not be empty for '//trim(namsed(l))
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
          endif
          !
          rhosol(l) = rmissval
          call prop_get(sedblock_ptr, '*', 'RhoSol', rhosol(l))
          !
          ! tpsnumber is pre-filled with values based on sediment type (0.7 for mud and 1.0 for sand)
          call prop_get(sedblock_ptr, '*', 'SchmidtNumber', tpsnumber(l))
          !
          ! Check if bed composition for this fraction needs to be updated (only if the master CmpUpd flag in the mor-file is true)
          !
          call prop_get(sedblock_ptr, '*', 'CmpUpd', cmpupdfrac(l))
          !
          ! Get the geometric standard deviation of the sediment fraction
          !
          sedsg = rmissval
          call prop_get(sedblock_ptr, '*', 'SedSg', sedsg)
          if (comparereal(sedsg,rmissval) /= 0) then
             logsedsig(l) = log(sedsg)
          endif
          !
          ! In case of one sediment fraction, it is possible to use a spatially
          ! varying grain size.
          !
          if (lsedtot == 1) then
             call prop_get(sedblock_ptr, '*', 'SedD50', flsdia)
             if (flsdia == ' ') then
                !
                ! Alternative for SedD50 is SedDia (backward compatibility)
                !
                call prop_get(sedblock_ptr, '*', 'SedDia', flsdia)
             endif
             !
             ! Intel 7.0 crashes on an inquire statement when file = ' '
             !
             if (flsdia == ' ') then
                ex = .false.
             else
                call combinepaths(filsed, flsdia)
                inquire (file = flsdia, exist = ex)
             endif
             if (ex) then
                !
                !  File with space varying data has been specified, read it now.
                !
                call depfil_stm(lundia    ,error     ,flsdia    ,fmttmp    , &
                              & sedd50fld ,1         ,1         ,griddim   , errmsg)
                if (error) then 
                    call write_error(errmsg, unit=lundia)
                    return
                endif      
             else
                flsdia = ' '
             endif
          else
             ex = .false.
          endif
          !
          ! If there are multiple sediment fractions, or if no spatially
          ! varying grain size file was specified, read the sediment size
          ! properties.
          !
          if (.not. ex) then
             do j = 0, 100
                seddxx = rmissval
                if (j == 0) then
                   call prop_get(sedblock_ptr, '*', 'SedMinDia', seddxx)
                elseif (j == 100) then
                   call prop_get(sedblock_ptr, '*', 'SedMaxDia', seddxx)
                else
                   write(seddxxstring(5:6),'(i2.2)') j
                   call prop_get(sedblock_ptr, '*', seddxxstring, seddxx)
                   if (j == 50 .and. comparereal(seddxx,rmissval) == 0) then
                      !
                      ! Alternative for SedD50 is SedDia (backward compatibility)
                      !
                      call prop_get(sedblock_ptr, '*', 'SedDia', seddxx)
                   endif
                endif
                if (comparereal(seddxx,rmissval) /= 0) then
                   if (seddxx <= 0.0_fp) then
                      !
                      ! error: sediment diameter less than or equal to zero!!
                      !
                      errmsg = 'Sediment diameters must be positive!'
                      call write_error(errmsg, unit=lundia)
                      error = .true.
                      return
                   endif
                   !
                   nseddia(l)       = nseddia(l) + 1
                   n                = nseddia(l)
                   logseddia(1,n,l) = real(j,fp)
                   logseddia(2,n,l) = log(seddxx)
                   if (n > 1) then
                      if (logseddia(2,n,l) <= logseddia(2,n-1,l)) then
                         !
                         ! error: sediment diameters not increasing!!
                         !
                         errmsg = 'Sediment diameters must be increasing!'
                         call write_error(errmsg, unit=lundia)
                         error = .true.
                         return
                      endif
                   endif
                endif
             enddo
          endif
          !
          if (l <= lsed) then
             !
             ! set default settling formula
             !
             if (flocmod /= FLOC_NONE .and. sedtyp(l) == SEDTYP_CLAY) then
                 select case (flocmod)
                 case (FLOC_MANNING_DYER)
                     if (nflocsizes == 1) then
                        iform_settle(l) = WS_FORM_MANNING_DYER
                     elseif (flocsize(l) == 1) then
                        iform_settle(l) = WS_FORM_MANNING_DYER_MICRO
                     else
                        iform_settle(l) = WS_FORM_MANNING_DYER_MACRO
                     endif
                 case (FLOC_CHASSAGNE_SAFAR)
                     if (nflocsizes == 1) then
                        iform_settle(l) = WS_FORM_CHASSAGNE_SAFAR
                     elseif (flocsize(l) == 1) then
                        iform_settle(l) = WS_FORM_CHASSAGNE_SAFAR_MICRO
                     else
                        iform_settle(l) = WS_FORM_CHASSAGNE_SAFAR_MACRO
                     endif
                 case (FLOC_VERNEY_ETAL) ! TODO: check what is appropriate ...
                     iform_settle(l) = WS_FORM_FUNCTION_SALTEMCON
                 end select
             elseif (sedtyp(l) <= sedpar%max_mud_sedtyp) then
                iform_settle(l) = WS_FORM_FUNCTION_SALTEMCON
             else
                iform_settle(l) = WS_FORM_FUNCTION_DSS
             endif
             !
             rec = ' '
             call prop_get(sedblock_ptr, '*', 'SettleLib', rec)
             dll_name_settle(l) = rec
             if (rec /= ' ') then
                write(rec,'(3a)') SHARED_LIB_PREFIX, trim(dll_name_settle(l)), SHARED_LIB_EXTENSION
                dll_name_settle(l) = rec
                istat_ptr = 0
                istat_ptr = open_shared_library(dll_handle_settle(l), dll_name_settle(l))
                if (istat_ptr /= 0) then
                   errmsg = FILE_NOT_FOUND //  'Cannot open shared library ' // trim(dll_name_settle(l))
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                endif
                !
                call prop_get(sedblock_ptr, '*', 'SettleFunction', dll_function_settle(l))
                call prop_get(sedblock_ptr, '*', 'SettleInput'   , dll_usrfil_settle(l))
                iform_settle(l) = WS_FORM_USER_ROUTINE
             else
                settle_str = ' '
                call prop_get(sedblock_ptr, '*', 'SettleFrm', settle_str)
                call str_lower(settle_str)
                select case (settle_str)
                case ('1','function_of_conditions')
                   iform_settle(l) = WS_FORM_FUNCTION_SALTEMCON
                case ('2','function_of_diameter')
                   iform_settle(l) = WS_FORM_FUNCTION_DSS
                case ('3','manning_dyer')
                   iform_settle(l) = WS_FORM_MANNING_DYER
                case ('4','chassagne_safar')
                   iform_settle(l) = WS_FORM_CHASSAGNE_SAFAR
                case (' ')
                   ! not explicitly specified, so use the default set above
                case default
                   errmsg = 'Invalid SettleFrm = '//trim(settle_str)//' specified for '//trim(sedname)//'.'
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                end select
             endif
             !
             par_settle(:,l) = rmissval
             select case (iform_settle(l))
             case (WS_FORM_FUNCTION_SALTEMCON)
                call prop_get(sedblock_ptr, '*', 'SalMax' , par_settle(1,l)) ! ppt
                call prop_get(sedblock_ptr, '*', 'WS0'    , par_settle(2,l)) ! m/s
                call prop_get(sedblock_ptr, '*', 'WSM'    , par_settle(3,l)) ! m/s
                par_settle(4,l) = 1.0_fp 
                call prop_get(sedblock_ptr, '*', 'TCoefWS', par_settle(4,l)) ! -
                par_settle(5,l) = 0.1_fp
                call prop_get(sedblock_ptr, '*', 'CFlocCr', par_settle(5,l)) ! kg/m3
                par_settle(6,l) = 0.0_fp
                call prop_get(sedblock_ptr, '*', 'CPowWS' , par_settle(6,l)) ! -
             case (WS_FORM_FUNCTION_DSS) 
                ! 
                ! These parameters will only be used for iform = -2, but unfortunately iform hasn't been determined yet.
                ! In the future we may have to read the parameters in a different order.
                !
                call prop_get(sedblock_ptr, '*', 'SalMax', par_settle(1,l))
                par_settle(2,l) = 1.0_fp
                call prop_get(sedblock_ptr, '*', 'GamFloc', par_settle(2,l))
             case (WS_FORM_CHASSAGNE_SAFAR, WS_FORM_CHASSAGNE_SAFAR_MACRO)
                par_settle(1,l) = d_micro
                par_settle(2,l) = ustar_macro
             end select
             !
             ! Tracer calibration factor
             !
             call prop_get(sedblock_ptr, '*', 'TracerCalibrationFactor', sedtrcfac(l))
          endif
          !
          cdryb(l) = rmissval
          call prop_get(sedblock_ptr, '*', 'CDryB', cdryb(l))
          !
          ! First assume that 'IniSedThick'/'SdBUni' contains a filename
          ! If the file does not exist, assume that 'SdBUni' contains a uniform value (real)
          !
          call prop_get(sedblock_ptr, '*', 'IniSedThick', flsdbd(l))
          if (flsdbd(l) /= ' ') then
             inisedunit(l) = 'm'
          else
             inisedunit(l) = 'kg/m2'
             call prop_get(sedblock_ptr, '*', 'SdBUni', flsdbd(l))
          endif
          !
          ! Intel 7.0 crashes on an inquire statement when file = ' '
          !
          if (flsdbd(l) == ' ') then
             ex = .false.
          else
             call combinepaths(filsed, flsdbd(l))
             inquire (file = flsdbd(l), exist = ex)
          endif
          if (.not. ex) then
             sdbuni(l) = rmissval
             if (inisedunit(l) == 'm') then
                call prop_get(sedblock_ptr, '*', 'IniSedThick', sdbuni(l), success, valuesfirst=.true.)
             else
                call prop_get(sedblock_ptr, '*', 'SdBUni', sdbuni(l), success, valuesfirst=.true.)
             endif
             if (.not. success .or. comparereal(sdbuni(l),rmissval) == 0) then
                if (inisedunit(l) == 'm') then
                   write (errmsg,'(5a)') 'Invalid file or value "',trim(flsdbd(l)),'" assigned to IniSedThick for fraction ',trim(sedname),'.'
                else
                   write (errmsg,'(5a)') 'Invalid file or value "',trim(flsdbd(l)),'" assigned to SdBUni for fraction ',trim(sedname),'.'
                endif
                call mess(LEVEL_ERROR, errmsg)
                error = .true.
                return
             endif
             flsdbd(l) = ' '
          endif
          !
          if (l <= lsed) then
             call prop_get(sedblock_ptr, '*', 'FacDSS', facdss(l))
          endif
          !
          filtrn = ' '
          call prop_get(sedblock_ptr, '*', 'TraFrm', filtrn)
          call prop_get(sedblock_ptr, '*', 'TranspFrm', filtrn)
          !
          ! Intel 7.0 crashes on an inquire statement when file = ' '
          !
          if (filtrn /= ' ') then
             call combinepaths(filsed, filtrn)
             inquire (file = filtrn, exist = ex)
             if (ex) flstrn(l) = filtrn
             !
             call prop_get(sedblock_ptr, '*', 'TraFrm', iform(l))
             call prop_get(sedblock_ptr, '*', 'TranspFrm', iform(l))
          else
             call prop_get(sedblock_ptr, '*', 'DLL', filtrn)
             call prop_get(sedblock_ptr, '*', 'TranspLib', filtrn)
             if (filtrn /= ' ') then
                 if (sedtyp(l) <= sedpar%max_mud_sedtyp) then
                    iform(l) = 21 ! user defined version of EROSILT
                 else
                    iform(l) = 15 ! user defined version of EQTRAN
                 endif
             endif
          endif
       enddo
    else
       !
       ! sediment input version is 0 or 1:
       ! No keywords
       !
       call opensedfil(lundia    ,error     ,filsed    ,luninp    ,version  )
       if (error) return
       !write (versionstring, '(i4)') version
       !
       ! version 0 and 1 files don't contain sediment names.
       ! copy the names from the constituent array.
       !
       do l = 1, lsed
          namsed(l) = namcon(max(0, lsal, ltem) + l)
       enddo
       call rdsed01(lsed      ,luninp    ,lundia    ,csoil     ,iopsus    , &
                  & facdss    ,sedtyp    ,rhosol    ,sedd50    ,par_settle, &
                  & sdbuni    ,flsdbd    ,cdryb     ,sedpar%sedblock      , &
                  & version   ,error     )
       !
       ! nodal relations are not supported in older version sed files 
       !
       do l = 0, lsedtot
           sedpar%flnrd(l) = ' '
       enddo
       !
       close (luninp)
       if (error) return
       !
       do l = 1, lsed
          if (sedtyp(l) <= sedpar%max_mud_sedtyp) then ! TODO: can we set iform at the same time for consistency?
             iform_settle(l) = WS_FORM_FUNCTION_SALTEMCON
             ! set the parameters that are not available in the old file
             par_settle(4,l) = 1.0_fp
             par_settle(5,l) = 0.1_fp
             par_settle(6,l) = 0.0_fp
          else
             iform_settle(l) = WS_FORM_FUNCTION_DSS
             ! set the parameters that are not available in the old file
             par_settle(2,l) = 1.0_fp ! gamflc default
          endif
       enddo
       !
       ! Old format: SedDia specified, but stored in "wrong" array.
       !
       do l = 1, lsedtot
          if (sedd50(l) > 0.0_fp) then
             nseddia(l)       = 1
             logseddia(1,1,l) = 50.0_fp
             logseddia(2,1,l) = log(sedd50(l))
          endif
       enddo
    endif
    if (error) return
    if (flocmod /= FLOC_NONE) then
       !
       ! process flocculation
       !
       istat = 0
       if (.not. associated(sedpar%floclist)) then
          !
          if (istat==0) allocate (sedpar%floclist  (nflocpop, nflocsizes ), stat = istat)
          !
          if (istat/=0) then
             errmsg = 'RDSED: memory alloc error - floclist'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          !
          ! update local pointers
          !
          floclist      => sedpar%floclist
       endif
       !
       floclist = 0
       !
       do l = 1, lsed
          if (sedtyp(l) /= SEDTYP_CLAY) cycle
          !
          do i = 1, nflocpop
             ! check if population name has already been encountered before
             if (namclay(l) == namflocpop(i)) exit

             ! if not, then insert it in the first empty slot
             if (namflocpop(i) == ' ') then
                namflocpop(i) = namclay(l)
                exit
             endif
          enddo
          if (i > nflocpop) then
             errmsg = 'Too many different clay labels.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          !
          isize = flocsize(l)
          if (isize <= 0) then
             errmsg = 'The floc size of fraction '//trim(namsed(l))//' is invalid.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          elseif (isize > nflocsizes) then
             errmsg = 'The floc size of fraction '//trim(namsed(l))//' is too big.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          !
          if (floclist(i,isize) /= 0) then
             errmsg = 'Fraction '//trim(namsed(l))//' and fraction '//trim(namsed(floclist(i,isize)))//' map to the same size of the same clay population'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          floclist(i,isize) = l
       enddo
       !
    endif
end subroutine rdsed


subroutine rdsed01(lsed      ,luninp    ,lundia    ,csoil     ,iopsus    , &
                 & facdss    ,sedtyp    ,rhosol    ,seddia    ,par_settle, &
                 & sdbuni    ,flsdbd    ,cdryb     ,sedblock  ,version   , &
                 & error     )
!!--description-----------------------------------------------------------------
!
! Reads sediment input version 0 (or no version number found) or 1
!
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use sediment_basics_module
    use message_module, only: write_error
    !
    implicit none
!
! Arguments
!
    integer                        , intent(out):: iopsus
    integer                        , intent(in) :: lundia !<  Description and declaration in inout.igs
    integer                        , intent(in) :: luninp
    integer                        , intent(in) :: version
    integer                                     :: lsed
    logical                        , intent(out):: error
    real(fp)                       , intent(out):: csoil
    real(fp)       , dimension(:)  , intent(out):: cdryb
    real(fp)       , dimension(:)  , intent(out):: facdss
    real(fp)       , dimension(:)  , intent(out):: rhosol
    real(fp)       , dimension(:,:), intent(out):: par_settle
    real(fp)       , dimension(:)  , intent(out):: sdbuni
    real(fp)       , dimension(:)  , intent(out):: seddia
    integer        , dimension(:)  , intent(out):: sedtyp !<  sediment type: 0=total/1=noncoh/2=coh
    character(256) , dimension(:)  , intent(out):: flsdbd
    type(tree_data), dimension(:)  , intent(out):: sedblock
!
! Local variables
!
    integer                  :: iocond
    integer                  :: l
    integer                  :: lenc
    integer                  :: lsedlc
    character(4)             :: sedtype
    character(256)           :: errmsg
    character(256)           :: line
    real(fp)                 :: dummy          !< dummy real used to skip values while reading
    type(tree_data), pointer :: sedblock_ptr
    type(tree_data), pointer :: anode
!
!! executable statements -------------------------------------------------------
!
    error  = .false.
    !
    if (version==0) then
       read (luninp, *, iostat = iocond) lsedlc, csoil
       iopsus = 0
    else
       read (luninp, *, iostat = iocond) lsedlc, csoil, iopsus
    endif
    !
    if (lsedlc /= lsed) then
       errmsg = 'Inconsistent number of sediment fractions'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif
    !
    lenc = 4
    do l = 1, lsed
       call tree_create('Sediment',sedblock_ptr)
       !
       sedtype   = ' '
       read (luninp, '(a)', iostat = iocond) sedtype
       if (iocond == 0) then
          call small(sedtype, lenc)
          if (index(sedtype, 'sand') == 1) then
              sedtyp(l) = SEDTYP_SAND
          elseif (index(sedtype, 'mud') == 1) then
              sedtyp(l) = SEDTYP_CLAY
          else
             errmsg = 'Invalid suspended sediment type (must start with sand or mud)'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
       endif
       read (luninp, *, iostat = iocond) rhosol(l)
       if (iocond == 0) read (luninp, *, iostat = iocond) seddia(l)
       if (sedtyp(l) == SEDTYP_SAND) then
          if (iocond == 0) read (luninp, *, iostat = iocond) par_settle(1,l) ! salmax
          if (iocond == 0) read (luninp, *, iostat = iocond) dummy ! ws0
          if (iocond == 0) read (luninp, *, iostat = iocond) dummy ! wsm
       elseif (sedtyp(l) == SEDTYP_CLAY) then
          if (iocond == 0) read (luninp, *, iostat = iocond) par_settle(1,l) ! salmax
          if (iocond == 0) read (luninp, *, iostat = iocond) par_settle(2,l) ! ws0
          if (iocond == 0) read (luninp, *, iostat = iocond) par_settle(3,l) ! wsm
       endif
       if (iocond == 0) read (luninp, '(a)', iostat = iocond) line ! tcd
       call tree_create_node(sedblock_ptr,'TcrSed',anode)
       call tree_put_data(anode,transfer(trim(line),node_value),"STRING")
       if (iocond == 0) read (luninp, '(a)', iostat = iocond) line ! tce
       call tree_create_node(sedblock_ptr,'TcrEro',anode)
       call tree_put_data(anode,transfer(trim(line),node_value),"STRING")
       if (iocond == 0) read (luninp, '(a)', iostat = iocond) line ! ero
       call tree_create_node(sedblock_ptr,'EroPar',anode)
       call tree_put_data(anode,transfer(trim(line),node_value),"STRING")
       if (iocond == 0) read (luninp, *, iostat = iocond) cdryb(l)
       if (iocond == 0) then
           read (luninp, '(a)', iostat = iocond) flsdbd(l)
           if (iocond == 0) then
               read (flsdbd(l), *, iostat = iocond) sdbuni(l)
               if (iocond == 0) then
                   flsdbd(l) = ' '
               else
                   iocond = 0
               endif
           endif
       endif
       if (version==0) then
          facdss(l) = 1.0_fp
       else
          if (iocond == 0) read (luninp, *, iostat = iocond) facdss(l)
       endif
       !
       sedblock(l) = sedblock_ptr
    enddo
    !
    deallocate(sedblock_ptr)
end subroutine rdsed01


subroutine opensedfil(lundia    ,error     ,filsed    ,luninp    ,version  )
!!--description-----------------------------------------------------------------
!
! Read sediment parameters from an input file
! File type:
!    ASCII-file if file version below 02.00
!    INI  -file if file version is 02.00 or higher
!
!!--declarations----------------------------------------------------------------
    use message_module
    use string_module
    implicit none
!
! Arguments
!
    integer                                  , intent(in)  :: lundia  !< unit of diagnostic file
    integer                                  , intent(out) :: luninp  !< unit of opened .sed file
    integer                                  , intent(out) :: version !< version of .sed file
    logical                                  , intent(out) :: error   !< .true. if error occured
    character(*)                                           :: filsed  !< name of .sed file
!
! Local variables
!
    integer                                                :: i
    integer                                                :: iocond
    character(256)                                         :: string
    character(256)                                         :: errmsg
!
!! executable statements -------------------------------------------------------
!
    error   = .false.
    version = 0
    !
    call remove_leading_spaces(filsed)
    open (newunit = luninp, file = filsed, form = 'formatted', status = 'old',            &
        & iostat = iocond)
    if (iocond /= 0) then
       errmsg = ERROR_FILE_OPEN // trim(filsed)
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif
    !
    ! Free formatted file, skip lines starting with a '*'
    !
    read (luninp, '(a)') string
    do while (string(:1) == '*')
       call str_lower(string)
       i = index(string, 'version')
       if (i /= 0) then
          read (string(i + 8:), '(i2)') version
       else
          write (lundia, '(a)') 'No version number found'
       endif
       read (luninp, '(a)') string
    enddo
    !
    rewind (luninp)
    call skipstarlines(luninp)
end subroutine opensedfil


subroutine echosed(lundia    ,error     ,lsed      ,lsedtot   , &
                 & iopsus    ,sedpar    ,trapar    ,cmpupd    )
!!--description-----------------------------------------------------------------
!
! Report sediment parameter to diag file
!
!!--declarations----------------------------------------------------------------
    use precision
    use message_module
    use morphology_data_module
    use sediment_basics_module
    use flocculation, only: FLOC_NONE, FLOC_MANNING_DYER, FLOC_CHASSAGNE_SAFAR, FLOC_VERNEY_ETAL
    use m_rdtrafrm, only:echotrafrm
    !
    implicit none
!
! Arguments
!
    integer                                  , intent(in)  :: iopsus
    integer                                  , intent(in)  :: lsed    !< Number of suspended sediment fractions
    integer                                  , intent(in)  :: lsedtot !< Total number of sediment fractions
    integer                                                :: lundia  !< Unit number of diagnostic file
    logical                                  , intent(in)  :: cmpupd  !< Indicates whether main flag for bed composition updating is switched on
    logical                                  , intent(out) :: error   !< Flag=TRUE if an error is encountered
    type(sedpar_type)                        , pointer     :: sedpar
    type(trapar_type)                        , pointer     :: trapar
!
! Local variables
!
    real(fp)                          , pointer :: csoil
    real(fp)                          , pointer :: mdcuni
    real(fp)                          , pointer :: kssilt
    real(fp)                          , pointer :: kssand
    real(fp)                          , pointer :: sc_cmf1
    real(fp)                          , pointer :: sc_cmf2
    real(fp)                          , pointer :: sc_flcf
    integer                           , pointer :: flocmod
    integer                           , pointer :: sc_mudfac
    logical         , dimension(:)    , pointer :: cmpupdfrac
    real(fp)        , dimension(:)    , pointer :: tpsnumber
    real(fp)        , dimension(:)    , pointer :: rhosol
    real(fp)        , dimension(:,:,:), pointer :: logseddia
    real(fp)        , dimension(:)    , pointer :: logsedsig
    real(fp)        , dimension(:)    , pointer :: sedd10
    real(fp)        , dimension(:)    , pointer :: sedd50
    real(fp)        , dimension(:)    , pointer :: sedd50fld
    real(fp)        , dimension(:)    , pointer :: seddm
    real(fp)        , dimension(:)    , pointer :: sedd90
    real(fp)        , dimension(:)    , pointer :: cdryb
    real(fp)        , dimension(:,:)  , pointer :: dss
    real(fp)        , dimension(:)    , pointer :: facdss
    real(fp)        , dimension(:)    , pointer :: sdbuni
    real(fp)        , dimension(:)    , pointer :: sedtrcfac
    real(fp)        , dimension(:)    , pointer :: tcguni
    real(fp)        , dimension(:)    , pointer :: pmcrit
    integer         , dimension(:)    , pointer :: nseddia
    integer         , dimension(:)    , pointer :: sedtyp
    integer         , dimension(:)    , pointer :: tratyp
    character(10)   , dimension(:)    , pointer :: inisedunit
    character(20)   , dimension(:)    , pointer :: namsed
    character(256)  , dimension(:)    , pointer :: flsdbd
    character(256)  , dimension(:)    , pointer :: flstcg
    logical                           , pointer :: anymud
    logical                           , pointer :: bsskin
    character(256)                    , pointer :: flsdia
    character(256)                    , pointer :: flsmdc
    character(256)                    , pointer :: flspmc
    character(256)  , dimension(:)    , pointer :: dll_function_settle
    character(256)  , dimension(:)    , pointer :: dll_name_settle
    character(256)  , dimension(:)    , pointer :: dll_usrfil_settle
    integer         , dimension(:)    , pointer :: iform_settle
    real(fp)        , dimension(:,:)  , pointer :: par_settle
    integer         , dimension(:)    , pointer :: iform
    !
    integer                   :: i
    integer                   :: l
    integer                   :: n                   !< Temporary storage for nseddia(l)
    integer                   :: nm
    real(fp)                  :: logsedd50
    real(fp)                  :: rmissval
    real(fp)                  :: xxinv               !< Help var. [1/xx or 1/(1-xx) in log unif distrib.]
    real(fp)                  :: xm
    logical                   :: cmpupdall           !< flag indicating whether bed composition is updated for all fractions
    character(45)             :: txtput1
    character(12)             :: txtput2
    character(100)            :: txtput3
    character(256)            :: errmsg
!
!! executable statements -------------------------------------------------------
!
    csoil                => sedpar%csoil
    mdcuni               => sedpar%mdcuni
    kssilt               => sedpar%kssilt
    kssand               => sedpar%kssand
    sc_cmf1              => sedpar%sc_cmf1
    sc_cmf2              => sedpar%sc_cmf2
    sc_flcf              => sedpar%sc_flcf
    flocmod              => sedpar%flocmod
    sc_mudfac            => sedpar%sc_mudfac
    cmpupdfrac           => sedpar%cmpupdfrac
    tpsnumber            => sedpar%tpsnumber
    rhosol               => sedpar%rhosol
    logseddia            => sedpar%logseddia
    logsedsig            => sedpar%logsedsig
    sedd10               => sedpar%sedd10
    sedd50               => sedpar%sedd50
    sedd50fld            => sedpar%sedd50fld
    seddm                => sedpar%seddm
    sedd90               => sedpar%sedd90
    cdryb                => sedpar%cdryb
    dss                  => sedpar%dss
    facdss               => sedpar%facdss
    sdbuni               => sedpar%sdbuni
    sedtrcfac            => sedpar%sedtrcfac
    tcguni               => sedpar%tcguni
    pmcrit               => sedpar%pmcrit
    nseddia              => sedpar%nseddia
    sedtyp               => sedpar%sedtyp
    tratyp               => sedpar%tratyp
    inisedunit           => sedpar%inisedunit
    namsed               => sedpar%namsed
    flsdbd               => sedpar%flsdbd
    flstcg               => sedpar%flstcg
    anymud               => sedpar%anymud
    bsskin               => sedpar%bsskin
    flsdia               => sedpar%flsdia
    flsmdc               => sedpar%flsmdc
    flspmc               => sedpar%flspmc
    dll_function_settle  => trapar%dll_function_settle
    dll_name_settle      => trapar%dll_name_settle
    dll_usrfil_settle    => trapar%dll_usrfil_settle
    iform_settle         => trapar%iform_settle
    par_settle           => trapar%par_settle
    iform                => trapar%iform
    !
    rmissval = -999.0_fp
    !
    ! echo input in diagnose-file
    !
    write (lundia, '(a)')   '*** Start  of sediment input'
    txtput1 = 'Ref concentration'
    write (lundia, '(2a,e12.4)') txtput1, ':', csoil
    if (csoil <= 0.0_fp) then
       errmsg = 'Reference concentration should be positive.'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif
    txtput1 = 'Option Dss'
    write (lundia, '(2a,i12)') txtput1, ':', iopsus
    if (anymud) then
       if (flsmdc /= ' ' .or. comparereal(mdcuni,0.0_fp) /= 0) then
          errmsg = 'User defined mud content ignored: mud fraction simulated.'
          call write_warning(errmsg, unit=lundia)
       endif
       flsmdc = ' '
       mdcuni = 0.0_fp
       !
       ! Sand-mud interaction
       !
       txtput1 = 'Crit mud frac for sand mud interaction'
       if (flspmc /= ' ') then
          write (lundia, '(3a)') txtput1, ':  ', trim(flspmc)
       else
          write (lundia, '(2a,e12.4)') txtput1, ':', pmcrit(1)
       endif
    else
       if (flsmdc /= ' ') then
          txtput1 = 'File mud content'
          write (lundia, '(3a)') txtput1, ':  ', trim(flsmdc)
       else
          txtput1 = 'Uniform mud content'
          write (lundia, '(2a,e12.4)') txtput1, ':', mdcuni
       endif
       !
       ! Sand-mud interaction
       !
       if (flspmc /= ' ' .or. comparereal(pmcrit(1),0.0_fp) > 0) then
          errmsg = 'Sand mud interaction ignored: no mud fraction simulated.'
          call write_warning(errmsg, unit=lundia)
       endif
       flspmc = ' '
       pmcrit = -1.0_fp
    endif
    if (flocmod /= FLOC_NONE) then
       txtput1 = 'Flocculation model'
       select case (flocmod)
       case (FLOC_MANNING_DYER)
           txtput3 = 'Manning & Dyer'
       case (FLOC_CHASSAGNE_SAFAR)
           txtput3 = 'Chassagne & Safar'
       case (FLOC_VERNEY_ETAL)
           txtput3 = 'Verney et al'
       end select
       write (lundia, '(3a)') txtput1, ':  ', trim(txtput3)
       if (flocmod == FLOC_VERNEY_ETAL) then
          errmsg = 'Verney flocculation model not yet implemented.'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       !
       txtput1 = 'Flocculation time scale'
       write (lundia, '(2a,e12.4)') txtput1, ':', sedpar%tfloc
       txtput1 = 'Floc break-up time scale'
       write (lundia, '(2a,e12.4)') txtput1, ':', sedpar%tbreakup
       txtput1 = 'Characteristic diameter of micro flocs'
       write (lundia, '(2a,e12.4)') txtput1, ':', sedpar%d_micro
       txtput1 = 'Characteristic shear velocity of macro flocs'
       write (lundia, '(2a,e12.4)') txtput1, ':', sedpar%ustar_macro
    endif
    if (bsskin) then
       txtput1 = 'Skin friction Soulsby 2004'
       write (lundia, '(a)') txtput1
       !
       select case (sc_mudfac)
       case (SC_MUDFRAC)
          txtput2 = '    fraction'
       case (SC_MUDTHC)
          txtput2 = '   thickness'
       end select
       txtput1 = 'Lower crit mud '//txtput2
       write (lundia, '(2a,f12.6)') txtput1,':', sc_cmf1
       txtput1 = 'Upper crit mud '//txtput2
       write (lundia, '(2a,f12.6)') txtput1,':', sc_cmf2
       !
       txtput1 = 'Kssilt '
       write (lundia, '(2a,f12.6)') txtput1,':', kssilt
       txtput1 = 'Kssand '
       write (lundia, '(2a,f12.6)') txtput1,':', kssand
       !
       txtput1 = 'Critical fluff layer coverage factor'
       write (lundia, '(2a,f12.6)') txtput1,':', sc_flcf
    endif
    if (sedpar%flnrd(0) /= ' ') then
       txtput1 = '1D nodal relations for bed/total load'
       write (lundia, '(3a)') txtput1, ':  ', trim(sedpar%flnrd(0))
    endif
    !
    cmpupdall = all(cmpupdfrac)
    do l = 1, lsedtot
       txtput1 = 'Sediment number'
       write (lundia, '(2a,i12)') txtput1, ':', l
       txtput1 = '  Name'
       write (lundia, '(3a)') txtput1, ': ', trim(namsed(l))
       txtput1 = '  Sediment Type'
       select case (sedtyp(l))
          case (SEDTYP_CLAY)
             txtput2 = 'clay'
          case (SEDTYP_SILT)
             txtput2 = 'silt'
          case (SEDTYP_SAND)
             txtput2 = 'sand'
          case (SEDTYP_GRAVEL)
             txtput2 = 'gravel'
       end select
       write (lundia, '(2a,a12)') txtput1, ':', trim(txtput2)
       txtput1 = '  Transport Type'
       select case (tratyp(l))
          case (TRA_COMBINE)
             txtput3 = 'algebraic formulae + advection-diffusion equation'
          case (TRA_ADVDIFF)
             txtput3 = 'advection-diffusion equation only'
          case (TRA_BEDLOAD)
             txtput3 = 'algebraic formulae only (total load)'
       end select
       write (lundia, '(3a)') txtput1, ':  ', trim(txtput3)
       if (sedtrcfac(l)>0.0_fp) then
           txtput1 = '  Tracer calibration factor '
           write (lundia, '(2a,e12.4)') txtput1, ':', sedtrcfac(l)
       endif
       !
       if (cmpupd .and. .not. cmpupdall) then
          txtput1 = '  Include fraction in bed composition update'
          if (cmpupdfrac(l)) then
             txtput2 = '         YES'
          else
             txtput2 = '          NO'
          endif
          write (lundia, '(3a)') txtput1, ':', txtput2
       endif
       !
       if (l <= lsed) then
          txtput1 = '  Turbulent Prandtl-Schmidt number'
          write (lundia, '(2a,e12.4)') txtput1, ':', tpsnumber(l)
       endif
       txtput1 = '  Solid density (RHOSOL)'
       write (lundia, '(2a,e12.4)') txtput1, ':', rhosol(l)
       if (flsdia /= ' ') then
          !
          ! One sediment diameter between 0 and 100%
          !
          write (lundia, '(3a)') txtput1, ':  ', 'lognormal'
          if (comparereal(logsedsig(l),rmissval) == 0) then
             !
             ! no standard deviation specified: use default geometric
             ! standard deviation of 1.34 which is in the middle of the
             ! range of sigma_g values representing "well sorted" mixtures
             ! indicated in Blott & Pye, 2001. Earth Surface Processes
             ! and Landforms 26, p. 1237-1248.
             !
             logsedsig(l) = log(1.34)
          endif
          !
          nseddia(l) = -999
          txtput1 = '  geom. st. dev.'
          write (lundia, '(2a,e12.4)') txtput1, ':', exp(logsedsig(l))
          txtput1 = '  SedD50'
          write (lundia, '(3a)') txtput1, ':  ', trim(flsdia)
       elseif (nseddia(l) > 0) then
          !
          ! Determine various sediment diameters in case of
          ! sand or bedload.
          !
          txtput1 = '  sed. distribution'
          if (nseddia(l) == 0) then
             !
             ! error: no sediment diameter specified!
             !
             errmsg = 'Missing sediment diameter data'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          elseif (nseddia(l) == 1) then
             !
             ! Just one sediment diameter
             !
             if (nint(logseddia(1,1,l)) == 0) then
                !
                ! error: only minimum sediment diameter is insufficient
                !
                errmsg = 'Missing maximum diameter data'
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             elseif (nint(logseddia(1,1,l)) == 100) then
                !
                ! error: only maximum sediment diameter is insufficient
                !
                errmsg = 'Missing minimum diameter data'
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             !
             ! One sediment diameter between 0 and 100%
             !
             if (comparereal(sedpar%version,3.0_fp) >= 0) then
                !
                ! New behaviour: lognormal distribution
                !
                write (lundia, '(3a)') txtput1, ':  ', 'lognormal'
                if (comparereal(logsedsig(l),rmissval) == 0) then
                   !
                   ! no standard deviation specified: use default geometric
                   ! standard deviation of 1.34 which is in the middle of the
                   ! range of sigma_g values representing "well sorted" mixtures
                   ! indicated in Blott & Pye, 2001. Earth Surface Processes
                   ! and Landforms 26, p. 1237-1248.
                   !
                   logsedsig(l) = log(1.34)
                endif
                txtput1 = '  geom. st. dev.'
                write (lundia, '(2a,e12.4)') txtput1, ':', exp(logsedsig(l))
                !
                ! Approximate lognormal distribution using the following
                ! percentiles:
                ! 0.1 (set to 0) --- ilognormal --- 99.9 (set to 100)
                !
                logsedd50        = logseddia(2,1,l) - logsedsig(l)*lognormal(nint(logseddia(1,1,l)))
                logseddia(1,1,l) = 0.0_fp
                logseddia(2,1,l) = logsedd50 - 3.0_fp*logsedsig(l)
                n = 1
                do i = 1, size(ilognormal)
                   n = n + 1
                   logseddia(1,n,l) = real(ilognormal(i),fp)
                   logseddia(2,n,l) = logsedd50 + lognormal(ilognormal(i))*logsedsig(l)
                enddo
                n = n + 1
                logseddia(1,n,l) = 100.0_fp
                logseddia(2,n,l) = logsedd50 + 3.0_fp*logsedsig(l)
                nseddia(l)       = n
                !
                ! Compute characteristic sediment diameters
                !
                sedd50(l) = exp(logsedd50)
                sedd10(l) = exp(logsedd50 + lognormal(10)*logsedsig(l))
                sedd90(l) = exp(logsedd50 + lognormal(90)*logsedsig(l))
                seddm(l)  = exp(logsedd50 + 0.5_fp*logsedsig(l)*logsedsig(l))
             else
                !
                ! Old behaviour: D10 = 0.75 * D50, D90 = 1.5 * D50
                ! Piecewise loguniform approach: requires D50
                !
                if (nint(logseddia(1,1,l)) /= 50) then
                   !
                   ! error: old approach requires D50
                   !
                   errmsg = 'Missing median diameter data'
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                endif
                !
                write (lundia, '(3a)') txtput1, ':  ', 'piecewise loguniform'
                !
                nseddia(l)       = 3
                logseddia(1,2,l) = 50.0_fp
                logseddia(2,2,l) = logseddia(2,1,l)
                !
                logseddia(1,1,l) = 0.0_fp
                logseddia(2,1,l) = logseddia(2,2,l) + 5.0_fp*log(0.75_fp)/4.0_fp
                !
                logseddia(1,3,l) = 100.0_fp
                logseddia(2,3,l) = logseddia(2,2,l) + 5.0_fp*log(1.5_fp)/4.0_fp
                !
                ! Compute characteristic sediment diameters
                !
                sedd50(l) = exp(logseddia(2,2,l))
                sedd10(l) = 0.75_fp * sedd50(l)
                sedd90(l) = 1.50_fp * sedd50(l)
                !
                seddm(l)  = 0.0_fp
                logsedsig(l) = 0.0_fp
                xm = 0.0_fp
                do n = 2, nseddia(l)
                   xxinv    = logseddia(1,n,l) - logseddia(1,n-1,l)
                   seddm(l) = seddm(l) &
                            & + xxinv * (exp(logseddia(2,n,l)) - exp(logseddia(2,n-1,l))) &
                            & / (logseddia(2,n,l) - logseddia(2,n-1,l))
                   logsedsig(l) = logsedsig(l) + xxinv * (&
                                & (logseddia(2,n,l)-logseddia(2,n-1,l))/sqrt(3.0_fp)/2.0_fp &
                                & + ((logseddia(2,n,l)+logseddia(2,n-1,l))/2.0_fp)**2 &
                                & )
                   xm = xm + xxinv * (logseddia(2,n,l)+logseddia(2,n-1,l))/2.0_fp
                enddo
                seddm(l) = seddm(l) / 100.0_fp
                xm = xm / 100.0_fp
                logsedsig(l) = logsedsig(l) / 100.0_fp - xm**2
             endif
          elseif (nseddia(l) == 2) then
             !
             ! Two sediment diameters specified
             !
             if (comparereal(logsedsig(l),rmissval) /= 0) then
                !
                ! standard deviation specified
                ! error: not allowed in combination with multiple
                ! sediment diameters
                !
                errmsg = 'Geom. st. dev. not allowed in combination with multiple sediment diameters'
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             if (nint(logseddia(1,1,l))==0 .or. nint(logseddia(1,2,l))==100) then
                !
                ! Minimum or maximum sediment diameter given:
                ! loguniform distribution
                !
                write (lundia, '(3a)') txtput1, ':  ', 'loguniform'
                !
                ! Compute characteristic sediment diameters
                !
                if (nint(logseddia(1,1,l)) > 0) then
                   !
                   ! Only maximum sediment diameter given and some
                   ! percentile: compute minimum sediment diameter.
                   !
                   xxinv            = 1.0_fp / (1.0_fp - (real(logseddia(1,1,l),fp)/100.0_fp))
                   logseddia(2,1,l) = logseddia(2,1,l)*xxinv + logseddia(2,2,l)*(1.0_fp-xxinv)
                   logseddia(1,1,l) = 0.0_fp
                elseif (nint(logseddia(1,2,l)) < 100) then
                   !
                   ! Only minimum sediment diameter given and some
                   ! percentile: compute maximum sediment diameter.
                   !
                   xxinv            = 100.0_fp / real(logseddia(1,2,l),fp)
                   logseddia(2,2,l) = logseddia(2,2,l)*xxinv + logseddia(2,1,l)*(1.0_fp-xxinv)
                   logseddia(1,2,l) = 100.0_fp
                endif
                !
                ! Both minimum and maximum sediment diameters given
                !
                sedd50(l) = exp(0.5_fp*logseddia(2,1,l) + 0.5_fp*logseddia(2,2,l))
                sedd10(l) = exp(0.9_fp*logseddia(2,1,l) + 0.1_fp*logseddia(2,2,l))
                sedd90(l) = exp(0.1_fp*logseddia(2,1,l) + 0.9_fp*logseddia(2,2,l))
                seddm(l)  = (exp(logseddia(2,2,l)) - exp(logseddia(2,1,l))) / &
                          & (logseddia(2,2,l) - logseddia(2,1,l))
                logsedsig(l) = (logseddia(2,2,l) - logseddia(2,1,l))/sqrt(3.0_fp)/2.0_fp
             else
                !
                ! Neither minimum nor maximum sediment diameter given:
                ! lognormal distribution
                !
                write (lundia, '(3a)') txtput1, ':  ', 'lognormal'
                !
                ! Compute geometric standard deviation
                !
                logsedsig(l) = (logseddia(2,2,l) - logseddia(2,1,l))/ &
                             & (lognormal(nint(logseddia(1,2,l))) -   &
                             &  lognormal(nint(logseddia(1,1,l))))
                txtput1 = ' geom. stand. dev.'
                write (lundia, '(2a,e12.4)') txtput1, ':', exp(logsedsig(l))
                !
                ! Approximate lognormal distribution using the following
                ! percentiles:
                ! 0.1 (set to 0) --- ilognormal --- 99.9 (set to 100)
                !
                logsedd50        = logseddia(2,1,l) - logsedsig(l)*lognormal(nint(logseddia(1,1,l)))
                logseddia(1,1,l) = 0.0_fp
                logseddia(2,1,l) = logsedd50 - 3.0_fp*logsedsig(l)
                n = 1
                do i = 1, size(ilognormal)
                   n = n + 1
                   logseddia(1,n,l) = real(ilognormal(i),fp)
                   logseddia(2,n,l) = logsedd50 + lognormal(ilognormal(i))*logsedsig(l)
                enddo
                n = n + 1
                logseddia(1,n,l) = 100.0_fp
                logseddia(2,n,l) = logsedd50 + 3.0_fp*logsedsig(l)
                nseddia(l)       = n
                !
                ! Compute characteristic sediment diameters
                !
                sedd50(l) = exp(logsedd50)
                sedd10(l) = exp(logsedd50 + lognormal(10)*logsedsig(l))
                sedd90(l) = exp(logsedd50 + lognormal(90)*logsedsig(l))
                seddm(l)  = exp(logsedd50 + 0.5_fp*logsedsig(l)*logsedsig(l))
             endif
          else
             !
             ! More than two sediment diameters specified
             !
             if (comparereal(logsedsig(l),rmissval) /= 0) then
                !
                ! standard deviation specified
                ! error: not allowed in combination with multiple
                ! sediment diameters
                !
                errmsg = 'Geom. std. dev. not allowed in combination with multiple sediment diameters'
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             !
             ! Always piecewise loguniform distribution
             !
             write (lundia, '(3a)') txtput1, ':  ', 'piecewise loguniform'
             !
             ! Compute characteristic sediment diameters
             !
             if (nint(logseddia(1,1,l)) > 0) then
                !
                ! sediment diameter table does not include sedmindia
                ! replace the first entry by extending the first loguniform range
                ! by keeping the density constant
                !
                xxinv            =  real(logseddia(1,2,l),fp) / &
                                 & (real(logseddia(1,2,l),fp) - real(logseddia(1,1,l),fp))
                logseddia(2,1,l) = logseddia(2,1,l)*xxinv + logseddia(2,2,l)*(1.0_fp-xxinv)
                logseddia(1,1,l) = 0.0_fp
             endif
             n = nseddia(l)
             if (nint(logseddia(1,n,l)) < 100) then
                !
                ! sediment diameter table does not include sedmaxdia
                ! replace the last entry by extending the last loguniform range
                ! by keeping the density constant
                !
                xxinv            = (100.0_fp - real(logseddia(1,n-1,l),fp)) / &
                                 & (real(logseddia(1,n,l),fp) - real(logseddia(1,n-1,l),fp))
                logseddia(2,n,l) = logseddia(2,n,l)*xxinv + logseddia(2,n-1,l)*(1.0_fp-xxinv)
                logseddia(1,n,l) = 100.0_fp
             endif
             !
             seddm(l)  = 0.0_fp
             do n = 2, nseddia(l)
                xxinv    = logseddia(1,n,l) - logseddia(1,n-1,l)
                seddm(l) = seddm(l) &
                         & + xxinv * (exp(logseddia(2,n,l)) - exp(logseddia(2,n-1,l))) &
                         & / (logseddia(2,n,l) - logseddia(2,n-1,l))
                xxinv    = 1.0_fp / xxinv
                if (logseddia(1,n-1,l) < 10.0_fp .and. logseddia(1,n,l) >= 10.0_fp) then
                   sedd10(l) = exp(logseddia(2,n-1,l)*((logseddia(1,n,l)-10.0_fp)*xxinv) + &
                                 & logseddia(2,n,l)*((10.0_fp-logseddia(1,n-1,l))*xxinv))
                endif
                if (logseddia(1,n-1,l) < 50.0_fp .and. logseddia(1,n,l) >= 50.0_fp) then
                   sedd50(l) = exp(logseddia(2,n-1,l)*((logseddia(1,n,l)-50.0_fp)*xxinv) + &
                                 & logseddia(2,n,l)*((50.0_fp-logseddia(1,n-1,l))*xxinv))
                endif
                if (logseddia(1,n-1,l) < 90.0_fp .and. logseddia(1,n,l) >= 90.0_fp) then
                   sedd90(l) = exp(logseddia(2,n-1,l)*((logseddia(1,n,l)-90.0_fp)*xxinv) + &
                                 & logseddia(2,n,l)*((90.0_fp-logseddia(1,n-1,l))*xxinv))
                endif
             enddo
             seddm(l) = seddm(l) / 100.0_fp
          endif
          !
          ! convert percentages to fractions
          !
          do n = 1, nseddia(l)
             logseddia(1,n,l) = logseddia(1,n,l) / 100.0_fp
          enddo
          !
          txtput1 = '  SedD10'
          write (lundia, '(2a,e12.4)') txtput1, ':', sedd10(l)
          txtput1 = '  SedD50'
          write (lundia, '(2a,e12.4)') txtput1, ':', sedd50(l)
          txtput1 = '  SedDM'
          write (lundia, '(2a,e12.4)') txtput1, ':', seddm(l)
          txtput1 = '  SedD90'
          write (lundia, '(2a,e12.4)') txtput1, ':', sedd90(l)
       endif
       txtput1 = '  Dry bed (bulk) density (CDRYB)'
       write (lundia, '(2a,e12.4)') txtput1, ':', cdryb(l)
       if (flsdbd(l) /= ' ') then
          if (inisedunit(l) == 'kg/m2') then
             txtput1 = '  File IniCon'
          else
             txtput1 = '  File IniThick'
          endif
          write (lundia, '(3a)') txtput1, ':  ', trim(flsdbd(l))
       else
          if (inisedunit(l) == 'kg/m2') then
             txtput1 = '  Uniform IniCon'
             txtput2 = ' [kg/m2]'
          else
             txtput1 = '  Uniform IniThick'
             txtput2 = ' [m]'
          endif
          write (lundia, '(2a,e12.4,a)') txtput1, ':', sdbuni(l), trim(txtput2)
       endif
       if (l <= lsed) then
          txtput1 = '  FACDSS'
          write (lundia, '(2a,e12.4)') txtput1, ':', facdss(l)
          if (facdss(l) <= 0.0_fp) then
             errmsg = 'FACDSS <= 0.0'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          !
          if (lsedtot==1 .and. flsdia/=' ') then ! l=1
             !
             ! Wrong: do nm=1,size(sedd50fld)
             ! nm goes from nmlb (negative!) to nmub
             !
             do nm = lbound(sedd50fld,1), ubound(sedd50fld,1)
                dss(nm, 1) = sedd50fld(nm)*facdss(1)
             enddo
          else
             !
             ! Explicit loop because of stack overflow
             !
             do nm = lbound(sedd50fld,1), ubound(sedd50fld,1)
                dss(nm, l) = sedd50(l)*facdss(l)
             enddo
          endif
       endif
       !
       call echotrafrm(lundia      ,trapar     ,l         )
       !
       if (l <= lsed) then
          txtput1 = '  Settling velocity formula'
          select case (iform_settle(l))
          case (WS_FORM_FUNCTION_SALTEMCON)
             write (lundia, '(2a)') txtput1, ':  function of salinity, temperature and concentration'
             txtput1 = '  SALMAX'
             write (lundia, '(2a,e12.4)') txtput1, ':', par_settle(1,l)
             txtput1 = '  WS0'
             write (lundia, '(2a,e12.4)') txtput1, ':', par_settle(2,l)
             txtput1 = '  WSM'
             write (lundia, '(2a,e12.4)') txtput1, ':', par_settle(3,l)
             txtput1 = '  TCoefWS'
             write (lundia, '(2a,e12.4)') txtput1, ':', par_settle(4,l)
             txtput1 = '  CFlocCr'
             write (lundia, '(2a,e12.4)') txtput1, ':', par_settle(5,l)
             txtput1 = '  CPowWS'
             write (lundia, '(2a,e12.4)') txtput1, ':', par_settle(6,l)
          
          case (WS_FORM_FUNCTION_DSS)
             write (lundia, '(2a)') txtput1, ':  computed from grain size'
             if (iform(l) == -2 .or. iform(l) == -4) then
                iform_settle(l) = WS_FORM_FUNCTION_DSS_2004
                txtput1 = '  SALMAX'
                write (lundia, '(2a,e12.4)') txtput1, ':', par_settle(1,l)
                txtput1 = '  Flocculation factor GamFloc'
                write (lundia, '(2a,e12.4)') txtput1, ':', par_settle(2,l)
             endif
          
          case (WS_FORM_MANNING_DYER_MACRO)
             write (lundia, '(2a)') txtput1, ':  Manning & Dyer (macro flocs)'
          
          case (WS_FORM_MANNING_DYER_MICRO)
             write (lundia, '(2a)') txtput1, ':  Manning & Dyer (micro flocs)'
          
          case (WS_FORM_MANNING_DYER)
             write (lundia, '(2a)') txtput1, ':  Manning & Dyer (macro/micro floc equilibrium)'
          
          case (WS_FORM_CHASSAGNE_SAFAR_MACRO)
             write (lundia, '(2a)') txtput1, ':  Chassagne & Safar (macro flocs)'
          
          case (WS_FORM_CHASSAGNE_SAFAR_MICRO)
             write (lundia, '(2a)') txtput1, ':  Chassagne & Safar (micro flocs)'
          
          case (WS_FORM_CHASSAGNE_SAFAR)
             write (lundia, '(2a)') txtput1, ':  Chassagne & Safar (macro/micro floc equilibrium)'
          
          case (WS_FORM_USER_ROUTINE)
             write (lundia, '(2a)') txtput1, ':  user specified library'
             !
             ! User defined settling velocity function
             !
             txtput1 = '  Settle library'
             write (lundia, '(3a)') txtput1, ': ', trim(dll_name_settle(l))
             txtput1 = '  Function in Settle lib'
             write (lundia, '(3a)') txtput1, ': ', trim(dll_function_settle(l))
             if (dll_usrfil_settle(l) /= ' ') then
                txtput1 = '  Input for Settle function'
                write (lundia, '(3a)') txtput1, ': ', trim(dll_usrfil_settle(l))
             endif
          
          case default
             write (lundia, '(2a,i0,a)') txtput1, ':  UNKNOWN (',iform_settle(l),')'
          end select
       endif

       if (sedpar%flnrd(l) /= ' ') then
          txtput1 = '  1D nodal relations for bed/total load'
          write (lundia, '(3a)') txtput1, ':  ', trim(sedpar%flnrd(l))
       endif
    enddo
    !
    write (lundia, '(a)') '*** End    of sediment input'
    write (lundia, *)
end subroutine echosed


subroutine count_sed(lundia    ,error     ,lsed      ,lsedtot   , &
                   & filsed    ,sedpar    ,sed_ptr   )
!!--description-----------------------------------------------------------------
!
! - Determines number of sediment fractions from sediment input file
!
!!--declarations----------------------------------------------------------------
    use properties
    use string_module
    use message_module
    use sediment_basics_module
    use morphology_data_module
    !
    implicit none
!
! Arguments
!
    integer                         , intent(out) :: lsed    !< Number of suspended sediment fractions
    integer                         , intent(out) :: lsedtot !< Total number of sediment fractions
    integer                         , intent(in)  :: lundia  !< Unit of diagnostic file
    logical                         , intent(out) :: error
    character(len=*)                , intent(in)  :: filsed
    type(sedpar_type)               , pointer     :: sedpar
    type(tree_data)                 , pointer     :: sed_ptr
!
! Local variables
!
    integer                                                :: i
    integer                                                :: ibl
    integer                                                :: istat
    integer                                                :: j
    integer                                                :: lsedbl        !< Number of bedload fractions
    integer                                                :: sedtypnr      !< Local sediment type number
    integer                                                :: tratypnr      !< Local transport type number
    integer         , dimension(:) , allocatable           :: typsedim      !< Type of the sediments - sorted by order of sediment blocks
    integer         , dimension(:) , allocatable           :: typtrans      !< (Initial) transport type - sorted by order of sediment blocks (TRA_BEDLOAD or TRA_COMBINE)
    logical                                                :: found
    logical                                                :: totalload
    character(20)                                          :: versionstring
    character(20)   , dimension(:) , allocatable           :: namsedim      !< Names of the sediments as read from sed-file
    character(20)                                          :: sedtyptmp     !< Sediment type in sed-file
    character(80)                                          :: parname
    character(300)                                         :: message
    type(tree_data)                            , pointer   :: asedblock_ptr
!
!! executable statements -------------------------------------------------------
!
    lsedbl        = 0
    lsed          = 0
    lsedtot       = 0
    istat         = 0
    error         = .false.
    !
    ! Check 'Filsed' record for attribute file containing parameters
    ! for sediment transport computation
    !
    if (filsed == ' ') then
       !
       ! file does not exist
       !
       message = 'No sediment file name specified.'
       call write_error(message,unit=lundia)
       error = .true.
       return
    endif 
    !
    ! Read sed-file into tree data structure
    !
    call prop_file('ini', trim(filsed), sed_ptr, istat)
    if (istat /= 0) then
       select case (istat)
          case(1)
             call write_error(FILE_NOT_FOUND//trim(filsed), unit=lundia)
          case(3)
             call write_error(PREMATURE_EOF//trim(filsed), unit=lundia)
          case default
             call write_error(FILE_READ_ERROR//trim(filsed), unit=lundia)
       endselect
       error = .true.
       return
    endif
    !
    if ( .not.associated(sed_ptr%child_nodes) ) then
       call write_error(FILE_READ_ERROR//trim(filsed), unit=lundia)
       error = .true.
       return
    endif
    !
    ! Check version number of sed input file
    !
    versionstring = ' '
    call prop_get(sed_ptr, 'SedimentFileInformation', 'FileVersion', versionstring)
    if (trim(versionstring) == '02.00') then
       !
       ! allocate temporary arrays with length equal to the number of data blocks in the file
       ! the sediment blocks will be a subset of that
       !
       allocate(namsedim(size(sed_ptr%child_nodes)))
       allocate(typsedim(size(sed_ptr%child_nodes)))
       allocate(typtrans(size(sed_ptr%child_nodes)))
       namsedim = ' '
       typsedim = -999
       typtrans = TRA_NONE
       !
       do j = 1, size(sed_ptr%child_nodes)
          !
          ! Does sed_ptr contain a child with name 'Sediment' (converted to lower case)?
          !
          asedblock_ptr => sed_ptr%child_nodes(j)%node_ptr
          parname = tree_get_name( asedblock_ptr )
          if (parname == 'sediment') then
             parname = ' '
             call prop_get(asedblock_ptr, '*', 'Name', parname)
             !
             ! Check if the same sediment name was used before
             !
             found = .false.
             do i = 1, j-1
                if (strcmpi(parname,namsedim(i))) then
                   found = .true.
                   exit
                endif
             enddo
             if (found) then
                message = 'Sediment #'//trim(parname)//'# is specified more than once in sediment file '//trim(filsed)
                call write_error(message, unit=lundia)
                error = .true.
                return
             endif
             !
             ! Determine sediment type
             !
             tratypnr = TRA_COMBINE
             sedtyptmp = ' '
             call prop_get(asedblock_ptr, '*', 'SedTyp', sedtyptmp)
             call small(sedtyptmp, 999)
             !
             if (index(sedtyptmp, 'clay') == 1) then
                sedtypnr = SEDTYP_CLAY
             elseif (index(sedtyptmp, 'silt') == 1) then
                sedtypnr = SEDTYP_SILT
             elseif (index(sedtyptmp, 'sand') == 1) then
                sedtypnr = SEDTYP_SAND
             elseif (index(sedtyptmp, 'gravel') == 1) then
                sedtypnr = SEDTYP_GRAVEL
             elseif (index(sedtyptmp, 'mud') == 1) then
                sedtypnr = SEDTYP_CLAY
                ! TODO: WARN DEPRECATED
             elseif (index(sedtyptmp,'bedload') == 1) then
                sedtypnr = SEDTYP_SAND
                tratypnr = TRA_BEDLOAD
                ! TODO: WARN DEPRECATED
             else
                message = 'Sediment type of '//trim(parname)//' invalid: '//trim(sedtyptmp)
                call write_error(message,unit=lundia)
                error = .true.
                return
             endif
             lsed = lsed+1
             !
             ! Determine sediment type
             !
             totalload = .false.
             call prop_get(asedblock_ptr, '*', 'TotalLoad', totalload)
             if (totalload) tratypnr = TRA_BEDLOAD
             !
             if (tratypnr == TRA_BEDLOAD) then
                 ! change transport type from combined to bedload if possible
                 if (sedtypnr <= SEDTYP_SILT) then
                    message = 'Sediment '//trim(parname)//': silt and clay fractions can''t be modelled using total load'
                    call write_error(message,unit=lundia)
                    error = .true.
                    return
                 endif
                 lsed = lsed - 1
                 lsedbl = lsedbl + 1
             endif
             !
             namsedim(j) = parname
             typsedim(j) = sedtypnr
             typtrans(j) = tratypnr
          endif
       enddo
    else
       !
       ! sediment input version is 0 or 1:
       ! No keywords and no bedload option
       !
       lsedbl = 0
       call count_sed01(lundia    ,error     ,lsed      ,filsed    , &
                      & namsedim  ,typsedim  )
       allocate(typtrans(lsed))
       !
       do j = 1, lsed
          if (typsedim(j) == SEDTYP_SAND) then
             typtrans(j) = TRA_COMBINE
          else
             typtrans(j) = TRA_ADVDIFF
    endif
       enddo
    endif
    lsedtot = lsed + lsedbl
    !
    ! rhosol, namsed and sedtyp must always be allocated
    !
    if (lsedtot == 0) then
       message = 'No sediment fractions defined in ' // trim(filsed)
       call write_error(message, unit=lundia)
       error = .true.
       return
    endif
    
                    allocate (sedpar%rhosol(lsedtot), stat = istat)
    if (istat == 0) allocate (sedpar%namsed(lsedtot), stat = istat)
    if (istat == 0) allocate (sedpar%sedtyp(lsedtot), stat = istat)
    if (istat == 0) allocate (sedpar%tratyp(lsedtot), stat = istat)
    if (istat == 0) allocate (sedpar%flnrd(0:lsedtot), stat = istat)
    if (istat /= 0) then
       call write_error('Memory allocation error in COUNT_SED', unit=lundia)
    endif
    !
    i   = 1
    ibl = lsed+1
    do j = 1,size(typtrans)
       select case (typtrans(j)) 
          case (TRA_NONE)
             ! not a sediment block, so continue
          case (TRA_BEDLOAD)
             sedpar%namsed(ibl) = namsedim(j)
             sedpar%sedtyp(ibl) = typsedim(j)
             sedpar%tratyp(ibl) = typtrans(j)
             ibl = ibl+1
          case default ! just TRA_COMBINE since TRA_ADVDIFF will only be set later on depending on iform
             sedpar%namsed(i) = namsedim(j)
             sedpar%sedtyp(i) = typsedim(j)
             sedpar%tratyp(i) = typtrans(j)
             i = i+1
       end select
    enddo
    !
    deallocate(namsedim)
    deallocate(typsedim)
    deallocate(typtrans)
end subroutine count_sed


subroutine count_sed01(lundia    ,error     ,lsed      ,filsed    , &
                     & namsedim  ,typsedim  )
!!--description-----------------------------------------------------------------
!
! - Determines number of sediment fractions from sediment input file
!   supports both VERSION 0 and VERSION 1
!
!!--declarations----------------------------------------------------------------
    use string_module
    use message_module
    use sediment_basics_module
    !
    implicit none
!
! Arguments
!
    integer                         , intent(out) :: lsed     !< Number of suspended sediment fractions
    integer                         , intent(in)  :: lundia   !< Unit of diagnostic file
    character(*)                                  :: filsed
    logical                         , intent(out) :: error    !< error flag
    integer         , dimension(:) , allocatable  :: typsedim !< Type of the sediments
    character(20)   , dimension(:) , allocatable  :: namsedim !< Names of the sediments as read from sed-file
!
! Local variables
!
    integer                                                :: i
    integer                                                :: iocond
    integer                                                :: l
    integer                                                :: luninp    !< Unit of .sed file
    integer                                                :: npar
    integer                                                :: version   !< version number (0 or 1)
    character(20)                                          :: sedtyptmp !< Sediment type in sed-file
    character(300)                                         :: line
    character(300)                                         :: errmsg
!
!! executable statements -------------------------------------------------------
!
    error  = .false.
    call opensedfil(lundia    ,error     ,filsed    ,luninp    ,version  )
    if (error) return
    !
    read (luninp, *, iostat = iocond) lsed ! skip other parameters on line
    !
    allocate(namsedim(lsed))
    allocate(typsedim(lsed))
    !
    loop: &
    do l = 1, lsed
       write(namsedim(l),'(a,1i0)') 'sed.fraction ',l
       read (luninp, '(a)', iostat = iocond) sedtyptmp
       if (iocond == 0) then
          call str_lower(sedtyptmp)
          if (index(sedtyptmp, 'sand') == 1) then
              typsedim(l) = SEDTYP_SAND
          elseif (index(sedtyptmp, 'mud') == 1) then
              typsedim(l) = SEDTYP_CLAY
          else
             errmsg = 'Invalid suspended sediment type (must start with sand or mud)'
             call write_error(errmsg, unit=lundia)
             error = .true.
             exit loop
          endif
       endif
       !
       ! number of parameter values is 10 for version 0 and 11 for version 1 files
       !
       npar = 10+version
       do i = 1,npar
          read (luninp, *, iostat = iocond) line
          if (iocond /= 0) then
             write(errmsg,'(A,I0)') 'Error reading parameters for sediment fraction',l
             call write_error(errmsg, unit=lundia)
             error = .true.
             exit loop
          endif
       enddo
    enddo loop
    !
    close (luninp)
end subroutine count_sed01


end module m_rdsed
