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
module m_rdmor
private
!
! functions and subroutines
!
public rdmor
public echomor

contains

!> Reads attribute file for 3D morphology computation
subroutine rdmor(lundia    ,error     ,filmor    ,lsec      ,lsedtot   , &
               & lsed      ,nmaxus    ,nto       ,lfbedfrm  , &
               & nambnd    ,julday    ,mor_ptr   ,sedpar    ,morpar    , &
               & fwfac     ,morlyr    ,griddim)
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use table_handles
    use bedcomposition_module
    use morphology_data_module, only: sedpar_type, morpar_type
    use grid_dimens_module, only: griddimtype
    use sediment_basics_module
    use string_module
    use m_rdmorlyr
    use message_module, only: write_error, write_warning, FILE_NOT_FOUND, FILE_READ_ERROR, PREMATURE_EOF
    !
    implicit none
   !
   ! Local parameters
   !
    integer, parameter :: MAX_NUSERFRAC     = 20
   !
   ! Arguments
   !
    integer                        , intent(in)  :: julday
    integer                        , intent(in)  :: nmaxus
    integer                        , intent(in)  :: nto
    integer                        , intent(in)  :: lundia  !<  Description and declaration in inout.igs
    integer                        , intent(in)  :: lsec
    integer                        , intent(in)  :: lsed    !<  Description and declaration in esm_alloc_int.f90
    integer                        , intent(in)  :: lsedtot !<  Description and declaration in esm_alloc_int.f90
    logical                        , intent(in)  :: lfbedfrm    
    logical                        , intent(out) :: error
    character(len=*)               , intent(in)  :: filmor
    character(20) , dimension(nto)               :: nambnd  !<  Description and declaration in esm_alloc_char.f90
    type(tree_data)                , pointer     :: mor_ptr
    type(sedpar_type)              , pointer     :: sedpar
    type(morpar_type)              , pointer     :: morpar
    type(bedcomp_data)             , pointer     :: morlyr
    real(fp)                       , intent(out) :: fwfac
    type(griddimtype)   , target   , intent(in)  :: griddim
!
! Local variables
!
    integer                                                           :: i
    integer                                                           :: ilun     !< Unit number for attribute file
    integer                                                           :: istat
    integer                                                           :: j
    integer                                                           :: lenc
    integer                                                           :: lfile    !< Length of file name
    integer                                                           :: nxxprog
    integer                                                           :: nxxuser
    integer                                                           :: version
    real(fp)                   , dimension(:) , allocatable           :: xxprog
    real(fp)                   , dimension(MAX_NUSERFRAC)             :: rfield
    logical                                                           :: ex       !< Logical flag for file existence
    character(10)                                                     :: versionstring
    character(256)                                                    :: errmsg
    character(256)                                                    :: pxxstr
    character(256)                                                    :: string
    character(11)                                                     :: fmttmp !< Format file ('formatted  ')
!
!! executable statements -------------------------------------------------------
!
    error         = .false.
    rfield(:)     = -999.0_fp
    version       = -1
    nxxuser       = 0
    fmttmp        = 'formatted'
    !
    ! allocate memory for boundary conditions
    !
    istat = 0
                  allocate (morpar%morbnd(nto), stat = istat)
    if (istat==0) allocate (morpar%thetsd(griddim%nmlb:griddim%nmub), stat = istat)
    !
    if (istat /= 0) then
       call write_error('RDMOR: memory alloc error',unit=lundia)
       error = .true.
       return
    end if
    !
    do j = 1, nto
       morpar%morbnd(j)%icond = 1
       morpar%morbnd(j)%ibcmt = 0
       morpar%morbnd(j)%npnt  = 0
       nullify(morpar%morbnd(j)%alfa_dist)
       nullify(morpar%morbnd(j)%alfa_mag)
       nullify(morpar%morbnd(j)%idir)
       nullify(morpar%morbnd(j)%nm)
       nullify(morpar%morbnd(j)%nxmx)
       nullify(morpar%morbnd(j)%lm)
    enddo
    versionstring = 'n.a.'
    !
    ! Sediment percentiles needed by the program
    !
    nxxprog   = 4
    allocate(xxprog (nxxprog))
    xxprog(1) = 10.0_fp
    xxprog(2) = 15.0_fp
    xxprog(3) = 50.0_fp
    xxprog(4) = 90.0_fp
    !
    ! Set default value for FWFac (only used if no mor file specified and
    ! parameter not specified in mdf file)
    !
    fwfac     = 1.0_fp
    !
    ! locate 'Filmor' record for attribute file containing parameters
    ! for morphological computation
    !
    if (filmor == ' ') then
       !
       ! file does not exist
       !
       errmsg = 'No morphological file defined. Using default values.'
       call write_warning(errmsg,unit=lundia)
    else 
       !
       call put_morfile_in_input_tree(mor_ptr, filmor, lundia, error)
       if (error) return
       !
       ! Check version number of mor input file
       !
       call prop_get(mor_ptr, 'MorphologyFileInformation', 'FileVersion', versionstring)
       if (trim(versionstring) == '02.00') then
          version = 2
          !
          ! === morphological timescale factor
          ! First assume that 'MorFac' contains a filename
          ! If the file does not exist, assume that 'MorFac' contains a uniform value (real)
          !
          string = ' '
          call prop_get(mor_ptr, 'Morphology', 'MorFac', string)
          !
          ! Intel 7.0 crashes on an inquire statement when file = ' '
          !
          if (string == ' ') then
             ex = .false.
          else
             call combinepaths(filmor, string)
             inquire (file = trim(string), exist = ex)
          end if
          if (.not. ex) then
             morpar%varyingmorfac = .false.
             call prop_get(mor_ptr, 'Morphology', 'MorFac', morpar%morfac)
          else
             morpar%varyingmorfac = .true.
             morpar%morfac = -1.0
             call readtable(morpar%morfacfile, trim(string), julday, errmsg)
             if (errmsg /= ' ') then
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
              end if
          end if
       
          call read_morphology_properties(mor_ptr, morpar, griddim, filmor, fmttmp, nto, sedpar%anymud, &
                                          lundia, fwfac, error)
          if (error) return
	   
          call read_morsys_parameters(mor_ptr, morpar, filmor, lsec, morpar%islope, lundia, error)
          if (error) return
          !
          call read_morphology_boundary_conditions(mor_ptr, morpar%morbnd, morpar%bcmfilnam, morpar%bcmfile,&
                    julday, nto, nambnd, filmor, lundia, error)
          if (error) return
          !
          call read_morphology_numerical_settings(mor_ptr, morpar%mornum)
          !
          call read_morphology_output_options(mor_ptr, morpar%moroutput, lsedtot, filmor, lundia, error)
          if (error) return
          !
          call set_sediment_percentiles(mor_ptr, morpar%moroutput, pxxstr)
          !
          call read_morphology_process_string(pxxstr, nxxuser, max_nuserfrac, rfield, filmor, lundia, error)
          if (error) return
       else
          call remove_leading_spaces(filmor, lfile)
          !
          inquire (file = filmor(1:lfile), exist = ex)
          if (ex) then
             open (newunit=ilun, file = filmor(1:lfile), form = 'formatted', status = 'old')
             !
             version = 0
             lenc    = 999
             read (ilun, '(a)') string
             do while (string(:1) == '*')
                call str_lower(string, lenc)
                i = index(string, 'version')
                if (i /= 0) then
                   read (string(i + 8:), '(i2)') version
                end if
                read (ilun, '(a)') string
             end do
             write (versionstring, '(i4)') version
             !
             rewind (ilun)
             call skipstarlines(ilun)
             !
             if (version == 0) then
                call rdmor0(ilun, morpar%morfac, morpar%tmor, morpar%thresh, morpar%bedupd, morpar%eqmbcsand, morpar%densin, &
                       morpar%aksfac, morpar%rwave, morpar%rouse, morpar%alfabs, morpar%alfabn, morpar%sus, morpar%bed, &
                       morpar%susw, morpar%bedw, morpar%sedthr, morpar%thetsduni, morpar%hmaxth, fwfac)
             else
                call rdmor1(ilun, morpar%morfac, morpar%tmor, morpar%thresh, morpar%bedupd, morpar%eqmbcsand, morpar%densin, &
                    morpar%aksfac, morpar%rwave, morpar%alfabs, morpar%alfabn, morpar%sus, morpar%bed, morpar%susw, morpar%bedw, &
                    morpar%sedthr, morpar%thetsduni, morpar%hmaxth, fwfac, morpar%epspar, morpar%iopkcw, morpar%rdc, morpar%rdw)
             end if
             morpar%thetsd = max(0.0_fp,min(morpar%thetsduni,1.0_fp))
             morpar%tcmp = morpar%tmor
             morpar%cmpupd = .true.
             close (ilun)
          else
             !
             ! file not found
             !
             call write_error(FILE_NOT_FOUND//trim(filmor), unit=lundia)
          end if
       end if
       !
    end if
    !
    call remove_double_percentiles(morpar, nxxuser, nxxprog, xxprog, max_nuserfrac, rfield)
    !
    ! allocate memory for percentiles
    !
    allocate (morpar%xx(morpar%nxx), stat = istat)
    !
    if (istat /= 0) then
       errmsg = 'RDMOR: memory alloc error'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    end if
    !
    call copy_and_sort_percentiles(morpar, nxxuser, nxxprog, xxprog, max_nuserfrac, rfield, lundia, error)
    if (error) return
    !
    if (sedpar%anymud) then
       call rdflufflyr(lundia    ,error    ,filmor    ,lsed    , &
                     & mor_ptr   ,morpar%flufflyr     ,sedpar  , &
                     & griddim   )
    end if
    !
    call rdmorlyr (lundia    ,error     ,filmor    , &
                 & nmaxus    ,nto       ,lfbedfrm  , &
                 & nambnd    ,version   ,lsedtot   , sedpar%namsed    , &
                 & morpar    ,morlyr    ,sedpar    ,mor_ptr   , &
                 & griddim   )
    !
    deallocate(xxprog)
end subroutine rdmor

!> put mor file in input tree 
subroutine put_morfile_in_input_tree(mor_ptr, filmor, lundia, error)
    use tree_data_types
    use properties
    use message_module, only: write_error, write_warning, FILE_NOT_FOUND, FILE_READ_ERROR, PREMATURE_EOF
    implicit none

    type(tree_data)               , pointer, intent(inout)  :: mor_ptr
    character(len=*)                       , intent(in)     :: filmor
    integer                                , intent(in)     :: lundia  !<  Description and declaration in inout.igs
    logical                                , intent(out)    :: error
    
    integer                                                 :: istat

    error = .false.
    call prop_file('ini', trim(filmor), mor_ptr, istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          call write_error(FILE_NOT_FOUND//trim(filmor), unit=lundia)
       case(3)
          call write_error(PREMATURE_EOF//trim(filmor), unit=lundia)
       case default
          call write_error(FILE_READ_ERROR//trim(filmor), unit=lundia)
       endselect
       error = .true.
    end if
end subroutine put_morfile_in_input_tree
    
!> read morphology properties               
subroutine read_morphology_properties(mor_ptr, morpar, griddim, filmor, fmttmp, nto, anymud, lundia, fwfac, error)			   
    use tree_data_types
    use morphology_data_module
    use grid_dimens_module
    use properties
    use precision
    use m_depfil_stm
    use message_module
    implicit none

    real(fp)                               , parameter      :: RMISSVAL = -999.0_fp
        
    type(tree_data)               , pointer, intent(inout)  :: mor_ptr
    type(morpar_type)             , pointer, intent(inout)  :: morpar
    type(griddimtype)             , target , intent(in)     :: griddim
    character(len=*)                       , intent(in)     :: filmor
    character(11)                          , intent(in)     :: fmttmp !< Format file ('formatted  ')
    integer                                , intent(in)     :: nto
    logical                                , intent(in)     :: anymud
    integer                                , intent(in)     :: lundia  !<  Description and declaration in inout.igs
    real(fp)                               , intent(out)    :: fwfac
    logical                                , intent(out)    :: error
    
    character(256)                                          :: errmsg
    character(256)                                          :: string
    logical                                                 :: exist
    integer                                                 :: j, nm
    !
    ! === start for calculating sediment transport
    !
    call prop_get(mor_ptr, 'Morphology', 'SedTransStt', morpar%ti_sedtrans)
    !
    ! === start for calculating morphological changes (backward compatibility)
    !
    call prop_get(mor_ptr, 'Morphology', 'MorStt', morpar%tmor)
    !
    ! === start for calculating morphological changes
    !
    call prop_get(mor_ptr, 'Morphology', 'BedUpdStt', morpar%tmor)       
    !
    ! === start for calculating bed composition changes
    !
    morpar%tcmp = morpar%tmor  ! by default, composition update starts when morphological update starts
    !
    call prop_get(mor_ptr, 'Morphology', 'CmpUpdStt', morpar%tcmp)   
    !
    ! === threshold value for slowing erosion near a fixed layer (m)
    !
    call prop_get(mor_ptr, 'Morphology', 'Thresh', morpar%thresh)
    !
    ! === flags for doing morphological updates
    ! First, there was morupd
    ! Then there came bedupd and cmpupd
    ! morupd corresponds to the new bedupd.
    ! By replacing morupd with bedupd, the parameter morupd is no longer needed.
    ! By default, cmpupd must be true
    !
    call prop_get(mor_ptr, 'Morphology', 'MorUpd', morpar%bedupd)
    morpar%cmpupd = .true.
    !
    ! flag for doing bed level updates
    !
    call prop_get(mor_ptr, 'Morphology', 'BedUpd', morpar%bedupd)
    !
    ! flag for doing composition updates
    !
    call prop_get(mor_ptr, 'Morphology', 'CmpUpd', morpar%cmpupd)
    !
    if (morpar%ti_sedtrans > morpar%tmor .or. morpar%ti_sedtrans > morpar%tcmp) then
        errmsg = 'SedTransStt must be smaller than or equal to CmpUpdStt and BedUpdStt (MorStt) in ' // trim(filmor)
        call write_error(errmsg, unit=lundia)
        error = .true.
        return   
    end if 
    !
    if (morpar%bedupd .and. morpar%tcmp > morpar%tmor) then
        errmsg = 'When BedUpd = true, CmpUpdStt must be smaller than or equal to BedUpdStt (MorStt) in ' // trim(filmor)
        call write_error(errmsg, unit=lundia)
        error = .true.
        return   
    end if 
    !
    call prop_get(mor_ptr, 'Morphology', 'NeglectEntrainment', morpar%neglectentrainment)
    !
    ! === flag for setting equilibrium sediment concentration profiles
    ! at the open boundaries for sand sediment
    ! First read it as string to check whether the keyword NeuBcSand is present.
    ! If not, try the old keyword EqmBc
    ! NeuBcSand will be used when both keywords are present
    !
    string = ' '
    call prop_get(mor_ptr, 'Morphology', 'NeuBcSand', string)
    if (string /= ' ') then
        call prop_get(mor_ptr, 'Morphology', 'NeuBcSand', morpar%eqmbcsand)
    else
        call prop_get(mor_ptr, 'Morphology', 'EqmBc', morpar%eqmbcsand)
    end if
    !
    ! === flag for setting equilibrium sediment concentration profiles
    ! at the open boundaries for mud sediment
    !
    call prop_get(mor_ptr, 'Morphology', 'NeuBcMud', morpar%eqmbcmud)
    !
    ! === flag for including sediment in fluid density calculations
    !
    call prop_get(mor_ptr, 'Morphology', 'DensIn', morpar%densin)
    !
    ! === factor for setting aks height
    !
    call prop_get(mor_ptr, 'Morphology', 'AksFac', morpar%aksfac)
    !
    ! === factor for calculating wave-related roughness from ripple dimensions
    !
    call prop_get(mor_ptr, 'Morphology', 'RWave', morpar%rwave)
    !
    ! Flag Rouse is skipped
    !
    ! === factor for longitudinal bed load transport
    !
    call prop_get(mor_ptr, 'Morphology', 'AlfaBs', morpar%alfabs)
    !
    ! === factor for transverse bed load transport
    !
    call prop_get(mor_ptr, 'Morphology', 'AlfaBn', morpar%alfabn)
    !
    ! === Parameters used in dune avalanching
    !
    call prop_get(mor_ptr, 'Morphology', 'WetSlope'  , morpar%wetslope)
    call prop_get(mor_ptr, 'Morphology', 'DrySlope'  , morpar%dryslope)
    call prop_get(mor_ptr, 'Morphology', 'DuneAvalan', morpar%duneavalan)
    call prop_get(mor_ptr, 'Morphology', 'Hswitch'   , morpar%hswitch)
    call prop_get(mor_ptr, 'Morphology', 'DzMaxDune' , morpar%dzmaxdune)
    !
    ! === time scale for avalanching  D3D style
    !
    call prop_get(mor_ptr, 'Morphology', 'AvalTime', morpar%avaltime)
    !
    ! === factor for calculating suspended load transport
    !
    call prop_get(mor_ptr, 'Morphology', 'Sus', morpar%sus)
    !
    ! === factor for calculating bed load transport
    !
    call prop_get(mor_ptr, 'Morphology', 'Bed', morpar%bed)
    !
    ! === wave-related suspended sediment factor
    !
    call prop_get(mor_ptr, 'Morphology', 'SusW', morpar%susw)
    !
    ! === wave-related bed-load sediment factor
    !
    call prop_get(mor_ptr, 'Morphology', 'BedW', morpar%bedw)
    !
    ! === minimum depth for sediment calculations
    !
    call prop_get(mor_ptr, 'Morphology', 'SedThr', morpar%sedthr)
    !
    ! === global / maximum dry cell erosion factor
    !
    call prop_get(mor_ptr, 'Morphology', 'ThetSD', morpar%flsthetsd)
    !
    !
    ! Intel 7.0 crashes on an inquire statement when file = ' '
    !
    if (morpar%flsthetsd == ' ') then
        exist = .false.
    else
        call combinepaths(filmor, morpar%flsthetsd)
        inquire (file = morpar%flsthetsd, exist = exist)
    end if
    if (exist) then
        !
        ! Space varying data has been specified
        ! Use routine that also read the depth file to read the data
        !
        call depfil_stm(lundia    ,error     ,morpar%flsthetsd    ,fmttmp    , &
                        & morpar%thetsd    ,1         ,1         ,griddim   , errmsg)
        if (error) then
            call write_error(errmsg, unit=lundia)
            return
        end if
        do nm = 1, griddim%nmmax
            morpar%thetsd(nm) = max(0.0_fp, min(morpar%thetsd(nm), 1.0_fp))
        enddo
    else
        morpar%flsthetsd = ' '
        morpar%thetsduni = 0.0_fp
        call prop_get(mor_ptr, 'Morphology', 'ThetSD', morpar%thetsduni)
        !
        ! Uniform data has been specified
        !
        morpar%thetsd = max(0.0_fp,min(morpar%thetsduni,1.0_fp))
    end if
    !
    ! === maximum depth for variable dry cell erosion factor
    !
    call prop_get(mor_ptr, 'Morphology', 'HMaxTH', morpar%hmaxth)
    !
    ! === factor for adjusting intensity of energy dissipation in wave boundary layer
    ! fwfac should be read from mdf-file (see rdnum)
    ! The reading of fwfac is left here for backwards compatibility
    ! [This value will only be used if fwfac is not read from mdf-file]
    !
    call prop_get(mor_ptr, 'Morphology', 'FWFac', fwfac)
    !
    ! === factor for adjusting shields critical shear stress
    !
    call prop_get(mor_ptr, 'Morphology', 'Factcr', morpar%factcr)
    !
    ! === flag for using eulerian vel iso glm velocities for susp transports
    !
    call prop_get(mor_ptr, 'Morphology', 'EulerisoGLM', morpar%eulerisoglm)
    !
    ! === flag for using glm vel iso eulerian velocities for bed load transports and ref concentration
    !
    call prop_get(mor_ptr, 'Morphology', 'GLMisoEuler', morpar%glmisoeuler)
    !
    ! === flag for correction of doublecounting sus/bed transport below aks
    !
    call prop_get(mor_ptr, 'Morphology', 'SusCor', morpar%l_suscor)
    !
    ! === phase lead for bed shear stress of Nielsen (1992) in TR2004
    !
    call prop_get(mor_ptr, 'Morphology', 'Pangle', morpar%pangle)
    !
    ! === coefficient for phase lag effects in wave-induced suspended transport in TR2004
    !
    call prop_get(mor_ptr, 'Morphology', 'Fpco', morpar%fpco)        
    !
    ! === wave period subdivision in TR2004
    !
    call prop_get(mor_ptr, 'Morphology', 'Subiw', morpar%subiw)
    !
    ! === flag for parametric epsilon distribution in case of K-Eps model
    !
    call prop_get(mor_ptr, 'Morphology', 'EpsPar', morpar%epspar)
    !
    call prop_get(mor_ptr, 'Morphology', 'IopKCW', morpar%iopkcw)
    !
    ! === calibration factor for 2D suspended load relaxation time
    !
    call prop_get(mor_ptr, 'Morphology', 'FacTsd', morpar%factsd)
    !
    call prop_get(mor_ptr, 'Morphology', 'RDC', morpar%rdc)
    !
    call prop_get(mor_ptr, 'Morphology', 'RDW', morpar%rdw)
    !
    ! === flag for updating bed level at inflow boundary
    !
    call prop_get(mor_ptr, 'Morphology', 'UpdInf', morpar%updinf)
    !
    ! === flag for merging bottoms of parallel runs
    !
    call prop_get(mor_ptr, 'Morphology', 'Multi', morpar%multi)
    !
       ! === bermslope (FM only)
       !
       call prop_get(mor_ptr, 'Morphology', 'bermslopetransport' , morpar%bermslopetransport)
       if (morpar%bermslopetransport) then
          call prop_get(mor_ptr, 'Morphology', 'bermslope'          , morpar%bermslope         )
          call prop_get(mor_ptr, 'Morphology', 'bermslopefac'       , morpar%bermslopefac      )
          call prop_get(mor_ptr, 'Morphology', 'bermslopegamma'     , morpar%bermslopegamma    )
          call prop_get(mor_ptr, 'Morphology', 'bermslopedepth'     , morpar%bermslopedepth    )
          call prop_get(mor_ptr, 'Morphology', 'bermslopesus'       , morpar%bermslopesus      )
          call prop_get(mor_ptr, 'Morphology', 'bermslopebed'       , morpar%bermslopebed      )
          if (morpar%bermslopefac>30) then
             errmsg = 'bermslopefac set to value higher than 30. It is advised to lower the value in order to maintain stable morphology.'
             call write_warning(errmsg, unit=lundia)
          endif
       endif
       !
    if (morpar%updinf) then
        !
        ! set bed boundary conditions to "free boundaries"
        !
        do j = 1, nto
            morpar%morbnd(j)%icond = 0
        enddo
    end if
    !
    call prop_get(mor_ptr, 'Morphology', 'DzMax', morpar%dzmax)
    call prop_get(mor_ptr, 'Morphology', 'CaMax', morpar%camax)
    call prop_get(mor_ptr, 'Morphology', 'OldMudFrac', morpar%oldmudfrac)
    !
    ! Hiding & exposure
    !
    call prop_get(mor_ptr, 'Morphology', 'IHidExp', morpar%ihidexp)
    if ( morpar%ihidexp < 1 .or. morpar%ihidexp > 5 ) then
        errmsg = 'IHidExp should be in the range 1 to 5 in ' // trim(filmor)
        call write_error(errmsg, unit=lundia)
        error = .true.
        return
    else if ( morpar%ihidexp > 1 .and. anymud ) then
        errmsg = 'Hiding-exposure with mud is an experimental feature. Hiding-exposure does not take into account the presence of mud.'
        call write_warning(errmsg, unit=lundia)
    end if
    select case(morpar%ihidexp)
    case(4) ! Parker, Klingeman, McLean
        call prop_get(mor_ptr, 'Morphology', 'ASKLHE', morpar%asklhe)
        if (comparereal(morpar%asklhe,RMISSVAL) == 0) then
            errmsg = 'ASKLHE missing: Alpha exponent for Soehngen et.al. hiding & exposure in file '//trim(filmor)
            call write_error(errmsg, unit=lundia)
            error = .true.
            return
        end if
    case(5) ! Wu, Wang, Jia
        call prop_get(mor_ptr, 'Morphology', 'MWWJHE', morpar%mwwjhe)
        if (comparereal(morpar%mwwjhe,RMISSVAL) == 0) then
            errmsg = 'MWWJHE missing: M exponent for Wu, Wang, Jia hiding & exposure in file '//trim(filmor)
            call write_error(errmsg, unit=lundia)
            error = .true.
            return
        end if
    endselect
    
	   
end subroutine read_morphology_properties

subroutine read_morsys_parameters(mor_ptr, morpar, filmor, lsec, islope, lundia, error)
    use tree_data_types
    use morphology_data_module
    use properties
    use message_module
    implicit none
    
    type(tree_data)               , pointer, intent(inout)  :: mor_ptr
    type(morpar_type)             , pointer, intent(inout)  :: morpar
    character(len=*)                       , intent(in)     :: filmor
    integer                                , intent(in)     :: lsec
    integer                                , intent(inout)  :: islope
    integer                                , intent(in)     :: lundia  !<  Description and declaration in inout.igs
    logical                                , intent(out)    :: error  
    
    character(256)                                          :: errmsg
    
    if (lsec > 0) then
        ! factor for spiral flow intensity effect
        call prop_get(mor_ptr, 'Morphology', 'Espir', morpar%espir)
    end if
    call prop_get(mor_ptr, 'Morphology', 'ISlope', islope)
    if (islope < 1 .or. islope > 4) then
        ! bedslope effect formulation
        errmsg = 'ISlope should be 1 to 4 in ' // trim(filmor)
        call write_error(errmsg, unit=lundia)
        error = .true.
        return
    end if
    if (islope == 3) then
        ! bedslope effect parameters
        call prop_get(mor_ptr, 'Morphology', 'AShld', morpar%ashld)
        call prop_get(mor_ptr, 'Morphology', 'BShld', morpar%bshld)
        call prop_get(mor_ptr, 'Morphology', 'CShld', morpar%cshld)
        call prop_get(mor_ptr, 'Morphology', 'DShld', morpar%dshld)
     elseif (islope == 4) then
        call prop_get(mor_ptr, 'Morphology', 'CoulFri', morpar%coulfri)
        call prop_get(mor_ptr, 'Morphology', 'FlFdRat', morpar%flfdrat)
        morpar%alfpa = morpar%coulfri / (1.0 + morpar%flfdrat * morpar%coulfri)
        call prop_get(mor_ptr, 'Morphology', 'ThetaCr', morpar%thcrpa)
    end if   
       
end subroutine read_morsys_parameters

!> reading boundary conditions
subroutine read_morphology_boundary_conditions(mor_ptr, morbnd, bcmfilnam, bcmfile, julday, nto, nambnd, filmor, lundia, error)
    use tree_data_types
    use tree_structures
    use morphology_data_module, only: bedbndtype
    use properties
    use table_handles
    use handles
    use message_module
    use dfparall, only: parll
    
    implicit none
!    
    type(tree_data)               , pointer, intent(inout)  :: mor_ptr
    type(bedbndtype), dimension(:), pointer, intent(inout)  :: morbnd
    character(256)                         , intent(inout)  :: bcmfilnam
    type(handletype)                       , intent(in)     :: bcmfile
    integer                                , intent(in)     :: julday
    integer                                , intent(in)     :: nto
    character(20) , dimension(nto)         , intent(in)     :: nambnd  !<  Description and declaration in esm_alloc_char.f90
    character(len=*)                       , intent(in)     :: filmor
    integer                                , intent(in)     :: lundia  !<  Description and declaration in inout.igs
    logical                                , intent(out)    :: error
!    
    character(256)                                          :: errmsg
    character(80)                                           :: bndname
    character(256)                                          :: txtput1
    logical                                                 :: found
    integer                                                 :: i, j
    type(tree_data)                            , pointer    :: morbound_ptr
!    
    bcmfilnam = ' '
    call prop_get(mor_ptr, 'Morphology', 'BcFil', bcmfilnam)
    if (bcmfilnam /= ' ') then
        call combinepaths(filmor, bcmfilnam)
        !write (lundia, '(3a)') txtput1, ': ', trim(bcmfilnam)
        call readtable(bcmfile, bcmfilnam, julday, errmsg)
        if (errmsg /= ' ') then
            call write_error(errmsg, unit=lundia)
            error = .true.
            return
        end if
    end if
    !
    do i = 1, size(mor_ptr%child_nodes)
        !
        ! Does mor_ptr contain a child with name 'Boundary' (converted to lower case)?
        !
        morbound_ptr => mor_ptr%child_nodes(i)%node_ptr
        bndname = tree_get_name( morbound_ptr )
        if ( trim(bndname) /= 'boundary') cycle
        bndname = ' '
        call prop_get(morbound_ptr, '*', 'Name', bndname)
        found = .false.
        do j = 1, nto
            !
            ! Search known boundaries for match
            !
            if (bndname == nambnd(j)) then
                found = .true.
                exit
            end if
        enddo
        if (.not.found) then
            if (parll) then
               txtput1 = 'Boundary "'//trim(bndname)//'" in '//trim(filmor)// ' not found. As this is a parallel run, it must be in another partition.'
               write (lundia, '(a)') txtput1
               cycle
            else    
               errmsg = 'Unknown boundary "'//trim(bndname)//'" in '//trim(filmor)
               call write_error(errmsg, unit=lundia)
               error = .true.
               return
            endif
        end if
        !
        ! Read bed boundary condition for open boundary
        !
        call prop_get(morbound_ptr, '*', 'IBedCond', morbnd(j)%icond)
        if (morbnd(j)%icond<0 .or. morbnd(j)%icond>8) then
            errmsg = 'Invalid bed boundary condition at "'//trim(bndname)//'" in '//trim(filmor)
            call write_error(errmsg, unit=lundia)
            error = .true.
            return
        end if
    enddo
       
end subroutine read_morphology_boundary_conditions
               
               
!> reading numerical setting for rdmor
subroutine read_morphology_numerical_settings(mor_ptr, mornum)
    use tree_data_types
    use morphology_data_module
    use properties
    use string_module, only: str_lower

    implicit none
    
    type(tree_data)                , pointer     :: mor_ptr
    type(mornumericstype)          , pointer     :: mornum
    
    character(20)                                :: fluxlimstring
    
    call prop_get(mor_ptr, 'Numerics', 'Pure1D', mornum%pure1d)
    call prop_get(mor_ptr, 'Numerics', 'UpwindBedload', mornum%upwindbedload)
    call prop_get(mor_ptr, 'Numerics', 'LaterallyAveragedBedload', mornum%laterallyaveragedbedload)
    call prop_get(mor_ptr, 'Numerics', 'MaximumWaterdepth', mornum%maximumwaterdepth)
    call prop_get(mor_ptr, 'Numerics', 'MaximumWaterdepthFraction', mornum%maximumwaterdepthfrac)
    fluxlimstring = ' '
    call prop_get(mor_ptr, 'Numerics', 'FluxLimiter', fluxlimstring)       
    call str_lower(fluxlimstring)
    select case(fluxlimstring)
        case('minmod')
            mornum%fluxlim = FLUX_LIMITER_MINMOD  
        case('mc')     
            mornum%fluxlim = FLUX_LIMITER_MC  
        case default 
            mornum%fluxlim = FLUX_LIMITER_NONE  
    end select
           
end subroutine read_morphology_numerical_settings

!> reading output options
subroutine read_morphology_output_options(mor_ptr, moroutput, lsedtot, filmor, lundia, error)
    use precision
    use tree_data_types
    use morphology_data_module
    use properties
    use message_module
    use string_module, only: str_lower
    
    implicit none
    
    type(tree_data)                , pointer     :: mor_ptr
    type(moroutputtype)            , pointer     :: moroutput
    integer                        , intent(in)  :: lsedtot !<  Description and declaration in esm_alloc_int.f90
    character(len=*)               , intent(in)  :: filmor
    integer                        , intent(in)  :: lundia  !<  Description and declaration in inout.igs
    logical                        , intent(out) :: error
    
    character(256)                               :: string
    character(256)                               :: errmsg
    logical                                      :: exist, found, defined
    integer                                      :: i, iqnt
    integer                    , dimension(6)    :: stat_flags
        
    call prop_get(mor_ptr, 'Output', 'Default', defined, success = found)
    if (.not.found) call prop_get(mor_ptr, 'Output', 'OutDefault', defined, success = found) ! backward compatibility
    if (found) then
        call initmoroutput(moroutput, defined)
    end if
    call prop_get(mor_ptr, 'Output', 'VelocAtZeta', moroutput%uuuvvv)
    call prop_get(mor_ptr, 'Output', 'VelocMagAtZeta', moroutput%umod)
    call prop_get(mor_ptr, 'Output', 'VelocZAtZeta', moroutput%zumod)
    call prop_get(mor_ptr, 'Output', 'ShearVeloc', moroutput%ustar)
    !
    call prop_get(mor_ptr, 'Output', 'TranspType',moroutput%transptype)
    if (moroutput%transptype < 0 .or. moroutput%transptype > 2) then
        errmsg = 'Invalid transport type in '//trim(filmor)
        call write_error(errmsg, unit=lundia)
        error = .true.
        return
    end if
    call prop_get(mor_ptr, 'Output', 'BedTranspAtFlux'             , moroutput%sbuuvv)
    call prop_get(mor_ptr, 'Output', 'SuspTranspAtFlux'            , moroutput%ssuuvv)
    call prop_get(mor_ptr, 'Output', 'BedTranspDueToCurrentsAtZeta', moroutput%sbcuv)
    call prop_get(mor_ptr, 'Output', 'BedTranspDueToCurrentsAtFlux', moroutput%sbcuuvv)
    call prop_get(mor_ptr, 'Output', 'BedTranspDueToWavesAtZeta'   , moroutput%sbwuv)
    call prop_get(mor_ptr, 'Output', 'BedTranspDueToWavesAtFlux'   , moroutput%sbwuuvv)
    call prop_get(mor_ptr, 'Output', 'SuspTranspDueToWavesAtZeta'  , moroutput%sswuv)
    call prop_get(mor_ptr, 'Output', 'SuspTranspDueToCurrentsAtZeta'  , moroutput%sscuv)
    call prop_get(mor_ptr, 'Output', 'SuspTranspDueToWavesAtFlux'  , moroutput%sswuuvv)
    call prop_get(mor_ptr, 'Output', 'NearBedRefConcentration'     , moroutput%rca)
    call prop_get(mor_ptr, 'Output', 'EquilibriumConcentration'    , moroutput%rsedeq)
    call prop_get(mor_ptr, 'Output', 'NearBedTranspCorrAtFlux'     , moroutput%suvcor)
    call prop_get(mor_ptr, 'Output', 'SourceSinkTerms'             , moroutput%sourcesink)
    call prop_get(mor_ptr, 'Output', 'ReferenceHeight'             , moroutput%aks)
    call prop_get(mor_ptr, 'Output', 'SettlingVelocity'            , moroutput%ws)
    call prop_get(mor_ptr, 'Output', 'RawTransportsAtZeta'         , moroutput%rawtransports)
    call prop_get(mor_ptr, 'Output', 'Seddif'                      , moroutput%seddif)
    call prop_get(mor_ptr, 'Output', 'SedParOut'                   , moroutput%sedpar) ! backward compatibility
    call prop_get(mor_ptr, 'Output', 'SedPar'                      , moroutput%sedpar)
    !
    call prop_get(mor_ptr, 'Output', 'Bedslope'                    , moroutput%dzduuvv)
    call prop_get(mor_ptr, 'Output', 'Taub'                        , moroutput%taub)
    call prop_get(mor_ptr, 'Output', 'Taurat'                      , moroutput%taurat)
    !
    call prop_get(mor_ptr, 'Output', 'Dm'                          , moroutput%dm)
    call prop_get(mor_ptr, 'Output', 'Dg'                          , moroutput%dg)
    call prop_get(mor_ptr, 'Output', 'Dgsd'                        , moroutput%dgsd)
    call prop_get(mor_ptr, 'Output', 'Frac'                        , moroutput%frac)
    call prop_get(mor_ptr, 'Output', 'MudFrac'                     , moroutput%mudfrac)
    call prop_get(mor_ptr, 'Output', 'SandFrac'                    , moroutput%sandfrac)
    call prop_get(mor_ptr, 'Output', 'FixFac'                      , moroutput%fixfac)
    call prop_get(mor_ptr, 'Output', 'HidExp'                      , moroutput%hidexp)
    !
    call prop_get(mor_ptr, 'Output', 'CumNetSedimentationFlux'     , moroutput%dmsedcum)
    call prop_get(mor_ptr, 'Output', 'BedLayerSedimentMass'        , moroutput%msed)
    call prop_get(mor_ptr, 'Output', 'BedLayerVolumeFractions'     , moroutput%lyrfrac)
    call prop_get(mor_ptr, 'Output', 'BedLayerDepth'               , moroutput%dpbedlyr)
    call prop_get(mor_ptr, 'Output', 'BedLayerPorosity'            , moroutput%poros)
    !
    call prop_get(mor_ptr, 'Output', 'AverageAtEachOutputTime'     , moroutput%cumavg)
    !
    call prop_get(mor_ptr, 'Output', 'MainChannelAveragedBedLevel' , moroutput%blave)
    !
    call prop_get(mor_ptr, 'Output', 'MainChannelCellArea'         , moroutput%bamor)
    !
    call prop_get(mor_ptr, 'Output', 'MainChannelWidthAtFlux'      , moroutput%wumor)
    !
    call prop_get(mor_ptr,         'Output', 'MorStatsOutputInterval'      , moroutput%avgintv, 3, exist)
    if (exist) then
        moroutput%morstats = .true.    ! only used in FM, separate _sed.nc file
    end if
    if (moroutput%avgintv(2) < 0.0_fp) then
        moroutput%avgintv(2) = 0.0_fp
        moroutput%avgintv(3) = 0.0_fp
    end if
    string = ' '
    call prop_get (mor_ptr, 'Output', 'MorstatsWeightFactor'         , string)
    call str_lower(string)
    if (index(string,'time') > 0) then
        moroutput%weightflg = MOR_STAT_TIME
    end if
    if (index(string,'sedimentation') > 0) then
        moroutput%weightflg = MOR_STAT_BODS            ! Delft3D behaviour
    end if
    i = 1+lsedtot ! index 1           used internally for weights
                  ! index 2,lsedtot+1 used for CumNetSedimentationFlux per fraction
                  ! rest follows below
    do iqnt = 1, 4
        string = ' '
        select case (iqnt)
        case (1)
            call prop_get (mor_ptr, 'Output', 'StatWaterDepth'             , string)
        case (2)
            call prop_get (mor_ptr, 'Output', 'StatVelocity'               , string)
        case (3)
            call prop_get (mor_ptr, 'Output', 'StatBedLoad'                , string)
        case (4)
            call prop_get (mor_ptr, 'Output', 'StatSuspLoad'               , string)
        endselect
        !
        stat_flags(:) = 0
        call str_lower(string)
        if (index(string,'min') > 0) then
            stat_flags(1) = stat_flags(1) + MOR_STAT_MIN
            i = i+1
            stat_flags(2) = i
        end if
        if (index(string,'max') > 0) then
            stat_flags(1) = stat_flags(1) + MOR_STAT_MAX
            i = i+1
            stat_flags(3) = i
        end if
        if (index(string,'mean') > 0) then
            stat_flags(1) = stat_flags(1) + MOR_STAT_MEAN
            i = i+1
            stat_flags(4) = i
        end if
        if (index(string,'std') > 0) then
            stat_flags(1) = stat_flags(1) + MOR_STAT_STD
            if (.not.btest(stat_flags(1),MOR_STAT_MEAN)) then
                i = i+1
                stat_flags(4) = i
            end if
            i = i+1
            stat_flags(5) = i
        end if
        if (index(string,'net') > 0) then
            stat_flags(1) = stat_flags(1) + MOR_STAT_CUM
            i = i+1
            stat_flags(6) = i
        end if
        moroutput%statflg(:,iqnt) = stat_flags
    enddo
    moroutput%nstatqnt = i
       

end subroutine read_morphology_output_options

!> Sediment percentiles. Requested by the user via the .mor attribute file
subroutine set_sediment_percentiles(mor_ptr, moroutput, pxxstr)
    use tree_data_types
    use morphology_data_module
    use properties
    use string_module
    
    implicit none
    
    type(tree_data)                , pointer, intent(inout)     :: mor_ptr
    type(moroutputtype)            , pointer, intent(in)        :: moroutput 
    character(256)                          , intent(inout)     :: pxxstr
    
    integer                                                     :: lenc
        
    pxxstr = ' '
    call prop_get(mor_ptr, 'Output', 'Percentiles', pxxstr)
    lenc = 999
    call str_lower(pxxstr, lenc)
    call remove_leading_spaces(pxxstr, lenc)
    if (pxxstr == ' ' .or. pxxstr == 'false') then
        pxxstr = ' '
        moroutput%percentiles = .false.
    elseif (pxxstr == 'true') then
        pxxstr = ' '
        moroutput%percentiles = .true.
    else
        moroutput%percentiles = .true.
    end if
       
end subroutine set_sediment_percentiles

!> process string
subroutine read_morphology_process_string(pxxstr, nxxuser, max_nuserfrac, rfield, filmor, lundia, error)
    use precision
    use properties
    use string_module
    use message_module
    
    implicit none
    
    character(256)                                  , intent(in)     :: pxxstr
    integer                                         , intent(inout)  :: nxxuser
    integer                                         , intent(in)     :: max_nuserfrac
    real(fp)            , dimension(max_nuserfrac)  , intent(inout)  :: rfield
    character(len=*)                                , intent(in)     :: filmor
    integer                                         , intent(in)     :: lundia  !<  Description and declaration in inout.igs
    logical                                         , intent(out)    :: error
        
    integer             , dimension(max_nuserfrac)                   :: itype
    integer             , dimension(max_nuserfrac)                   :: ifield
    character(10)       , dimension(max_nuserfrac)                   :: cfield
    integer             , dimension(max_nuserfrac)                   :: lenchr
    character(256)                                                   :: errmsg
    integer                                                          :: i 
        
    call scannr( pxxstr,       1,      len(pxxstr), nxxuser,   itype, &
                 &  ifield,  rfield,  cfield,  lenchr,  max_nuserfrac,  .true., &
                 & .false., .false.)
    if (nxxuser < 0) then
        errmsg = 'Cannot interpret Percentiles string in '//trim(filmor)
        call write_error(errmsg, unit=lundia)
        error = .true.
        return
    end if
    do i = 1, nxxuser
        if (itype(i) == 1) then
            rfield(i) = ifield(i)
        elseif (itype(i) == 3) then
            errmsg = 'Cannot interpret Percentiles string in '//trim(filmor)
            call write_error(errmsg, unit=lundia)
            error = .true.
            exit
        end if
    enddo
end subroutine read_morphology_process_string

!> remove double percentiles
subroutine remove_double_percentiles(morpar, nxxuser, nxxprog, xxprog, max_nuserfrac, rfield)
    use precision
    use morphology_data_module
    implicit none
    
    type(morpar_type)                           , pointer, intent(inout)  :: morpar
    integer                                              , intent(in)     :: nxxuser
    integer                                              , intent(inout)  :: nxxprog
    real(fp)                   , dimension(nxxprog)      , intent(inout)  :: xxprog
    integer                                              , intent(in)     :: max_nuserfrac
    real(fp)                   , dimension(max_nuserfrac), intent(inout)  :: rfield


    integer :: i, j, jj

    i = 0
    do while (i < nxxuser)
       i = i + 1
       j = 0
       do while (j < nxxprog)
          j = j + 1
          if (abs(rfield(i)-xxprog(j)) < 0.01) then
             rfield(i) = xxprog(j)
             do jj = j + 1, nxxprog
                xxprog(jj-1) = xxprog(jj)
             enddo
             nxxprog = nxxprog - 1
          end if
       end do
    end do
    morpar%nxx = nxxuser + nxxprog
    
end subroutine remove_double_percentiles

subroutine copy_and_sort_percentiles(morpar, nxxuser, nxxprog, xxprog, max_nuserfrac, rfield, lundia, error)
    use precision
    use morphology_data_module
     use message_module, only: write_error
    implicit none
    
    type(morpar_type)                           , pointer, intent(inout)  :: morpar
    integer                                              , intent(in)     :: nxxuser
    integer                                              , intent(inout)  :: nxxprog
    real(fp)                   , dimension(nxxprog)      , intent(inout)  :: xxprog
    integer                                              , intent(in)     :: max_nuserfrac
    real(fp)                   , dimension(max_nuserfrac), intent(inout)  :: rfield
    integer                                              , intent(in)     :: lundia  !  Description and declaration in inout.igs
    logical                                              , intent(inout)  :: error

    integer                                                         :: i, j, jmin, ilist
    real(fp)                                                        :: xxmin
    character(256)                                                  :: errmsg
    
    do i = 1, morpar%nxx
       jmin = -1
       xxmin = 100.0_fp
       ilist = 1
       do j = 1, nxxuser
          if (rfield(j) < xxmin) then
             xxmin = rfield(j)
             jmin  = j
          end if
       end do
       !
       do j = 1, nxxprog
          if (xxprog(j) < xxmin) then
             xxmin = xxprog(j)
             jmin  = j
             ilist = 2
          end if
       end do
       !
       if (xxmin <= 0.0_fp .or. xxmin >= 100.0_fp) then
          errmsg = 'Sediment diameter percentiles should lie between 0 and 100%'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       end if
       morpar%xx(i) = xxmin / 100.0_fp
       !
       if (abs(xxmin-10.0_fp) < 0.01_fp) morpar%i10 = i
       if (abs(xxmin-15.0_fp) < 0.01_fp) morpar%i15 = i
       if (abs(xxmin-50.0_fp) < 0.01_fp) morpar%i50 = i
       if (abs(xxmin-90.0_fp) < 0.01_fp) morpar%i90 = i
       !
       if (ilist == 1) then
          rfield(jmin) = 200.0_fp
       else ! ilist==2
          xxprog(jmin) = 200.0_fp
       end if
   end do

end subroutine copy_and_sort_percentiles

!> Reads morphology input version 0 (or no version number found)
subroutine rdmor0(ilun      ,morfac    ,tmor      ,thresh    ,morupd    , &
                & eqmbc     ,densin    ,aksfac    ,rwave     ,rouse     , &
                & alfabs    ,alfabn    ,sus       ,bed       ,susw      , &
                & bedw      ,sedthr    ,thetsd    ,hmaxth    ,fwfac     )
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Arguments
!
    integer, intent(in)  :: ilun
    logical, intent(out) :: densin !  Description and declaration in morpar.igs
    logical, intent(out) :: eqmbc !  Description and declaration in morpar.igs
    logical, intent(out) :: morupd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: aksfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabn !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabs !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bed !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bedw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: fwfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: hmaxth !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: morfac !  Description and declaration in morpar.igs
    logical, intent(out)    :: rouse !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: rwave !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sedthr !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sus !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: susw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thetsd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thresh !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: tmor !  Description and declaration in morpar.igs
!
!
!! executable statements -------------------------------------------------------
!
    ! morphological timescale factor
    read (ilun, *) morfac
    ! start for calculating morphological changes
    read (ilun, *) tmor
    ! threshold value for slowing erosion near a fixed layer (m)
    read (ilun, *) thresh
    ! flag for doing bottom updates
    read (ilun, *) morupd
    ! flag for setting equilibrium sediment concentration profiles
    ! at the open boundaries for sand sediment
    read (ilun, *) eqmbc
    ! flag for including sediment in fluid density calculations
    read (ilun, *) densin
    ! factor for setting aks height
    read (ilun, *) aksfac
    ! factor for calculating wave-related roughness from ripple dimensions
    read (ilun, *) rwave
    ! flag for setting equilibrium concentrations to standard Rouse profiles
    read (ilun, *) rouse
    ! factor for longitudinal bed load transport
    read (ilun, *) alfabs
    ! factor for transverse bed load transport
    read (ilun, *) alfabn
    ! factor for calculating suspended load transport
    read (ilun, *) sus
    ! factor for calculating bed load transport
    read (ilun, *) bed
    ! wave-related suspended sediment factor
    read (ilun, *) susw
    ! wave-related bed-load sediment factor
    read (ilun, *) bedw
    ! minimum depth for sediment calculations
    read (ilun, *) sedthr
    ! global / maximum dry cell erosion factor
    read (ilun, *) thetsd
    ! maximum depth for variable dry cell erosion factor
    read (ilun, *) hmaxth
    ! factor for adjusting intensity of energy dissipation in wave boundary layer
    read (ilun, *) fwfac
end subroutine rdmor0


!> Read  morphology input version 1
!! The first line in the file must be:
!! * version 1
subroutine rdmor1(ilun      ,morfac    ,tmor      ,thresh    ,morupd    , &
                & eqmbc     ,densin    ,aksfac    ,rwave     ,alfabs    , &
                & alfabn    ,sus       ,bed       ,susw      ,bedw      , &
                & sedthr    ,thetsd    ,hmaxth    ,fwfac     ,epspar    , &
                & iopkcw    ,rdc       ,rdw       )
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Arguments
!
    integer, intent(in)  :: ilun
    integer, intent(out) :: iopkcw
    logical, intent(out) :: densin !  Description and declaration in morpar.igs
    logical, intent(out) :: epspar
    logical, intent(out) :: eqmbc !  Description and declaration in morpar.igs
    logical, intent(out) :: morupd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: aksfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabn !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabs !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bed !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bedw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: fwfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: hmaxth !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: morfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: rdc
    real(fp), intent(out)    :: rdw
    real(fp), intent(out)    :: rwave !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sedthr !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sus !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: susw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thetsd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thresh !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: tmor !  Description and declaration in morpar.igs
!
!
! Local variables
!
    integer       :: iost
    character(78) :: string
!
!
!! executable statements -------------------------------------------------------
!
    ! morphological timescale factor
    read (ilun, *) morfac
    ! start for calculating morphological changes
    read (ilun, *) tmor
    ! threshold value for slowing erosion near a fixed layer (m)
    read (ilun, *) thresh
    ! flag for doing bottom updates
    read (ilun, *) morupd
    ! flag for setting equilibrium sediment concentration profiles
    ! at the open boundaries for sand sediment
    read (ilun, *) eqmbc
    ! flag for including sediment in fluid density calculations
    read (ilun, *) densin
    ! factor for setting aks height
    read (ilun, *) aksfac
    ! factor for calculating wave-related roughness from ripple dimensions
    read (ilun, *) rwave
    !
    ! IF Rouse skip
    !
    read (ilun, '(A)') string
    call small(string, len(string))
    if (index(string, 'rouse')==0) backspace (ilun)
    ! factor for longitudinal bed load transport
    read (ilun, *) alfabs
    ! factor for transverse bed load transport
    read (ilun, *) alfabn
    ! factor for calculating suspended load transport
    read (ilun, *) sus
    ! factor for calculating bed load transport
    read (ilun, *) bed
    ! wave-related suspended sediment factor
    read (ilun, *) susw
    ! wave-related bed-load sediment factor
    read (ilun, *) bedw
    ! minimum depth for sediment calculations
    read (ilun, *) sedthr
    ! global / maximum dry cell erosion factor
    read (ilun, *) thetsd
    ! maximum depth for variable dry cell erosion factor
    read (ilun, *) hmaxth
    ! factor for adjusting intensity of energy dissipation in wave boundary layer
    read (ilun, *) fwfac
    ! flag for parametric epsilon distribution in case of K-Eps model
    read (ilun, *) epspar
    !
    read (ilun, *, iostat = iost) iopkcw, rdc, rdw
    ! IOSTAT to prevent problems if rdc and rdw are not given
end subroutine rdmor1


!> Report morphology settings to diag file
subroutine echomor(lundia    ,error     ,lsec      ,lsedtot   ,nto       , &
                 & nambnd    ,sedpar    ,morpar    ,dtunit    ,cmpupdall , &
                 & cmpupdany )
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use table_handles
    use morphology_data_module
    use sediment_basics_module
    use message_module, only: write_error
    !
    implicit none
    !
    ! The following list of pointer parameters is used to point inside the data structures
    !
    integer                                , pointer :: subiw
    integer                                , pointer :: i10
    integer                                , pointer :: i15
    integer                                , pointer :: i50
    integer                                , pointer :: i90
    integer                                , pointer :: ihidexp
    integer                                , pointer :: iopkcw
    integer                                , pointer :: islope
    integer                                , pointer :: morfacpar
    integer                                , pointer :: morfacrec
    integer                                , pointer :: morfactable
    integer                                , pointer :: nxx
    integer                                , pointer :: nmudfrac
    integer                , dimension(:)  , pointer :: sedtyp
    integer                , dimension(:)  , pointer :: tratyp
    real(fp)                               , pointer :: morfac
    real(fp)                               , pointer :: thresh
    real(fp)                               , pointer :: aksfac
    real(fp)                               , pointer :: rwave
    real(fp)                               , pointer :: alfabs
    real(fp)                               , pointer :: alfabn
    real(fp)                               , pointer :: camax
    real(fp)                               , pointer :: dzmax
    real(fp)                               , pointer :: sus
    real(fp)                               , pointer :: bed
    real(fp)                               , pointer :: ti_sedtrans
    real(fp)                               , pointer :: tmor
    real(fp)                               , pointer :: tcmp
    real(fp)              , dimension(:)   , pointer :: thetsd
    real(fp)                               , pointer :: thetsduni
    real(fp)                               , pointer :: susw
    real(fp)                               , pointer :: sedthr
    real(fp)                               , pointer :: hmaxth
    real(fp)                               , pointer :: bedw
    real(fp)                               , pointer :: rdc
    real(fp)                               , pointer :: rdw
    real(fp)                               , pointer :: espir
    real(fp)                               , pointer :: ashld
    real(fp)                               , pointer :: bshld
    real(fp)                               , pointer :: cshld
    real(fp)                               , pointer :: dshld
    real(fp)                               , pointer :: coulfri
    real(fp)                               , pointer :: flfdrat
    real(fp)                               , pointer :: alfpa
    real(fp)                               , pointer :: thcrpa
    real(fp)                               , pointer :: asklhe
    real(fp)                               , pointer :: mwwjhe
    real(fp)                               , pointer :: pangle
    real(fp)                               , pointer :: fpco
    real(fp)                               , pointer :: factcr
    real(fp)                               , pointer :: wetslope
    real(fp)                               , pointer :: dryslope
    logical                                , pointer :: duneavalan
    real(fp)                               , pointer :: avaltime
    real(fp)                               , pointer :: hswitch
    real(fp)                               , pointer :: dzmaxdune
    real(fp)                               , pointer :: suscorfac
    real(fp)              , dimension(:)   , pointer :: xx
    logical                                , pointer :: bedupd
    logical                                , pointer :: cmpupd
    logical                                , pointer :: eqmbcsand
    logical                                , pointer :: eqmbcmud
    logical                                , pointer :: densin
    logical                                , pointer :: rouse
    logical                                , pointer :: epspar
    logical                                , pointer :: updinf
    logical                                , pointer :: neglectentrainment
    logical                                , pointer :: oldmudfrac
    logical                                , pointer :: varyingmorfac
    logical                                , pointer :: multi
    logical                                , pointer :: eulerisoglm
    logical                                , pointer :: glmisoeuler
    logical                                , pointer :: l_suscor    
    logical                                , pointer :: upwindbedload
    logical                                , pointer :: pure1d_mor
    character(256)                         , pointer :: bcmfilnam
    character(256)                         , pointer :: flsthetsd
    character(20)          , dimension(:)  , pointer :: namsed
    type(handletype)                       , pointer :: bcmfile
    type(handletype)                       , pointer :: morfacfile
    type(moroutputtype)                    , pointer :: moroutput
    type(mornumericstype)                  , pointer :: mornum
    type(bedbndtype)       , dimension(:)  , pointer :: morbnd
!
! Arguments
!
    integer                        , intent(in)  :: nto
    integer                                      :: lundia  !  Description and declaration in inout.igs
    integer                        , intent(in)  :: lsec
    integer                        , intent(in)  :: lsedtot !  Description and declaration in iidim.f90
    logical                        , intent(out) :: error
    character(20) , dimension(nto)               :: nambnd  !  Description and declaration in ckdim.f90
    character(*)                   , intent(in)  :: dtunit
    type(sedpar_type)              , pointer     :: sedpar
    type(morpar_type)              , pointer     :: morpar
    logical                        , intent(in)  :: cmpupdall !< flag indicating that bed composition will be updated for all fractions
    logical                        , intent(in)  :: cmpupdany !< flag indicating whether bed composition would be preferred for any fractions
!
! Local variables
!
    integer                                                           :: i
    integer                                                           :: ibndtyp
    integer                                                           :: istat
    integer                                                           :: j
    integer                                                           :: l
    integer                                                           :: nval
    character(20)                                                     :: parname
    character(45)                                                     :: txtput1
    character(20)                                                     :: txtput2
    character(120)                                                    :: txtput3
    character(256)                                                    :: errmsg
    character(MAXTABLECLENGTH) , dimension(:)               , pointer :: parnames
!
!! executable statements -------------------------------------------------------
!
    !
    ! Let local variables point to fields within data structures
    !
    morfac              => morpar%morfac
    thresh              => morpar%thresh
    aksfac              => morpar%aksfac
    rwave               => morpar%rwave
    alfabs              => morpar%alfabs
    alfabn              => morpar%alfabn
    camax               => morpar%camax
    dzmax               => morpar%dzmax
    sus                 => morpar%sus
    bed                 => morpar%bed
    ti_sedtrans         => morpar%ti_sedtrans
    tmor                => morpar%tmor
    tcmp                => morpar%tcmp
    thetsd              => morpar%thetsd
    susw                => morpar%susw
    sedthr              => morpar%sedthr
    hmaxth              => morpar%hmaxth
    bedw                => morpar%bedw
    rdc                 => morpar%rdc
    rdw                 => morpar%rdw
    espir               => morpar%espir
    ashld               => morpar%ashld
    bshld               => morpar%bshld
    cshld               => morpar%cshld
    dshld               => morpar%dshld
    coulfri             => morpar%coulfri
    flfdrat             => morpar%flfdrat
    alfpa               => morpar%alfpa
    thcrpa              => morpar%thcrpa
    asklhe              => morpar%asklhe
    mwwjhe              => morpar%mwwjhe
    i10                 => morpar%i10
    i15                 => morpar%i15
    i50                 => morpar%i50
    i90                 => morpar%i90
    ihidexp             => morpar%ihidexp
    iopkcw              => morpar%iopkcw
    islope              => morpar%islope
    morfacpar           => morpar%morfacpar
    morfacrec           => morpar%morfacrec
    morfactable         => morpar%morfactable
    nxx                 => morpar%nxx
    bcmfile             => morpar%bcmfile
    morfacfile          => morpar%morfacfile
    moroutput           => morpar%moroutput
    mornum              => morpar%mornum
    morbnd              => morpar%morbnd
    xx                  => morpar%xx
    bedupd              => morpar%bedupd
    cmpupd              => morpar%cmpupd
    eqmbcsand           => morpar%eqmbcsand
    eqmbcmud            => morpar%eqmbcmud
    densin              => morpar%densin
    rouse               => morpar%rouse
    epspar              => morpar%epspar
    updinf              => morpar%updinf
    neglectentrainment  => morpar%neglectentrainment
    oldmudfrac          => morpar%oldmudfrac
    varyingmorfac       => morpar%varyingmorfac
    multi               => morpar%multi
    bcmfilnam           => morpar%bcmfilnam
    nmudfrac            => sedpar%nmudfrac
    namsed              => sedpar%namsed
    sedtyp              => sedpar%sedtyp
    tratyp              => sedpar%tratyp
    pangle              => morpar%pangle
    fpco                => morpar%fpco
    factcr              => morpar%factcr
    wetslope            => morpar%wetslope
    dryslope            => morpar%dryslope
    duneavalan          => morpar%duneavalan
    hswitch             => morpar%hswitch
    dzmaxdune           => morpar%dzmaxdune
    avaltime            => morpar%avaltime
    subiw               => morpar%subiw
    eulerisoglm         => morpar%eulerisoglm
    glmisoeuler         => morpar%glmisoeuler
    l_suscor            => morpar%l_suscor
    flsthetsd           => morpar%flsthetsd
    thetsduni           => morpar%thetsduni
    suscorfac           => morpar%suscorfac
    upwindbedload       => mornum%upwindbedload
    pure1d_mor          => mornum%pure1d
    !
    ! output values to file
    !
    write (lundia, '(a)' ) '*** Start  of morphological input'
    txtput3 = 'Sediment transport computation starts after ('//trim(dtunit)//')'
    write (lundia, '(2a,e12.4)') txtput3(1:53), ':', ti_sedtrans
    !write (lundia, '(2a)') '    Morphology File Version: ', trim(versionstring)
    txtput1 = 'Morphological Timescale Factor'
    if (varyingmorfac) then
       write (lundia, '(3a)') txtput1, ': ', trim(getfilename(morfacfile))
       call gettable(morfacfile, ' ' , 'MorFac'  , &
                   & morfactable, morfacpar , nval, 1, errmsg    )
       if (errmsg /= ' ') then
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       end if
       if (nval /= 1) then
          write(errmsg,'(i3,3a)') nval, ' MorFac parameters specified in file ', &
                                & trim(getfilename(morfacfile)), 'instead of 1.'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       end if
       call checktable(morfacfile , morfactable    , &
                         & morfacpar  , 1              , &
                         & CHKTAB_POSITIVE+CHKTAB_BLOCK, &
                     & errmsg     )
       if (errmsg /= ' ') then
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       end if
       morfacrec = 1
    else
       write (lundia, '(2a,e20.4)') txtput1, ':', morfac
    end if
    !
    if (cmpupd .and. .not.cmpupdany) then
       txtput1 = 'Bed level updating  '
       if (bedupd) write (lundia, '(3a)') txtput1, ':', ' deactivated by CmpUpd = FALSE for all fractions.'
       txtput1 = 'Composition updating'
       write (lundia, '(3a)') txtput1, ':', ' deactivated by CmpUpd = FALSE for all fractions.'
       !
       bedupd = .false.
       cmpupd = .false.
    endif
    !
    txtput1 = 'Bed level updating  '
    if (bedupd) then
       txtput2 = '              ACTIVE'
    else
       txtput2 = '            INACTIVE'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2
    if (bedupd) then
       txtput1 = 'Bed level updates start after ('//trim(dtunit)//')'
       write (lundia, '(2a,e20.4)') txtput1, ':', tmor
    end if
    !
    txtput1 = 'Composition updating'
    if (cmpupd .and. .not.cmpupdany) then
       write (lundia, '(3a)') txtput1, ':', ' deactivated for all fracions.'
       cmpupd = .false.
    endif
    if (cmpupd .and. cmpupdall) then
       txtput2 = '              ACTIVE'
    elseif (cmpupd) then
       txtput2 = '  FRACTION DEPENDENT'
    else
       txtput2 = '            INACTIVE'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2
    if (cmpupd) then
       txtput1 = 'Composition updates start after ('//trim(dtunit)//')'
       write (lundia, '(2a,e20.4)') txtput1, ':', tcmp    
    end if
    !
    txtput1 = 'Fixed Layer Erosion Threshold'
    write (lundia, '(2a,e20.4)') txtput1, ':', thresh
    txtput1 = 'Entrainment/deposition flux in mass bal.'
    if (neglectentrainment) then
       txtput2 = '           NEGLECTED'
    else
       txtput2 = '            INCLUDED'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    txtput1 = 'Sand Equili. conc. profs. at boundaries'
    if (eqmbcsand) then
       txtput2 = '                USED'
    else
       txtput2 = '            NOT USED'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    txtput1 = 'Mud  Equili. conc. profs. at boundaries'
    if (eqmbcmud) then
       txtput2 = '                USED'
    else
       txtput2 = '            NOT USED'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    txtput1 = 'Sediment included in fluid density calc.'
    if (densin) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2
    txtput1 = 'AKSFAC'
    write (lundia, '(2a,e20.4)') txtput1, ':', aksfac
    txtput1 = 'RWAVE'
    write (lundia, '(2a,e20.4)') txtput1, ':', rwave
    txtput1 = 'Equilibrium sed. conc. profiles'
    if (rouse) then
       txtput2 = '      Rouse profiles'
    else
       txtput2 = 'calculated (D3D mix)'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2
    txtput1 = 'Suspended sed. multiplication factor'
    write (lundia, '(2a,e20.4)') txtput1, ':', sus
    txtput1 = 'Bed load transp. multiplication factor'
    write (lundia, '(2a,e20.4)') txtput1, ':', bed
    txtput1 = 'wave-rel. susp.sed.transp.fact.(SUSW)'
    write (lundia, '(2a,e20.4)') txtput1, ':', susw
    txtput1 = 'wave-rel. bed-ld.sed.transp.fact.(BEDW)'
    write (lundia, '(2a,e20.4)') txtput1, ':', bedw
    txtput1 = 'Min.depth for sed. calculations(SEDTHR)'
    write (lundia, '(2a,e20.4)') txtput1, ':', sedthr
    if (flsthetsd /= ' ') then
       txtput1 = 'File dry cell erosion fact(THETSD)'
       write (lundia, '(3a)') txtput1, ':  ', trim(flsthetsd)
    else
       txtput1 = 'Uniform dry cell erosion fact(THETSD)'
       write (lundia, '(2a,e12.4)') txtput1, ':', thetsduni
    end if
    
    txtput1 = 'Max depth for variable THETSD (HMAXTH)'
    write (lundia, '(2a,e20.4)') txtput1, ':', hmaxth
    if (hmaxth<=sedthr) then
       txtput1 = 'CONSTANT THETSD for dry bank erosion'
    else
       txtput1 = 'Computing THETSD for dry bank erosion'
    end if
    write (lundia, '(a)') txtput1
    txtput1 = 'Tuning param. Shields Taucr (FACTCR)'
    write (lundia, '(2a,e20.4)') txtput1, ':', factcr
    txtput3 = 'Eulerian velocities i.s.o GLM velocities for' //       &
             & ' suspended transports'
    if (eulerisoglm) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    end if
    write (lundia, '(3a)') txtput3(1:82), ':', txtput2
    txtput3 = 'GLM velocities i.s.o Eulerian velocities for' //       &
             & ' bed load transport and reference concentrations'
    if (glmisoeuler) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    end if
    write (lundia, '(3a)') txtput3(1:82), ':', txtput2 
    txtput3 = 'Correct 3D suspended load for doublecounting' //       &
             & ' below the reference height aks (SUSCOR)'
    if (l_suscor) then
      if (suscorfac >= 1.0_fp) then
       txtput2 = '                 YES'
    else
          txtput2 = '          YES (___%)'
          write(txtput2(16:18),'(I3)') int(suscorfac * 100.0_fp)
       end if
    else
       txtput2 = '                  NO'
    end if
    write (lundia, '(3a)') txtput3(1:82), ':', txtput2 
    txtput3 = 'EPSPAR: Always use Van Rijns param. mix. dist.'
    if (epspar) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    end if
    write (lundia, '(3a)') txtput3(1:47), ':', txtput2
    if (iopkcw == 1) then
       txtput3 = 'Standard option: Rc from Flow, Rw=RWAVE*0.025'
       write (lundia, '(2a,i20)') txtput3(1:46), ':', iopkcw
    else
       txtput1 = 'Constant Rc and Rw Prescribed'
       write (lundia, '(2a,2e20.4)') txtput1, ':', rdc, rdw
    end if
    !
    txtput1 = 'Update bed level at inflow boundaries'
    if (updinf) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    txtput1 = 'Merge bottoms from parallel runs     '
    if (multi) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    txtput1 = 'Source/sink limiter DZMAX(depth frac.)'
    write (lundia, '(2a,e20.4)') txtput1, ':', dzmax
    !
    txtput1 = 'Max. volumetric ref. conc. CAMAX'
    write (lundia, '(2a,e20.4)') txtput1, ':', camax
    !
    txtput1 = 'Hiding & exposure formulation'
    select case(ihidexp)
    case(1)
       txtput2 = '                None'
    case(2)
       txtput2 = '          Egiazaroff'
    case(3)
       txtput2 = '    Ashida & Michiue'
    case(4)
       txtput2 = '       Parker et.al.'
    case(5)
       txtput2 = '      Wu, Wang & Jia'
    case default
       txtput2 = '                   ?'
    endselect
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    if (lsec > 0) then
       txtput1 = 'Spiral flow factor'
       write (lundia, '(2a,e20.4)') txtput1, ':', espir
    end if
    !
    txtput1 = 'Bed slope effect formulation'
    select case(islope)
    case(1)
       txtput2 = '                None'
    case(2)
       txtput2 = '             Bagnold'
    case(3)
       txtput2 = '     Koch & Flokstra'
    case(4)
       txtput2 = '    Parker & Andrews'
    case default
       txtput2 = '                   ?'
    endselect
    write (lundia, '(3a)') txtput1, ':', txtput2
    select case(islope)
    case(2)
       txtput1 = 'ALFABS'
       write (lundia, '(2a,e20.4)') txtput1, ':', alfabs
       txtput1 = 'ALFABN'
       write (lundia, '(2a,e20.4)') txtput1, ':', alfabn
    case(3)
       txtput1 = 'ALFABS'
       write (lundia, '(2a,e20.4)') txtput1, ':', alfabs
       txtput1 = 'AShield parameter'
       write (lundia, '(2a,e20.4)') txtput1, ':', ashld
       txtput1 = 'BShield parameter'
       write (lundia, '(2a,e20.4)') txtput1, ':', bshld
       txtput1 = 'CShield parameter'
       write (lundia, '(2a,e20.4)') txtput1, ':', cshld
       txtput1 = 'DShield parameter'
       write (lundia, '(2a,e20.4)') txtput1, ':', dshld
    case(4)
       txtput1 = 'ALFABS'
       write (lundia, '(2a,e20.4)') txtput1, ':', alfabs
       txtput1 = 'Coulomb friction'
       write (lundia, '(2a,e20.4)') txtput1, ':', coulfri
       txtput1 = 'Lift/drag ratio'
       write (lundia, '(2a,e20.4)') txtput1, ':', flfdrat
       txtput1 = 'Critical Shields'
       write (lundia, '(2a,e20.4)') txtput1, ':', thcrpa
    case default
    endselect
    !
    if (wetslope<9.99_fp .and. .not. duneavalan) then
       txtput1 = 'Maximum wet slope used for avalanching'
       write (lundia, '(2a,e20.4)') txtput1, ':', wetslope  
       txtput1 = 'Time scale avalanching (in seconds)'
       write (lundia, '(2a,f20.0)') txtput1, ':', avaltime
    end if
    !
    if (duneavalan) then
       txtput1 = 'Dune slope slumping mechanism activated :'
       write (lundia, '(a)') txtput1
       txtput1 = '   Maximum wet slope'
       write (lundia, '(2a,e20.4)') txtput1, ':', wetslope
       txtput1 = '   Maximum dry slope'
       write (lundia, '(2a,e20.4)') txtput1, ':', dryslope
       txtput1 = '   Max bed level change/timestep'
       write (lundia, '(2a,e20.4)') txtput1, ':', dzmaxdune
       txtput1 = '   Depth switch wetslope to dryslope'
       write (lundia, '(2a,e20.4)') txtput1, ':', hswitch
    end if
    !
    ! Numerics
    !
    txtput1 = 'Numerical parameters:'
    write (lundia, '(a)') txtput1
    txtput1 = '   Upwind scheme for bedload'
    if (upwindbedload) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2
    txtput1 = '   Pure1D for morphodynamics'
    if (pure1d_mor) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    end if
    write (lundia, '(3a)') txtput1, ':', txtput2 
    !
    if (morpar%bermslopetransport) then
       txtput1 = 'Berm slope adjustment mechanism activated'
       txtput2 = '                 YES'
       write (lundia, '(3a)') txtput1,':', txtput2
       txtput1 = 'Berm slope adjustment for bedload'
       if (morpar%bermslopebed) then
          txtput2 = '                 YES'
       else
          txtput2 = '                  NO'
       endif       
       write (lundia, '(3a)') txtput1,':', txtput2
       txtput1 = 'Berm slope adjustment for suspended load'
       if (morpar%bermslopesus) then
          txtput2 = '                 YES'
       else
          txtput2 = '                  NO'
       endif       
       write (lundia, '(3a)') txtput1,':', txtput2
       txtput1 = '   Equilibrium slope'
       write (lundia, '(2a,f20.4)') txtput1, ':', morpar%bermslope
       txtput1 = '   Transport proportionality factor'
       write (lundia, '(2a,f20.4)') txtput1, ':', morpar%bermslopefac
       txtput1 = '   Breaker index to activate cells'
       write (lundia, '(2a,f20.4)') txtput1, ':', morpar%bermslopegamma
       txtput1 = '   Depth limit to activate cells'
       write (lundia, '(2a,f20.4)') txtput1, ':', morpar%bermslopedepth 
    endif
    !
    ! User requested sediment percentiles
    !
    txtput1 = 'Requested percentile(s)'
    do i = 1, nxx
       write (lundia, '(2a,f20.4)') txtput1, ':', xx(i)*100.0
       txtput1 = ' '
    enddo
    !
    txtput1 = 'Output transport rates'
    select case(moroutput%transptype)
    case (0)
       txtput2 = '                mass'
    case (1)
       txtput2 = '  volume incl. pores'
    case (2)
       txtput2 = '  volume excl. pores'
    end select
    write (lundia, '(3a)') txtput1, ':', txtput2
    if (oldmudfrac) then
       txtput1 = 'Old mud source term calculation         '
       txtput2 = '                 YES'
       write (lundia, '(3a)') txtput1, ':', txtput2
    end if
    !
    ! errortrap in case user is using old morph.inp file
    !
    if (sus < 0.0_fp .or. bed < 0.0_fp) then
       error  = .true.
       errmsg = 'SUS or BED less than 0.0'
       call write_error(errmsg, unit=lundia)
    end if
    !
    ! errortrap THETSD
    !
    if (any(thetsd < 0.0_fp) .or. any(thetsd > 1.0_fp)) then
       error  = .true.
       errmsg = 'THETSD must be in range 0 - 1'
       call write_error(errmsg, unit=lundia)
    end if
    !
    allocate(parnames(lsedtot*2), stat = istat)
    if (istat /= 0) then
       errmsg = 'RDMOR: memory alloc error'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    end if
    parnames = ' '
    do j = 1, nto
       txtput1 = 'Boundary name'
       write (lundia, '(2a,a20)') txtput1, ':', trim(nambnd(j))
       !
       ! Display boundary conditions
       !
       txtput1 = '  Depth prescribed'
       select case(morbnd(j)%icond)
       case (0)
          txtput2 = '                free'
          parname = ' '
       case (1)
          txtput2 = '               fixed'
          parname = ' '
       case (2)
          txtput2 = '         time series'
          parname = 'depth               '
          ibndtyp = 1
          nval    = 1
       case (3)
          txtput1 = '  Depth change prescribed'
          txtput2 = '         time series'
          parname = 'depth change        '
          ibndtyp = 1
          nval    = 1
       case (4)
          txtput1 = '  Transport incl pores prescribed'
          txtput2 = '         time series'
          parname = 'transport incl pores'
          ibndtyp = 2
          nval    = lsedtot - nmudfrac
       case (5)
          txtput1 = '  Transport excl pores prescribed'
          txtput2 = '         time series'
          parname = 'transport excl pores'
          ibndtyp = 2
          nval    = lsedtot - nmudfrac
       case (6)
          txtput1 = '  Bed level prescribed'
          txtput2 = '         time series'
          parname = 'bed level           '
          ibndtyp = 1
          nval    = 1
       case (7)
          txtput1 = '  Bed level change prescribed'
          txtput2 = '         time series'
          parname = 'bed level change    '
          ibndtyp = 1
          nval    = 1
       case (8)
          txtput1 = '  Transport mass prescribed'
          txtput2 = '         time series'
          parname = 'transport mass'
          ibndtyp = 2
          nval    = lsedtot - nmudfrac
       end select
       write (lundia, '(3a)') txtput1, ':', txtput2
       !
       ! Check boundary conditions
       !
       if (parname /= ' ') then
          if (bcmfilnam /= ' ') then
             !
             ! Find entries in table
             !
             call gettable(bcmfile     ,nambnd(j) ,parname   , &
                    & morbnd(j)%ibcmt(1)   ,morbnd(j)%ibcmt(2)   , &
                & morbnd(j)%ibcmt(3)   ,1         ,errmsg    )
             if (errmsg /= ' ') then
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             end if
             morbnd(j)%ibcmt(4) = 1
             txtput1 = '  Variation along boundary'
             !
             ! Check entries in table
             !
             if (morbnd(j)%ibcmt(3) == nval) then
                !
                ! Uniform values
                !
                txtput2 = '             uniform'
                write (lundia, '(3a)') txtput1, ':', txtput2
                if (ibndtyp == 1) then
                   parnames(1) = trim(parname)
                else
                   i = 0
                   do l = 1, lsedtot
                      if (has_bedload(tratyp(l))) then
                         i = i + 1
                         parnames(i) = trim(parname) // ' ' // trim(namsed(l))
                      end if
                   enddo
                end if
                !
                call checktableparnames(bcmfile   ,parnames  , &
                    & morbnd(j)%ibcmt(1)   ,morbnd(j)%ibcmt(2)   , &
                    & morbnd(j)%ibcmt(3)   ,errmsg    )
                if (errmsg /= ' ') then
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                end if
             elseif (morbnd(j)%ibcmt(3) == nval*2) then
                !
                ! Values at "end A" and "end B"
                !
                txtput2 = '              linear'
                write (lundia, '(3a)') txtput1, ':', txtput2
                if (ibndtyp == 1) then
                   parnames(1) = trim(parname) // ' end A'
                   parnames(2) = trim(parname) // ' end B'
                else
                   i = 0
                   do l = 1, lsedtot
                      if (has_bedload(tratyp(l))) then
                         i = i + 1
                         parnames(i)      = trim(parname) // ' ' // trim(namsed(l)) // ' end A'
                         parnames(nval+i) = trim(parname) // ' ' // trim(namsed(l)) // ' end B'
                      end if
                   enddo
                end if
                !
                call checktableparnames(bcmfile   ,parnames  , &
                   & morbnd(j)%ibcmt(1)   , morbnd(j)%ibcmt(2)   , &
                   & morbnd(j)%ibcmt(3)   , errmsg    )
                if (errmsg /= ' ') then
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                end if
             else
                !
                ! Invalid number of values specified
                !
                errmsg = 'Invalid number of parameters specified for ''' // &
                   & trim(parname) // ''' at ''' // nambnd(j) // ''' in ' // &
                   & trim(bcmfilnam)
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             end if
          else
             errmsg = 'Missing input file for morphological boundary conditions'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          end if
       end if
    enddo
    !
    write (lundia, '(a)') '*** End    of morphological input'
    write (lundia, *)
    !
    deallocate(parnames)
    !
    call echoflufflyr(lundia    ,error    ,morpar%flufflyr)
end subroutine echomor


!> Read fluff layer parameters from an input file
subroutine rdflufflyr(lundia   ,error    ,filmor   ,lsed     ,mor_ptr ,flufflyr,sedpar ,griddim )
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use morphology_data_module
    use message_module, only: write_error !, write_warning, FILE_NOT_FOUND, FILE_READ_ERROR, PREMATURE_EOF
    use grid_dimens_module, only: griddimtype
    use m_depfil_stm
    !
    implicit none
!
! Global variables
!
    character(*)                                           :: filmor
    type(fluffy_type)                        , pointer     :: flufflyr
    type(tree_data)                          , pointer     :: mor_ptr
    integer                                  , intent(in)  :: lsed     ! number of suspended fractions
    integer                                                :: lundia
    logical                                  , intent(out) :: error
    type(sedpar_type)                        , pointer     :: sedpar
    type(griddimtype)             , target   , intent(in)  :: griddim
!
! Local variables
!
    integer                         , pointer :: iflufflyr
    real(fp)      , dimension(:)    , pointer :: mfluni
    real(fp)      , dimension(:,:)  , pointer :: bfluff0
    real(fp)      , dimension(:,:)  , pointer :: bfluff1
    real(fp)      , dimension(:,:)  , pointer :: depfac
    character(256), dimension(:)    , pointer :: mflfil
    !
    integer                   :: istat
    integer                   :: l
    integer                   :: nm
    integer                   :: nmlb
    integer                   :: nmub
    real(fp)                  :: rmissval
    logical                   :: ex
    type(tree_data), pointer  :: sedblock_ptr
    character(256)            :: filfluff
    character(11)             :: fmttmp       ! Format file ('formatted  ') 
    character(256)            :: errmsg
!
!! executable statements -------------------------------------------------------
!
    iflufflyr            => flufflyr%iflufflyr
    !
    error      = .false.
    rmissval   = -999.0
    fmttmp     = 'formatted'
    !
    call prop_get(mor_ptr, 'FluffLayer', 'Type', iflufflyr)
    if (iflufflyr==0) return
    !
    nmlb = griddim%nmlb
    nmub = griddim%nmub
    !
    istat = allocfluffy(flufflyr, lsed, nmlb, nmub)
    !
    if (istat/=0) then
       errmsg = 'RDFLUFFLYR: memory alloc error'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    end if
    !
    mfluni  => flufflyr%mfluni
    depfac  => flufflyr%depfac
    bfluff0 => flufflyr%bfluff0
    bfluff1 => flufflyr%bfluff1
    mflfil  => flufflyr%mflfil
    !
    mfluni = 0.0_fp
    mflfil = ' '
    do l = 1,lsed
        sedblock_ptr => sedpar%sedblock(l)
        if (sedpar%sedtyp(l) <= sedpar%max_mud_sedtyp) then
            call prop_get(sedblock_ptr, '*', 'IniFluffMass', mflfil(l))
            !
            ! Intel 7.0 crashes on an inquire statement when file = ' '
            !
            if (mflfil(l) == ' ') mflfil(l) = 'dummyname'
            inquire (file = mflfil(l), exist = ex)
            !
            if (.not.ex) then
                call prop_get(sedblock_ptr, '*', 'IniFluffMass', mfluni(l))
            end if
        end if
    enddo
    !
    if (iflufflyr==1) then
        bfluff0 = 0.0_fp
        bfluff1 = 0.0_fp
        !
        ! Burial term 1 fluff layer constant in time:
        ! uniform or spatially varying value
        !
        filfluff = ''
        call prop_get(mor_ptr, 'FluffLayer', 'BurFluff0', filfluff)
        !
        ! Intel 7.0 crashes on an inquire statement when file = ' '
        !
        if (filfluff == ' ') filfluff = 'dummyname'
        inquire (file = filfluff, exist = ex)
        !
        if (ex) then
            !
            ! read data from file
            !
            call depfil_stm(lundia    ,error     ,filfluff  ,fmttmp    , &
                          & bfluff0   ,lsed      ,1         ,griddim   ,errmsg )
            if (error) then
                call write_error(errmsg, unit=lundia)
                errmsg = 'Unable to read burial term 1 from ' // trim(filfluff)
                call write_error(errmsg, unit=lundia)
                return
            end if
            flufflyr%bfluff0_fil = filfluff
            !
            ! check input
            !
            do nm = nmlb, nmub
                if (bfluff0(1,nm) < 0.0_fp .and. bfluff0(1,nm) /= rmissval ) then
                    errmsg = 'Burial term 1 should be positive in ' // trim(filfluff)
                    call write_error(errmsg, unit=lundia)
                    return
                end if
            enddo
        else
            filfluff = ' '
            call prop_get(mor_ptr, 'FluffLayer', 'BurFluff0', bfluff0(1,1))
            if (bfluff0(1,1) < 0.0_fp) then
                errmsg = 'Burial term 1 should be positive in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
                return
            end if
            bfluff0(1,:) = bfluff0(1,1)
        end if
        do l = 2, lsed
            bfluff0(l,:) = bfluff0(1,:)
        enddo
        !
        ! Burial term 2 fluff layer constant in time:
        ! uniform or spatially varying value
        !
        filfluff = ''
        call prop_get(mor_ptr, 'FluffLayer', 'BurFluff1', filfluff)
        !
        ! Intel 7.0 crashes on an inquire statement when file = ' '
        !
        if (filfluff == ' ') filfluff = 'dummyname'
        inquire (file = filfluff, exist = ex)
        !
        if (ex) then
            !
            ! read data from file
            !
            call depfil_stm(lundia    ,error     ,filfluff  ,fmttmp    , &
                          & bfluff1   ,lsed      ,1         ,griddim   ,errmsg)
            if (error) then
                call write_error(errmsg, unit=lundia)
                errmsg = 'Unable to read burial term 2 from ' // trim(filfluff)
                call write_error(errmsg, unit=lundia)
                return
            end if
            flufflyr%bfluff1_fil = filfluff
            !
            ! check input
            !
            do nm = nmlb, nmub
                if (bfluff1(1,nm) < 0.0_fp .and. bfluff1(1,nm) /= rmissval ) then
                    errmsg = 'Burial term 2 should be positive in ' // trim(filfluff)
                    call write_error(errmsg, unit=lundia)
                    return
                end if
            enddo
        else
            filfluff = ' '
            call prop_get(mor_ptr, 'FluffLayer', 'BurFluff1', bfluff1(1,1))
            if (bfluff1(1,1) < 0.0_fp ) then
                errmsg = 'Burial term 2 should be positive in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
                return
            end if
            bfluff1(1,:) = bfluff1(1,1)
        end if
        do l = 2, lsed
            bfluff1(l,:) = bfluff1(1,:)
        enddo
        !
    elseif (iflufflyr==2) then
        depfac = 0.0_fp   
        !
        ! Deposition factor constant in time:
        ! uniform or spatially varying value
        !
        filfluff = ''
        call prop_get(mor_ptr, 'FluffLayer', 'DepFac', filfluff)
        !
        ! Intel 7.0 crashes on an inquire statement when file = ' '
        !
        if (filfluff == ' ') filfluff = 'dummyname'
        inquire (file = filfluff, exist = ex)
        !
        if (ex) then
            !
            ! read data from file
            !
            call depfil_stm(lundia    ,error     ,filfluff  ,fmttmp    , &
                          & depfac    ,lsed      ,1         ,griddim   , errmsg)
            if (error) then
                call write_error(errmsg, unit=lundia)
                errmsg = 'Unable to read deposition factor from ' // trim(filfluff)
                call write_error(errmsg, unit=lundia)
                return
            end if
            flufflyr%depfac_fil = filfluff
            !
            ! check input
            !
            do nm = nmlb, nmub
                if ((depfac(1,nm) < 0.0_fp .or. depfac(1,nm)>1.0_fp) .and. depfac(1,nm) /= rmissval  ) then
                    errmsg = 'Deposition factor should be between 0 and 1 in ' // trim(filfluff)
                    call write_error(errmsg, unit=lundia)
                    return
                end if
            enddo
        else
            filfluff = ' '
            call prop_get(mor_ptr, 'FluffLayer', 'DepFac', depfac(1,1))
            if (depfac(1,1) < 0.0_fp .or. depfac(1,1) > 1.0_fp) then
                errmsg = 'Deposition factor should be between 0 and 1 in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
                return
            end if
            depfac(1,:) = depfac(1,1)
        end if
        do l = 2, lsed
            depfac(l,:) = depfac(1,:)
        enddo 
    end if                                                                                                                                                         
end subroutine rdflufflyr


!> Report fluff layer settings to diag file
subroutine echoflufflyr(lundia    ,error    ,flufflyr)
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module
    !
    implicit none
!
! Global variables
!
    type(fluffy_type)                        , pointer     :: flufflyr
    integer                                                :: lundia
    logical                                  , intent(out) :: error
!
! Local variables
!
    integer         , pointer :: iflufflyr
    !
    character(30)             :: txtput1
!
!! executable statements -------------------------------------------------------
!
    error      = .false.
    iflufflyr            => flufflyr%iflufflyr
    if (iflufflyr==0) return
    !
    write (lundia, '(a)')   '*** Start  of fluff layer input'
    txtput1 = 'Fluff layer mechanism'
    write (lundia, '(2a,i20)') txtput1, ':', iflufflyr
    !
    if (iflufflyr==1) then
        txtput1 = 'Burial coefficient 1'
        if (flufflyr%bfluff0_fil /= ' ') then
            write(lundia,'(3a)') txtput1, ':', trim(flufflyr%bfluff0_fil)
        else
            write(lundia,'(2a,e20.4)') txtput1, ':', flufflyr%bfluff0(1,1)
        end if
        !
        txtput1 = 'Burial coefficient 2'
        if (flufflyr%bfluff1_fil /= ' ') then
            write(lundia,'(3a)') txtput1, ':', trim(flufflyr%bfluff1_fil)
        else
            write(lundia,'(2a,e20.4)') txtput1, ':', flufflyr%bfluff1(1,1)
        end if
    else if ( iflufflyr == 2 ) then
        txtput1 = 'Deposition factor'
        if (flufflyr%depfac_fil /= ' ') then
            write(lundia,'(3a)') txtput1, ':', trim(flufflyr%depfac_fil)
        else
            write(lundia,'(2a,e20.4)') txtput1, ':', flufflyr%depfac(1,1)
        end if
    end if
    !
    write (lundia, '(a)')   '*** End    of fluff layer input'
    write (lundia, *)
end subroutine echoflufflyr

end module m_rdmor
