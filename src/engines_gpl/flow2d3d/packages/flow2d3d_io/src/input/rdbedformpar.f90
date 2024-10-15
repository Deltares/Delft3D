subroutine rdbedformpar(lundia    ,error     ,nmax      ,mmax      ,nmaxus    , &
                      & nmmax     ,sedim     ,gdp)
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
  ! Read bed form parameters from MDF file
  !
  !!--pseudo code and references--------------------------------------------------
  ! NONE
  !!--declarations----------------------------------------------------------------
    use precision
    use properties
    use flow_tables
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
        
    character(256)                  , pointer :: flbdfh
    character(256)                  , pointer :: flnmD50
    character(256)                  , pointer :: flnmD90
    logical                         , pointer :: lfbedfrm
    logical                         , pointer :: lfbedfrmout
    logical                         , pointer :: lfbedfrmrou
    logical                         , pointer :: lfbedfrmCFL
    logical                         , pointer :: lfbedfrmADV
    logical                         , pointer :: lfbdfmor
    logical                         , pointer :: spatial_bedform
    integer                         , pointer :: bedformheighttype
    integer                         , pointer :: bedformlengthtype
    integer                         , pointer :: bdfrpt
    integer                         , pointer :: bdfrlxtype
    real(fp)                        , pointer :: bdfC_Hn
    real(fp)                        , pointer :: bdfC_Hp
    real(fp)                        , pointer :: bdfGmin
    real(fp)                        , pointer :: bdfHmax
    real(fp)                        , pointer :: bdfL_Hc
    real(fp)                        , pointer :: bdfL_Hp
    real(fp)                        , pointer :: bdfPmax
    real(fp)      , dimension(:)    , pointer :: bedformD50
    real(fp)      , dimension(:)    , pointer :: bedformD90
    real(fp)                        , pointer :: bedformL_H
    real(fp)                        , pointer :: bedformT_H
    real(fp)                        , pointer :: bdfuni
    real(fp)                        , pointer :: thetacdune
    real(fp)      , dimension(:)    , pointer :: hdpar
    real(fp)      , dimension(:)    , pointer :: ldpar
    real(fp)      , dimension(:)    , pointer :: cdpar
    real(fp)      , dimension(:)    , pointer :: kdpar
    real(fp)      , dimension(:)    , pointer :: duneheight
    real(fp)      , dimension(:)    , pointer :: duneheightequi
    real(fp)      , dimension(:)    , pointer :: dunelength
    real(fp)      , dimension(:)    , pointer :: qbedformx
    real(fp)      , dimension(:)    , pointer :: qbedformy
    real(fp)      , dimension(:)    , pointer :: ubedform
    real(fp)      , dimension(:)    , pointer :: rksr
    real(fp)      , dimension(:)    , pointer :: rksmr
    real(fp)      , dimension(:)    , pointer :: rksd
    real(fp)                        , pointer :: dt
    real(fp)                        , pointer :: tunit
!
! Global variables
!
    integer                                       , intent(in)  :: lundia !  Description and declaration in inout.igs
    integer                                       , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                       , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                       , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                       , intent(in)  :: nmmax  !  Description and declaration in esm_alloc_int.f90
    logical                                       , intent(out) :: error  !  Flag=TRUE if an error is encountered
    logical                                       , intent(in)  :: sedim  !  Flag=TRUE if sediment process is active
!
! Local variables
!
    character(6)      :: keyw
    character(20)     :: ctemp
    character(60)     :: txtput2
    character(40)     :: txtput1 
    real(fp)          :: rdum    !< value used for initialization of array
    real(fp)          :: rtemp
    logical           :: bdfhfile_exists
    logical           :: hdread
    logical           :: ldread
    logical           :: success
    logical           :: successD50
    logical           :: successD90
    logical           :: existD50
    logical           :: existD90
    integer           :: istat
    integer           :: i
    character(11)     :: fmttmp ! Format file ('formatted  ') 
!
!! executable statements -------------------------------------------------------
!
    bdfC_Hn                 => gdp%gdbedformpar%bdfC_Hn
    bdfC_Hp                 => gdp%gdbedformpar%bdfC_Hp
    bdfGmin                 => gdp%gdbedformpar%bdfGmin
    bdfHmax                 => gdp%gdbedformpar%bdfHmax
    bdfL_Hc                 => gdp%gdbedformpar%bdfL_Hc
    bdfL_Hp                 => gdp%gdbedformpar%bdfL_Hp
    bdfPmax                 => gdp%gdbedformpar%bdfPmax
    bdfrpt                  => gdp%gdbedformpar%bdfrpt
    bdfrlxtype              => gdp%gdbedformpar%bdfrlxtype
    bdfuni                  => gdp%gdbedformpar%bdfuni
    spatial_bedform         => gdp%gdbedformpar%spatial_bedform
    flnmD50                 => gdp%gdbedformpar%flnmD50
    flnmD90                 => gdp%gdbedformpar%flnmD90
    bedformheighttype       => gdp%gdbedformpar%bedformheighttype
    bedformlengthtype       => gdp%gdbedformpar%bedformlengthtype
    bedformL_H              => gdp%gdbedformpar%bedformL_H
    bedformT_H              => gdp%gdbedformpar%bedformT_H 
    flbdfh                  => gdp%gdbedformpar%flbdfh
    lfbedfrm                => gdp%gdbedformpar%lfbedfrm
    lfbedfrmout             => gdp%gdbedformpar%lfbedfrmout
    lfbedfrmrou             => gdp%gdbedformpar%lfbedfrmrou
    lfbedfrmCFL             => gdp%gdbedformpar%lfbedfrmCFL
    lfbedfrmADV             => gdp%gdbedformpar%lfbedfrmADV
    lfbdfmor                => gdp%gdbedformpar%lfbdfmor
    thetacdune              => gdp%gdbedformpar%thetacdune
    dt                      => gdp%gdexttim%dt
    tunit                   => gdp%gdexttim%tunit
    !
    ! Read Bedform sediment diameter
    !
    if (.not.sedim) then
       call prop_get(gdp%mdfile_ptr, '*', 'BdfD50', flnmD50, successD50)
       call prop_get(gdp%mdfile_ptr, '*', 'BdfD90', flnmD90, successD90)
       !
       ! check if D50 or D90 is spatial varying; if only one is given, both are treated so.
       !
       if (.not. successD50 .and. .not. successD90) then
          spatial_bedform = .false.
          existD50 = .false.
          existD90 = .false.
       else
          if (successD50) then
             inquire (file = trim(flnmD50), exist = existD50)
          else
             existD50 = .false.
          endif
          if (successD90) then
             inquire (file = trim(flnmD90), exist = existD90)
          else
             existD90 = .false.
          endif
          spatial_bedform = (existD50 .or. existD90)
       endif
       if (spatial_bedform) then
                        allocate(gdp%gdbedformpar%bedformD50(gdp%d%nmlb:gdp%d%nmub), stat = istat)
          if (istat==0) allocate(gdp%gdbedformpar%bedformD90(gdp%d%nmlb:gdp%d%nmub), stat = istat)
       else
                        allocate(gdp%gdbedformpar%bedformD50(1), stat = istat)
          if (istat==0) allocate(gdp%gdbedformpar%bedformD90(1), stat = istat)
       endif
       if (istat /= 0) then
          call prterr(lundia, 'G020', 'bedformD50/90')
          call d3stop(1, gdp)
       endif
       bedformD50 => gdp%gdbedformpar%bedformD50
       bedformD90 => gdp%gdbedformpar%bedformD90
       !
       ! default values:
       !
       bedformD50 = 0.0002_fp
       bedformD90 = 0.0003_fp

       if (successD50) then
          if (.not. existD50) then
             call prop_get(gdp%mdfile_ptr,'*', 'BdfD50', bedformD50(1), success)
             if (.not. success) then
                call prterr(lundia, 'P004', &
                   'Error in rdbedformpar: ' // trim(flnmD50) // ' is not a file and not a value.')
                call d3stop(1, gdp)
             endif
             if (spatial_bedform) then
                bedformD50(:) = bedformD50(1)
             endif
          else
             fmttmp = 'formatted'
             call depfil(lundia    ,error     ,flnmD50   ,fmttmp    , &
                       & bedformD50,1         ,1         ,gdp%griddim)
             if (error) then
                call prterr(lundia, 'P004', 'Error while reading bedformD50 from file ' // trim(flnmD50))
                call d3stop(1, gdp)
             endif
          endif
       endif
       if (successD90) then
          if (.not. existD90) then
             call prop_get(gdp%mdfile_ptr,'*', 'BdfD90', bedformD90(1), success)
             if (.not. success) then
                call prterr(lundia, 'P004', &
                   'Error in rdbedformpar: ' // trim(flnmD90) // ' is not a file and not a value.')
                call d3stop(1, gdp)
             endif
             if (spatial_bedform) then
                bedformD90(:) = bedformD90(1)
             endif
          else
             fmttmp = 'formatted'
             call depfil(lundia    ,error     ,flnmD90   ,fmttmp    , &
                       & bedformD90,1         ,1         ,gdp%griddim)
             if (error) then
                call prterr(lundia, 'P004', 'Error while reading bedformD90 from file ' // trim(flnmD50))
                call d3stop(1, gdp)
             endif
          endif
       else
          ! The do loop is needed (instead of an array operation), because otherwise
          ! the full array is copied to the stack. This goes wrong with big models
          ! 
          if (spatial_bedform) then
             do i = gdp%d%nmlb, gdp%d%nmub
                bedformD90(i) = 1.5_fp * bedformD50(i)
             enddo
          else
             bedformD90(1) = 1.5_fp * bedformD50(1)
          endif 
       endif
    endif
    !
    call prop_get(gdp%mdfile_ptr,'*','Bdf',lfbedfrm)
    !
    call prop_get(gdp%mdfile_ptr,'*','BdfOut',lfbedfrmout)
    !
    !-----------------------------------------------------
    ! Allocation of memory for bedform roughness arrays
    !
    istat = 0
    if (istat==0) allocate (gdp%gdbedformpar%kdpar(6)                             , stat = istat)
    if (istat==0) allocate (gdp%gdbedformpar%rksr(gdp%d%nmlb:gdp%d%nmub)          , stat = istat)
    if (istat==0) allocate (gdp%gdbedformpar%rksmr(gdp%d%nmlb:gdp%d%nmub)         , stat = istat)
    if (istat==0) allocate (gdp%gdbedformpar%rksd(gdp%d%nmlb:gdp%d%nmub)          , stat = istat)
    !
    if (istat/=0) then
       call prterr(lundia, 'U021', 'RDBEDFORMPAR: memory alloc error [1]')
       call d3stop(1, gdp)
    endif
    !
    kdpar                   => gdp%gdbedformpar%kdpar
    rksr                    => gdp%gdbedformpar%rksr
    rksmr                   => gdp%gdbedformpar%rksmr
    rksd                    => gdp%gdbedformpar%rksd
    !
    kdpar = 0.0_fp
    rksr  = 0.01_fp
    rksmr = 0.0_fp
    rksd  = 0.0_fp
    !
    ! if Bdf keyword turned out to be NO,
    ! then try to read only Van Rijn 2004 bedform roughness height parameters.
    !
    if (.not.lfbedfrm) goto 8888
    !
    ! Allocation of memory for other arrays
    !
    istat = 0
    if (istat==0) allocate (gdp%gdbedformpar%duneheight(gdp%d%nmlb:gdp%d%nmub)    , stat = istat)
    if (istat==0) allocate (gdp%gdbedformpar%duneheightequi(gdp%d%nmlb:gdp%d%nmub), stat = istat)
    if (istat==0) allocate (gdp%gdbedformpar%dunelength(gdp%d%nmlb:gdp%d%nmub)    , stat = istat)
    if (istat==0) allocate (gdp%gdbedformpar%qbedformx(gdp%d%nmlb:gdp%d%nmub)     , stat = istat)
    if (istat==0) allocate (gdp%gdbedformpar%qbedformy(gdp%d%nmlb:gdp%d%nmub)     , stat = istat) 
    if (istat==0) allocate (gdp%gdbedformpar%ubedform(gdp%d%nmlb:gdp%d%nmub)      , stat = istat)
    !
    if (istat==0) allocate (gdp%gdbedformpar%hdpar(2)                             , stat = istat)
    if (istat==0) allocate (gdp%gdbedformpar%ldpar(2)                             , stat = istat)
    if (istat==0) allocate (gdp%gdbedformpar%cdpar(2)                             , stat = istat) 
    !
    if (istat/=0) then
       call prterr(lundia, 'U021', 'RDBEDFORMPAR: memory alloc error [2]')
       call d3stop(1, gdp)
    endif
    !
    duneheight              => gdp%gdbedformpar%duneheight
    duneheightequi          => gdp%gdbedformpar%duneheightequi
    dunelength              => gdp%gdbedformpar%dunelength
    qbedformx               => gdp%gdbedformpar%qbedformx
    qbedformy               => gdp%gdbedformpar%qbedformy
    ubedform                => gdp%gdbedformpar%ubedform
    !
    cdpar                   => gdp%gdbedformpar%cdpar
    hdpar                   => gdp%gdbedformpar%hdpar
    ldpar                   => gdp%gdbedformpar%ldpar
    !
    duneheight        = 0.0_fp
    duneheightequi    = 0.0_fp
    dunelength        = 0.0_fp
    qbedformx         = 0.0_fp
    qbedformy         = 0.0_fp
    ubedform          = 0.0_fp
    !
    hdpar = 0.0_fp
    ldpar = 0.0_fp
    cdpar = 0.0_fp
    !-----------------------------------------------------
    !
    write (lundia, '(a)') '*** Start of bedform input'
    !
    ! If BdfMor then the morphological time scale is used for bedform adaptation.
    ! By default the hydrodynamic time scale is used for bedform adaptation.
    !
    if (sedim) then
       call prop_get(gdp%mdfile_ptr,'*','BdfMor',lfbdfmor)
       !
       txtput1 = 'Bedform adaptation based on'
       if (lfbdfmor) then
          write(lundia, '(a,a)') txtput1, ': morphologic time scale'
       else 
          write(lundia, '(a,a)') txtput1, ': hydrodynamic time scale'
       endif
    else
       write(lundia,'(a)') 'Sediment not included in simulation'
       txtput1 = 'Using characteristic sediment diameters'
       write(lundia,'(a,a)') txtput1, ':'
       txtput1 = '  D50'
       write(lundia,'(a,a,e20.4)') txtput1, ':', bedformD50
       txtput1 = '  D90'
       write(lundia,'(a,a,e20.4)') txtput1, ':', bedformD90
    endif
    !
    !---------------------------
    ! Reading choice for Bedform height
    !
    ctemp = ''
    call prop_get(gdp%mdfile_ptr,'*','BdfH',ctemp)
    call small(ctemp     ,20         )
    select case( ctemp )
    case ('vanrijn84') 
       bedformheighttype = 1
       txtput2 = 'Van Rijn (1984)'
    case ('fredsoempm') 
       bedformheighttype = 2
       txtput2 = 'Fredsoe (1982) for MPM (1948)'
    case ('fredsoeeh') 
       bedformheighttype = 3
       txtput2 = 'Fredsoe (1982) for EH (1967)'
    case ('powerrelation')
       bedformheighttype = 4
       txtput2 = 'Power relation aH*(H^bH)'
    case default
       bedformheighttype = 1
       txtput2 = 'Van Rijn (1984)'
    end select
    txtput1 = 'Dune height predictor'
    write(lundia,'(a,a,a)') txtput1, ': ', txtput2
    !
    select case (bedformheighttype)
    case (1:3)
       hdpar(1) = 1.0_fp !Epsilon parameter in FredsoeMPM and FredsoeEH
                         !Constant between 0.5 and 1.5 (APPENDIX A of report Q4190)
       call prop_get(gdp%mdfile_ptr,'*','BdfEps',hdpar(1))
       !
       txtput1 = '  eps'
       write(lundia,'(a,a,e20.4)') txtput1, ':', hdpar(1)
       !
       if (bedformheighttype<=2) then
          call prop_get(gdp%mdfile_ptr,'*','BdfThC',thetacdune)
          if (thetacdune<0.0) then
             write(lundia,'(a)') '  Critical shear stress for dunes calculated from shields curve'
          else
             txtput1 = '  Critical shear stress for dunes'
             write(lundia,'(a,a,e20.4)') txtput1,':', thetacdune
          endif
       endif
    case (4)
       hdpar(1) = 0.0_fp
       hdpar(2) = 1.0_fp
       !
       call prop_get(gdp%mdfile_ptr,'*','BdfaH',hdpar(1))
       call prop_get(gdp%mdfile_ptr,'*','BdfbH',hdpar(2))
       if (hdpar(1) <= 0.0) then
          call prterr(lundia, 'U190', 'Dune height coefficients in .mdf -> Dune height <= 0.0')
       endif
       !
       txtput1 = '  aH'
       write(lundia,'(a,a,e20.4)') txtput1, ':', hdpar(1)
       txtput1 = '  bH'
       write(lundia,'(a,a,e20.4)') txtput1, ':', hdpar(2)
    end select
    !
    !---------------------------
    ! Reading choice for Bedform relaxation
    !
    ctemp = ''
    call prop_get(gdp%mdfile_ptr,'*','BdfRlx',ctemp)
    call small(ctemp     ,20         )
    select case( ctemp )
    case ('thconst') 
       bdfrlxtype  = 1
       txtput2 = 'Constant T_H'
    case ('lhconst') 
       bdfrlxtype  = 2
       txtput2 = 'T_H = L_H / C_H'
    case ('lhfactor') 
       bdfrlxtype  = 3
       txtput2 = 'T_H = L_H / C_H'
    case ('lhchhmax')
       bdfrlxtype  = 4
       txtput2 = 'T_H = L_H / C_H'
    case default
       bdfrlxtype  = 0
       txtput2 = 'None (equilibrium)'
    end select
    txtput1 = 'Dune height relaxation'
    write(lundia,'(a,a,a)') txtput1, ': ', txtput2
    !
    select case (bdfrlxtype)
    case (1)
       rtemp  = 0.0_fp
       call prop_get(gdp%mdfile_ptr,'*','BdfT_H',rtemp) !Read adaptation time scale
       bedformT_H = rtemp*tunit
       !
       txtput1 = '  T_H (s)'
       write(lundia,'(a,a,e20.4)') txtput1, ':', bedformT_H
    case (2) 
       call prop_get(gdp%mdfile_ptr,'*','BdfL_H',bedformL_H) !Read adaptation length scale
       !
       txtput1 = '  L_H (m)'
       write(lundia,'(a,a,e20.4)') txtput1, ':', bedformL_H
    case (3) 
       txtput1 = 'Length scale L_H'
       txtput2 = 'LHc*Ld'
       write(lundia,'(a,a,a)') txtput1, ': ', txtput2
       !
       call prop_get(gdp%mdfile_ptr,'*','BdfLHc',bdfL_Hc)    !Read adaptation length scale factor
       !
       txtput1 = '  LHc (-)'
       write(lundia,'(a,a,e20.4)') txtput1, ':', bdfL_Hc
    case (4) 
       txtput1 = 'Length scale L_H'
       txtput2 = 'min[(H_max/H)^LHp,phi_max]*Ld'
       write(lundia,'(a,a,a)') txtput1, ': ', txtput2
       !
       call prop_get(gdp%mdfile_ptr,'*','BdfHMx',bdfHmax)    !Read maximum water depth
       call prop_get(gdp%mdfile_ptr,'*','BdfLHp',bdfL_Hp)    !Read length scale phi power
       call prop_get(gdp%mdfile_ptr,'*','BdfPMx',bdfPmax)    !Read maximum phi
       !
       txtput1 = '  H_max'
       write(lundia,'(a,a,e20.4)') txtput1, ':', bdfHmax
       txtput1 = '  LHp'
       write(lundia,'(a,a,e20.4)') txtput1, ':', bdfL_Hp
       txtput1 = '  phi_max'
       write(lundia,'(a,a,e20.4)') txtput1, ':', bdfPmax
    end select
    !
    !---------------------------
    ! Reading choice for Bedform advection
    !
    if (bdfRlxtype > 0) then
       call prop_get(gdp%mdfile_ptr,'*','BdfADV',lfbedfrmADV)
       !
       txtput1 = 'Dune height advection'
       txtput2 = '                  NO'
       if (lfbedfrmADV) txtput2 = '                 YES' 
       write(lundia,'(a,a,a)') txtput1, ': ', txtput2
       !
       if (lfbedfrmADV) then
          call prop_get(gdp%mdfile_ptr,'*','BdfCFL',lfbedfrmCFL)
          !
          txtput1 = 'CFL check'
          txtput2 = '                  NO'
          if (lfbedfrmCFL) txtput2 = '                 YES' 
          write(lundia,'(a,a,a)') txtput1, ': ', txtput2
       endif
       !
       if (lfbedfrmADV .or. bdfRlxtype>1) then
          select case (bdfRlxtype)
          case (1:3)
             txtput1 = 'Celerity C_H'
             txtput2 = 'Power Relation aC*(U^bC)'
             write(lundia,'(a,a,a)') txtput1, ': ', txtput2
             !
             cdpar(1) = 0.0_fp
             cdpar(2) = 1.0_fp
             !
             call prop_get(gdp%mdfile_ptr,'*','BdfaC',cdpar(1))
             call prop_get(gdp%mdfile_ptr,'*','BdfbC',cdpar(2))
             !
             txtput1 = '  aC'
             write(lundia,'(a,a,e20.4)') txtput1, ':', cdpar(1)
             txtput1 = '  bC'
             write(lundia,'(a,a,e20.4)') txtput1, ':', cdpar(2)
          case (4)
             txtput1 = 'Celerity C_H'
             txtput2 = 'max[(H/H_max)^CHp,gamma_min] * CHn * S / [H * (1-Fr^2)]'
             write(lundia,'(a,a,a)') txtput1, ': ', txtput2
             !
             call prop_get(gdp%mdfile_ptr,'*','BdfCHp',bdfC_Hp)    !Read bedform migration speed gamma power
             call prop_get(gdp%mdfile_ptr,'*','BdfCHn',bdfC_Hn)    !Read bedform migration non-linearity parameter
             call prop_get(gdp%mdfile_ptr,'*','BdfGMn',bdfGmin)    !Read bedform maximum gamma
             !
             txtput1 = '  CHp'
             write(lundia,'(a,a,e20.4)') txtput1, ':', bdfC_Hp
             txtput1 = '  CHn'
             write(lundia,'(a,a,e20.4)') txtput1, ':', bdfC_Hn
             txtput1 = '  gamma_min'
             write(lundia,'(a,a,e20.4)') txtput1, ':', bdfGmin
          end select
       endif
    endif
    !
    !---------------------------
    ! Reading choice for Bedform length
    !
    ctemp = ''
    call prop_get(gdp%mdfile_ptr,'*','BdfL',ctemp)
    call small(ctemp     ,20         )
    select case( ctemp )
    case ('vanrijn84') 
       bedformlengthtype = 1
       txtput2 = 'Van Rijn (1984)'
    case ('powerrelation') 
       bedformlengthtype = 2
       txtput2 = 'Power relation aL*(H^bL)'
    case default
       bedformlengthtype = 1
       txtput2 = 'Van Rijn (1984)'
    end select
    txtput1 = 'Dune length predictor'
    write(lundia,'(a,a,a)') txtput1, ': ', txtput2
    !
    if (bedformlengthtype == 2) then 
       ldpar(1) = 0.0_fp
       ldpar(2) = 1.0_fp
       !
       call prop_get(gdp%mdfile_ptr,'*','BdfaL',ldpar(1))
       call prop_get(gdp%mdfile_ptr,'*','BdfbL',ldpar(2))
       if (ldpar(1) <= 0.0) then
          call prterr(lundia, 'U190', 'Dune length coefficients in .mdf -> Dune length <= 0.0')
       endif
       !
       txtput1 = '  aL'
       write(lundia,'(a,a,e20.4)') txtput1, ':', ldpar(1)
       txtput1 = '  bL'
       write(lundia,'(a,a,e20.4)') txtput1, ':', ldpar(2)
    endif
    !
    !---------------------------
    ! Reading choice for Bedform roughness predictor
    !
    ctemp = ''
    call prop_get(gdp%mdfile_ptr,'*','BdfRou',ctemp)
    call small(ctemp     ,20         )
    select case( ctemp )
    case ('vanrijn07')
       bdfrpt = 0
       txtput2 = 'Van Rijn (2007)'
    case ('vanrijn84') 
       bdfrpt = 1
       txtput2 = 'Van Rijn (1984)'
    case ('powerrelation') 
       bdfrpt = 2
       txtput2 = 'Power Relation aR*(Hd^bR)'
    case default
       bdfrpt = 1
       txtput2 = 'Van Rijn (1984)'
    end select
    txtput1 = 'Dune roughness height predictor'
    write(lundia,'(a,a,a)') txtput1, ': ', txtput2
    !
8888 continue
    ! if Bdf keyword turned out to be NO, then bdfrpt will be 0 (Van Rijn 2004).
    ! read those paremeters
    !
    select case (bdfrpt)
    case (0)
       kdpar(1) = 1.0_fp
       kdpar(2) = 0.0_fp
       kdpar(3) = 0.0_fp
       !
       kdpar(4) = 0.0_fp
       kdpar(5) = 0.0_fp
       kdpar(6) = 0.0_fp
       !
       call prop_get(gdp%mdfile_ptr,'*','BdfRpC',kdpar(1))
       call prop_get(gdp%mdfile_ptr,'*','BdfMrC',kdpar(2))
       call prop_get(gdp%mdfile_ptr,'*','BdfDnC',kdpar(3))
       !
       call prop_get(gdp%mdfile_ptr,'*','BdfRpR',kdpar(4))
       call prop_get(gdp%mdfile_ptr,'*','BdfMrR',kdpar(5))
       call prop_get(gdp%mdfile_ptr,'*','BdfDnR',kdpar(6))
       !
       if (lfbedfrm) then
          txtput1 = '  Ripple calibration (-)'
          write(lundia,'(a,a,e20.4)') txtput1, ':', kdpar(1)
          txtput1 = '  Ripple relaxation factor (-)'
          write(lundia,'(a,a,e20.4)') txtput1, ':', kdpar(4)
          !
          txtput1 = '  Mega-ripple calibration (-)'
          write(lundia,'(a,a,e20.4)') txtput1, ':', kdpar(2)
          txtput1 = '  Mega-ripple relaxation factor (-)'
          write(lundia,'(a,a,e20.4)') txtput1, ':', kdpar(5)
          !
          txtput1 = '  Dune calibration (-)'
          write(lundia,'(a,a,e20.4)') txtput1, ':', kdpar(3)
          txtput1 = '  Dune relaxation factor (-)'
          write(lundia,'(a,a,e20.4)') txtput1, ':', kdpar(6)
       endif
    case (2)
       kdpar(1) = 0.0_fp
       kdpar(2) = 1.0_fp
       !
       call prop_get(gdp%mdfile_ptr,'*','BdfaR',kdpar(1))
       call prop_get(gdp%mdfile_ptr,'*','BdfbR',kdpar(2))
       if (kdpar(1) <= 0.0) then
          call prterr(lundia, 'U190', 'Dune roughness coefficients in .mdf -> Dune roughness <= 0.0')
       endif
       !
       txtput1 = '  aR'
       write(lundia,'(a,a,e20.4)') txtput1, ':', kdpar(1)
       txtput1 = '  bR'
       write(lundia,'(a,a,e20.4)') txtput1, ':', kdpar(2)
    end select
    !
    ! if Bdf keyword turned out to be NO, skip remainder
    !
    if (.not.lfbedfrm) goto 9999
    !
    !---------------------------
    ! Reading initial dune height/dune length
    !
    rdum = -9999999.0_fp
    call restart_trim_bdf(lundia   ,nmaxus   ,mmax     ,duneheight, &
                        & hdread   ,dunelength,ldread  ,rdum      , &
                        & gdp      )
    !
    if (.not.hdread) then
       !
       ! First assume that 'BdfUni' contains a filename
       ! If the file does not exist, assume that 'BdfUni' contains a uniform value (real)
       !
       call prop_get(gdp%mdfile_ptr,'*','BdfUni', flbdfh)
       !
       ! Intel 7.0 crashes on an inquire statement when file = ' '
       !
       if (flbdfh == ' ') flbdfh = 'dummyname'
       inquire (file = flbdfh, exist = bdfhfile_exists)
       txtput1 = 'Initial dune height'
       if (.not. bdfhfile_exists) then
          flbdfh = ' '
          call prop_get(gdp%mdfile_ptr,'*','BdfUni', bdfuni)
          duneheight        = bdfuni
          write(lundia,'(a,a,e20.4)') txtput1, ':', bdfuni
       else
          !
          !Now if the bedformheightfile exists fill duneheight with the specified information
          !
          fmttmp = 'formatted'
          call depfil(lundia    ,error     ,flbdfh    ,fmttmp    , &
                    & duneheight,1         ,1         ,gdp%griddim)
          !
          write(lundia,'(a,a,a)') txtput1, ':', flbdfh
          if (error) call prterr(lundia, 'U021', 'RDBEDFORMPAR: error reading initial bedform heights from BdfUni')
          if (error) goto 9999
       endif
    endif
    !
    if (.not.ldread) then
       txtput1 = 'Initial dune length'
       dunelength        = 0.0_fp
       write(lundia,'(a,a,e20.4)') txtput1, ':', 0.0_fp
    endif
    !
    write (lundia, '(a)') '*** End of bedform input'
    write (lundia, *)
    !
9999 continue
    !
end subroutine rdbedformpar
