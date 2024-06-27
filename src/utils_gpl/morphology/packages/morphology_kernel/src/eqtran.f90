!> \page morphology_lib Library of sediment transport formulations
!! \section eqtran Gateway for all sediment transport formulations
!! The subroutine \em eqtran provides a standardized interface for calling
!! any sediment transport in the library.

subroutine eqtran(sig       ,thick     ,kmax      ,ws        ,ltur      , &
                & frac      ,sigmol    ,dicww     ,lundia    ,taucr0    , &
                & rksrs     ,i2d3d     ,lsecfl    ,spirint   ,suspfrac  , &
                & tetacr    ,concin    , &
                & dzduu     ,dzdvv     ,ubot      ,tauadd    ,sus       , &
                & bed       ,susw      ,bedw      ,espir     ,wave      , &
                & scour     ,ubot_from_com        ,camax     ,eps       , &
                & iform     ,npar      ,par       ,numintpar ,numrealpar, &
                & numstrpar ,dllfunc   ,dllhandle ,intpar    ,realpar   , &
                & strpar    , & !output:
                & aks       ,caks      ,taurat    ,seddif    ,rsedeq    , &
                & kmaxsd    ,conc2d    ,sbcu      ,sbcv      ,sbwu      , &
                & sbwv      ,sswu      ,sswv      ,dss       ,caks_ss3d , &
                & aks_ss3d  ,ust2      ,t_relax   ,error     )

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
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use message_module, only: write_error
    use mathconsts, only: pi, ee
    use iso_c_binding, only: c_char
    use morphology_data_module
    !
    implicit none
!
! Arguments
!
    integer(pntrsize)                   , intent(in)    :: dllhandle
    integer                             , intent(in)    :: i2d3d
    integer                             , intent(in)    :: iform
    integer                             , intent(in)    :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                             , intent(in)    :: lsecfl   !  Description and declaration in esm_alloc_int.f90
    integer                             , intent(in)    :: ltur     !  Description and declaration in esm_alloc_int.f90
    integer                             , intent(in)    :: lundia   !  Description and declaration in inout.igs
    integer                             , intent(in)    :: npar
    integer                             , intent(in)    :: numintpar
    integer                             , intent(in)    :: numrealpar
    integer                             , intent(in)    :: numstrpar
    integer      , dimension(numintpar) , intent(inout) :: intpar
    real(fp)                            , intent(in)    :: bed
    real(fp)                            , intent(in)    :: bedw
    real(fp)                            , intent(in)    :: camax
    real(fp)     , dimension(kmax)      , intent(inout) :: concin
    real(fp)     , dimension(0:kmax)    , intent(in)    :: dicww    !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: dzduu     !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: dzdvv     !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: eps
    real(fp)                            , intent(in)    :: espir
    real(fp)                            , intent(in)    :: frac     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(npar)      , intent(inout) :: par
    real(fp)                            , intent(in)    :: rksrs    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax)      , intent(in)    :: sig      !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: sigmol   !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: spirint  !  Spiral flow intensity
    real(fp)                            , intent(in)    :: sus
    real(fp)                            , intent(in)    :: susw
    real(fp)                            , intent(in)    :: tauadd
    real(fp)                            , intent(in)    :: taucr0
    real(fp)                            , intent(in)    :: tetacr
    real(fp)     , dimension(kmax)      , intent(in)    :: thick    !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: ubot     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(0:kmax)    , intent(in)    :: ws       !  Description and declaration in esm_alloc_real.f90
    real(hp)     , dimension(numrealpar), intent(inout) :: realpar
    logical                             , intent(in)    :: scour
    logical                             , intent(in)    :: suspfrac !  suspended sediment fraction
    logical                             , intent(in)    :: ubot_from_com
    logical                             , intent(in)    :: wave
    character(256)                      , intent(in)    :: dllfunc
    character(256), dimension(numstrpar), intent(inout) :: strpar
!
    logical                         , intent(out)  :: error
    integer                         , intent(out)  :: kmaxsd
    real(fp)                        , intent(inout):: aks ! out parameter for Van Rijn, in parameter for others
    real(fp)                        , intent(out)  :: aks_ss3d
    real(fp)                        , intent(out)  :: caks
    real(fp)                        , intent(out)  :: caks_ss3d
    real(fp)                        , intent(out)  :: conc2d
    real(fp)                        , intent(out)  :: dss
    real(fp), dimension(kmax)       , intent(out)  :: rsedeq
    real(fp)                        , intent(out)  :: sbcu
    real(fp)                        , intent(out)  :: sbcv
    real(fp)                        , intent(out)  :: sbwu
    real(fp)                        , intent(out)  :: sbwv
    real(fp), dimension(0:kmax)     , intent(out)  :: seddif
    real(fp)                        , intent(out)  :: sswu
    real(fp)                        , intent(out)  :: sswv
    real(fp)                        , intent(out)  :: t_relax
    real(fp)                        , intent(out)  :: taurat
    real(fp)                        , intent(out)  :: ust2
!
! Local variables
!
    integer                     :: i
    integer(pntrsize)           :: ierror_ptr
    integer                     :: k
    integer(pntrsize), external :: perf_function_eqtran
    real(fp)                    :: ag
    real(fp)                    :: alphaspir
    real(fp)                    :: bakdif
    real(fp)                    :: cesus
    real(fp)                    :: chezy
    real(fp)                    :: cosa
    real(fp)                    :: d10
    real(fp)                    :: d15
    real(fp)                    :: d90
    real(fp)                    :: dg
    real(fp)                    :: dgsd
    real(fp)                    :: di50
    real(fp)                    :: dstar
    real(fp)                    :: fac3d2d
    real(fp)                    :: h1
    real(fp)                    :: hidexp
    real(fp)                    :: hrms
    real(fp)                    :: mudfrac
    real(fp)                    :: rhosol
    real(fp)                    :: rhowat
    real(fp)                    :: rlabda
    real(fp)                    :: sag
    real(fp)                    :: salinity
    real(fp)                    :: sandfrac
    real(fp)                    :: sbot
    real(fp)                    :: sina
    real(fp)                    :: ssus
    real(fp)                    :: sscu
    real(fp)                    :: sscv
    real(fp)                    :: taub
    real(fp)                    :: teta
    real(fp)                    :: timhr
    real(fp)                    :: tp
    real(fp)                    :: txg
    real(fp)                    :: tyg
    real(fp)                    :: u
    real(fp)                    :: umod
    real(fp)                    :: uorb
    real(fp)                    :: ustarc
    real(fp)                    :: utot
    real(fp)                    :: uuu
    real(fp)                    :: v
    real(fp)                    :: vicmol
    real(fp)                    :: vonkar
    real(fp)                    :: vvv
    real(fp)                    :: z0cur
    real(fp)                    :: z0rou
    real(fp)                    :: zumod
    real(fp)                    :: kwtur
    real(fp)                    :: dzbdt
    real(fp)                    :: dzdx
    real(fp)                    :: dzdy
    real(fp)                    :: poros
    real(fp)                    :: ua
    real(fp)                    :: va
    real(fp)                    :: wsb
    real(fp)                    :: zb
    !
    ! Interface to dll is in High precision!
    !
    real(hp)               :: cesus_dll
    real(hp)               :: sbc_dll
    real(hp)               :: sbcu_dll
    real(hp)               :: sbcv_dll
    real(hp)               :: sbwu_dll
    real(hp)               :: sbwv_dll
    real(hp)               :: ssus_dll
    real(hp)               :: sswu_dll
    real(hp)               :: sswv_dll
    real(hp)               :: t_relax_dll
    character(1024)        :: errmsg
    character(256)         :: message        ! Contains message from internal or external transport formula
    character(kind=c_char) :: message_c(257) ! C- version of "message", including C_NULL_CHAR
                                             ! Calling perf_function_eqtran with "message" caused problems
                                             ! Solved by using "message_c"
    logical                :: equi_conc      ! equilibrium concentration given (instead of susp. transport rate)
    logical                :: sbc_total      ! total bed load given (instead of m,n components)
    logical                :: sus_total      ! total suspended load given (instead of m,n components)
!
!! executable statements -------------------------------------------------------
!
    ierror_ptr = 0
    error      = .false.
    !
    timhr     = real(realpar(RP_TIME) ,fp)/3600.0_fp
    utot      = real(realpar(RP_EFVLM),fp)
    u         = real(realpar(RP_EFUMN),fp)
    v         = real(realpar(RP_EFVMN),fp)
    uuu       = real(realpar(RP_UCHAR),fp)
    vvv       = real(realpar(RP_VCHAR),fp)
    umod      = real(realpar(RP_VELCH),fp)
    zumod     = real(realpar(RP_ZVLCH),fp)
    h1        = real(realpar(RP_DEPTH),fp)
    chezy     = real(realpar(RP_CHEZY),fp)
    hrms      = real(realpar(RP_HRMS) ,fp)
    tp        = real(realpar(RP_TPEAK),fp)
    teta      = real(realpar(RP_TETA) ,fp)
    rlabda    = real(realpar(RP_RLAMB),fp)
    uorb      = real(realpar(RP_UORB) ,fp)
    kwtur     = real(realpar(RP_KWTUR),fp)
    dzbdt     = real(realpar(RP_BLCHG),fp)
    dzdx      = real(realpar(RP_DZDX) ,fp)
    dzdy      = real(realpar(RP_DZDY) ,fp)
    di50      = real(realpar(RP_D50)  ,fp)
    dss       = real(realpar(RP_DSS)  ,fp)
    dstar     = real(realpar(RP_DSTAR),fp)
    d10       = real(realpar(RP_D10MX),fp)
    d15       = real(realpar(RP_D15MX),fp)
    d90       = real(realpar(RP_D90MX),fp)
    mudfrac   = real(realpar(RP_MUDFR),fp)
    hidexp    = real(realpar(RP_HIDEX),fp)
    wsb       = real(realpar(RP_SETVL),fp)
    rhosol    = real(realpar(RP_RHOSL),fp)
    rhowat    = real(realpar(RP_RHOWT),fp)
    salinity  = real(realpar(RP_SALIN),fp)
    !temp      = real(realpar(RP_TEMP) ,fp)
    ag        = real(realpar(RP_GRAV) ,fp)
    vicmol    = real(realpar(RP_VICML),fp)
    taub      = real(realpar(RP_TAUB) ,fp)
    !ubed      = real(realpar(RP_UBED ),fp)
    !vbed      = real(realpar(RP_VBED ),fp)
    !velb      = real(realpar(RP_VELBD),fp)
    !zvelb     = real(realpar(RP_ZVLBD),fp)
    vonkar    = real(realpar(RP_VNKAR),fp)
    z0cur     = real(realpar(RP_Z0CUR),fp)
    z0rou     = real(realpar(RP_Z0ROU),fp)
    ustarc    = real(realpar(RP_USTAR),fp)
    dg        = real(realpar(RP_DG)   ,fp)
    dgsd      = real(realpar(RP_DGSD) ,fp)
    sandfrac  = real(realpar(RP_SNDFR),fp)
    zb        = real(realpar(RP_ZB)   ,fp)
    !
    cesus  = 0.0_fp
    sbot   = 0.0_fp
    sbcu   = 0.0_fp
    sbcv   = 0.0_fp
    ssus   = 0.0_fp
    sscu   = 0.0_fp
    sscv   = 0.0_fp
    sbwu   = 0.0_fp
    sbwv   = 0.0_fp
    sswu   = 0.0_fp
    sswv   = 0.0_fp
    ua     = 0.0_fp
    va     = 0.0_fp
    sag    = sqrt(ag)
    bakdif = vicmol / sigmol
    !
    if (iform > 0) then
       if (suspfrac) then
          !
          ! By default entrainment and deposition (re)move sediment into/from
          ! the bottom-most layer.
          !
          kmaxsd = kmax
          !
          ! Use diffusivity of turbulence model as vertical sediment diffusion
          ! coefficient. In the future, we may OPTIONALLY enable Van Rijn's
          ! analytical 1984, 1993 or 2004 formulations here.
          !
          do k = 0, kmax
              seddif(k) = dicww(k)
          enddo
          ! seddif(kmax) = vonkar*z0rou*ustarc
       endif
    endif
    !
    equi_conc = .false.
    sbc_total = .false.
    sus_total = .false.
    if (iform == -1) then
       !
       ! Van Rijn 1993
       !
       call tram1(numrealpar,realpar   ,wave      ,npar      ,par       , &
                & kmax      ,bed       , &
                & tauadd    ,taucr0    ,aks       ,eps       ,camax     , &
                & frac      ,sig       ,thick     ,ws        , &
                & dicww     ,ltur      , &
                & kmaxsd    ,taurat    ,caks      , &
                & seddif    ,sigmol    ,rsedeq    ,scour     ,bedw      , &
                & susw      ,sbcu      ,sbcv      ,sbwu      ,sbwv      , &
                & sswu      ,sswv                 ,conc2d    ,error     , &
                & message   )
       !
       if (error) then
          call write_error(message, unit=lundia)
          return
       endif
       !
       caks_ss3d = caks
       aks_ss3d  = aks
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == -2 .or. iform == -4) then
       !
       ! Van Rijn 2004
       !
       call tram2(numrealpar,realpar   ,wave      ,i2d3d     ,npar      , &
                & par       ,kmax      ,bed       ,dzduu     ,dzdvv     , &
                & rksrs     ,tauadd    ,taucr0    ,aks       ,eps       , &
                & camax     ,frac      ,sig       ,thick     ,ws        , &
                & dicww     ,ltur      ,aks_ss3d  ,iform     , &
                & kmaxsd    ,taurat    ,caks      ,caks_ss3d ,concin    , &
                & seddif    ,sigmol    ,rsedeq    ,scour     ,bedw      , &
                & susw      ,sbcu      ,sbcv      ,sbwu      ,sbwv      , &
                & sswu      ,sswv      ,tetacr    ,conc2d    ,error     , &
                & message   )
       !
       if (error) then
          call write_error(message, unit=lundia)
          return
       endif
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 1) then
       !
       ! Engelund-Hansen
       !
       call tranb1(utot      ,di50      ,chezy     ,h1        ,npar      , &
                 & par       ,sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 2) then
       !
       ! Meyer-Peter-Muller
       !
       call tranb2(utot      ,di50       ,d90       ,chezy     ,h1        , &
                 & npar      ,par        ,hidexp    ,sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 3) then
       !
       ! Ackers-White
       !
       call tranb3(utot      ,d90       ,chezy     ,h1        ,npar      , &
                 & par       ,sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 4) then
       !
       ! general relation for bed load
       !
       call tranb4(utot      ,di50      ,chezy     ,npar      ,par        , &
                 & hidexp    ,sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 5) then
       !
       ! Bijker
       !
       call tranb5(u         ,v         ,di50      ,d90       ,chezy     , &
                 & h1        ,hrms      ,tp        ,teta      ,npar      , &
                 & par       ,dzduu     ,dzdvv     ,vonkar    ,wsb       , &
                 & poros     ,sbcu      ,sbcv      ,sscu      ,sscv      , &
                 & cesus     )
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 6) then
       !
       ! Bailard
       !
       errmsg = 'Bailard method is disabled'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
       !
       !call tranb6(utot      ,u         ,v         ,chezy     ,h1        , &
       !          & hrms      ,tp        ,teta      ,diss      ,dzduu     , &
       !          & dzdvv     ,npar      ,par       ,sbcu      ,sbcv      , &
       !          & sscu      ,sscv      )
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 7) then
       !
       ! Van Rijn (1984, modified)
       !
       call tranb7(utot      ,di50       ,d90       ,h1        ,npar      , &
                 & par       ,sbot      ,ssus       ,vonkar    ,mudfrac   )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 8) then
       !
       ! Van Rijn / Ribberink (1994)
       !
       errmsg = 'Van Rijn/Ribberink (1994) method is disabled'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
       !
       !call tranb8(u         ,v         ,hrms      ,h1        ,teta      , &
       !          & tp        ,di50      ,d90       ,diss      ,dzduu     , &
       !          & dzdvv     ,npar      ,par       ,sbcu      ,sbcv      , &
       !          & sscu      ,sscv      )
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 9) then
       !
       ! Silt module
       !
       errmsg = 'Original Delft3D-MOR Silt module is disabled'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
       !
       !call tranb9(utot      ,h1        ,alfs      ,sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 10) then
       !
       ! Ashida and Michiue
       !
       errmsg = 'Ashida and Michiue method is disabled'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
       !
       !call trab10(utot      ,di50      ,chezy     ,h1        ,cosa      , &
       !          & sina      ,dzduu     ,dzdvv     ,npar      ,par       , &
       !          & sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 11) then
       !
       ! Soulsby and Van Rijn
       !
       call trab11(u         ,v         ,hrms      ,h1        ,tp        , &
                 & di50      ,npar      ,par       ,sbcu      ,sbcv      , &
                 & sscu      ,sscv      ,ubot      ,vonkar    ,ubot_from_com)
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 12) then
       !
       ! Soulsby
       !
       call trab12(u         ,v         ,hrms      ,h1        ,tp        , &
                 & teta      ,di50      ,npar      ,par       ,sbcu      , &
                 & sbcv      ,sscu      ,sscv      ,ubot      ,vonkar    , &
                 & ubot_from_com)
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 13) then
       !
       ! test transport (Wang) Fredsoe
       !
       call tran9t(utot      ,di50      ,d90       ,chezy     ,h1        , &
                 & ustarc    ,npar      ,par       ,sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 14) then
       !
       ! generalized Ashida and Michiue
       !
       call trab14(utot      ,di50      ,chezy     ,npar      ,par        , &
                 & hidexp    ,sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 16) then 
       !
       ! Wilcock & Crowe
       !
       call trabwc(utot      ,di50      ,taub      ,npar      ,par       , &
                 & sbot      ,ssus      ,dg        ,sandfrac  ,chezy     )
       !
       sbc_total = .true.
       sus_total = .true. 
    elseif (iform == 17) then 
       !
       ! Modified Wilcock & Crowe
       !
       call trabwc2(utot      ,di50      ,taub       ,npar      ,par       , &
                  & sbot      ,ssus      ,dg        ,dgsd       ,chezy     )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 18) then 
       !
       ! Gaeuman et al. (development of Wilcock & Crowe)
       !
       call trabg(utot      ,di50      ,taub      ,npar      ,par       , &
                & sbot      ,ssus      ,dg        ,dgsd      ,chezy     )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 19) then
       !
       ! van Thiel / Van Rijn (2008)
       !
       equi_conc = .true.
       call trab19(u         ,v         ,hrms      ,rlabda    ,teta      ,h1        ,tp        , &
                 & di50      ,d15       ,d90       ,npar      ,par       ,dzbdt     ,vicmol    , &
                 & poros     ,chezy     ,dzdx      ,dzdy      ,sbcu      ,sbcv      ,cesus      , &
                 & ua        ,va        ,ubot      ,kwtur     ,ubot_from_com )
       !
       realpar(RP_UAU) = real(ua      ,hp)  ! needed for suspended transport
       realpar(RP_VAU) = real(va      ,hp)
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 20) then
       !
       ! Soulsby / Van Rijn with XBeach adaptations
       !
       equi_conc = .true.
       call trab20(u         ,v         ,hrms      ,rlabda    ,teta      ,h1         ,tp        , &
                 & di50      ,d15       ,d90       ,npar      ,par       ,dzbdt     ,vicmol    , &
                 & poros     ,chezy     ,dzdx      ,dzdy      ,sbcu      ,sbcv     ,cesus     , &
                 & ua        ,va        ,ubot      ,kwtur     ,ubot_from_com )
       !
       realpar(RP_UAU) = real(ua      ,hp)  ! needed for suspended transport
       realpar(RP_VAU) = real(va      ,hp)
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 22) then
       !
       ! ASMITA
       !
       call asmita(zb, timhr, npar, par, &
                 & sbot, cesus, t_relax)
       cesus = cesus/rhosol
       !
       sbc_total = .true.
       sus_total = .true.
       equi_conc = .true.
    elseif (iform == 15) then
       !
       ! User defined formula in DLL
       ! Input parameters are passed via realpar/intpar/strpar-arrays that have
       ! been mostly filled in calling routine
       !
       realpar(RP_UMEAN) = real(u      ,hp)
       realpar(RP_VMEAN) = real(v      ,hp)
       realpar(RP_VELMN) = real(utot   ,hp)
       ! Initialisation of output variables of user defined transport formulae
       !
       sbc_total   = .false. ! may be changed by user defined formulae
       sus_total   = .true.  ! always true for user defined formulae
       sbc_dll     = 0.0_hp
       sbcu_dll    = 0.0_hp
       sbcv_dll    = 0.0_hp
       sbwu_dll    = 0.0_hp
       sbwv_dll    = 0.0_hp
       !
       equi_conc   = .false.
       cesus_dll   = 0.0_hp
       ssus_dll    = 0.0_hp
       sswu_dll    = 0.0_hp
       sswv_dll    = 0.0_hp
       t_relax_dll = 0.0_hp
       message     = ' '
       do i=1,256
          message_c(i) = message(i:i)
       enddo
       message_c(257) = C_NULL_CHAR
       !
       ! psem/vsem is used to be sure this works fine in DD calculations
       !
       call psemlun
       ierror_ptr = perf_function_eqtran(dllhandle       , dllfunc           , &
                                         intpar          , numintpar         , &
                                         realpar         , numrealpar        , &
                                         strpar          , numstrpar         , &
                                         sbc_total, sbc_dll  , sbcu_dll      , &
                                         sbcv_dll , sbwu_dll , sbwv_dll      , &
                                         equi_conc, cesus_dll, ssus_dll      , &
                                         sswu_dll , sswv_dll , t_relax_dll   , &
                                         message_c)
       message = transfer(message_c(1:256), message)
       call vsemlun
       if (ierror_ptr /= 0) then
          errmsg = 'Cannot find function "'//trim(dllfunc)//'" in dynamic library.'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       if (message /= ' ') then
          errmsg = 'Message from user defined transport formulae '//trim(dllfunc)//' :'
          call write_error(errmsg, unit=lundia)
          write (lundia,'(a,a  )') '          ', trim(message)
          write (lundia,'(a    )') ' '
          error = .true.
          return
       endif
       !
       ! Output parameters
       !
       sbot    = real(sbc_dll ,fp)
       sbcu    = real(sbcu_dll,fp)
       sbcv    = real(sbcv_dll,fp)
       sbwu    = real(sbwu_dll,fp)
       sbwv    = real(sbwv_dll,fp)
       !
       cesus   = real(cesus_dll,fp)
       ssus    = real(ssus_dll ,fp)
       sswu    = real(sswu_dll ,fp)
       sswv    = real(sswv_dll ,fp)
       !
       t_relax = real(t_relax_dll,fp)
    else
       errmsg = 'Transport formula not recognized'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif
    !
    if (iform > 0) then
       !
       ! Change from volume to mass concentrations/transport rates.
       !
       ! Van Rijn 1993 or Van Rijn 2004 include rhosol in the transport
       ! rates, the other formulae don't. So, multiply the transport
       ! rates and concentrations by rhosol now.
       !
       if (sbc_total) then
          sbot  = sbot * rhosol
       else
          sbcu  = sbcu * rhosol
          sbcv  = sbcv * rhosol
       endif
       !
       cesus = cesus * rhosol
       if (sus_total) then
          ssus = ssus * rhosol
       else
          sscu  = sscu * rhosol
          sscv  = sscv * rhosol
          ssus  = sqrt(sscu**2 + sscv**2)
       endif
       !
       sbwu = sbwu * rhosol
       sbwv = sbwv * rhosol
       sswu = sswu * rhosol
       sswv = sswv * rhosol
    endif
    !
    ! If only bed load transport magnitude is given, then the bed load
    ! should be oriented based on the near bed velocity. The near bed
    ! velocity is given by either the velocity (uuu,vvv) in 3D or
    ! the depth averaged velocity (uuu,vvv) corrected for spiral flow in 2D.
    !
    ust2 = ustarc**2
    if (sbc_total) then
       if (umod > 0.0_fp) then
          !
          ! Correct bed load transport direction for spiral flow intensity
          !
          if (lsecfl == 0) then
             alphaspir = 0.0_fp
          else
             alphaspir = sqrt(ag) / 0.4_fp / chezy
             alphaspir = 12.5_fp * espir * (1.0_fp-alphaspir) ! 12.5 = 2.0_fp/(von Karman)^2
             alphaspir = alphaspir * spirint / umod
          endif
          txg  = ust2 * (uuu + alphaspir*vvv) / umod
          tyg  = ust2 * (vvv - alphaspir*uuu) / umod
          ust2 = sqrt(txg**2 + tyg**2)
          if (ust2 > eps) then
             cosa = txg / ust2
             sina = tyg / ust2
          else
             cosa = 0.0_fp
             sina = 0.0_fp
          endif
          sbcu  = sbot * cosa
          sbcv  = sbot * sina
       else
          sbcu  = 0.0_fp
          sbcv  = 0.0_fp
      endif
    endif
    !
    ! Adjust for calibration factors
    !
    sbcu = bed  * sbcu
    sbcv = bed  * sbcv
    sbwu = bedw * sbwu
    sbwv = bedw * sbwv
    sswu = susw * sswu
    sswv = susw * sswv
    !
    if (iform>0) then
       cesus    = sus * cesus
       ssus     = sus * ssus
       sscu     = sus * sscu
       sscv     = sus * sscv
    else
       caks     = sus * caks
       caks_ss3d= sus * caks_ss3d
       do k = 1, kmax
          rsedeq(k) = sus * rsedeq(k)
       enddo
       conc2d   = sus * conc2d
    endif
    !
    if (suspfrac) then
       !
       ! For this fraction a 2D or 3D advection-diffusion will be solved.
       ! In 2D case we need to provide: conc2d
       ! In 3D case we need to provide: aks and caks
       !
       if (iform <= 0) then
          !
          ! Van Rijn 1993 or 2004 formula
          ! NOTE: conc2d, aks, and caks have already been computed in Van Rijn
          ! specific manner.
          !
       else
          !
          ! If we are not using Van Rijn 1993 or Van Rijn 2004
          ! then we still need to provide values for conc2d, aks, and caks.
          !
          if (equi_conc) then
              !
              ! Concentration given by transport formula
              !
          else
              !
              ! Suspended transport rate given by transport formula,
              ! derive concentration. 
              !
              cesus = ssus / (utot + eps) / h1
          endif
          !
          ! Concentration needs to be multiplied by frac to match Van Rijn
          ! formulae.
          !
          conc2d    = cesus * frac     ! this is rsedeq used for 2d runs
          !
          ! Convert depth averaged concentration to reference concentration
          ! at distance aks from bed.
          !
          kmaxsd    = kmax
          call factor3d2d(kmax      ,aks       ,kmaxsd    ,sig       , &
                        & thick     ,seddif    ,ws        ,bakdif    , &
                        & z0rou     ,h1        ,fac3d2d   )
          caks      = conc2d / (fac3d2d+eps) / rhosol
          rsedeq    = 0.0_fp
          !
          aks_ss3d  = aks
          caks_ss3d = caks
       endif
    else ! .not.suspfrac
       !
       ! Note: in case of bedload sediment type, the suspended load is added to the
       ! bed load to compute the total load. Bed slope effect will be applied to
       ! both the bed- and the suspended part.
       !
       ! The effect of frac will be included in the bed load at a later stage.
       !
       if (iform <= 0) then
           !
           ! Van Rijn 1993 or 2004 formula
           ! NOTE: conc2d is based on caks which has been multiplied by
           ! frac above. Since sbcu/v will be multiplied by frac later (again)
           ! there is the risk of double correction. So, undo the multiplication.
           !
           if (frac>0.0_fp) then
              sbcu = sbcu + conc2d * h1 * u / frac
              sbcv = sbcv + conc2d * h1 * v / frac
           endif
       elseif (equi_conc) then
           !
           ! Concentration given by transport formula
           !
           sbcu = sbcu + cesus * h1 * u
           sbcv = sbcv + cesus * h1 * v
       elseif (sus_total) then
           !
           ! Total suspended transport rate given by transport formula,
           ! assume that it is transported in depth-averaged direction
           !
           sbcu = sbcu + ssus * u / (utot+eps)
           sbcv = sbcv + ssus * v / (utot+eps)
       else
           !
           ! Suspended transport rate components given by transport
           ! formula, add them individually to the bed load
           !
           sbcu = sbcu + sscu
           sbcv = sbcv + sscv
       endif
    endif
    dss = real(realpar(RP_DSS)  ,fp)
end subroutine eqtran
