subroutine adi(dischy    ,solver    ,icreep    ,stage     ,nst       , &
             & nfltyp    ,lsecfl    ,betac     ,mmax      ,nmax      , &
             & zmodel    ,j         ,nmmaxj    ,nmmax     ,kmax      , &
             & lstsci    ,nocol     ,norow     ,nsrc      ,dismmt    , &
             & irocol    ,mnksrc    ,kfu       ,kfv       ,kfs       , &
             & kcu       ,kcv       ,kcs       ,kfumin    ,kfumax    , &
             & kfvmin    ,kfvmax    ,kspu      ,kspv      ,kadu      , &
             & kadv      ,porosu    ,porosv    ,areau     ,areav     , &
             & volum1    ,s0        ,s1        ,u0        ,u1        , &
             & v0        ,v1        ,w1        ,hu        ,hv        , &
             & umean     ,vmean     ,qxk       ,qyk       ,qzk       , &
             & circ2d    ,circ3d    ,dps       ,dpu       ,dpv       , &
             & evap      ,hkru      ,hkrv      ,dteu      ,dtev      , &
             & disch     ,umdis     ,vmdis     ,sig       ,thick     , &
             & guu       ,guv       ,gvv       ,gvu       ,guz       , &
             & gvz       ,gud       ,gvd       ,gsqs      ,gsqiu     , &
             & gsqiv     ,taubpu    ,taubpv    ,taubsu    ,taubsv    , &
             & rho       ,sumrho    ,dddksi    ,dddeta    ,dzdksi    , &
             & dzdeta    ,wsu       ,wsv       ,hu0       ,hv0       , &
             & fxw       ,fyw       ,crbc      ,dfu       ,dfv       , &
             & deltau    ,deltav    ,tp        ,rlabda    ,dzu1      , &
             & dzv1      ,vicuv     ,vnu2d     ,vicww     ,rxx       , &
             & rxy       ,ryy       ,cfurou    ,cfvrou    , &
             & r0        ,diapl     ,rnpl      ,wsbodyu   ,wsbodyv   , &
             & windsu    ,windsv    ,patm      ,fcorio    ,dpdksi    , &
             & dpdeta    ,ubrlsu    ,ubrlsv    ,uwtypu    ,uwtypv    , &
             & pship     ,tgfsep    ,soumud    ,excbed    ,wrka1     , &
             & wrka2     ,wrka3     ,wrka4     ,wrka5     ,wrka6     , &
             & wrka7     ,wrka8     ,wrka9     ,wrka15    ,wrka16    , &
             & wrkb1     ,wrkb2     ,wrkb3     ,wrkb4     ,wrkb5     , &
             & wrkb6     ,wrkb7     ,wrkb8     ,wrkb9     ,wrkb10    , &
             & wrkb11    ,wrkb12    ,wrkb13    ,wrkb14    ,wrkb15    , &
             & wrkb16    ,sbkol     ,precip    ,gdp       )
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
!    Function: ADI performs one time step of the Alternating
!              Direction Implicit (ADI) method
! Method used: A.D.I. method is used.
!              Upwind-approach for wet cross section in shallow
!              areas or if the model area contains structures.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow2d3d_timers
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp), dimension(:,:,:) , pointer :: rttfu
    real(fp), dimension(:,:,:) , pointer :: rttfv
    real(fp), dimension(:,:)   , pointer :: ustokes
    real(fp), dimension(:,:)   , pointer :: vstokes
    include 'flow_steps_f.inc'
!
! Global variables
!
    integer                                                  :: icreep  !  Description and declaration in tricom.igs
    integer                                                  :: j       !!  Begin pointer for arrays which have
                                                                        !!  been transformed into 1D arrays.
                                                                        !!  Due to the shift in the 2nd (M-)
                                                                        !!  index, J = -2*NMAX + 1
    integer                                                  :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: lsecfl  !  Description and declaration in dimens.igs
    integer                                                  :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nfltyp  !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nmmax   !  Description and declaration in dimens.igs
    integer                                                  :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                                  :: nocol   !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nst     !!  Time step number
    integer    , dimension(5, norow + nocol)                 :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(7, nsrc)                          :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kcv     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfumax  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfumin  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfvmax  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfvmin  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)    :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)    :: kspv    !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: kadu    !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: kadv    !  Description and declaration in esm_alloc_int.f90
    logical                                                  :: sbkol   !  Description and declaration in procs.igs
    logical                                                  :: zmodel  !  Description and declaration in procs.igs
    real(fp)                                                 :: betac   !  Description and declaration in tricom.igs
    real(fp), dimension(12, norow + nocol)                   :: crbc    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(4, norow + nocol)                    :: circ2d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dddeta  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dddksi  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: deltav  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dfv     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)             :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dpv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dteu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dtev    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dzdeta  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dzdksi  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: evap    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: excbed  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: fcorio  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: fxw     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: fyw     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gsqiu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gsqiv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gud     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: guv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: guz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gvd     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gvz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hkru    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hkrv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hu0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hv0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: precip  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: s1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: soumud  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: taubpv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: taubsv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: tgfsep  !!  Water elev. induced by tide gen.force
                                                                        !!  Internal work array WRKB17 used
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: uwtypu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: uwtypv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: vmean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: windsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: windsv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka15  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka16  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka2   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka3   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka4   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka5   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka6   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka7   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka8   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka9   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wsv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wsbodyv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       :: qzk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       :: w1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)            :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)            :: cfvrou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)     :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: areau   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: areav   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: dpdeta  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: dpdksi  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: dzu1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: dzv1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: porosu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: porosv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: rho     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: rnpl    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: rxx     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: rxy     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: ryy     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: sumrho  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: ubrlsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: ubrlsv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: v0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: volum1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb10  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb11  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb12  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb13  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb14  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb15  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb16  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb2   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb3   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb4   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb5   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb6   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb7   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb8   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb9   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) :: r0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                :: sig     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax, 2, norow + nocol)              :: circ3d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                :: umdis   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                :: vmdis   !  Description and declaration in esm_alloc_real.f90
    character(1), dimension(nsrc)                            :: dismmt  !  Description and declaration in esm_alloc_char.f90
    character(8)                                             :: dischy  !  Description and declaration in tricom.igs
    character(8)                                             :: solver  !  Description and declaration in tricom.igs
    character(8)                               , intent(in)  :: stage   !!  First or Second half time step
!
! Local variables
!
    integer :: icx
    integer :: icy
    integer :: idry
    integer :: nhystp
    integer :: nmaxddb
    logical :: flood   ! Flag for activating flooding part of checku subroutine
!
!! executable statements -------------------------------------------------------
!
    rttfu         => gdp%gdtrachy%rttfu
    rttfv         => gdp%gdtrachy%rttfv
    ustokes       => gdp%gdtrisol%ustokes
    vstokes       => gdp%gdtrisol%vstokes
    !
    nmaxddb = nmax + 2*gdp%d%ddbound
    !
    ! =====================================
    ! COMPUTATION OF STAGE 1 FOR ADI METHOD
    ! Computation of V1, i.e. evaluate momentum equation for one half timest
    ! =====================================
    !
    if (stage=='stage1') then
       !
       ! Calculate HV and set KFV = 0 for HV < HTRSH (.5*DRYFLC)
       ! hv is already calculated in SUD for the wet points (kfv=1)
       ! Calling checku is only necessary because the hv calculation
       ! is performed on the full computational domain (kcv=1) instead of
       ! only the wet points (kfv=1)
       !
       idry  = 0
       flood = .false.
       icx   = 1
       icy   = nmaxddb
       call timer_start(timer_checku, gdp)
       call checku(hv        ,s0        ,dpv       ,vmean     , &
                 & kfv       ,kcs       ,kcv       , &
                 & kspv      ,hkrv      ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,icx       ,flood     ,dps       , &
                 & gdp       )
       call timer_stop(timer_checku, gdp)
       call timer_start(timer_uzd, gdp)
       gdp%dd%uzditer = 0
       icx            = 1
       icy            = nmaxddb
       call timer_start(timer_1stuzd, gdp)
       call uzd(icreep    ,dpdeta    ,s0        ,v0        , &
              & v1        ,u0        ,w1        ,vmean     , &
              & hv        ,hu        ,gvv       ,guu       ,guv       ,gsqs      , &
              & gud       ,gvd       ,guz       ,gsqiv     ,qyk       ,qxk       , &
              & disch     ,vmdis     ,dismmt    ,mnksrc    ,kcv       , &
              & kcs       ,kfv       ,kfu       ,kfs       , &
              & kspv      ,kadv      ,kadu      ,nocol     ,icx       ,icy       , &
              & irocol(1, norow + 1) ,j         ,nmmaxj    ,nmmax     ,kmax      , &
              & nsrc      ,lsecfl    ,lstsci    ,betac     ,nst       , &
              & wrkb1     ,wrkb2     ,wrkb3     ,wrkb4     ,wrkb5     , &
              & wrkb6     ,wrkb7     ,wrkb8     ,wrkb9     ,wrkb10    , &
              & wrkb11    ,wrkb12    ,wrkb13    ,wrkb14    ,wrkb15    , &
              & wrkb16    ,taubpv    ,taubsv    ,rho       ,sumrho    , &
              & thick     ,sig       ,dps       ,wsv       ,fyw       ,wsbodyv   , &
              & vicuv     ,vnu2d     ,vicww     ,ryy       ,rxy       , &
              & dfv       ,deltav    ,tp        ,rlabda    , &
              & diapl     ,rnpl      , &
              & cfvrou    ,cfurou    ,rttfv     ,r0        ,windsv    , &
              & patm      ,fcorio    ,ubrlsv    ,hkrv      , &
              & pship     ,tgfsep    ,dtev      ,vstokes   ,.false.   ,gdp       )
       call timer_stop(timer_1stuzd, gdp)
       call timer_stop(timer_uzd, gdp)
       !
       !     computation proceeds in X direction
       !
       ! CHECK FOR FLOODING AND DRYING IN "U" POINTS
       !
       flood = .true.
       icx   = nmaxddb
       icy   = 1
       call timer_start(timer_checku, gdp)
       call checku(hu        ,s0        ,dpu       ,umean     , &
                 & kfu       ,kcs       ,kcu       , &
                 & kspu      ,hkru      ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,icx       ,flood     ,dps       , &
                 & gdp       )
       call timer_stop(timer_checku, gdp)
11111  continue
       icx            = nmaxddb
       icy            = 1
       if (sbkol) then
          !
          ! Correction for open discharge boundaries in explicit direction
          !
          call bccor(j         ,nmmaxj    ,kmax      ,nocol     ,icy       , &
                   & icx       ,zmodel    ,irocol(1, norow + 1) , &
                   & kcs       ,kfv       ,qyk       ,  &
                   & thick     ,circ2d(1, norow + 1) ,gdp       )
       endif
       !
       ! Computation of U1 and S1, i.e. evaluation of coupled momentum and
       ! continuity equation for one half time step
       !
       gdp%dd%suditer = 0
       call timer_start(timer_sud, gdp)
       call timer_start(timer_1stsud, gdp)
       call sud(dischy    ,nst       ,icreep    ,betac     ,mmax      , &
              & nmax      ,j         ,nmmaxj    ,nmmax     ,kmax      , &
              & lstsci    ,nsrc      ,lsecfl    ,norow     ,icx       , &
              & icy       ,dismmt    ,irocol(1, 1)         ,mnksrc    , &
              & kfu       ,kfv       ,kfs       ,kcs       ,kspu      , &
              & kadu      ,kadv      ,kcu       ,kfumin    ,kfumax    , &
              & porosu    ,s0        ,s1        ,u0        ,u1        , &
              & v1        ,w1        ,r0        ,qxk       ,qyk       , &
              & qzk       ,guu       ,gvv       ,gvu       ,gsqs      , &
              & gud       ,gvd       ,gvz       ,gsqiu     ,dteu      , &
              & circ2d(1, 1)         ,circ3d(1, 1, 1)      ,disch     , &
              & umdis     ,umean     ,hu        ,hv        ,dpu       ,dzu1      , &
              & dpdksi    ,thick     ,sig       ,dps       ,taubpu    , &
              & taubsu    ,rho       ,sumrho    ,wsu       ,fxw       ,wsbodyu   , &
              & idry      ,crbc(1,1) ,vicuv     ,wrka9     , &
              & vnu2d     ,vicww     ,rxx       ,rxy       ,dfu       , &
              & deltau    ,tp        ,rlabda    ,cfurou    ,cfvrou    , &
              & rttfu     ,diapl     ,rnpl      , &
              & windsu    ,patm      ,fcorio    ,evap      ,ubrlsu    , &
              & uwtypu    ,hkru      ,pship     ,tgfsep    ,wrka1     , &
              & wrka2     ,wrka3     ,wrka4     ,wrka5     ,wrka6     , &
              & wrka7     ,wrka8     ,wrka15    ,wrkb1     ,wrkb2     , &
              & wrkb3     ,wrkb4     ,wrkb5     ,wrkb6     ,wrkb7     , &
              & wrkb8     ,wrkb15    ,wrkb16    ,soumud    ,            &
              & precip    ,ustokes   ,gdp       )
       call timer_stop(timer_1stsud, gdp)
       call timer_stop(timer_sud, gdp)
       !
       ! Check for drying in waterlevel points in the X-direction
       !
       icx = nmaxddb
       icy = 1
       call timer_start(timer_drychk, gdp)
       call drychk(idry      ,s1        ,qxk       ,qyk       ,icx       , &
                 & icy       ,dps       ,kfu       ,kfv       ,kfs       , &
                 & j         ,nmmaxj    ,nmmax     ,kmax      ,nfltyp    , &
                 & excbed    ,kcs       ,gdp       )
       call timer_stop(timer_drychk, gdp)
       !
       ! Compute Volume and Areas to be used in routines that computes 
       ! the transport of matter (consistency with WAQ)
       ! Use dummy arrays to represent the unused Z-model arrays
       !
       call timer_start(timer_comvol, gdp)
       call comvol(nmmax     ,kmax      ,zmodel    ,kcs       ,kcu       , &
                 & thick     ,guu       ,gsqs      ,dps       ,s1        , &
                 & wrkb8     ,wrkb9     ,hu        ,porosu    ,volum1    , &
                 & areau     ,gdp       )
       call timer_stop(timer_comvol, gdp)
       !
       ! DD code added:
       !
       !
       ! Synchronize on Dry Point
       !
       if (nfltyp==0) then
          nhystp = nxtdry(d3dflow_check_adi_dry, 0, gdp)
       else
          nhystp = nxtdry(d3dflow_check_adi_dry, idry, gdp)
       endif
       if (nfltyp/=0) then
          !
          ! If waterlevel is below bottom then isolate waterlevel point by setting
          ! the surrounding velocities to zero and repeat the computation of SUD
          !
          if (nhystp==d3dflow_build_adi_zeta .or.                               &
            & (nhystp==noneighbors .and. idry==1)) goto 11111
       !
       ! DD code added end
       !
       endif
    !
    ! END OF COMPUTATION OF STAGE 1 FOR ADI METHOD
    !
    endif
    !
    ! =====================================
    ! COMPUTATION OF STAGE 2 FOR ADI METHOD
    ! Computation of U1, i.e. evaluate momentum equation for one half timest
    ! =====================================
    !
    if (stage=='stage2') then
       !
       ! Calculate HU and set KFU = 0 for HU < HTRSH (.5*DRYFLC)
       ! hu is already calculated in SUD for the wet points (kfu=1)
       ! Calling checku is only necessary because the hu calculation
       ! is performed on the full computational domain (kcu=1) instead of
       ! only the wet points (kfu=1)
       !
       idry  = 0
       flood = .false.
       icx   = nmaxddb
       icy   = 1
       call timer_start(timer_checku, gdp)
       call checku(hu        ,s0        ,dpu       ,umean     , &
                 & kfu       ,kcs       ,kcu       , &
                 & kspu      ,hkru      ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,icx       ,flood     ,dps       , &
                 & gdp       )
       call timer_stop(timer_checku, gdp)
       call timer_start(timer_uzd, gdp)
       gdp%dd%uzditer = 0
       icx            = nmaxddb
       icy            = 1
       call timer_start(timer_2nduzd, gdp)
       call uzd(icreep    ,dpdksi    ,s0        ,u0        , &
              & u1        ,v0        ,w1        ,umean     , &
              & hu        ,hv        ,guu       ,gvv       ,gvu       ,gsqs      , &
              & gvd       ,gud       ,gvz       ,gsqiu     ,qxk       ,qyk       , &
              & disch     ,umdis     ,dismmt    ,mnksrc    ,kcu       , &
              & kcs       ,kfu       ,kfv       ,kfs       , &
              & kspu      ,kadu      ,kadv      ,norow     ,icx       ,icy       , &
              & irocol    ,j         ,nmmaxj    ,nmmax     ,kmax      , &
              & nsrc      ,lsecfl    ,lstsci    ,betac     ,nst       , &
              & wrkb1     ,wrkb2     ,wrkb3     ,wrkb4     ,wrkb5     , &
              & wrkb6     ,wrkb7     ,wrkb8     ,wrkb9     ,wrkb10    , &
              & wrkb11    ,wrkb12    ,wrkb13    ,wrkb14    ,wrkb15    , &
              & wrkb16    ,taubpu    ,taubsu    ,rho       ,sumrho    , &
              & thick     ,sig       ,dps       ,wsu       ,fxw       ,wsbodyu   , &
              & vicuv     ,vnu2d     ,vicww     ,rxx       ,rxy       , &
              & dfu       ,deltau    ,tp        ,rlabda    , &
              & diapl     ,rnpl      , &
              & cfurou    ,cfvrou    ,rttfu     ,r0        ,windsu    , &
              & patm      ,fcorio    ,ubrlsu    ,hkru      , &
              & pship     ,tgfsep    ,dteu      ,ustokes   ,.false.   ,gdp       )
       call timer_stop(timer_2nduzd, gdp)
       call timer_stop(timer_uzd, gdp)
       !
       ! CHECK FOR FLOODING AND DRYING IN "V" POINTS
       !
       flood = .true.
       icx   = 1
       icy   = nmaxddb
       call timer_start(timer_checku, gdp)
       call checku(hv        ,s0        ,dpv       ,vmean     , &
                 & kfv       ,kcs       ,kcv       , &
                 & kspv      ,hkrv      ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,icx       ,flood     ,dps       , &
                 & gdp       )
       call timer_stop(timer_checku, gdp)
22222  continue
       icx            = 1
       icy            = nmaxddb
       if (sbkol) then
          !
          ! Correction for open discharge boundaries in explicit direction
          !
          call bccor(j         ,nmmaxj    ,kmax      ,norow     ,icy       , &
                   & icx       ,zmodel    ,irocol(1,1),kcs      ,kfu       , &
                   & qxk       ,thick     ,circ2d(1,1)          ,gdp       ) 
       endif
       !
       ! Computation of V1 and S1, i.e. evaluation of coupled momentum and
       ! continuity equation for one half time step
       !
       gdp%dd%suditer = 0
       call timer_start(timer_sud, gdp)
       call timer_start(timer_2ndsud, gdp)
       call sud(dischy    ,nst       ,icreep    ,betac     ,nmax      , &
              & mmax      ,j         ,nmmaxj    ,nmmax     ,kmax      , &
              & lstsci    ,nsrc      ,lsecfl    ,nocol     ,icx       , &
              & icy       ,dismmt    ,irocol(1, norow + 1) ,mnksrc    , &
              & kfv       ,kfu       ,kfs       ,kcs       ,kspv      , &
              & kadv      ,kadu      ,kcv       ,kfvmin    ,kfvmax    , &
              & porosv    ,s0        ,s1        ,v0        ,v1        , &
              & u1        ,w1        ,r0        ,qyk       ,qxk       , &
              & qzk       ,gvv       ,guu       ,guv       ,gsqs      , &
              & gvd       ,gud       ,guz       ,gsqiv     ,dtev      , &
              & circ2d(1, norow + 1) ,circ3d(1, 1, norow + 1)         ,disch     , &
              & vmdis     ,vmean     ,hv        ,hu        ,dpv       ,dzv1      , &
              & dpdeta    ,thick     ,sig       ,dps       ,taubpv    , &
              & taubsv    ,rho       ,sumrho    ,wsv       ,fyw       ,wsbodyv   , &
              & idry      ,crbc(1, norow + 1)   ,vicuv     ,wrka9     , &
              & vnu2d     ,vicww     ,ryy       ,rxy       ,dfv       , &
              & deltav    ,tp        ,rlabda    ,cfvrou    ,cfurou    , &
              & rttfv     ,diapl     ,rnpl      , &
              & windsv    ,patm      ,fcorio    ,evap      ,ubrlsv    , &
              & uwtypv    ,hkrv      ,pship     ,tgfsep    ,wrka1     , &
              & wrka2     ,wrka3     ,wrka4     ,wrka5     ,wrka6     , &
              & wrka7     ,wrka8     ,wrka16    ,wrkb1     ,wrkb2     , &
              & wrkb3     ,wrkb4     ,wrkb5     ,wrkb6     ,wrkb7     , &
              & wrkb8     ,wrkb15    ,wrkb16    ,soumud    ,            &
              & precip    ,vstokes   ,gdp       )
       call timer_stop(timer_2ndsud, gdp)
       call timer_stop(timer_sud, gdp)
       !
       ! Check for drying in waterlevel points in the X-direction
       !
       icx = nmaxddb
       icy = 1
       call timer_start(timer_drychk, gdp)
       call drychk(idry      ,s1        ,qxk       ,qyk       ,icx       , &
                 & icy       ,dps       ,kfu       ,kfv       ,kfs       , &
                 & j         ,nmmaxj    ,nmmax     ,kmax      ,nfltyp    , &
                 & excbed    ,kcs       ,gdp       )
       call timer_stop(timer_drychk, gdp)
       !
       ! Compute Volume and Areas to be used in routines that computes
       ! the transport of matter (consistency with WAQ)
       !
       call timer_start(timer_comvol, gdp)
       call comvol(nmmax     ,kmax      ,zmodel    ,kcs       ,kcv       , &
                 & thick     ,gvv       ,gsqs      ,dps       ,s1        , &
                 & wrkb8     ,wrkb9     ,hv        ,porosv    ,volum1    , &
                 & areav     ,gdp       )
       call timer_stop(timer_comvol, gdp)
       !
       !
       ! DD code added:
       !
       !
       ! Synchronize on Dry Point
       !
       if (nfltyp==0) then
          nhystp = nxtdry(d3dflow_check_adi_dry, 0, gdp)
       else
          nhystp = nxtdry(d3dflow_check_adi_dry, idry, gdp)
       endif
       if (nfltyp/=0) then
          !
          ! If waterlevel is below bottom then isolate waterlevel point by setting
          ! the surrounding velocities to zero and repeat the computation of SUD
          !
          if (nhystp==d3dflow_build_adi_zeta .or.                               &
            & (nhystp==noneighbors .and. idry==1)) goto 22222
       !
       ! DD code added end
       !
       endif
    !
    ! END OF COMPUTATION OF STAGE 2 FOR ADI METHOD
    !
    endif
end subroutine adi
