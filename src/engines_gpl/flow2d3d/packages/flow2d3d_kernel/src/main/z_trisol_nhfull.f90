subroutine z_trisol_nhfull(dischy    ,solver    ,icreep   ,ithisc    , &
                         & timnow    ,nst       ,itiwec    ,trasol    ,forfuv    , &
                         & forfww    ,nfltyp    , &
                         & saleqs    ,temeqs    , &
                         & sferic    ,grdang    ,ktemp     ,temint    ,keva      , &
                         & evaint    ,anglat    ,anglon    ,rouflo    ,rouwav    , &
                         & betac     ,tkemod    ,gdp       )
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
! Z-model
! This routine basically carries out the hydrodynamics
! and the online advection diffusion computation for one complete time step.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sync_flm
    use SyncRtcFlow
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
    include 'flow_steps_f.inc'
    include 'fsm.i'
    integer(pntrsize)                    , pointer :: iwrk1
    integer(pntrsize)                    , pointer :: iwrk2
    integer(pntrsize)                    , pointer :: wrka1
    integer(pntrsize)                    , pointer :: wrka2
    integer(pntrsize)                    , pointer :: wrka3
    integer(pntrsize)                    , pointer :: wrka4
    integer(pntrsize)                    , pointer :: wrka5
    integer(pntrsize)                    , pointer :: wrka6
    integer(pntrsize)                    , pointer :: wrka7
    integer(pntrsize)                    , pointer :: wrka8
    integer(pntrsize)                    , pointer :: wrka10
    integer(pntrsize)                    , pointer :: wrka11
    integer(pntrsize)                    , pointer :: wrka14
    integer(pntrsize)                    , pointer :: wrka15
    integer(pntrsize)                    , pointer :: wrka16
    integer(pntrsize)                    , pointer :: wrkb1
    integer(pntrsize)                    , pointer :: wrkb2
    integer(pntrsize)                    , pointer :: wrkb3
    integer(pntrsize)                    , pointer :: wrkb4
    integer(pntrsize)                    , pointer :: wrkb5
    integer(pntrsize)                    , pointer :: wrkb6
    integer(pntrsize)                    , pointer :: wrkb7
    integer(pntrsize)                    , pointer :: wrkb8
    integer(pntrsize)                    , pointer :: wrkb9
    integer(pntrsize)                    , pointer :: wrkb10
    integer(pntrsize)                    , pointer :: wrkb11
    integer(pntrsize)                    , pointer :: wrkb12
    integer(pntrsize)                    , pointer :: wrkb13
    integer(pntrsize)                    , pointer :: wrkb14
    integer(pntrsize)                    , pointer :: wrkb15
    integer(pntrsize)                    , pointer :: wrkb16
    integer(pntrsize)                    , pointer :: wrkb17
    integer(pntrsize)                    , pointer :: wrkb18
    integer(pntrsize)                    , pointer :: wrkc1
    integer(pntrsize)                    , pointer :: wrkc2
    integer(pntrsize)                    , pointer :: wrkc3
    integer(pntrsize)                    , pointer :: wrkc4
    integer(pntrsize)                    , pointer :: zwork
    logical , dimension(:)               , pointer :: flbub
    integer                              , pointer :: nmax
    integer                              , pointer :: mmax
    integer                              , pointer :: nlb
    integer                              , pointer :: nub
    integer                              , pointer :: mlb
    integer                              , pointer :: mub
    integer                              , pointer :: ddbound
    integer                              , pointer :: nmaxus
    integer                              , pointer :: kmax
    integer                              , pointer :: nmaxd
    integer                              , pointer :: jstart
    integer                              , pointer :: nmmaxj
    integer                              , pointer :: nmmax
    integer                              , pointer :: lsts
    integer                              , pointer :: lstsc
    integer                              , pointer :: lstsci
    integer                              , pointer :: lsal
    integer                              , pointer :: lsed
    integer                              , pointer :: lsedtot
    integer                              , pointer :: ltem
    integer                              , pointer :: lsecfl
    integer                              , pointer :: lsec
    integer                              , pointer :: ltur
    integer                              , pointer :: ltur2d
    integer                              , pointer :: kmxdt
    integer                              , pointer :: npiwe
    integer                              , pointer :: nbub
    integer                              , pointer :: nxbub
    integer                              , pointer :: noroco
    integer                              , pointer :: norow
    integer                              , pointer :: nocol
    integer                              , pointer :: nto
    integer                              , pointer :: ntof
    integer                              , pointer :: ntoq
    integer                              , pointer :: kc
    integer                              , pointer :: kcd
    integer                              , pointer :: nrob
    integer                              , pointer :: nsrc
    integer                              , pointer :: nsrcd
    integer                              , pointer :: ndro
    integer                              , pointer :: nsluv
    integer                              , pointer :: lundia
    real(fp)                             , pointer :: timsec
    real(fp)                             , pointer :: timhr
    integer                              , pointer :: itiwei
    integer                              , pointer :: iti_sedtrans
    integer                              , pointer :: itdiag
    integer                              , pointer :: julday
    integer                              , pointer :: ntstep
    logical                              , pointer :: densin
    real(fp)                             , pointer :: dtsec
    character(6)                         , pointer :: momsol
    real(fp)                             , pointer :: rhow
    real(fp)                             , pointer :: ag
    real(fp)                             , pointer :: z0
    real(fp)                             , pointer :: z0v
    integer                              , pointer :: iro
    integer                              , pointer :: irov
    logical                              , pointer :: wind
    logical                              , pointer :: salin
    logical                              , pointer :: temp
    logical                              , pointer :: const
    logical                              , pointer :: culvert
    logical                              , pointer :: drogue
    logical                              , pointer :: wave
    logical                              , pointer :: iweflg
    logical                              , pointer :: struct
    logical                              , pointer :: cdwstruct
    logical                              , pointer :: sedim
    logical                              , pointer :: online
    logical                              , pointer :: htur2d
    logical                              , pointer :: zmodel
    logical                              , pointer :: nonhyd
    logical                              , pointer :: roller
    logical                              , pointer :: sbkol
    logical                              , pointer :: bubble
    logical                              , pointer :: lfsdu
    integer(pntrsize)                    , pointer :: sbuu
    integer(pntrsize)                    , pointer :: sbvv
    integer(pntrsize)                    , pointer :: seddif
    integer(pntrsize)                    , pointer :: entr
    integer(pntrsize)                    , pointer :: wstau
    logical                              , pointer :: flmd2l
    integer(pntrsize)                    , pointer :: rksd
    integer(pntrsize)                    , pointer :: depchg
    logical                              , pointer :: bedupd
    logical                              , pointer :: eqmbcsand
    logical                              , pointer :: eqmbcmud
    integer              , dimension(:)  , pointer :: sedtyp
    integer(pntrsize)                    , pointer :: alfas
    integer(pntrsize)                    , pointer :: alpha
    integer(pntrsize)                    , pointer :: areau
    integer(pntrsize)                    , pointer :: areav
    integer(pntrsize)                    , pointer :: bruvai
    integer(pntrsize)                    , pointer :: c
    integer(pntrsize)                    , pointer :: cbuv
    integer(pntrsize)                    , pointer :: cbuvrt
    integer(pntrsize)                    , pointer :: cdwlsu
    integer(pntrsize)                    , pointer :: cdwlsv
    integer(pntrsize)                    , pointer :: cdwzbu
    integer(pntrsize)                    , pointer :: cdwzbv
    integer(pntrsize)                    , pointer :: cdwztu
    integer(pntrsize)                    , pointer :: cdwztv
    integer(pntrsize)                    , pointer :: cfurou
    integer(pntrsize)                    , pointer :: cfvrou
    integer(pntrsize)                    , pointer :: cvalu0
    integer(pntrsize)                    , pointer :: cvalv0
    integer(pntrsize)                    , pointer :: circ2d
    integer(pntrsize)                    , pointer :: circ3d
    integer(pntrsize)                    , pointer :: crbc
    integer(pntrsize)                    , pointer :: dddeta
    integer(pntrsize)                    , pointer :: dddksi
    integer(pntrsize)                    , pointer :: disch0
    integer(pntrsize)                    , pointer :: disch1
    integer(pntrsize)                    , pointer :: decay
    integer(pntrsize)                    , pointer :: deltau
    integer(pntrsize)                    , pointer :: deltav
    integer(pntrsize)                    , pointer :: dfu
    integer(pntrsize)                    , pointer :: dfv
    integer(pntrsize)                    , pointer :: diapl
    integer(pntrsize)                    , pointer :: dicuv
    integer(pntrsize)                    , pointer :: dicww
    integer(pntrsize)                    , pointer :: dis
    integer(pntrsize)                    , pointer :: df
    integer(pntrsize)                    , pointer :: disch
    integer(pntrsize)                    , pointer :: disinp
    integer(pntrsize)                    , pointer :: discum
    integer(pntrsize)                    , pointer :: dpd
    integer(pntrsize)                    , pointer :: dps
    integer(pntrsize)                    , pointer :: dpu
    integer(pntrsize)                    , pointer :: dpv
    integer(pntrsize)                    , pointer :: drodep
    integer(pntrsize)                    , pointer :: rint0
    integer(pntrsize)                    , pointer :: rint1
    integer(pntrsize)                    , pointer :: dudz
    integer(pntrsize)                    , pointer :: umdis0
    integer(pntrsize)                    , pointer :: umdis1
    integer(pntrsize)                    , pointer :: dvdz
    integer(pntrsize)                    , pointer :: vmdis0
    integer(pntrsize)                    , pointer :: vmdis1
    integer(pntrsize)                    , pointer :: dxydro
    integer(pntrsize)                    , pointer :: enstro
    integer(pntrsize)                    , pointer :: eroll0
    integer(pntrsize)                    , pointer :: eroll1
    integer(pntrsize)                    , pointer :: evap
    integer(pntrsize)                    , pointer :: ewabr0
    integer(pntrsize)                    , pointer :: ewabr1
    integer(pntrsize)                    , pointer :: ewave0
    integer(pntrsize)                    , pointer :: ewave1
    integer(pntrsize)                    , pointer :: fcorio
    integer(pntrsize)                    , pointer :: fuiwe
    integer(pntrsize)                    , pointer :: fviwe
    integer(pntrsize)                    , pointer :: fxw
    integer(pntrsize)                    , pointer :: fyw
    integer(pntrsize)                    , pointer :: grmasu
    integer(pntrsize)                    , pointer :: grmasv
    integer(pntrsize)                    , pointer :: grmsur
    integer(pntrsize)                    , pointer :: grmsvr
    integer(pntrsize)                    , pointer :: grfacu
    integer(pntrsize)                    , pointer :: grfacv
    integer(pntrsize)                    , pointer :: gsqs
    integer(pntrsize)                    , pointer :: guu
    integer(pntrsize)                    , pointer :: guv
    integer(pntrsize)                    , pointer :: gvu
    integer(pntrsize)                    , pointer :: gvv
    integer(pntrsize)                    , pointer :: hkru
    integer(pntrsize)                    , pointer :: hkrv
    integer(pntrsize)                    , pointer :: hrms
    integer(pntrsize)                    , pointer :: hu
    integer(pntrsize)                    , pointer :: hu0
    integer(pntrsize)                    , pointer :: hv
    integer(pntrsize)                    , pointer :: hv0
    integer(pntrsize)                    , pointer :: hydrbc
    integer(pntrsize)                    , pointer :: omega
    integer(pntrsize)                    , pointer :: patm
    integer(pntrsize)                    , pointer :: porosu
    integer(pntrsize)                    , pointer :: porosv
    integer(pntrsize)                    , pointer :: precip
    integer(pntrsize)                    , pointer :: procbc
    integer(pntrsize)                    , pointer :: pship
    integer(pntrsize)                    , pointer :: qsrcrt
    integer(pntrsize)                    , pointer :: qtfrac
    integer(pntrsize)                    , pointer :: qtfrct
    integer(pntrsize)                    , pointer :: qtfrt2
    integer(pntrsize)                    , pointer :: qu
    integer(pntrsize)                    , pointer :: qv
    integer(pntrsize)                    , pointer :: qxk
    integer(pntrsize)                    , pointer :: qyk
    integer(pntrsize)                    , pointer :: qzk
    integer(pntrsize)                    , pointer :: r0
    integer(pntrsize)                    , pointer :: r1
    integer(pntrsize)                    , pointer :: rbnd
    integer(pntrsize)                    , pointer :: rettim
    integer(pntrsize)                    , pointer :: rho
    integer(pntrsize)                    , pointer :: rhowat
    integer(pntrsize)                    , pointer :: rich
    integer(pntrsize)                    , pointer :: rint
    integer(pntrsize)                    , pointer :: rintsm
    integer(pntrsize)                    , pointer :: rlabda
    integer(pntrsize)                    , pointer :: rmneg
    integer(pntrsize)                    , pointer :: rnpl
    integer(pntrsize)                    , pointer :: rob
    integer(pntrsize)                    , pointer :: rthbnd
    integer(pntrsize)                    , pointer :: rtu2d0
    integer(pntrsize)                    , pointer :: rtu2d1
    integer(pntrsize)                    , pointer :: rtubnd
    integer(pntrsize)                    , pointer :: rtur0
    integer(pntrsize)                    , pointer :: rtur1
    integer(pntrsize)                    , pointer :: rxx
    integer(pntrsize)                    , pointer :: rxy
    integer(pntrsize)                    , pointer :: ryy
    integer(pntrsize)                    , pointer :: rxz
    integer(pntrsize)                    , pointer :: ryz
    integer(pntrsize)                    , pointer :: s0
    integer(pntrsize)                    , pointer :: s1
    integer(pntrsize)                    , pointer :: sig
    integer(pntrsize)                    , pointer :: sigdif
    integer(pntrsize)                    , pointer :: sigmol
    integer(pntrsize)                    , pointer :: sink
    integer(pntrsize)                    , pointer :: sour
    integer(pntrsize)                    , pointer :: stil
    integer(pntrsize)                    , pointer :: sumrho
    integer(pntrsize)                    , pointer :: taubmx
    integer(pntrsize)                    , pointer :: taubpu
    integer(pntrsize)                    , pointer :: taubpv
    integer(pntrsize)                    , pointer :: taubsu
    integer(pntrsize)                    , pointer :: taubsv
    integer(pntrsize)                    , pointer :: teta
    integer(pntrsize)                    , pointer :: tgarkt
    integer(pntrsize)                    , pointer :: tgarkx
    integer(pntrsize)                    , pointer :: tgarnp
    integer(pntrsize)                    , pointer :: thick
    integer(pntrsize)                    , pointer :: thtim
    integer(pntrsize)                    , pointer :: tkedis
    integer(pntrsize)                    , pointer :: tkepro
    integer(pntrsize)                    , pointer :: tp
    integer(pntrsize)                    , pointer :: u0
    integer(pntrsize)                    , pointer :: u1
    integer(pntrsize)                    , pointer :: ubrlsu
    integer(pntrsize)                    , pointer :: ubrlsv
    integer(pntrsize)                    , pointer :: umdis
    integer(pntrsize)                    , pointer :: umean
    integer(pntrsize)                    , pointer :: umnflc
    integer(pntrsize)                    , pointer :: umnldf
    integer(pntrsize)                    , pointer :: uorb
    integer(pntrsize)                    , pointer :: ubot
    integer(pntrsize)                    , pointer :: v0
    integer(pntrsize)                    , pointer :: v1
    integer(pntrsize)                    , pointer :: vicuv
    integer(pntrsize)                    , pointer :: vicww
    integer(pntrsize)                    , pointer :: vmdis
    integer(pntrsize)                    , pointer :: vmean
    integer(pntrsize)                    , pointer :: vmnflc
    integer(pntrsize)                    , pointer :: vmnldf
    integer(pntrsize)                    , pointer :: vnu2d
    integer(pntrsize)                    , pointer :: vnu3d
    integer(pntrsize)                    , pointer :: voldis
    integer(pntrsize)                    , pointer :: volum0
    integer(pntrsize)                    , pointer :: volum1
    integer(pntrsize)                    , pointer :: vortic
    integer(pntrsize)                    , pointer :: w1
    integer(pntrsize)                    , pointer :: w10mag
    integer(pntrsize)                    , pointer :: windcd
    integer(pntrsize)                    , pointer :: windsu
    integer(pntrsize)                    , pointer :: windsv
    integer(pntrsize)                    , pointer :: windu
    integer(pntrsize)                    , pointer :: windv
    integer(pntrsize)                    , pointer :: wphy
    integer(pntrsize)                    , pointer :: ws
    integer(pntrsize)                    , pointer :: wsu
    integer(pntrsize)                    , pointer :: wsv
    integer(pntrsize)                    , pointer :: wsbodyu
    integer(pntrsize)                    , pointer :: wsbodyv
    integer(pntrsize)                    , pointer :: xcor
    integer(pntrsize)                    , pointer :: xydro
    integer(pntrsize)                    , pointer :: xz
    integer(pntrsize)                    , pointer :: ycor
    integer(pntrsize)                    , pointer :: yz
    integer(pntrsize)                    , pointer :: z0ucur
    integer(pntrsize)                    , pointer :: z0vcur
    integer(pntrsize)                    , pointer :: z0urou
    integer(pntrsize)                    , pointer :: z0vrou
    integer(pntrsize)                    , pointer :: zstep
    integer(pntrsize)                    , pointer :: drhodx
    integer(pntrsize)                    , pointer :: drhody
    integer(pntrsize)                    , pointer :: dzs0
    integer(pntrsize)                    , pointer :: dzs1
    integer(pntrsize)                    , pointer :: dzu0
    integer(pntrsize)                    , pointer :: dzu1
    integer(pntrsize)                    , pointer :: dzv0
    integer(pntrsize)                    , pointer :: dzv1
    integer(pntrsize)                    , pointer :: res
    integer(pntrsize)                    , pointer :: fact
    integer(pntrsize)                    , pointer :: rl
    integer(pntrsize)                    , pointer :: xj
    integer(pntrsize)                    , pointer :: p1
    integer(pntrsize)                    , pointer :: p0
    integer(pntrsize)                    , pointer :: p00
    integer(pntrsize)                    , pointer :: pnhcor
    integer(pntrsize)                    , pointer :: w0
    integer(pntrsize)                    , pointer :: s00
    integer(pntrsize)                    , pointer :: guz
    integer(pntrsize)                    , pointer :: gvz
    integer(pntrsize)                    , pointer :: gud
    integer(pntrsize)                    , pointer :: gvd
    integer(pntrsize)                    , pointer :: gsqiu
    integer(pntrsize)                    , pointer :: gsqiv
    integer(pntrsize)                    , pointer :: ibuff
    integer(pntrsize)                    , pointer :: idifu
    integer(pntrsize)                    , pointer :: irocol
    integer(pntrsize)                    , pointer :: itbcc
    integer(pntrsize)                    , pointer :: itbct
    integer(pntrsize)                    , pointer :: itdis
    integer(pntrsize)                    , pointer :: itdro
    integer(pntrsize)                    , pointer :: kadu
    integer(pntrsize)                    , pointer :: kadv
    integer(pntrsize)                    , pointer :: kcs
    integer(pntrsize)                    , pointer :: kcu
    integer(pntrsize)                    , pointer :: kcv
    integer(pntrsize)                    , pointer :: kfs
    integer(pntrsize)                    , pointer :: kfu
    integer(pntrsize)                    , pointer :: kfv
    integer(pntrsize)                    , pointer :: kspu
    integer(pntrsize)                    , pointer :: kspv
    integer(pntrsize)                    , pointer :: kstp
    integer(pntrsize)                    , pointer :: mnbar
    integer(pntrsize)                    , pointer :: mnbnd
    integer(pntrsize)                    , pointer :: mndro
    integer(pntrsize)                    , pointer :: mnksrc
    integer(pntrsize)                    , pointer :: nob
    integer(pntrsize)                    , pointer :: kcshyd
    integer(pntrsize)                    , pointer :: kfumin
    integer(pntrsize)                    , pointer :: kfvmin
    integer(pntrsize)                    , pointer :: kfsmin
    integer(pntrsize)                    , pointer :: kfumax
    integer(pntrsize)                    , pointer :: kfvmax
    integer(pntrsize)                    , pointer :: kfsmax
    integer(pntrsize)                    , pointer :: kfumx0
    integer(pntrsize)                    , pointer :: kfvmx0
    integer(pntrsize)                    , pointer :: kfsmx0
    integer(pntrsize)                    , pointer :: kfumn0
    integer(pntrsize)                    , pointer :: kfvmn0
    integer(pntrsize)                    , pointer :: kfsmn0
    integer(pntrsize)                    , pointer :: kfsz0
    integer(pntrsize)                    , pointer :: kfsz1
    integer(pntrsize)                    , pointer :: kfuz0
    integer(pntrsize)                    , pointer :: kfuz1
    integer(pntrsize)                    , pointer :: kfvz0
    integer(pntrsize)                    , pointer :: kfvz1
    integer(pntrsize)                    , pointer :: kcscut
    integer(pntrsize)                    , pointer :: kcu45
    integer(pntrsize)                    , pointer :: kcv45
    integer(pntrsize)                    , pointer :: disint
    integer(pntrsize)                    , pointer :: dismmt
    integer(pntrsize)                    , pointer :: nambnd
    integer(pntrsize)                    , pointer :: namsrc
    integer(pntrsize)                    , pointer :: tprofc
    integer(pntrsize)                    , pointer :: tprofu
    integer(pntrsize)                    , pointer :: ubnd
    integer(pntrsize), dimension(:, :)   , pointer :: nprptr
    integer                              , pointer :: rtcact
    real(fp)      , dimension(:)         , pointer :: rhosol
    integer                              , pointer :: ifirst
    integer                              , pointer :: nubnd
    real(fp)                             , pointer :: windxt
    real(fp)                             , pointer :: windyt
    real(fp)                             , pointer :: windft
    integer                              , pointer :: nprocs
    integer      , dimension(:)          , pointer :: nread
    real(fp)     , dimension(:)          , pointer :: rcousr
    character*20 , dimension(:)          , pointer :: procs
    integer                              , pointer :: nrcmp
    logical                              , pointer :: dryrun
    integer(pntrsize)                    , pointer :: typbnd
    integer      , dimension(:)          , pointer :: modify_dzsuv
    logical                              , pointer :: ztbml
    logical                              , pointer :: ztbml_upd_r1
!
    include 'tri-dyn.igd'
!
! Global variables
!
    integer             :: ithisc      !! History file output time step
    integer             :: icreep      !  Description and declaration in tricom.igs
    integer             :: itiwec      !!  Current time counter for the calibration of internal wave energy
    integer, intent(in) :: keva        !  Description and declaration in tricom.igs
    integer             :: ktemp       !  Description and declaration in tricom.igs
    integer             :: nfltyp      !  Description and declaration in esm_alloc_int.f90
    integer             :: nst         !!  Current time step counter
    logical             :: sferic      !  Description and declaration in tricom.igs
    real(fp)            :: anglat      !!  - Angle of latitude of the model centre (used to determine the coef. for the coriolis force)
                                       !!  - In spherical coordinates this parameter equals the angle of latitude for the origin (water level point) after INIPHY anglat = 0.
    real(fp)            :: anglon      !!  - Angle of longitude of the model centre (used to determine solar radiation)
    real(fp)            :: betac       !  Description and declaration in tricom.igs
    real(fp)            :: grdang      !  Description and declaration in tricom.igs
    real(fp)            :: saleqs      !  Description and declaration in tricom.igs
    real(fp)            :: temeqs      !  Description and declaration in tricom.igs
    real(fp)            :: timnow      !!  Current timestep (multiples of dt)
    character(1)        :: evaint      !  Description and declaration in tricom.igs
    character(1)        :: forfuv      !  Description and declaration in tricom.igs
    character(1)        :: forfww      !  Description and declaration in tricom.igs
    character(1)        :: temint      !  Description and declaration in tricom.igs
    character(12)       :: tkemod      !  Description and declaration in tricom.igs
    character(13)       :: trasol      !  Description and declaration in tricom.igs
    character(4)        :: rouflo      !  Description and declaration in esm_alloc_char.f90
    character(4)        :: rouwav      !  Description and declaration in tricom.igs
    character(8)        :: dischy      !  Description and declaration in tricom.igs
    character(8)        :: solver      !  Description and declaration in tricom.igs
!
! Local variables
!
    integer      :: icx
    integer      :: icy
    integer      :: imode
    integer      :: itemp
    integer      :: itype
    integer      :: n
    integer      :: nhystp
    integer      :: nmaxddb
    integer      :: nreal       ! Pointer to real array RCOUSR for UDF particle wind factor parameters
    integer      :: ifirst_dens !  Flag to initialize the water density array
    logical      :: sscomp    
    logical      :: success      
    character(8) :: stage       ! First or second half time step
!
!! executable statements -------------------------------------------------------
!
    iwrk1               => gdp%gdaddress%iwrk1
    iwrk2               => gdp%gdaddress%iwrk2
    wrka1               => gdp%gdaddress%wrka1
    wrka2               => gdp%gdaddress%wrka2
    wrka3               => gdp%gdaddress%wrka3
    wrka4               => gdp%gdaddress%wrka4
    wrka5               => gdp%gdaddress%wrka5
    wrka6               => gdp%gdaddress%wrka6
    wrka7               => gdp%gdaddress%wrka7
    wrka8               => gdp%gdaddress%wrka8
    wrka10              => gdp%gdaddress%wrka10
    wrka11              => gdp%gdaddress%wrka11
    wrka14              => gdp%gdaddress%wrka14
    wrka15              => gdp%gdaddress%wrka15
    wrka16              => gdp%gdaddress%wrka16
    wrkb1               => gdp%gdaddress%wrkb1
    wrkb2               => gdp%gdaddress%wrkb2
    wrkb3               => gdp%gdaddress%wrkb3
    wrkb4               => gdp%gdaddress%wrkb4
    wrkb5               => gdp%gdaddress%wrkb5
    wrkb6               => gdp%gdaddress%wrkb6
    wrkb7               => gdp%gdaddress%wrkb7
    wrkb8               => gdp%gdaddress%wrkb8
    wrkb9               => gdp%gdaddress%wrkb9
    wrkb10              => gdp%gdaddress%wrkb10
    wrkb11              => gdp%gdaddress%wrkb11
    wrkb12              => gdp%gdaddress%wrkb12
    wrkb13              => gdp%gdaddress%wrkb13
    wrkb14              => gdp%gdaddress%wrkb14
    wrkb15              => gdp%gdaddress%wrkb15
    wrkb16              => gdp%gdaddress%wrkb16
    wrkb17              => gdp%gdaddress%wrkb17
    wrkb18              => gdp%gdaddress%wrkb18
    wrkc1               => gdp%gdaddress%wrkc1
    wrkc2               => gdp%gdaddress%wrkc2
    wrkc3               => gdp%gdaddress%wrkc3
    wrkc4               => gdp%gdaddress%wrkc4
    zwork               => gdp%gdaddress%zwork
    flbub               => gdp%gdbubble%flbub
    nmax                => gdp%d%nmax
    mmax                => gdp%d%mmax
    nlb                 => gdp%d%nlb
    nub                 => gdp%d%nub
    mlb                 => gdp%d%mlb
    mub                 => gdp%d%mub
    ddbound             => gdp%d%ddbound
    nmaxus              => gdp%d%nmaxus
    kmax                => gdp%d%kmax
    nmaxd               => gdp%d%nmaxd
    jstart              => gdp%d%jstart
    nmmaxj              => gdp%d%nmmaxj
    nmmax               => gdp%d%nmmax
    lsts                => gdp%d%lsts
    lstsc               => gdp%d%lstsc
    lstsci              => gdp%d%lstsci
    lsal                => gdp%d%lsal
    lsed                => gdp%d%lsed
    lsedtot             => gdp%d%lsedtot
    ltem                => gdp%d%ltem
    lsecfl              => gdp%d%lsecfl
    lsec                => gdp%d%lsec
    ltur                => gdp%d%ltur
    ltur2d              => gdp%d%ltur2d
    kmxdt               => gdp%d%kmxdt
    npiwe               => gdp%d%npiwe
    nbub                => gdp%d%nbub
    nxbub               => gdp%d%nxbub
    noroco              => gdp%d%noroco
    norow               => gdp%d%norow
    nocol               => gdp%d%nocol
    nto                 => gdp%d%nto
    ntof                => gdp%d%ntof
    ntoq                => gdp%d%ntoq
    kc                  => gdp%d%kc
    kcd                 => gdp%d%kcd
    nrob                => gdp%d%nrob
    nsrc                => gdp%d%nsrc
    nsrcd               => gdp%d%nsrcd
    ndro                => gdp%d%ndro
    nsluv               => gdp%d%nsluv
    lundia              => gdp%gdinout%lundia
    timsec              => gdp%gdinttim%timsec
    timhr               => gdp%gdinttim%timhr
    itiwei              => gdp%gdinttim%itiwei
    itdiag              => gdp%gdinttim%itdiag
    julday              => gdp%gdinttim%julday
    ntstep              => gdp%gdinttim%ntstep
    densin              => gdp%gdmorpar%densin
    dtsec               => gdp%gdtricom%dtsec
    momsol              => gdp%gdnumeco%momsol
    rhow                => gdp%gdphysco%rhow
    ag                  => gdp%gdphysco%ag
    z0                  => gdp%gdphysco%z0
    z0v                 => gdp%gdphysco%z0v
    iro                 => gdp%gdphysco%iro
    irov                => gdp%gdphysco%irov
    wind                => gdp%gdprocs%wind
    salin               => gdp%gdprocs%salin
    temp                => gdp%gdprocs%temp
    const               => gdp%gdprocs%const
    culvert             => gdp%gdprocs%culvert
    drogue              => gdp%gdprocs%drogue
    wave                => gdp%gdprocs%wave
    iweflg              => gdp%gdprocs%iweflg
    struct              => gdp%gdprocs%struct
    cdwstruct           => gdp%gdprocs%cdwstruct
    sedim               => gdp%gdprocs%sedim
    htur2d              => gdp%gdprocs%htur2d
    zmodel              => gdp%gdprocs%zmodel
    nonhyd              => gdp%gdprocs%nonhyd
    roller              => gdp%gdprocs%roller
    sbkol               => gdp%gdprocs%sbkol
    bubble              => gdp%gdprocs%bubble
    lfsdu               => gdp%gdprocs%lfsdu
    alfas               => gdp%gdr_i_ch%alfas
    alpha               => gdp%gdr_i_ch%alpha
    areau               => gdp%gdr_i_ch%areau
    areav               => gdp%gdr_i_ch%areav
    bruvai              => gdp%gdr_i_ch%bruvai
    c                   => gdp%gdr_i_ch%c
    cbuv                => gdp%gdr_i_ch%cbuv
    cbuvrt              => gdp%gdr_i_ch%cbuvrt
    cdwlsu              => gdp%gdr_i_ch%cdwlsu
    cdwlsv              => gdp%gdr_i_ch%cdwlsv
    cdwzbu              => gdp%gdr_i_ch%cdwzbu
    cdwzbv              => gdp%gdr_i_ch%cdwzbv
    cdwztu              => gdp%gdr_i_ch%cdwztu
    cdwztv              => gdp%gdr_i_ch%cdwztv
    cfurou              => gdp%gdr_i_ch%cfurou
    cfvrou              => gdp%gdr_i_ch%cfvrou
    cvalu0              => gdp%gdr_i_ch%cvalu0
    cvalv0              => gdp%gdr_i_ch%cvalv0
    circ2d              => gdp%gdr_i_ch%circ2d
    circ3d              => gdp%gdr_i_ch%circ3d
    crbc                => gdp%gdr_i_ch%crbc
    dddeta              => gdp%gdr_i_ch%dddeta
    dddksi              => gdp%gdr_i_ch%dddksi
    disch0              => gdp%gdr_i_ch%disch0
    disch1              => gdp%gdr_i_ch%disch1
    decay               => gdp%gdr_i_ch%decay
    deltau              => gdp%gdr_i_ch%deltau
    deltav              => gdp%gdr_i_ch%deltav
    dfu                 => gdp%gdr_i_ch%dfu
    dfv                 => gdp%gdr_i_ch%dfv
    diapl               => gdp%gdr_i_ch%diapl
    dicuv               => gdp%gdr_i_ch%dicuv
    dicww               => gdp%gdr_i_ch%dicww
    dis                 => gdp%gdr_i_ch%dis
    df                  => gdp%gdr_i_ch%df
    disch               => gdp%gdr_i_ch%disch
    disinp              => gdp%gdr_i_ch%disinp
    discum              => gdp%gdr_i_ch%discum
    dpd                 => gdp%gdr_i_ch%dpd
    dps                 => gdp%gdr_i_ch%dps
    dpu                 => gdp%gdr_i_ch%dpu
    dpv                 => gdp%gdr_i_ch%dpv
    drodep              => gdp%gdr_i_ch%drodep
    rint0               => gdp%gdr_i_ch%rint0
    rint1               => gdp%gdr_i_ch%rint1
    dudz                => gdp%gdr_i_ch%dudz
    umdis0              => gdp%gdr_i_ch%umdis0
    umdis1              => gdp%gdr_i_ch%umdis1
    dvdz                => gdp%gdr_i_ch%dvdz
    vmdis0              => gdp%gdr_i_ch%vmdis0
    vmdis1              => gdp%gdr_i_ch%vmdis1
    dxydro              => gdp%gdr_i_ch%dxydro
    enstro              => gdp%gdr_i_ch%enstro
    eroll0              => gdp%gdr_i_ch%eroll0
    eroll1              => gdp%gdr_i_ch%eroll1
    evap                => gdp%gdr_i_ch%evap
    ewabr0              => gdp%gdr_i_ch%ewabr0
    ewabr1              => gdp%gdr_i_ch%ewabr1
    ewave0              => gdp%gdr_i_ch%ewave0
    ewave1              => gdp%gdr_i_ch%ewave1
    fcorio              => gdp%gdr_i_ch%fcorio
    fuiwe               => gdp%gdr_i_ch%fuiwe
    fviwe               => gdp%gdr_i_ch%fviwe
    fxw                 => gdp%gdr_i_ch%fxw
    fyw                 => gdp%gdr_i_ch%fyw
    grmasu              => gdp%gdr_i_ch%grmasu
    grmasv              => gdp%gdr_i_ch%grmasv
    grmsur              => gdp%gdr_i_ch%grmsur
    grmsvr              => gdp%gdr_i_ch%grmsvr
    grfacu              => gdp%gdr_i_ch%grfacu
    grfacv              => gdp%gdr_i_ch%grfacv
    gsqs                => gdp%gdr_i_ch%gsqs
    guu                 => gdp%gdr_i_ch%guu
    guv                 => gdp%gdr_i_ch%guv
    gvu                 => gdp%gdr_i_ch%gvu
    gvv                 => gdp%gdr_i_ch%gvv
    hkru                => gdp%gdr_i_ch%hkru
    hkrv                => gdp%gdr_i_ch%hkrv
    hrms                => gdp%gdr_i_ch%hrms
    hu                  => gdp%gdr_i_ch%hu
    hu0                 => gdp%gdr_i_ch%hu0
    hv                  => gdp%gdr_i_ch%hv
    hv0                 => gdp%gdr_i_ch%hv0
    hydrbc              => gdp%gdr_i_ch%hydrbc
    omega               => gdp%gdr_i_ch%omega
    patm                => gdp%gdr_i_ch%patm
    porosu              => gdp%gdr_i_ch%porosu
    porosv              => gdp%gdr_i_ch%porosv
    precip              => gdp%gdr_i_ch%precip
    procbc              => gdp%gdr_i_ch%procbc
    pship               => gdp%gdr_i_ch%pship
    qsrcrt              => gdp%gdr_i_ch%qsrcrt
    qtfrac              => gdp%gdr_i_ch%qtfrac
    qtfrct              => gdp%gdr_i_ch%qtfrct
    qtfrt2              => gdp%gdr_i_ch%qtfrt2
    qu                  => gdp%gdr_i_ch%qu
    qv                  => gdp%gdr_i_ch%qv
    qxk                 => gdp%gdr_i_ch%qxk
    qyk                 => gdp%gdr_i_ch%qyk
    qzk                 => gdp%gdr_i_ch%qzk
    r0                  => gdp%gdr_i_ch%r0
    r1                  => gdp%gdr_i_ch%r1
    rbnd                => gdp%gdr_i_ch%rbnd
    rettim              => gdp%gdr_i_ch%rettim
    rho                 => gdp%gdr_i_ch%rho
    rhowat              => gdp%gdr_i_ch%rhowat
    rich                => gdp%gdr_i_ch%rich
    rint                => gdp%gdr_i_ch%rint
    rintsm              => gdp%gdr_i_ch%rintsm
    rlabda              => gdp%gdr_i_ch%rlabda
    rmneg               => gdp%gdr_i_ch%rmneg
    rnpl                => gdp%gdr_i_ch%rnpl
    rob                 => gdp%gdr_i_ch%rob
    rthbnd              => gdp%gdr_i_ch%rthbnd
    rtu2d0              => gdp%gdr_i_ch%rtu2d0
    rtu2d1              => gdp%gdr_i_ch%rtu2d1
    rtubnd              => gdp%gdr_i_ch%rtubnd
    rtur0               => gdp%gdr_i_ch%rtur0
    rtur1               => gdp%gdr_i_ch%rtur1
    rxx                 => gdp%gdr_i_ch%rxx
    rxy                 => gdp%gdr_i_ch%rxy
    ryy                 => gdp%gdr_i_ch%ryy
    rxz                 => gdp%gdr_i_ch%rxz
    ryz                 => gdp%gdr_i_ch%ryz
    s0                  => gdp%gdr_i_ch%s0
    s1                  => gdp%gdr_i_ch%s1
    sig                 => gdp%gdr_i_ch%sig
    sigdif              => gdp%gdr_i_ch%sigdif
    sigmol              => gdp%gdr_i_ch%sigmol
    sink                => gdp%gdr_i_ch%sink
    sour                => gdp%gdr_i_ch%sour
    stil                => gdp%gdr_i_ch%stil
    sumrho              => gdp%gdr_i_ch%sumrho
    taubmx              => gdp%gdr_i_ch%taubmx
    taubpu              => gdp%gdr_i_ch%taubpu
    taubpv              => gdp%gdr_i_ch%taubpv
    taubsu              => gdp%gdr_i_ch%taubsu
    taubsv              => gdp%gdr_i_ch%taubsv
    teta                => gdp%gdr_i_ch%teta
    tgarkt              => gdp%gdr_i_ch%tgarkt
    tgarkx              => gdp%gdr_i_ch%tgarkx
    tgarnp              => gdp%gdr_i_ch%tgarnp
    thick               => gdp%gdr_i_ch%thick
    thtim               => gdp%gdr_i_ch%thtim
    tkedis              => gdp%gdr_i_ch%tkedis
    tkepro              => gdp%gdr_i_ch%tkepro
    tp                  => gdp%gdr_i_ch%tp
    u0                  => gdp%gdr_i_ch%u0
    u1                  => gdp%gdr_i_ch%u1
    ubrlsu              => gdp%gdr_i_ch%ubrlsu
    ubrlsv              => gdp%gdr_i_ch%ubrlsv
    umdis               => gdp%gdr_i_ch%umdis
    umean               => gdp%gdr_i_ch%umean
    umnflc              => gdp%gdr_i_ch%umnflc
    umnldf              => gdp%gdr_i_ch%umnldf
    uorb                => gdp%gdr_i_ch%uorb
    ubot                => gdp%gdr_i_ch%ubot
    v0                  => gdp%gdr_i_ch%v0
    v1                  => gdp%gdr_i_ch%v1
    vicuv               => gdp%gdr_i_ch%vicuv
    vicww               => gdp%gdr_i_ch%vicww
    vmdis               => gdp%gdr_i_ch%vmdis
    vmean               => gdp%gdr_i_ch%vmean
    vmnflc              => gdp%gdr_i_ch%vmnflc
    vmnldf              => gdp%gdr_i_ch%vmnldf
    vnu2d               => gdp%gdr_i_ch%vnu2d
    vnu3d               => gdp%gdr_i_ch%vnu3d
    voldis              => gdp%gdr_i_ch%voldis
    volum0              => gdp%gdr_i_ch%volum0
    volum1              => gdp%gdr_i_ch%volum1
    vortic              => gdp%gdr_i_ch%vortic
    w1                  => gdp%gdr_i_ch%w1
    w10mag              => gdp%gdr_i_ch%w10mag
    windcd              => gdp%gdr_i_ch%windcd
    windsu              => gdp%gdr_i_ch%windsu
    windsv              => gdp%gdr_i_ch%windsv
    windu               => gdp%gdr_i_ch%windu
    windv               => gdp%gdr_i_ch%windv
    wphy                => gdp%gdr_i_ch%wphy
    ws                  => gdp%gdr_i_ch%ws
    wsu                 => gdp%gdr_i_ch%wsu
    wsv                 => gdp%gdr_i_ch%wsv
    wsbodyu             => gdp%gdr_i_ch%wsbodyu
    wsbodyv             => gdp%gdr_i_ch%wsbodyv
    xcor                => gdp%gdr_i_ch%xcor
    xydro               => gdp%gdr_i_ch%xydro
    xz                  => gdp%gdr_i_ch%xz
    ycor                => gdp%gdr_i_ch%ycor
    yz                  => gdp%gdr_i_ch%yz
    z0ucur              => gdp%gdr_i_ch%z0ucur
    z0vcur              => gdp%gdr_i_ch%z0vcur
    z0urou              => gdp%gdr_i_ch%z0urou
    z0vrou              => gdp%gdr_i_ch%z0vrou
    zstep               => gdp%gdr_i_ch%zstep
    drhodx              => gdp%gdr_i_ch%drhodx
    drhody              => gdp%gdr_i_ch%drhody
    dzs0                => gdp%gdr_i_ch%dzs0
    dzs1                => gdp%gdr_i_ch%dzs1
    dzu0                => gdp%gdr_i_ch%dzu0
    dzu1                => gdp%gdr_i_ch%dzu1
    dzv0                => gdp%gdr_i_ch%dzv0
    dzv1                => gdp%gdr_i_ch%dzv1
    res                 => gdp%gdr_i_ch%res
    fact                => gdp%gdr_i_ch%fact
    rl                  => gdp%gdr_i_ch%rl
    xj                  => gdp%gdr_i_ch%xj
    p1                  => gdp%gdr_i_ch%p1
    p0                  => gdp%gdr_i_ch%p0
    p00                 => gdp%gdr_i_ch%p00
    pnhcor              => gdp%gdr_i_ch%pnhcor
    w0                  => gdp%gdr_i_ch%w0
    s00                 => gdp%gdr_i_ch%s00
    guz                 => gdp%gdr_i_ch%guz
    gvz                 => gdp%gdr_i_ch%gvz
    gud                 => gdp%gdr_i_ch%gud
    gvd                 => gdp%gdr_i_ch%gvd
    gsqiu               => gdp%gdr_i_ch%gsqiu
    gsqiv               => gdp%gdr_i_ch%gsqiv
    ibuff               => gdp%gdr_i_ch%ibuff
    idifu               => gdp%gdr_i_ch%idifu
    irocol              => gdp%gdr_i_ch%irocol
    itbcc               => gdp%gdr_i_ch%itbcc
    itbct               => gdp%gdr_i_ch%itbct
    itdis               => gdp%gdr_i_ch%itdis
    itdro               => gdp%gdr_i_ch%itdro
    kadu                => gdp%gdr_i_ch%kadu
    kadv                => gdp%gdr_i_ch%kadv
    kcs                 => gdp%gdr_i_ch%kcs
    kcu                 => gdp%gdr_i_ch%kcu
    kcv                 => gdp%gdr_i_ch%kcv
    kfs                 => gdp%gdr_i_ch%kfs
    kfu                 => gdp%gdr_i_ch%kfu
    kfv                 => gdp%gdr_i_ch%kfv
    kspu                => gdp%gdr_i_ch%kspu
    kspv                => gdp%gdr_i_ch%kspv
    kstp                => gdp%gdr_i_ch%kstp
    mnbar               => gdp%gdr_i_ch%mnbar
    mnbnd               => gdp%gdr_i_ch%mnbnd
    mndro               => gdp%gdr_i_ch%mndro
    mnksrc              => gdp%gdr_i_ch%mnksrc
    nob                 => gdp%gdr_i_ch%nob
    kcshyd              => gdp%gdr_i_ch%kcshyd
    kfumin              => gdp%gdr_i_ch%kfumin
    kfvmin              => gdp%gdr_i_ch%kfvmin
    kfsmin              => gdp%gdr_i_ch%kfsmin
    kfumax              => gdp%gdr_i_ch%kfumax
    kfvmax              => gdp%gdr_i_ch%kfvmax
    kfsmax              => gdp%gdr_i_ch%kfsmax
    kfumx0              => gdp%gdr_i_ch%kfumx0
    kfvmx0              => gdp%gdr_i_ch%kfvmx0
    kfsmx0              => gdp%gdr_i_ch%kfsmx0
    kfsmn0              => gdp%gdr_i_ch%kfsmn0
    kfumn0              => gdp%gdr_i_ch%kfumn0
    kfvmn0              => gdp%gdr_i_ch%kfvmn0
    kfsz0               => gdp%gdr_i_ch%kfsz0
    kfuz0               => gdp%gdr_i_ch%kfuz0
    kfvz0               => gdp%gdr_i_ch%kfvz0
    kfsz1               => gdp%gdr_i_ch%kfsz1
    kfuz1               => gdp%gdr_i_ch%kfuz1
    kfvz1               => gdp%gdr_i_ch%kfvz1
    kcscut              => gdp%gdr_i_ch%kcscut
    kcu45               => gdp%gdr_i_ch%kcu45
    kcv45               => gdp%gdr_i_ch%kcv45
    disint              => gdp%gdr_i_ch%disint
    dismmt              => gdp%gdr_i_ch%dismmt
    nambnd              => gdp%gdr_i_ch%nambnd
    namsrc              => gdp%gdr_i_ch%namsrc
    tprofc              => gdp%gdr_i_ch%tprofc
    tprofu              => gdp%gdr_i_ch%tprofu
    rtcact              => gdp%gdrtc%rtcact
    rhosol              => gdp%gdsedpar%rhosol
    ifirst              => gdp%gdtrisol%ifirst
    nubnd               => gdp%gdtrisol%nubnd
    ubnd                => gdp%gdtrisol%ubnd
    windxt              => gdp%gdtrisol%windxt
    windyt              => gdp%gdtrisol%windyt
    windft              => gdp%gdtrisol%windft
    nprocs              => gdp%gdusrpar%nprocs
    nread               => gdp%gdusrpar%nread
    nprptr              => gdp%gdusrpar%nprptr
    rcousr              => gdp%gdusrpar%rcousr
    procs               => gdp%gdusrpar%procs
    nrcmp               => gdp%gdtfzeta%nrcmp
    dryrun              => gdp%gdtmpfil%dryrun
    typbnd              => gdp%gdr_i_ch%typbnd
    modify_dzsuv        => gdp%gdzmodel%modify_dzsuv
    ztbml               => gdp%gdzmodel%ztbml
    ztbml_upd_r1        => gdp%gdzmodel%ztbml_upd_r1
    flmd2l              => gdp%gdprocs%flmd2l
    depchg              => gdp%gdr_i_ch%depchg
    sbuu                => gdp%gdr_i_ch%sbuu
    sbvv                => gdp%gdr_i_ch%sbvv
    seddif              => gdp%gdr_i_ch%seddif
    wstau               => gdp%gdr_i_ch%wstau
    entr                => gdp%gdr_i_ch%entr
    bedupd              => gdp%gdmorpar%bedupd
    eqmbcsand           => gdp%gdmorpar%eqmbcsand
    eqmbcmud            => gdp%gdmorpar%eqmbcmud
    iti_sedtrans        => gdp%gdmorpar%iti_sedtrans
    sedtyp              => gdp%gdsedpar%sedtyp
    !
    icx     = 0
    icy     = 0
    nmaxddb = nmax + 2*gdp%d%ddbound
    stage   = 'both'
    !
    ! DD code added:
    !
    !
    !     D3dFlowMap_InitTimeStep: set up virtual points for next time step
    !
    nhystp = nxtstp(d3dflow_inittimestep, gdp)
    !
    ! DD code added end
    !
    !
    ! ********************** SET USER DEF FUNCT PARAMETERS ****************
    !
    call timer_start(timer_trisol_ini, gdp)
    if (ifirst == 1) then
       !
       ! initialisation of user defined parameters and array pointers
       !        if user def. function not requested then flag is set 0 and
       !        array pointers are work array pointer
       !
       nubnd  = 0
       ubnd   = wrkb1
       windxt = 0.0
       windyt = 0.0
       windft = 0.0
       do n = 1, nprocs
          if (procs(n) == 'bc turbulence model ') then
             nubnd = nread(n)
             if (nubnd /= 0) ubnd = nprptr(1, n)
          endif
          if (procs(n) == 'particle wind factor') then
             if (nread(n) /= 0) then
                nreal  = nprptr(1, n)
                windxt = rcousr(nreal)
                windyt = rcousr(nreal + 1)
                windft = rcousr(nreal + 2)
             endif
          endif
       enddo
       !
       if (bubble) then
          !
          ! Fill bubble screens initially;
          !
          if (nxbub > 0) then
             flbub = .true.
             icx   = nmaxddb
             icy   = 1
             call cnvbub(kmax      ,nsrcd     ,nsrc      ,nbub      ,nxbub     , &
                       & icx       ,icy       ,ch(namsrc),i(mnksrc) , &
                       & r(disch)  ,gdp       )
             icx = nmaxddb
             icy = 1
             call z_disbub(kmax      ,nsrcd     ,nsrc      ,nxbub     , &
                         & lstsci    ,lstsc     ,icx       ,icy       , &
                         & ch(namsrc),i(mnksrc) , &
                         & i(kfsmn0) ,i(kfsmx0) ,r(gsqs)   ,r(disinp) , &
                         & r(sour)   ,r(sink)   ,r(xcor)   ,r(ycor)   , &
                         & r(r0)     ,r(disch)  ,r(rint)   ,r(sig)    , &
                         & r(s0)     ,d(dps)    ,ifirst    ,gdp       )
          endif
       endif
       !
       ifirst = 0
       !
    endif
    call timer_stop(timer_trisol_ini, gdp)
    !
    !+++++++++++++++++++++++ START OF COMPLETE TIMESTEP++++++++++++++++++++
    !
    timnow = timnow + 1.0_fp
    call psemnefis
    call setcurrentdatetime(timnow, gdp)
    call vsemnefis
    !
    ! Set time dependent data
    ! Some of the new features are not yet supported in ZMODEL
    ! (see routine CHKZMOD)!!!
    !
    if (rtcact /= noRTC) then
       call rtc_comm_get((nst+1)*dtsec, r(cbuvrt), nsluv, r(qsrcrt) , nsrc, gdp)
    endif
    if (kc > 0 .or. nrcmp > 0) then
       call timer_start(timer_nodal_factor, gdp)
       call update_nodal_factors(timnow, kc, ntof, nto, kcd, r(hydrbc), r(omega), gdp)
       call timer_stop(timer_nodal_factor, gdp)
    endif
    !
    if (wind) then
       !
       ! call incwnd is replaced by a call to the meteo module
       !
       call timer_start(timer_incmeteo, gdp)
       call incmeteo(timhr     , grdang   , &
                   & r(windu) , r(windv) , r(patm)  ,    &
                   & i(kcs)   , r(alfas) , r(windcd),    &
                   & r(windsu), r(windsv), r(w10mag), gdp)
       call timer_stop(timer_incmeteo, gdp)
    endif
    if (nto > 0) then
       !
       ! Get online external (i.e. 1D) boundary conditions
       !
       if (sbkol) then
          call timer_start(timer_wait, gdp)
          write(*,*) '1st half time step: d3s_get_discharges'
          call d3s_get_discharges(ntstep, nto, kcd, r(hydrbc))
          write(*,*) '1st half time step: d3s_get_discharges DONE'
          call timer_stop(timer_wait, gdp)
       endif
    endif       
    !
    ! Boundary conditions; hydrodynamic conditions
    ! Incbc must be called even if nto<=0 due to synchronisation when running parallel
    !
    call timer_start(timer_incbc, gdp)
    call incbc(lundia    ,timnow    ,zmodel    ,nmax      ,mmax      , &
             & kmax      ,kcd       ,nto       ,ntof      ,ntoq      , &
             & kc        ,nrob      ,noroco    , &
             & ch(tprofu),i(itbct)  ,i(mnbnd)  ,i(nob)    ,i(kfumn0) , &
             & i(kfumx0) ,i(kfvmn0) ,i(kfvmx0) ,r(hydrbc) ,r(circ2d) , &
             & r(circ3d) ,r(patm)   ,r(guu)    ,r(gvv)    , &
             & r(hu)     ,r(hv)     ,r(omega)  ,r(alpha)  , &
             & r(z0urou) ,r(z0vrou) ,r(qxk)    ,r(qyk)    ,r(s0)     , &
             & r(u0)     ,r(v0)     ,r(grmasu) ,r(grmasv) ,r(cfurou) , &
             & r(cfvrou) ,r(qtfrac) ,r(qtfrct) ,r(qtfrt2) ,r(thick)  , &
             & r(dzu0)   ,r(dzv0)   ,r(zwork)  ,i(kcu)    ,i(kcv)    , &
             & i(kfu)    ,i(kfv)    ,i(kcs)    ,timhr     ,ch(nambnd), & 
             & ch(typbnd),gdp       )
    call timer_stop(timer_incbc, gdp)
    !
    ! Constituent (excl. turbulence & secondary flow)
    !
    if (nto > 0) then
       if (lstsc > 0) then
          call timer_start(timer_incbcc, gdp)
          call incbcc(lundia    ,timnow    ,zmodel    ,nmax      ,mmax      , &
                    & kmax      ,nto       ,nrob      ,lstsc     ,noroco    , &
                    & ch(tprofc),i(itbcc)  ,i(mnbnd)  ,i(nob)    ,i(kstp)   , &
                    & i(kfsmn0) ,i(kfsmx0) ,r(rob)    ,r(rbnd)   ,r(guu)    , &
                    & r(gvv)    ,d(dps)    ,r(s0)     ,r(sig)    ,r(procbc) , &
                    & r(zstep)  ,r(dzs0)   ,r(sig)    ,gdp       )
          call timer_stop(timer_incbcc, gdp)
       endif
    endif
    !
    ! Discharges; constituent (excl. turbulence & secondary flow)
    !
    if (nsrcd > 0) then
       icx = nmaxddb
       icy = 1
       call timer_start(timer_incdis, gdp)
       call incdis(lundia    ,sferic    ,grdang    ,timnow    ,nsrcd     , &
                 & lstsc     ,lstsci    ,jstart    ,nmmaxj    ,kmax      , &
                 & icx       ,icy       ,i(kfsmn0) ,i(kfsmx0) , &
                 & ch(disint),ch(dismmt),i(itdis)  ,i(kcu)    ,i(kcv)    , &
                 & i(kfs)    ,i(ibuff)  ,i(mnksrc) ,r(alfas)  ,r(xcor)   , &
                 & r(ycor)   ,r(dpd)    ,r(disch)  ,r(voldis) , &
                 & r(disch0) ,r(disch1) ,r(rint)   ,r(rint0)  ,r(rint1)  , &
                 & r(umdis)  ,r(umdis0) ,r(umdis1) ,r(vmdis)  ,r(vmdis0) , &                 
                 & r(vmdis1) ,bubble    ,r(r0)     ,r(thick)  ,r(zwork)  , &
                 & r(dzs0)   ,d(dps)    ,r(s0)     ,r(qsrcrt) ,gdp       )
       call timer_stop(timer_incdis, gdp)
       !
       ! Computation of discharge in case of culverts
       !
       if (culvert) then
           call timer_start(timer_culver, gdp)
           call culver(icx       ,icy       ,kmax      ,nsrcd     ,i(kfs)    , &
                     & i(kfsmx0) ,i(kfsmn0) ,i(mnksrc) ,r(disch)  ,d(dps)    , &
                     & r(s0)     ,r(sig)    ,r(thick)  ,r(voldis) ,timsec    , &
                     & r(sumrho) ,gdp       )
           call timer_stop(timer_culver, gdp)
       endif
    endif
    !
    ! 5 heat modules, input depends on value of KTEMP
    !
    call timer_start(timer_trisol_heat, gdp)
    if (ktemp > 0) then
       call inctem(ktemp     ,timnow    ,temint    ,gdp       )
    endif
    !
    ! Rain/evaporation as time dependent input
    !
    if (keva > 0) then
       call inceva(timnow    ,evaint    ,jstart    ,nmmaxj    ,nmmax     , &
                 & r(evap)   ,r(precip) ,gdp       )
    endif
    call timer_stop(timer_trisol_heat, gdp)
    !
    ! skip calculation in case of dryrun
    !
    if (.not. dryrun) then
       !
       ! Input values depend on local situations (e.g. floating structures)
       ! WARNING: structures filter w.r.t. radiation is handled in HEATU
       !
       icx = nmaxddb
       icy = 1
       call timer_start(timer_filterstr, gdp)
       call filterstructures(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                           & icy       ,i(kspu)   ,i(kspv)   ,r(evap)   ,r(windsu) , &
                           & r(windsv) ,r(w10mag) ,r(uorb)   ,r(tp)     ,r(teta)   , &
                           & r(dis)    ,r(wsu)    ,r(wsv)    ,r(grmasu) ,r(grmasv) , &
                           & r(df)     ,gdp       )
       call timer_stop(timer_filterstr, gdp)
       !
       ! Computation proceeds in X direction
       !
       !
       ! Compute densities
       !
       if (lsal>0 .or. ltem>0 .or. (lsed>0 .and. densin) .and. nst<itdiag) then
          !
          ! note: DENS may still be called if sal or tem even if densin = false
          !
          ifirst_dens = 0
          call timer_start(timer_dens, gdp)
          call dens(jstart    ,nmmaxj    ,nmmax     ,kmax       ,lstsci    , &
                  & lsal      ,ltem      ,lsed      ,i(kcs)     ,saleqs    ,temeqs    , &
                  & densin    ,zmodel    ,r(thick)  ,r(r0)      ,r(rho)    , &
                  & r(sumrho) ,r(rhowat) ,rhosol    ,ifirst_dens,gdp       )
          call timer_stop(timer_dens, gdp)
          !
          ! z_DENGRA: compute DRHODX/DRHODY terms (only in Z-MODEL)
          !
          icx = nmaxddb
          icy = 1
          call timer_start(timer_dengra, gdp)
          call z_dengra(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                      & icy       ,i(kfsz0)  ,i(kfumn0) ,i(kfumx0) ,i(kfvmn0) , &
                      & i(kfvmx0) ,i(kfu)    ,i(kfv)    , &
                      & r(rho)    ,r(gvu)    ,r(guv)    ,r(drhodx) , &
                      & r(drhody) ,r(dzu0)   ,r(dzv0)   ,gdp       )
          call timer_stop(timer_dengra, gdp)
       endif
       !
       ! Source and sink terms
       ! Initial SOUR and SINK are set to 0.
       ! For temperature, evaporation is added to SINK and rain is added to
       ! SOUR. For conservative constituents, a decay rate <> 0. is added
       ! to SINK (except for sediment).
       !
       ! Decay rate (array SINK) in Zmodel is based on old Volumes (VOLUM0)
       !
       call timer_start(timer_sousin, gdp)
       call sousin(jstart    ,nmmaxj    ,nmmax     ,kmax      ,lstsci    , &
                 & lstsc     ,lsal      ,ktemp     ,ltem      ,lsts      , &
                 & i(kfs)    ,i(kfsmn0) ,i(kfsmx0) ,r(gsqs)   ,r(thick)  , &
                 & r(s0)     ,d(dps)    ,r(volum0) ,r(sour)   ,r(sink)   , &
                 & r(evap)   ,r(precip) ,r(decay)  ,i(kcs)    ,gdp       )
       call timer_stop(timer_sousin, gdp)
       !
       if (bubble) then
          call timer_start(timer_trisol_rest, gdp)
          !
          ! Fill DISCH array for bubble screens;
          !
          if (nsrcd > 0) then
             if (nxbub > 0) then
                icx = nmaxddb
                icy = 1
                call cnvbub(kmax      ,nsrcd     ,nsrc      ,nbub      ,nxbub     , &
                          & icx       ,icy       ,ch(namsrc),i(mnksrc) , &
                          & r(disch)  ,gdp       )
             endif
             !
             ! Fill SOUR and SINK arrays  for bubble screens;
             !
             if (nxbub > 0) then
                icx = nmaxddb
                icy = 1
                call z_disbub(kmax      ,nsrcd     ,nsrc      ,nxbub     , &
                            & lstsci    ,lstsc     ,icx       ,icy       , &
                            & ch(namsrc),i(mnksrc) , &
                            & i(kfsmn0) ,i(kfsmx0) ,r(gsqs)   ,r(disinp) , &
                            & r(sour)   ,r(sink)   ,r(xcor)   ,r(ycor)   , &
                            & r(r0)     ,r(disch)  ,r(rint)   ,r(sig)    , &
                            & r(s0)     ,d(dps)    ,ifirst    ,gdp       )
             endif
          endif
          call timer_stop(timer_trisol_rest, gdp)
       endif
       !
       ! Eddy viscosity and diffusivity
       ! Use SIG for array ZK
       !
       call timer_start(timer_turbulence, gdp)
       !
       ! The velocities are corrected for mass flux and
       ! temporary set in WRKB3 (UEUL) and WRKB4
       ! (VEUL) these are used in Z_TURCLO
       !
       icx = nmaxddb
       icy = 1
       call timer_start(timer_euler, gdp)
       call euler(jstart    ,nmmax     ,nmmaxj    ,kmax      ,icx       , &
                & i(kcu)    ,i(kcv)    ,i(kfu)    ,i(kfv)    ,i(kfumx0) , &
                & i(kfumn0) ,i(kfvmx0) ,i(kfvmn0) ,r(dzu0)   ,r(dzv0)   , &
                & r(u0)     ,r(wrkb3)  ,r(v0)     ,r(wrkb4)  , &
                & r(grmasu) ,r(grmasv) ,r(hu)     ,r(hv)     , &
                & r(tp)     ,r(hrms)   ,r(sig)    ,r(thick)  ,r(teta)   , &
                & r(grmsur) ,r(grmsvr) ,r(grfacu) ,r(grfacv) ,gdp       )
       call timer_stop(timer_euler, gdp)
       !
       icx = nmaxddb
       icy = 1
       call timer_start(timer_turclo, gdp)
       call z_turclo(jstart    ,nmmaxj    ,nmmax     ,kmax      ,ltur      , &
                   & icx       ,icy       ,tkemod    , &
                   & i(kcs)    ,i(kfu)    ,i(kfv)    ,i(kfs)    ,i(kfuz0)  , &
                   & i(kfvz0)  ,i(kfsz0)  ,i(kfumn0) ,i(kfumx0) ,i(kfvmn0) , &
                   & i(kfvmx0) ,i(kfsmn0) ,i(kfsmx0) ,r(s0)     ,d(dps)    , &
                   & r(hu)     ,r(hv)     ,r(u0)     ,r(v0)     ,r(rtur1)  , &
                   & r(thick)  ,r(sig)    ,r(rho)    ,r(vicuv)  ,r(vicww)  , &
                   & r(dicuv)  ,r(dicww)  ,r(windsu) ,r(windsv) ,r(z0urou) , &
                   & r(z0vrou) ,r(bruvai) ,r(rich)   ,r(dudz)   ,r(dvdz)   , &
                   & r(dzu0)   ,r(dzv0)   ,r(dzs0)   ,r(sig)    ,r(wrkb3)  , &
                   & r(wrkb4)  ,gdp       )
       call timer_stop(timer_turclo, gdp)
       call timer_stop(timer_turbulence, gdp)
       !
       ! Check horizontal eddy viscosity and diffusivity
       !
       if (htur2d .or. irov>0) then
          itype = 2
          call timer_start(timer_chkvic, gdp)
          call chkvic(lundia    ,jstart    ,nmmaxj    ,nmmax     ,kmax      , &
                    & icx       ,icy       ,timnow    ,i(kfs)    ,i(kfu)    , &
                    & i(kfv)    ,i(kcs)    ,lstsci    ,r(guv)    ,r(gvu)    , &
                    & r(vicuv)  ,r(dicuv)  ,itype     ,i(kfsmn0) ,i(kfsmx0) , &
                    & gdp       )
          call timer_stop(timer_chkvic, gdp)
       endif
       !
       ! Optional IWE model only for IWEFLG=.true. and NST <= ITIWEC
       ! Afterwards update ITIWEC
       !
       if (iweflg) then
          if (nst <= itiwec) then
             itiwec = itiwec + itiwei
             call timer_start(timer_iwe00, gdp)
             call iwe_00(nmax      ,mmax      ,kmax      ,kmxdt     ,npiwe     , &
                       & ltur      ,lundia    ,r(w10mag) ,r(s0)     ,d(dps)    , &
                       & r(u0)     ,r(v0)     ,r(dudz)   ,r(dvdz)   ,r(windsu) , &
                       & r(windsv) ,r(taubpu) ,r(taubpv) ,r(sig)    ,r(thick)  , &
                       & r(rich)   ,r(bruvai) ,r(rtur0)  ,r(vicww)  ,i(kfs)    , &
                       & i(kfu)    ,i(kfv)    ,r(tgarkx) ,r(tgarkt) ,r(tgarnp) , &
                       & r(tkepro) ,r(tkedis) ,r(fuiwe)  ,r(fviwe)  ,gdp       )
             call timer_stop(timer_iwe00, gdp)
          endif
       endif
       !
       ! Calculation of the water elevation generated by Tide Generating
       ! Forces.
       ! Use array WRKB17 to store the water elevation due to this force
       !
       call timer_start(timer_tfzeta, gdp)
       call tfzeta(timnow    ,nmax      ,mmax      ,r(wrkb17) ,r(xz)     , &
                 & r(yz)     ,gdp       )
       call timer_stop(timer_tfzeta, gdp)
       !
       ! Define KSPU/V and POROSU/V for CDW type of structure (fixed gate with
       ! - OPTIONALLY - layers with enhanced friction below it).
       ! Array SIG is passed on twice; the first one represents the SIGma coordinates
       ! (zmodel == .FALSE.) the second represent the Z-coordinates (zmodel == .TRUE.).
       ! This is a trick to enable CDWKAD routine to be used for both coordinate types.
       ! Work array ZWORK has the length of 5*KMAX
       !
       if (cdwstruct) then
          call timer_start(timer_cdwkad, gdp)
          call cdwkad(nmmax     ,kmax      ,zmodel    ,i(kspu)   ,i(kfsmx0) , &
                    & i(kfsmn0) ,i(kfumx0) ,i(kfumn0) ,r(sig)    ,r(thick)  , &
                    & r(sig)    ,r(zwork)  ,r(zwork+kmax)  ,r(zwork+2*kmax) , &
                    & r(dpu)    ,r(hu)     ,r(dzu0)   ,r(porosu) ,r(ubrlsu) , &
                    & r(cdwztu) ,r(cdwzbu) ,r(cdwlsu) ,gdp       )
          call cdwkad(nmmax     ,kmax      ,zmodel    ,i(kspv)   ,i(kfsmx0) , &
                   & i(kfsmn0) ,i(kfvmx0) ,i(kfvmn0) ,r(sig)    ,r(thick)  , &
                    & r(sig)    ,r(zwork)  ,r(zwork+kmax)  ,r(zwork+2*kmax) , &
                    & r(dpv)    ,r(hv)     ,r(dzv0)   ,r(porosv) ,r(ubrlsv) , &
                    & r(cdwztv) ,r(cdwzbv) ,r(cdwlsv) ,gdp       )
          call timer_stop(timer_cdwkad, gdp)
       endif
       !
       ! Define KADU/V for hydrodynamics
       !
       call timer_start(timer_hydkad, gdp)
       call hydkad(jstart    ,nmmaxj    ,nmmax     ,kmax      ,i(kspu)   , &
                 & i(kspv)   ,i(kadu)   ,i(kadv)   ,gdp       )
       call timer_stop(timer_hydkad, gdp)
       !
       ! Calculate for all barrier points :
       ! - open or closed in mask arrays KSPU and KSPV
       ! - extra energy losses due to quadratic friction
       ! as a function of gate height and waterdepth
       !
       if (nsluv > 0) then
          call timer_start(timer_updbar, gdp)
          call updbar(nsluv     ,i(mnbar)  ,r(cbuv)   ,r(cbuvrt) ,nmax      , &
                    & mmax      ,kmax      ,r(thick)  ,i(kspu)   ,i(kspv)   , &
                    & i(kfumn0) ,i(kfumx0) ,i(kfvmn0) ,i(kfvmx0) ,r(ubrlsu) , &
                    & r(ubrlsv) ,r(hu)     ,r(hv)     ,r(dpu)    ,r(dpv)    , &
                    & r(sig)    ,r(zwork)  ,gdp       )
          call timer_stop(timer_updbar, gdp)
       endif
       !
       ! Computation of U1 and V1, i.e. evaluate momentum equations with explicit
       ! pressure term (water level gradient and non-hydrostatic pressure)
       !
       call z_predict_nhfull(jstart    ,nmmaxj    ,nmmax     ,kmax      , &
                           & mmax      ,nmax      ,nfltyp    ,nst       , &
                           & nocol     ,norow     ,nsrc      ,ch(dismmt),i(irocol) , &
                           & i(mnksrc) ,i(kfu)    ,i(kfv)    ,i(kfs)    ,            &
                           & i(kspu)   ,i(kspv)   ,i(kadu)   ,i(kadv)   ,i(kcs)    , &
                           & i(kcu)    ,i(kcv)    ,i(kfsmn0) ,i(kfsmx0) , &
                           & i(kfumn0) ,i(kfumx0) ,i(kfvmn0) , &
                           & i(kfvmx0) ,i(kfuz0)  ,i(kfvz0)  ,i(kfsz0)  ,i(kcu45)  , &
                           & i(kcv45)  ,i(kcscut) ,r(s0)     ,r(w0)     , &
                           & r(u0)     ,r(u1)     ,r(v0)     ,r(v1)     ,r(hu)     , &
                           & r(hv)     ,r(thick)  ,r(umean)  ,r(ubrlsu) ,r(ubrlsv) , &
                           & r(vmean)  ,r(dpu)    ,r(dpv)    ,d(dps)    ,r(dzu0)   , &
                           & r(dzv0)   ,r(dzs0)   ,r(qxk)    ,r(qyk)    ,r(circ2d) , &
                           & r(circ3d) ,r(drhodx) ,r(drhody) ,r(disch)  ,r(umdis)  , &
                           & r(vmdis)  ,r(wsu)    ,r(wsv)    ,r(dfu)    ,r(dfv)    , &
                           & r(deltau) ,r(deltav) ,r(tp)     ,r(rlabda) ,r(wsbodyu), &
                           & r(wsbodyv),r(fxw)    ,r(fyw)    ,r(gud)    ,r(gvd)    , &
                           & r(guu)    ,r(guv)    ,r(gvv)    ,r(gvu)    ,r(guz)    , &
                           & r(gvz)    ,r(gsqs)   ,r(gsqiu)  ,r(gsqiv)  ,r(taubpu) , &
                           & r(taubpv) ,r(taubsu) ,r(taubsv) ,r(vicuv)  ,r(vnu2d)  , &
                           & r(vicww)  ,r(rxx)    ,r(rxy)    ,r(ryy)    ,r(windsu) , &
                           & r(windsv) ,r(patm)   ,r(fcorio) ,r(wrkb17) ,r(wrkb1)  , &
                           & r(wrkb2)  ,r(wrkb3)  ,r(wrkb4)  ,r(wrkb5)  ,r(wrkb6)  , &
                           & r(wrkb7)  ,r(wrkb8)  ,r(wrkb9)  ,r(wrkb10) ,r(wrkb11) , &
                           & r(wrkb12) ,r(wrkb13) ,r(wrkb14) , &
                           & r(sig)    ,r(p0)     ,r(crbc)   , &
                           & r(pship)  ,r(diapl)  ,r(rnpl)   ,r(cfurou) ,r(cfvrou) , &
                           & r(precip) ,gdp       )
       !
       ! Non hydrostatic pressure
       ! w0 = non-hydrostatic vertical velocity after complete time step
       !
       if (nonhyd) then
          icx = nmaxddb
          icy = 1
          !
          ! Computation of W1 evaluate momentum equations with explicit
          ! pressure term (non-hydrostaic pressure)
          !
          if (momsol == 'finvol') then
             call z_vermom_finvol(nmmax     ,kmax      ,icx       ,icy       ,r(u0)     , &
                                & r(v0)     ,r(w0)     ,r(vicww)  ,r(rxz)    ,r(ryz)    , &
                                & r(guu)    ,r(gvv)    ,r(guv)    ,r(gvu)    ,r(guz)    , &
                                & r(gvz)    ,r(gsqs)   ,i(kfs)    ,i(kcs)    ,r(wrkb1)  ,r(wrkb2)  , &
                                & r(wrkb3)  ,r(wrkb4)  ,i(kfuz0)  ,i(kfvz0)  ,i(kfsz0)  , &
                                & i(kfsmn0) ,i(kfsmx0) ,i(kcshyd) ,r(dzs0)   ,r(dzu0)   , &
                                & r(dzv0)   ,r(w1)     ,r(p0)     ,r(sig)    ,gdp       )
          else
             call z_vermom_nhfull(nmmax     ,kmax      ,icx       ,icy       ,r(u0)     , &
                                & r(v0)     ,r(w0)     ,r(vicww)  ,r(rxz)    ,r(ryz)    , &
                                & r(guu)    ,r(gvv)    ,r(guv)    ,r(gvu)    ,i(kfs)    , &
                                & i(kcs)    ,r(wrkb1)  ,r(wrkb2)  ,r(wrkb3)  ,r(wrkb4)  , &
                                & r(wrkb7)  ,r(wrkb8)  ,r(wrkb9)  ,r(wrkb10) ,r(wrkb5)  ,r(wrkb6) , &
                                & i(kfuz0)  ,i(kfvz0)  ,i(kfsz0)  ,i(kfsmn0) ,i(kfsmx0) , &
                                & i(kcshyd) ,r(w1)     ,r(p0)     ,r(sig)    ,nst       , &
                                & gdp       )
          endif
          !
          ! pressure correction step non-hydrostatic pressure and water levels
          ! SIG-array contains layer positions ZK!!!
          ! Added work arrays wrkb15 to wrkb18 for use in BiCGSTAB, Ullmann 21/02/2008.
          !
          icx = nmaxddb
          icy = 1
          call z_hydpres_nhfull(mmax      ,nmax      ,jstart    ,nmmaxj    ,nmmax     , &
                              & kmax      ,nst       ,icx       ,icy       ,nsrc      , &
                              & norow     ,nocol     ,i(irocol) ,r(s1)     ,r(s0 )    , &
                              & r(u1)     ,r(v1)     ,r(w1)     ,r(guu)    ,r(gvv)    , &
                              & r(u0)     ,r(v0)     ,r(w0)     ,r(circ2d) ,            &
                              & r(guv)    ,r(gvu)    ,r(gsqs)   ,r(guz)    ,r(gvz )   , &
                              & i(kfu)    ,i(kfv)    ,i(kfs)    ,i(kcs)    ,i(kfuz0)  , &
                              & i(kfvz0)  ,i(kfsz0)  ,i(kfsmn0) ,i(kfsmx0) ,i(kcshyd) , &
                              & i(mnksrc) ,r(umean)  ,r(vmean)  ,d(dps)    ,            &
                              & r(dzu0)   ,r(dzv0)   ,r(dzs0)   ,r(disch)  ,            &
                              & r(p1)     ,r(p0)     ,r(pnhcor) ,r(wrkb1)  ,r(sig)    , &
                              & r(wrkb2)  ,r(wrkb3)  ,r(wrkb4)  ,r(wrkb5)  ,r(wrkb6)  , &
                              & r(wrkb7)  ,r(wrkb8)  ,r(wrkb9)  ,r(wrkb10) ,r(wrkb11) , &
                              & r(wrkb12) ,r(wrkb13) ,r(wrkb14) ,r(wrkb15) ,r(wrkb16) , &
                              & r(wrkb17) ,r(wrkb18) ,gdp       )
          !
          ! Correct the momentum
          !
          icx = nmaxddb
          icy = 1
          call z_momcor_nhfull(nmmax     ,kmax      ,icx       ,icy       ,r(s1)     , &
                             & r(u1)     ,r(v1)     ,r(w1)     ,r(umean)  ,r(vmean)  , &
                             & r(qxk)    ,r(qyk)    ,r(qzk)    ,r(guu)    ,r(gvv)    , &
                             & r(guv)    ,r(gvu)    ,r(gsqs)   ,i(kcs)    ,i(kcshyd) , &
                             & i(kfumn0) ,i(kfumx0) ,i(kfuz0)  ,i(kfvz0)  , &
                             & i(kfvmn0) ,i(kfvmx0) ,i(kfsmn0) ,i(kfsmx0) ,i(kfsz0)  , &
                             & r(dzs0)   ,r(dzu0)   ,r(dzv0)   ,r(p1)     ,r(p0)     , &
                             & r(u0)     ,r(v0)     ,r(w0)     ,r(s0)     ,r(disch)  , &
                             & r(evap)   ,i(mnksrc) ,nsrc      ,r(wrkb1)  ,d(dps)    , &
                             & norow     ,nocol     ,i(irocol) ,r(sig)    ,            &
                             & i(kfs)    ,i(kfu)    ,i(kfv)    ,nst       ,r(precip) , &
                             & gdp       )
          if (nfltyp/=0) then
             !
             itemp = 0
             icx   = nmaxddb
             icy   = 1
             !
             ! To enable computation of new grid administration due to water level
             ! correction in Non-hydro static module
             ! itemp has been defined to call Z_DRYCHK (and set to zero) replacing idry
             ! Fluxes are based on geometry at old time level, so drying does not
             ! lead to repetition of timestep.
             ! NOTE: Z_DRYCHK is called with arrays KFUZ0, KFVZ0 and KFSZ1. 
             !       KFUZ0 and KFVZ0 are the arrays corresponding to the original geometry (S0)
             !       KFSZ1 is to be determined, corresponding to the new geometry (S1)
             !
             call z_drychk(itemp     ,jstart    ,nmmaxj    ,nmmax     ,kmax      , &
                         & nfltyp    ,icx       ,icy       ,i(kfu)    ,i(kfv)    , &
                         & i(kfs)    ,i(kcs)    ,i(kfuz0)  ,i(kfvz0)  ,i(kfsz1)  , &
                         & i(kfsmin) ,i(kfsmn0) ,i(kfsmax) ,i(kfsmx0) ,r(s1)     , &
                         & r(r0)     ,d(dps)    ,r(qxk)    ,r(qyk)    ,r(w1)     , &
                         & lstsci    ,r(dzs1)   ,r(sig)    ,nst       ,gdp       )
             !
             ! Update the layer geometry in U-velocity points based on the new water levels S1
             ! NOTE: Z_DRYCHKU is called with arrays KFUZ1, KFVZ1 and KFSZ1. 
             !       KFSZ1 has just been determined, corresponding to the new geometry (S1)
             !       KFUZ1 is to be determined, corresponding to the new geometry (S1)
             !       KFVZ1 is updated to the new geomety in the second call to Z_DRYCHKU
             !
             icx   = nmaxddb
             icy   = 1
             call z_drychku(jstart    ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                          & i(kcs)    ,i(kfu)    ,i(kcu)    ,i(kspu)   ,i(kfsmax) , &
                          & i(kfsmin) ,i(kfsz1)  ,i(kfuz1)  ,i(kfumin) ,i(kfumn0) ,i(kfumax) , &
                          & i(kfumx0) ,r(hu)     ,r(s1)     ,r(dpu)    ,d(dps)    , &
                          & r(umean)  ,r(u0)     ,r(u1)     ,r(dzu0)   ,r(dzu1)   , &
                          & r(dzs1)   ,r(sig)    ,i(kfsmx0) ,r(guu)    ,r(qxk)    , &
                          & gdp       )             
             !
             ! Update the layer geometry in V-velocity points based on the new water levels S1
             ! NOTE: Z_DRYCHKU is called with arrays KFUZ1, KFVZ1 and KFSZ1. 
             !       KFSZ1 has just been determined, corresponding to the new geometry (S1)
             !       KFVZ1 is to be determined, corresponding to the new geometry (S1)
             !       KFUZ1 was updated to the new geomety after the first half time step
             !
             icx = 1
             icy = nmaxddb
             call z_drychku(jstart    ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                          & i(kcs)    ,i(kfv)    ,i(kcv)    ,i(kspv)   ,i(kfsmax) , &
                          & i(kfsmin) ,i(kfsz1)  ,i(kfvz1)  ,i(kfvmin) ,i(kfvmn0) ,i(kfvmax) , &
                          & i(kfvmx0) ,r(hv)     ,r(s1)     ,r(dpv)    ,d(dps)    , &
                          & r(vmean)  ,r(v0)     ,r(v1)     ,r(dzv0)   ,r(dzv1)   , &
                          & r(dzs1)   ,r(sig)    ,i(kfsmx0) ,r(gvv)    ,r(qyk)    , &
                          & gdp       )
          endif
          !
          ! If requested by keyword ZTBML 
          ! (Z-model TauBottom Modified Layering)
          ! --> modify the near-bed layering to obtain smoother bottom shear stress representation in z-layer models
          !
          if (ztbml) then
             !
             ! Call with modify_dzsuv set to 1 for all 3 components, to modify both dzs1, dzu1 and dzv1
             !
             modify_dzsuv(:) = 1
             ztbml_upd_r1    = .false.
             call z_taubotmodifylayers(nmmax    ,kmax       ,lstsci       ,icx      ,icy          , & 
                                     & i(kfs)   ,i(kfsmin)  ,i(kfsmax)    ,d(dps)   ,r(dzs1)      , &
                                     & i(kfu)   ,i(kfumin)  ,i(kfumax)    ,r(dpu)   ,r(dzu1)      , &
                                     & i(kfv)   ,i(kfvmin)  ,i(kfvmax)    ,r(dpv)   ,r(dzv1)      , &
                                     & r(r1)    ,r(s0)      ,r(s1)        ,r(sig)   ,modify_dzsuv , &
                                     & dtsec    ,r(gsqs)    ,i(kfsmx0)    ,r(qzk)   ,r(umean)     , &
                                     & r(vmean) ,r(dzs0)    ,ztbml_upd_r1 ,gdp     )
          endif
          !
          ! Re-Compute Volume (Areas actually need no update) to be used in routines that computes
          ! the transport of matter (consistency with WAQ)
          !
          call z_updtvol(nmmax     ,kmax      ,i(kcs)    ,i(kcu)    ,i(kcv)    , &
                       & r(gsqs)   ,r(dzs1)   ,r(dzu0)   ,r(dzv0)   , &
                       & r(volum1) ,r(areau)  ,r(areav)  ,gdp       )
       endif
       !
       ! Calculate tau_bottom values using local values for HU and HV
       ! use work array WRKA3 for this purpose.
       ! U-point and V-point component of TAUBMX are calculated in WRKA1,
       ! resp. WRKA2 and in CALTMX defined in scalar entity TAUBMX
       ! The velocities are corrected for mass flux and temporary set in
       ! WRKB3 (U1) and WRKB4 (V1) which will be used in TAUBOT
       !
       ! CVALU0 and CVALV0 contain the actual 2D-chezy value
       ! to be used in detvic
       !
       ! Adapt velocities U1 and V1 for mass fluxes due to waves
       !
       icx = nmaxddb
       icy = 1
       call timer_start(timer_euler, gdp)
       call euler(jstart    ,nmmax     ,nmmaxj    ,kmax      ,icx       , &
                & i(kcu)    ,i(kcv)    ,i(kfu)    ,i(kfv)    ,i(kfumax) , &
                & i(kfumin) ,i(kfvmax) ,i(kfvmin) ,r(dzu1)   ,r(dzv1)   , &
                & r(u1)     ,r(wrkb3)  ,r(v1)     ,r(wrkb4)  , &
                & r(grmasu) ,r(grmasv) ,r(hu)     ,r(hv)     , &
                & r(tp)     ,r(hrms)   ,r(sig)    ,r(thick)  ,r(teta)   , &
                & r(grmsur) ,r(grmsvr) ,r(grfacu) ,r(grfacv) ,gdp       )
       call timer_stop(timer_euler, gdp)
       !
       ! Update bed shear stress in U-direction
       !
       call timer_start(timer_taubot, gdp)
       icx = nmaxddb
       icy = 1
       call taubot(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,rouflo    ,rouwav    ,i(kfu)    ,i(kfv)    , &
                 & i(kfumin) ,i(kfumax) ,i(kspu)   ,i(kcs)    ,i(kcscut) , &
                 & d(dps)    ,r(s1)     ,r(wrkb3)  ,r(wrkb4)  , &
                 & r(guu)    ,r(xcor)   ,r(ycor)   ,r(rho)    , &
                 & r(taubpu) ,r(taubsu) ,r(wrka1)  ,r(dis)    ,r(rlabda) , &
                 & r(teta)   ,r(uorb)   ,r(tp)     ,r(wsu)    ,r(wsv)    , &
                 & r(grmasu) ,r(dfu)    ,r(deltau) ,r(hrms)   , &
                 & r(cfurou) ,r(z0urou) ,r(hu)     ,r(dzu1)   ,r(sig)    , &
                 & r(z0ucur) ,r(cvalu0) ,r(grmsur) ,r(grfacu) ,gdp       )
       !
       ! Update bed shear stress in V-direction
       !
       icx = 1
       icy = nmaxddb
       call taubot(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,rouflo    ,rouwav    ,i(kfv)    ,i(kfu)    , &
                 & i(kfvmin) ,i(kfvmax) ,i(kspv)   ,i(kcs)    ,i(kcscut) , &
                 & d(dps)    ,r(s1)     ,r(wrkb4)  ,r(wrkb3)  , &
                 & r(gvv)    ,r(ycor)   ,r(xcor)   ,r(rho)    , &
                 & r(taubpv) ,r(taubsv) ,r(wrka2)  ,r(dis)    ,r(rlabda) , &
                 & r(teta)   ,r(uorb)   ,r(tp)     ,r(wsv)    ,r(wsu)    , &
                 & r(grmasv) ,r(dfv)    ,r(deltav) ,r(hrms)   , &
                 & r(cfvrou) ,r(z0vrou) ,r(hv)     ,r(dzv1)   ,r(sig)    , &
                 & r(z0vcur) ,r(cvalv0) ,r(grmsvr) ,r(grfacv) ,gdp       )
       call timer_stop(timer_taubot, gdp)
       !
       icx = nmaxddb
       icy = 1
       call timer_start(timer_caltmx, gdp)
       call caltmx(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,zmodel    ,i(kfu)    ,i(kfv)    ,i(kfs)    , &
                 & i(kfuz1)  ,i(kfvz1)  ,i(kfsmin) ,r(wrka1)  ,r(wrka2)  , &
                 & r(taubmx) ,r(hu)     ,r(hv)     ,d(dps)    ,r(s1)     , &
                 & gdp       )
       call timer_stop(timer_caltmx, gdp)
       if (htur2d) then
          !
          ! HLES/Smagorinsky with bottom friction
          ! Calculate fluctuating velocity components using lp filter
          !
          call timer_start(timer_trisol_hles, gdp)
          call lpfluc(jstart    ,nmmaxj    ,nmmax     ,i(kfu)    ,i(kfv)    , &
                    & r(umean)  ,r(vmean)  ,r(umnldf) ,r(vmnldf) ,r(umnflc) , &
                    & r(vmnflc) ,gdp       )
          !
          ! Calculate Turbulent Kinetic Energy production due to velocity
          ! fluctuation
          ! wrka3 is used to store the result (S2) to be used in DETVIC
          !
          icx = nmaxddb
          icy = 1
          call protke(jstart    ,nmmaxj    ,nmmax     ,icx       ,icy       , &
                    & i(kfs)    ,i(kfu)    ,i(kfv)    ,i(kcs)    ,r(umnflc) , &
                    & r(vmnflc) ,r(guu)    ,r(gvv)    ,r(wrka1)  ,r(wrka2)  , &
                    & r(wrka3)  ,gdp       )
          !
          ! Calculate subgridscale eddy viscosity/diffusivity
          ! CVALU0 and CVALV0 contain the actual 2D-chezy value
          ! WRKA3 contains TKE production (S2)
          ! result is put in vicuv/dicuv in layer kmax+2
          !
          icx = nmaxddb
          icy = 1
          call detvic(lundia    ,jstart    ,nmmaxj    ,nmmax     ,kmax      , &
                    & icx       ,icy       ,i(kfs)    ,i(kfu)    , &
                    & i(kfv)    ,i(kcs)    ,d(dps)    ,r(s1)     ,r(umean)  , &
                    & r(vmean)  ,r(cvalu0) ,r(cvalv0) ,r(guv)    ,r(gvu)    , &
                    & r(gsqs)   ,r(wrka3)  ,r(vicuv)  ,r(dicuv)  , &
                    & gdp       )
          call timer_stop(timer_trisol_hles, gdp)
       endif
       !
       ! Constituents: salinity, temperature, user defined,
       ! secondary flow (2d) & turbulence (3d)
       !
       !
       ! Fill discharges;
       ! constituent (excl. turbulence & secondary flow)
       !
       if (lstsc > 0) then
          icx = nmaxddb
          icy = 1
          call timer_start(timer_discha, gdp)
          call z_discha(kmax      ,nsrc      ,nbub      ,lstsci       ,lstsc     ,jstart    , &
                      & nmmaxj    ,icx       ,icy       ,ch(namsrc)   ,i(mnksrc) , &
                      & i(kfs)    ,i(kcs)    ,i(kfsmn0) ,i(kfsmx0) ,r(sour)   , &
                      & r(sink)   ,d(dps)    ,r(s0)     ,r(dzs0)      ,r(r0)     , &
                      & r(disch)  ,r(rint)   ,r(rintsm) ,r(zwork)  ,r(zwork+kmax),bubble    ,gdp       )
          call timer_stop(timer_discha, gdp)
       endif
       !
       ! Temperature model KTEMP > 0 (only if LTEM > 0 per definition)
       !
       if (ktemp > 0) then
          icx = nmaxddb
          icy = 1
          call timer_start(timer_heatu, gdp)
          call heatu(ktemp     ,anglat    ,sferic    ,timhr     ,keva      , &
                   & ltem      ,lstsci    ,icx       ,icy       , &
                   & nmmax     ,kmax      ,i(kfs)    ,i(kfsmx0) ,i(kfsmax) , &
                   & i(kfsmn0) ,i(kspu)   ,i(kspv)   ,r(dzs0)   ,r(dzs1)   , &
                   & r(sour)   ,r(sink)   ,r(r0)     ,r(evap)   ,d(dps)    , &
                   & r(s0)     ,r(s1)     ,r(thick)  ,r(w10mag) ,r(patm)   , &
                   & r(xcor)   ,r(ycor)   ,r(gsqs)   ,r(xz)     ,r(yz)     , &
                   & anglon    ,gdp       )
          call timer_stop(timer_heatu, gdp)
       endif
       !
       ! Thatcher Harleman return times;
       ! constituent (excl. turbulence & secondary flow)
       !
       if (lstsc > 0) then
          icx = 1
          icy = nmaxddb
          call timer_start(timer_thahbc, gdp)
          call thahbc(jstart    ,nmmaxj    ,icx       ,icy       ,kmax      , &
                    & lstsci    ,lstsc     ,nrob      ,noroco    ,nto       , &
                    & nst       ,i(kfsmn0) ,i(nob)    ,r(thtim)  ,r(rettim) , &
                    & r(v0)     ,r(u0)     ,r(r0)     ,r(rbnd)   ,r(rthbnd) , &
                    & r(sig)    ,r(dzs0)   ,d(dps)    ,r(s0)     ,gdp       )
          call timer_stop(timer_thahbc, gdp)
       endif
       !
       ! Define KADU/V for transport and turbulence
       !
       icx = nmaxddb
       icy = 1
       call timer_start(timer_trakad, gdp)
       call trakad(nmmax     ,kmax      ,i(kcs)    , &
                 & icx       ,icy       , &
                 & i(kspu)   ,i(kspv)   ,i(kadu)   ,i(kadv)   ,gdp       )
       call timer_stop(timer_trakad, gdp)
       !
       ! Call sediment transport routines
       !
       if (lsedtot>0 .and. nst >= iti_sedtrans) then
          call timer_start(timer_3dmor, gdp)
          icx = nmaxddb
          icy = 1
          !
          if (lsed > 0) then
             call timer_start(timer_fallve, gdp)
             call d3d4_flocculate(nmmax, kmax, lstsci, lsal, ltem, zmodel, &
                     & r(r0), i(kfs), i(kfsmn0), i(kfsmx0), dtsec, gdp)
             call fallve(kmax    ,nmmax     ,lsal      ,ltem      ,lsed      , &
                     & i(kcs)    ,i(kfs)    ,r(u0)     ,r(v0)     , &
                     & r(wphy)   ,r(r0)     ,r(rtur0)  ,ltur      ,r(thick)  , &
                     & saleqs    ,temeqs    ,r(rhowat) ,r(ws)     , &
                     & icx       ,icy       ,lundia    ,d(dps)    ,r(s0)     , &
                     & r(umean)  ,r(vmean)  ,r(z0urou) ,r(z0vrou) ,i(kfu)    , &
                     & i(kfv)    ,zmodel    ,i(kfsmx0) ,i(kfsmn0) ,r(dzs0)   , &
                     & r(taubmx) ,lstsci    ,r(rich)   ,gdp       )
             call timer_stop(timer_fallve, gdp)
          endif
          !
          ! Suspended sediment source and sink terms
          ! Bed load sediment transport vector components
          ! Vertical sediment diffusion coefficient
          !
          call timer_start(timer_erosed, gdp)
          call z_erosed(nmmax ,kmax      ,icx       ,icy       ,lundia    , &
                  & nst       ,lsed      ,lsedtot   ,lsal      ,ltem      , &
                  & lsecfl    ,i(kfs)    ,i(kfu)    ,i(kfv)    ,r(dzs1)   , &
                  & r(r0)     ,r(u1)     ,r(v1)     ,r(s0)     ,d(dps)    , &
                  & r(z0urou) ,r(z0vrou) ,r(sour)   ,r(sink)   ,r(rhowat) , &
                  & r(ws)     ,r(z0ucur) ,r(z0vcur) ,r(sigmol) , &
                  & r(taubmx) ,r(s1)     ,r(uorb)   ,r(tp)     , &
                  & lstsci    ,r(thick)  ,r(dicww)  ,i(kcs)    , &
                  & i(kcu)    ,i(kcv)    ,r(guv)    ,r(gvu)    ,r(sbuu)   , &
                  & r(sbvv)   ,r(seddif) ,r(hrms)   ,ltur      , &
                  & r(teta)   ,r(rlabda) ,saleqs    , &
                  & r(wrka14) ,r(wrka15) ,r(entr)   ,r(wstau)  ,r(hu)     , &                   
                  & r(hv)     ,r(ubot)   ,r(rtur0)  , &
                  & temeqs    ,r(gsqs)   ,r(guu)    ,r(gvv)    ,i(kfsmin) , &
                  & i(kfsmax) ,r(dzs0)   ,i(kfumin) ,i(kfumax) ,i(kfvmin) , &
                  & i(kfvmax) ,r(dzu1)   ,r(dzv1)   ,dtsec     ,1         , &
                  & gdp       )
          call timer_stop(timer_erosed, gdp)
          call timer_stop(timer_3dmor, gdp)
       endif
       !
       ! Transport of constituents (excl. turbulence)
       !
       if (lstsci>0 .and. nst<itdiag) then
          call timer_start(timer_difu, gdp)
          icx = nmaxddb
          icy = 1
          !
          ! NO ADI voor transport eq. In relation with the usual Z_DIFU,
          ! the boundary condition is imposed in both directions.
          !
          call timer_start(timer_tritra, gdp)
          call z_difu(lundia    ,nst       ,icx       ,icy       ,jstart    , &
                    & nmmaxj    ,nmmax     ,kmax      ,lstsci    ,lstsc     , &
                    & norow     ,nocol     ,i(irocol) ,i(kcs)    ,i(kcu)    , &
                    & i(kcv)    ,i(kfs)    ,i(kfsmin) ,i(kfsmax) ,i(kfsmn0) , &
                    & i(kfsmx0) ,i(kfumin) ,i(kfumax) ,i(kfumn0) ,i(kfumx0) , &
                    & i(kfvmin) ,i(kfvmax) ,i(kfvmn0) ,i(kfvmx0) ,i(kfsz0)  , &
                    & i(kfuz0)  ,i(kfvz0)  ,i(kfu)    ,i(kfv)    ,i(kfsz1)  , &
                    & i(kfuz1)  ,i(kfvz1)  ,r(qxk)    ,r(qyk)    ,r(qzk)    , &
                    & r(u1)     ,r(v1)     ,r(guv)    ,r(gvu)    ,r(gsqs)   , &
                    & r(rbnd)   ,r(sigdif) ,r(sigmol) ,r(dicuv)  ,r(dicww)  , &
                    & r(r0)     ,r(r1)     ,r(sour)   ,r(sink)   ,r(wrkb1)  , &
                    & r(wrkb2)  ,r(wrkb3)  ,r(wrkb5)  ,r(wrkb6)  ,r(wrkb7)  , &
                    & r(wrkb8)  ,r(wrkb13) ,r(wrkb14) ,r(wrkb18) ,r(dzu1)   , &
                    & r(dzv1)   ,r(wrkc1)  ,r(wrkc2)  ,r(wrkc3)  ,r(wrkc4)  , &
                    & r(dzs1)   ,r(areau)  ,r(areav)  ,r(volum0) ,r(volum1) , &
                    & r(guu)    ,r(gvv)    ,r(bruvai) ,sedtyp    ,r(seddif) , &
                    & r(ws)     ,lsed      ,lsal      ,ltem      ,eqmbcsand , &
                    & eqmbcmud  ,lsts      ,r(s1)     ,d(dps)    ,gdp       )    
          call z_difuflux(stage  ,lundia ,kmax      ,nmmax     ,nmmaxj    , &
                  & lstsci    ,r(r0)     ,r(r1)     ,r(qxk)    ,r(qyk)    , &
                  & r(u1)     ,r(v1)     ,r(s1)     ,d(dps)    , &
                  & r(dicuv)  ,r(guv)    ,r(gvu)    ,r(areau)  ,r(areav)  , &
                  & i(kfuz1)  ,i(kfvz1)  ,i(kfsz1)  ,i(kcs)    ,i(kfs)    , &
                  & i(kfu)    ,i(kfuz0)  ,i(kfv)    ,i(kfvz0)  , &
                  & i(kfsmx0) ,i(kfsmax) ,i(kfsz0)  , &
                  & i(kfumn0) ,i(kfumx0) ,i(kfvmn0) ,i(kfvmx0) ,r(sigdif) , &
                  & dtsec     ,icx       ,icy       ,gdp       )
          call timer_stop(timer_tritra, gdp)
          call timer_stop(timer_difu, gdp)
       endif
       !
       ! Turbulence
       ! user defined BC for turbulence model in NUBND and UBND
       !
       call timer_start(timer_turbulence, gdp)
       if (ltur > 0) then
          !
          ! The velocities are corrected for mass flux and
          ! temporary set in WRKB13 (UEUL) and WRKB14 (VEUL)
          ! these are used in Z_TRATUR
          !
          icx = nmaxddb
          icy = 1
          call timer_start(timer_euler, gdp)
          call euler(jstart    ,nmmax     ,nmmaxj    ,kmax      ,icx       , &
                   & i(kcu)    ,i(kcv)    ,i(kfu)    ,i(kfv)    ,i(kfumax) , &
                   & i(kfumin) ,i(kfvmax) ,i(kfvmin) ,r(dzu1)   ,r(dzv1)   , &
                   & r(u1)     ,r(wrkb13) ,r(v1)     ,r(wrkb14) , &
                   & r(grmasu) ,r(grmasv) ,r(hu)     ,r(hv)     , &
                   & r(tp)     ,r(hrms)   ,r(sig)    ,r(thick)  ,r(teta)   , &
                   & r(grmsur) ,r(grmsvr) ,r(grfacu) ,r(grfacv) ,gdp       )
          call timer_stop(timer_euler, gdp)
          !
          icx = nmaxddb
          icy = 1
          call timer_start(timer_tratur, gdp)
          call z_tratur(dischy    ,nubnd     ,jstart    ,nmmaxj    ,nmmax     , &
                      & nmax      ,mmax      ,kmax      ,ltur      ,nto       , &
                      & icx       ,icy       ,i(kfs)    ,i(kfu)    ,i(kfv)    , &
                      & i(kcs)    ,i(mnbnd)  ,r(s1)     ,d(dps)    ,r(u1)     , &
                      & r(v1)     ,r(w1)     ,r(rtur0)  ,r(rtur1)  ,r(thick)  , &
                      & r(sig)    ,r(guu)    ,r(gvv)    ,r(guv)    ,r(gvu)    , &
                      & r(vicww)  ,r(dicww)  ,r(cfurou) ,r(cfvrou) ,r(z0urou) , &
                      & r(z0vrou) ,r(windsu) ,r(windsv) ,r(bruvai) ,r(dudz)   , &
                      & r(dvdz)   ,r(tkepro) ,r(tkedis) ,r(deltau) ,r(deltav) ,r(dfu)    , &
                      & r(dfv)    ,r(dis)    ,r(hrms)   ,r(uorb)   ,r(tp)     , &
                      & r(wrkb1)  ,r(wrkb2)  ,r(wrkb3)  ,r(wrkb4)  ,r(wrkb6)  , &
                      & r(wrkb7)  ,r(wrkb8)  ,r(wrkb9)  ,r(wrkb10) ,r(wrkb11) , &
                      & r(ubnd)   ,i(iwrk2)  , &
                      & r(wrka2)  ,r(wrkb5)  ,i(kfsmin) ,i(kfsmn0) ,i(kfsmax) , &
                      & i(kfsmx0) ,i(kfuz1)  ,i(kfvz1)  ,r(dzs1)   ,r(wrkb13) , &
                      & r(wrkb14) ,gdp       )
          call timer_stop(timer_tratur, gdp)
          !
          ! Eddy viscosity and diffusivity
          ! Use SIG for array ZK
          !
          icx = nmaxddb
          icy = 1
          call z_turclo(jstart    ,nmmaxj    ,nmmax     ,kmax      ,ltur      , &
                      & icx       ,icy       ,tkemod    , &
                      & i(kcs)    ,i(kfu)    ,i(kfv)    ,i(kfs)    ,i(kfuz1)  , &
                      & i(kfvz1)  ,i(kfsz1)  ,i(kfumin) ,i(kfumax) ,i(kfvmin) , &
                      & i(kfvmax) ,i(kfsmin) ,i(kfsmax) ,r(s1)     ,d(dps)    , &
                      & r(hu)     ,r(hv)     ,r(u1)     ,r(v1)     ,r(rtur1)  , &
                      & r(thick)  ,r(sig)    ,r(rho)    ,r(vicuv)  ,r(vicww)  , &
                      & r(dicuv)  ,r(dicww)  ,r(windsu) ,r(windsv) ,r(z0urou) , &
                      & r(z0vrou) ,r(bruvai) ,r(rich)   ,r(dudz)   ,r(dvdz)   , &
                      & r(dzu1)   ,r(dzv1)   ,r(dzs1)   ,r(sig)    ,r(wrkb13) , &
                      & r(wrkb14) ,gdp       )
          !
          ! Check horizontal eddy viscosity and diffusivity
          !
          if (htur2d .or. irov>0) then
             itype = 2
             call chkvic(lundia    ,jstart    ,nmmaxj    ,nmmax     ,kmax      , &
                       & icx       ,icy       ,timnow    ,i(kfs)    ,i(kfu)    , &
                       & i(kfv)    ,i(kcs)    ,lstsci    ,r(guv)    ,r(gvu)    , &
                       & r(vicuv)  ,r(dicuv)  ,itype     ,i(kfsmin) ,i(kfsmax) , &
                       & gdp       )
          endif
       endif
       !
       ! 2D Turbulence; BC already in RDQ2EB (see READMD)
       ! USE IBUFF as work array
       !
       if (ltur2d > 0) then
          icx = nmaxddb
          icy = 1
          call timer_start(timer_tur2d, gdp)
          call tur2d(dischy    ,jstart    ,nmmaxj    ,nmmax     ,nmax      , &
                   & mmax      ,kmax      ,icx       ,icy       ,i(kfs)    , &
                   & i(kfu)    ,i(kfv)    ,i(kcs)    ,i(ibuff)  ,r(dpd)    , &
                   & d(dps)    ,r(s1)     ,r(umean)  ,r(vmean)  ,r(rtu2d0) , &
                   & r(rtu2d1) ,r(rtubnd) ,r(thick)  ,r(guu)    ,r(gvv)    , &
                   & r(guv)    ,r(gvu)    ,r(vicww)  ,r(dicww)  ,r(vicuv)  , &
                   & r(vnu2d)  ,r(vnu3d)  ,r(cfurou) ,r(cfvrou) ,r(dddksi) , &
                   & r(dddeta) ,r(z0urou) ,r(z0vrou) ,r(windsu) ,r(windsv) , &
                   & r(tkepro) ,r(tkedis) ,r(wrkb2)  ,r(wrkb4)  ,r(wrkb5)  , &
                   & r(wrkb6)  ,r(wrkb7)  ,r(wrkb8)  ,r(wrka1)  ,r(wrka2)  , &
                   & r(wrka3)  ,r(wrka4)  ,r(wrkb9)  ,r(wrkb10) ,gdp       )
          call timer_stop(timer_tur2d, gdp)
       endif
       call timer_stop(timer_turbulence, gdp)
       !
       ! Forester filter
       !
       if (lstsci > 0) then
          icx = nmaxddb
          icy = 1
          call timer_start(timer_forfil, gdp)
          call z_forfil(nmmax     ,kmax      ,lstsci    , &
                      & lsecfl    ,lsal      ,ltem      ,icx       ,icy       , &
                      & nst       ,forfuv    ,forfww    , &
                      & i(kfs)    ,i(kcs)    ,i(kcu)    ,i(kcv)    ,i(kfsmin) , &
                      & i(kfsmax) ,i(kfuz1)  ,i(kfvz1)  ,i(kfsz1)  ,i(idifu)  , &
                      & r(dzs1)   ,r(r0)     , &
                      & r(r1)     ,r(rmneg)  ,r(volum1) ,r(dicww)  ,r(w1)     , &
                      & r(sigdif) ,r(sigmol) ,gdp       )
          call timer_stop(timer_forfil, gdp)
       endif
       !
       ! Compute drogues (DROGUE = .true.)
       !
       if (drogue) then
          icx = nmaxddb
          icy = 1
          call timer_start(timer_drotim, gdp)
          call drotim(nst       ,jstart    ,nmmaxj    ,kmax      ,ndro      , &
                    & icx       ,icy       ,windxt    ,windyt    ,windft    , &
                    & i(kcu)    ,i(kcv)    ,i(kcs)    ,i(kfu)    ,i(kfv)    , &
                    & i(mndro)  ,i(itdro)  ,r(u1)     ,r(v1)     ,r(xcor)   , &
                    & r(ycor)   ,r(guu)    ,r(gvv)    ,r(guv)    ,r(gvu)    , &
                    & r(dxydro) ,r(xydro)  ,r(hu)     ,r(hv)     ,r(s1)     , &
                    & r(dpu)    ,r(dpv)    ,r(thick)  ,r(drodep) , &
                    & i(kfumin) ,i(kfumax) ,i(kfvmin) ,i(kfvmax) , &
                    & r(dzu1)   ,r(dzv1)   ,r(sig)    ,gdp       )
          call timer_stop(timer_drotim, gdp)
       endif
       !
       ! Compute change in bottom sediment and bottom elevation
       ! except when run parallel to fluid mud
       ! Suspended transport correction vector
       ! Suspended transport vector for output
       !
       call timer_start(timer_3dmor, gdp)
       if (lsedtot>0 .and. nst >= iti_sedtrans) then
          !
          ! don't compute suspended transport vector in middle of timestep
          ! note: IWRK1 used as local work array
          !
          sscomp = .true.
          icx = nmaxddb
          icy = 1
          call timer_start(timer_bott3d, gdp)
          call z_bott3d(nmmax     ,kmax      ,lsed      , &
                      & lsedtot   ,lsal      ,ltem      ,i(kfs)    ,i(kfu)    , &
                      & i(kfv)    ,r(r1)     ,r(s0)     ,i(kcs)    , &
                      & d(dps)    ,r(gsqs)   ,r(guu)    , &
                      & r(gvv)    ,r(s1)     ,r(thick)  ,r(dpd)    , &
                      & r(umean)  ,r(vmean)  ,r(sbuu)   ,r(sbvv)   , &
                      & r(depchg) ,nst       ,r(hu)     , &
                      & r(hv)     ,r(sig)    ,r(u1)     ,r(v1)     , &
                      & sscomp    ,i(iwrk1)  , &
                      & r(guv)    ,r(gvu)    ,i(kcu)    , &
                      & i(kcv)    ,icx       ,icy       ,timhr     , &
                      & nto       ,r(volum0) ,r(volum1) ,r(dzs1)   ,r(dzu1)   , &
                      & r(dzv1)   ,i(kfsmin) ,i(kfumin) ,i(kfumax) ,i(kfvmin) , &
                      & i(kfvmax) ,dtsec     ,gdp       )
          call timer_stop(timer_bott3d, gdp)
       endif
       !
       ! Subsidence/Uplift
       !
       if (lfsdu) then 
          call incsdu(timhr  ,d(dps)   ,r(s1)   ,i(kcs)  ,i(kfs) ,gdp    )
       endif
       !
       if (((lsedtot>0) .and. bedupd) .or. lfsdu) then
          ! icx = nmaxddb
          ! icy = 1
          !   call z_updzm(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
          !              & icy       ,fout      ,i(kcu)    ,i(kcv)    ,i(kcs)    , &
          !              & i(kfu)    ,i(kfv)    ,i(kfs)    ,i(kfsz1)  ,i(kfuz1)  , &
          !              & i(kfvz1)  ,i(kfsmin) ,i(kfsmax) ,i(kfumin) ,i(kfumax) , &
          !              & i(kfvmin) ,i(kfvmax) ,i(kspu)   ,i(kspv)   ,i(kcshyd) , &
          !              & d(dps)    ,r(dpu)    ,r(dpv)    ,r(s1)     ,r(thick)  , &
          !              & r(hu)     ,r(hv)     ,r(dzu1)   ,r(dzu0)   ,r(dzv1)   , &
          !              & r(dzv0)   ,r(dzs1)   ,r(dzs0)   ,r(sig)    ,'update'  ,gdp       )
          !
          itemp = 0
          icx   = nmaxddb
          icy   = 1
          !
          ! To enable computation of new grid administration due to water level
          ! correction in Non-hydro static module itemp has been defined
          ! to call Z_DRYCHK (and set to zero) replacing idry.
          ! Fluxes are based on geometry at old time level, so drying does not
          ! lead to repetition of timestep.
          ! NOTE: Z_DRYCHK is called with arrays KFUZ0, KFVZ0 and KFSZ1. 
          !       KFUZ0 and KFVZ0 are the arrays corresponding to the original geometry (S0)
          !       KFSZ1 is to be determined, corresponding to the new geometry (S1)
          !
          call z_drychk(itemp     ,jstart    ,nmmaxj    ,nmmax     ,kmax      , &
                      & nfltyp    ,icx       ,icy       ,i(kfu)    ,i(kfv)    , &
                      & i(kfs)    ,i(kcs)    ,i(kfuz1)  ,i(kfvz1)  ,i(kfsz1)  , &
                      & i(kfsmin) ,i(kfsmn0) ,i(kfsmax) ,i(kfsmx0) ,r(s1)     , &
                      & r(r1)     ,d(dps)    ,r(qxk)    ,r(qyk)    ,r(w1)     , &
                      & lstsci    ,r(dzs1)   ,r(sig)    ,nst       ,gdp       )
          !
          ! Update the layer geometry in U-velocity points based on the new water levels S1
          ! NOTE: Z_DRYCHKU is called with arrays KFUZ1, KFVZ1 and KFSZ1. 
          !       KFSZ1 has just been determined, corresponding to the new geometry (S1)
          !       KFUZ1 is to be determined, corresponding to the new geometry (S1)
          !       KFVZ1 is updated to the new geomety in the second call to Z_DRYCHKU
          !
          icx   = nmaxddb
          icy   = 1
          call z_drychku(jstart    ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                       & i(kcs)    ,i(kfu)    ,i(kcu)    ,i(kspu)   ,i(kfsmax) , &
                       & i(kfsmin) ,i(kfsz1)  ,i(kfuz1)  ,i(kfumin) ,i(kfumn0) ,i(kfumax) , &
                       & i(kfumx0) ,r(hu)     ,r(s1)     ,r(dpu)    ,d(dps)    , &
                       & r(umean)  ,r(u0)     ,r(u1)     ,r(dzu0)   ,r(dzu1)   , &
                       & r(dzs1)   ,r(sig)    ,i(kfsmx0) ,r(guu)    ,r(qxk)    , &
                       & gdp       )             
          !
          ! Update the layer geometry in V-velocity points based on the new water levels S1
          ! NOTE: Z_DRYCHKU is called with arrays KFUZ1, KFVZ1 and KFSZ1. 
          !       KFSZ1 has just been determined, corresponding to the new geometry (S1)
          !       KFVZ1 is to be determined, corresponding to the new geometry (S1)
          !       KFUZ1 was updated to the new geomety after the first half time step
          !
          icx = 1
          icy = nmaxddb
          call z_drychku(jstart    ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                       & i(kcs)    ,i(kfv)    ,i(kcv)    ,i(kspv)   ,i(kfsmax) , &
                       & i(kfsmin) ,i(kfsz1)  ,i(kfvz1)  ,i(kfvmin) ,i(kfvmn0) ,i(kfvmax) , &
                       & i(kfvmx0) ,r(hv)     ,r(s1)     ,r(dpv)    ,d(dps)    , &
                       & r(vmean)  ,r(v0)     ,r(v1)     ,r(dzv0)   ,r(dzv1)   , &
                       & r(dzs1)   ,r(sig)    ,i(kfsmx0) ,r(gvv)    ,r(qyk)    , &
                       & gdp       )
          !
          ! If requested by keyword ZTBML 
          ! (Z-model TauBottom Modified Layering)
          ! --> modify the near-bed layering to obtain smoother bottom shear stress representation in z-layer models
          !
          if (ztbml) then
             !
             ! Call with modify_dzsuv set to 1 for all components, to modify dzs1, dzu1, dzv1
             ! (and possibly R1 and qzk)
             !
             modify_dzsuv(1:3) = 1
             ztbml_upd_r1      = .true.
             call z_taubotmodifylayers(nmmax   ,kmax       ,lstsci       ,icx      ,icy          , & 
                                     & i(kfs)  ,i(kfsmin)  ,i(kfsmax)    ,d(dps)   ,r(dzs1)      , &
                                     & i(kfu)  ,i(kfumin)  ,i(kfumax)    ,r(dpu)   ,r(dzu1)      , &
                                     & i(kfv)  ,i(kfvmin)  ,i(kfvmax)    ,r(dpv)   ,r(dzv1)      , &
                                     & r(r1)   ,r(s00)     ,r(s1)        ,r(sig)   ,modify_dzsuv , &
                                     & dtsec   ,r(gsqs)    ,i(kfsmx0)    ,r(qzk)   ,r(umean)     , &
                                     & r(vmean),r(dzs0)    ,ztbml_upd_r1 ,gdp      )
          endif
          !
          ! Recalculate DPU/DPV (depth at velocity points)
          !
          call caldpu( lundia    ,mmax      ,nmaxus    ,kmax      , &
                    &  zmodel    , &
                    &  i(kcs)    ,i(kcu)    ,i(kcv)    , &
                    &  i(kspu)   ,i(kspv)   ,r(hkru)   ,r(hkrv)   , &
                    &  r(umean)  ,r(vmean)  ,r(dpd)    ,r(dpu)    ,r(dpv)   , &
                    &  d(dps)    ,r(dzs1)   ,r(u1)     ,r(v1)     ,r(s1)    , &
                    &  r(thick)  ,gdp       )
       endif
       call timer_stop(timer_3dmor, gdp)
       !
       if (nst+1 == ithisc) then
          imode = 3 ! output needed, so update fluxes and volumes
       else
          imode = 2 ! no output needed, so just update fluxes
       endif
       call updmassbal(imode    ,r(qxk)    ,r(qyk)    ,i(kcs)    ,r(r1)     , &
                    & r(volum0) ,r(volum1) ,r(sbuu)   ,r(sbvv)   ,r(disch)  , &
                    & i(mnksrc) ,r(sink)   ,r(sour)   ,r(gsqs)   ,r(guu)    , &
                    & r(gvv)    ,d(dps)    ,r(rintsm) ,dtsec     ,gdp       )
       !
       ! Check Courant numbers for U and V velocities in U-points
       ! Check is based on the old geometry (corresponding to S0)
       !
       icx = nmaxddb
       icy = 1
       call z_chkadv(lundia    ,nmmax     ,kmax      ,icx       , &
                   & icy       ,i(kfu)    ,i(kfuz0)  ,i(kfvz0)  , &
                   & r(guu)    ,r(gvu)    ,r(u0)     ,r(v0)     , &
                   & i(kfumx0) ,i(kfumn0) ,r(dzu0)   ,r(dzs0)   , &
                   & nst       ,i(kcs)    ,gdp       )
       !
       ! Check Courant numbers for U and V velocities in V-points
       !
       icx = 1
       icy = nmaxddb
       call z_chkadv(lundia    ,nmmax     ,kmax      ,icx       , &
                   & icy       ,i(kfv)    ,i(kfvz0)  ,i(kfuz0)  , &
                   & r(gvv)    ,r(guv)    ,r(v0)     ,r(u0)     , &
                   & i(kfvmx0) ,i(kfvmn0) ,r(dzv0)   ,r(dzs0)   , &
                   & nst       ,i(kcs)    ,gdp       )
       !
       ! Reset arrays for next half time step
       ! S0=S1, U0=U1, V0=V1, R0=R1 etc
       !
       call timer_start(timer_f0isf1, gdp)
       call f0isf1(stage     ,dischy    ,nst       ,zmodel    ,jstart    , &
                 & nmmax     ,nmmaxj    ,nmax      ,kmax      ,lstsci    , &
                 & ltur      ,nsrc      ,i(kcu)    ,i(kcv)    ,i(kcs)    , &
                 & i(kfs)    ,i(kfu)    ,i(kfv)    ,i(kfsmin) ,i(kfsmax) , &
                 & i(kfumin) ,i(kfumax) ,i(kfvmin) ,i(kfvmax) ,i(kfsmn0) , &
                 & i(kfumn0) ,i(kfvmn0) ,i(kfsmx0) ,i(kfumx0) ,i(kfvmx0) , &
                 & i(kfsz0)  ,i(kfuz0)  ,i(kfvz0)  , &
                 & i(kfsz1)  ,i(kfuz1)  ,i(kfvz1)  , &
                 & r(s0)     ,r(s1)     ,r(u0)     , &
                 & r(u1)     ,r(v0)     ,r(v1)     ,r(volum0) ,r(volum1) , &
                 & r(r0)     ,r(r1)     ,r(rtur0)  ,r(rtur1)  ,r(disch)  , &
                 & r(discum) ,r(hu)     ,r(hv)     ,r(dzu1)   ,r(dzv1)   , &
                 & r(dzs1)   ,r(dzu0)   ,r(dzv0)   ,r(dzs0)   ,r(qxk)    , &
                 & r(qyk)    ,r(s00)    ,r(w0)     , &
                 & r(w1)     ,r(p0)     ,r(p1)     ,r(hu0)    ,r(hv0)    , &
                 & r(ewabr0) ,r(ewabr1) , &
                 & r(ewave0) ,r(ewave1) ,r(eroll0) ,r(eroll1) ,roller    , &
                 & gdp       )
       call timer_stop(timer_f0isf1, gdp)
    endif
    !
    if (rtcact /= noRTC) then
       call rtc_comm_put(i(kfs)    ,i(kfsmin) ,i(kfsmax) ,r(sig)    , &
                       & r(sig)    ,r(s1)     ,d(dps)    ,r(r0)     , &
                       & nsluv     ,r(cbuv)   ,nsrc      ,r(disch)  , &
                       & gdp)
    endif
    if (sbkol) then
       !
       ! Communicate with 1D application
       !
       call timer_start(timer_wait, gdp)
       call D3S_put_levels(ntstep   ,            &
                           gdp%d%mlb, gdp%d%mub, &
                           gdp%d%nlb, gdp%d%nub, &
                           r(s1)    , i(kfs)     )
       call timer_stop(timer_wait, gdp)
    endif
    !
    !++++++++++++++++++++++++++++ LOOP COMPLETE ++++++++++++++++++++++++++++
    !
    !
    ! skip calculation in case of dryrun
    !
    if (.not. dryrun) then
       !
       ! Define wphy
       !
       if (kmax > 1) then
          icx = nmaxddb
          icy = 1
          call timer_start(timer_wphys, gdp)
          call wphys(r(s1)     ,r(u1)     ,r(v1)     ,r(w1)     ,r(wphy)   , &
                   & i(irocol) ,norow     ,nocol     ,icx       ,icy       , &
                   & jstart    ,nmmaxj    ,kmax      ,nsrc      ,zmodel    , &
                   & i(mnksrc) ,r(disch)  ,r(thick)  ,r(sig)    ,r(guu)    , &
                   & r(gvv)    ,r(gsqs)   ,d(dps)    ,nmmax     ,i(kcs)    , &
                   & r(dpu)    ,r(dpv)    ,i(kfsmin) ,i(kfsmax) , &
                   & r(porosu) ,r(porosv) ,gdp       )
          call timer_stop(timer_wphys, gdp)
       endif
       !
       ! To avoid problems with GPP, arrays VORTIC (vorticity) and ENSTRO
       ! (enstrophy) are always computed and stored in HIS and MAP files
       ! even when HLES is not activated.
       ! These arrays are computed at the end of each time step for
       ! post-processing purpose only
       !
       call timer_start(timer_cvort, gdp)
       call c_vort(mmax      ,nmax      ,kmax      ,nmaxus    ,i(kcs)    ,i(kfu)    , &
                 & i(kfv)    ,r(u1)     ,r(v1)     ,r(gud)    ,r(gvd)    , &
                 & r(vortic) ,r(enstro) ,r(wrkb1)  ,gdp       )
       call timer_stop(timer_cvort, gdp)
    endif
end subroutine z_trisol_nhfull
