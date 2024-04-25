subroutine gtptrs(gdp)
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
! Gets the pointers of the dynamically allocated arrays.
! Before the use of this subroutine, MD-VER did not get all pointers
! (for example: work-arrays are not used).
! This subroutine is used in both TRICOM and MD-VER for the following reasons:
! 1) Minimize the chance on errors: getting a pointer to a new array
!    is now located only in this subroutine.
! 2) Memory is now allocated unnecessary in MD-VER, but it is deallocated
!    as soon as the verification is finished.
! 3) Actually MD-VER now checks whether there is enough free allocatable memory
!    to run TRISIM properly.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer(pntrsize), pointer :: alfas
    integer(pntrsize), pointer :: alpha
    integer(pntrsize), pointer :: ampbc
    integer(pntrsize), pointer :: areau
    integer(pntrsize), pointer :: areav
    integer(pntrsize), pointer :: atr
    integer(pntrsize), pointer :: bruvai
    integer(pntrsize), pointer :: c
    integer(pntrsize), pointer :: cbuv
    integer(pntrsize), pointer :: cbuvrt
    integer(pntrsize), pointer :: cdwlsu
    integer(pntrsize), pointer :: cdwlsv
    integer(pntrsize), pointer :: cdwzbu
    integer(pntrsize), pointer :: cdwzbv
    integer(pntrsize), pointer :: cdwztu
    integer(pntrsize), pointer :: cdwztv
    integer(pntrsize), pointer :: cfurou
    integer(pntrsize), pointer :: cfvrou
    integer(pntrsize), pointer :: cgc
    integer(pntrsize), pointer :: cgdghf
    integer(pntrsize), pointer :: cgdghl
    integer(pntrsize), pointer :: cvalu0
    integer(pntrsize), pointer :: cvalv0
    integer(pntrsize), pointer :: circ2d
    integer(pntrsize), pointer :: circ3d
    integer(pntrsize), pointer :: crbc
    integer(pntrsize), pointer :: ctbf
    integer(pntrsize), pointer :: ctbl
    integer(pntrsize), pointer :: ctif
    integer(pntrsize), pointer :: ctil
    integer(pntrsize), pointer :: ctr
    integer(pntrsize), pointer :: ctrf
    integer(pntrsize), pointer :: ctrl
    integer(pntrsize), pointer :: czusus
    integer(pntrsize), pointer :: czvsus
    integer(pntrsize), pointer :: dddeta
    integer(pntrsize), pointer :: dddksi
    integer(pntrsize), pointer :: disch0
    integer(pntrsize), pointer :: disch1
    integer(pntrsize), pointer :: decay
    integer(pntrsize), pointer :: deltau
    integer(pntrsize), pointer :: deltav
    integer(pntrsize), pointer :: depchg
    integer(pntrsize), pointer :: dfu
    integer(pntrsize), pointer :: dfv
    integer(pntrsize), pointer :: diapl
    integer(pntrsize), pointer :: dicuv
    integer(pntrsize), pointer :: dicww
    integer(pntrsize), pointer :: dircom
    integer(pntrsize), pointer :: dircos
    integer(pntrsize), pointer :: dirsin
    integer(pntrsize), pointer :: dis
    integer(pntrsize), pointer :: df
    integer(pntrsize), pointer :: disch
    integer(pntrsize), pointer :: discom
    integer(pntrsize), pointer :: discum
    integer(pntrsize), pointer :: disinp
    integer(pntrsize), pointer :: dldeta
    integer(pntrsize), pointer :: dldksi
    integer(pntrsize), pointer :: dp
    integer(pntrsize), pointer :: dpc
    integer(pntrsize), pointer :: dpdeta
    integer(pntrsize), pointer :: dpdksi
    integer(pntrsize), pointer :: dps
    integer(pntrsize), pointer :: dpsed
    integer(pntrsize), pointer :: dpu
    integer(pntrsize), pointer :: dpv
    integer(pntrsize), pointer :: drodep
    integer(pntrsize), pointer :: rint0
    integer(pntrsize), pointer :: rint1
    integer(pntrsize), pointer :: dsdeta
    integer(pntrsize), pointer :: dsdksi
    integer(pntrsize), pointer :: dtdeta
    integer(pntrsize), pointer :: dtdksi
    integer(pntrsize), pointer :: dteu
    integer(pntrsize), pointer :: dtev
    integer(pntrsize), pointer :: dtr
    integer(pntrsize), pointer :: dudz
    integer(pntrsize), pointer :: umdis0
    integer(pntrsize), pointer :: umdis1
    integer(pntrsize), pointer :: dvdz
    integer(pntrsize), pointer :: vmdis0
    integer(pntrsize), pointer :: vmdis1
    integer(pntrsize), pointer :: dxydro
    integer(pntrsize), pointer :: dzdeta
    integer(pntrsize), pointer :: dzdksi
    integer(pntrsize), pointer :: encgf
    integer(pntrsize), pointer :: encgl
    integer(pntrsize), pointer :: enstro
    integer(pntrsize), pointer :: entr
    integer(pntrsize), pointer :: eroll0
    integer(pntrsize), pointer :: eroll1
    integer(pntrsize), pointer :: evap
    integer(pntrsize), pointer :: ewabr0
    integer(pntrsize), pointer :: ewabr1
    integer(pntrsize), pointer :: ewave0
    integer(pntrsize), pointer :: ewave1
    integer(pntrsize), pointer :: excbed
    integer(pntrsize), pointer :: fcorio
    integer(pntrsize), pointer :: fltr
    integer(pntrsize), pointer :: fuiwe
    integer(pntrsize), pointer :: fviwe
    integer(pntrsize), pointer :: fxw
    integer(pntrsize), pointer :: fyw
    integer(pntrsize), pointer :: grmasu
    integer(pntrsize), pointer :: grmasv
    integer(pntrsize), pointer :: grmsur
    integer(pntrsize), pointer :: grmsvr
    integer(pntrsize), pointer :: grfacu
    integer(pntrsize), pointer :: grfacv
    integer(pntrsize), pointer :: gro
    integer(pntrsize), pointer :: gsqd
    integer(pntrsize), pointer :: gsqs
    integer(pntrsize), pointer :: guu
    integer(pntrsize), pointer :: guv
    integer(pntrsize), pointer :: gvu
    integer(pntrsize), pointer :: gvv
    integer(pntrsize), pointer :: hkru
    integer(pntrsize), pointer :: hkrv
    integer(pntrsize), pointer :: hrmcom
    integer(pntrsize), pointer :: hrms
    integer(pntrsize), pointer :: hu
    integer(pntrsize), pointer :: hu0
    integer(pntrsize), pointer :: huvw
    integer(pntrsize), pointer :: hv
    integer(pntrsize), pointer :: hv0
    integer(pntrsize), pointer :: hydrbc
    integer(pntrsize), pointer :: msucom
    integer(pntrsize), pointer :: msvcom
    integer(pntrsize), pointer :: ombc
    integer(pntrsize), pointer :: omega
    integer(pntrsize), pointer :: patm
    integer(pntrsize), pointer :: phibc
    integer(pntrsize), pointer :: porosu
    integer(pntrsize), pointer :: porosv
    integer(pntrsize), pointer :: precip
    integer(pntrsize), pointer :: procbc
    integer(pntrsize), pointer :: pship
    integer(pntrsize), pointer :: qsrcrt
    integer(pntrsize), pointer :: qtfrac
    integer(pntrsize), pointer :: qtfrct
    integer(pntrsize), pointer :: qtfrt2
    integer(pntrsize), pointer :: qu
    integer(pntrsize), pointer :: qv
    integer(pntrsize), pointer :: qxk
    integer(pntrsize), pointer :: qxkr
    integer(pntrsize), pointer :: qxkw
    integer(pntrsize), pointer :: qyk
    integer(pntrsize), pointer :: qykr
    integer(pntrsize), pointer :: qykw
    integer(pntrsize), pointer :: qzk
    integer(pntrsize), pointer :: r0
    integer(pntrsize), pointer :: r1
    integer(pntrsize), pointer :: rbnd
    integer(pntrsize), pointer :: rbuff
    integer(pntrsize), pointer :: rettim
    integer(pntrsize), pointer :: rho
    integer(pntrsize), pointer :: rhowat
    integer(pntrsize), pointer :: rich
    integer(pntrsize), pointer :: rint
    integer(pntrsize), pointer :: rintsm
    integer(pntrsize), pointer :: rlabda
    integer(pntrsize), pointer :: rmneg
    integer(pntrsize), pointer :: rnpl
    integer(pntrsize), pointer :: rob
    integer(pntrsize), pointer :: rsed
    integer(pntrsize), pointer :: rthbnd
    integer(pntrsize), pointer :: rtu2d0
    integer(pntrsize), pointer :: rtu2d1
    integer(pntrsize), pointer :: rtubnd
    integer(pntrsize), pointer :: rtur0
    integer(pntrsize), pointer :: rtur1
    integer(pntrsize), pointer :: rxx
    integer(pntrsize), pointer :: rxy
    integer(pntrsize), pointer :: ryy
    integer(pntrsize), pointer :: rxz
    integer(pntrsize), pointer :: ryz
    integer(pntrsize), pointer :: s0
    integer(pntrsize), pointer :: s1
    integer(pntrsize), pointer :: sbtr
    integer(pntrsize), pointer :: sbtrc
    integer(pntrsize), pointer :: sbuu
    integer(pntrsize), pointer :: sbvv
    integer(pntrsize), pointer :: seddif
    integer(pntrsize), pointer :: sepsus
    integer(pntrsize), pointer :: sig
    integer(pntrsize), pointer :: sigdif
    integer(pntrsize), pointer :: sigmol
    integer(pntrsize), pointer :: sink
    integer(pntrsize), pointer :: sinkr
    integer(pntrsize), pointer :: sinkw
    integer(pntrsize), pointer :: soumud
    integer(pntrsize), pointer :: sour
    integer(pntrsize), pointer :: sourr
    integer(pntrsize), pointer :: sourw
    integer(pntrsize), pointer :: sstr
    integer(pntrsize), pointer :: sstrc
    integer(pntrsize), pointer :: stbf
    integer(pntrsize), pointer :: stbl
    integer(pntrsize), pointer :: stif
    integer(pntrsize), pointer :: stil
    integer(pntrsize), pointer :: sumrho
    integer(pntrsize), pointer :: taubmx
    integer(pntrsize), pointer :: taubpu
    integer(pntrsize), pointer :: taubpv
    integer(pntrsize), pointer :: taubsu
    integer(pntrsize), pointer :: taubsv
    integer(pntrsize), pointer :: teta
    integer(pntrsize), pointer :: tgarkt
    integer(pntrsize), pointer :: tgarkx
    integer(pntrsize), pointer :: tgarnp
    integer(pntrsize), pointer :: thetbc
    integer(pntrsize), pointer :: thick
    integer(pntrsize), pointer :: thtim
    integer(pntrsize), pointer :: tkedis
    integer(pntrsize), pointer :: tkepro
    integer(pntrsize), pointer :: tp
    integer(pntrsize), pointer :: tpcom
    integer(pntrsize), pointer :: u0
    integer(pntrsize), pointer :: u1
    integer(pntrsize), pointer :: ubrlsu
    integer(pntrsize), pointer :: ubrlsv
    integer(pntrsize), pointer :: umdis
    integer(pntrsize), pointer :: umean
    integer(pntrsize), pointer :: umeanf
    integer(pntrsize), pointer :: umeanl
    integer(pntrsize), pointer :: umnflc
    integer(pntrsize), pointer :: umnldf
    integer(pntrsize), pointer :: uorb
    integer(pntrsize), pointer :: ubot
    integer(pntrsize), pointer :: ubcom
    integer(pntrsize), pointer :: usus
    integer(pntrsize), pointer :: uvdist
    integer(pntrsize), pointer :: uwtypu
    integer(pntrsize), pointer :: uwtypv
    integer(pntrsize), pointer :: v0
    integer(pntrsize), pointer :: v1
    integer(pntrsize), pointer :: vicuv
    integer(pntrsize), pointer :: vicww
    integer(pntrsize), pointer :: vmdis
    integer(pntrsize), pointer :: vmean
    integer(pntrsize), pointer :: vmnflc
    integer(pntrsize), pointer :: vmnldf
    integer(pntrsize), pointer :: vnu2d
    integer(pntrsize), pointer :: vnu3d
    integer(pntrsize), pointer :: voldis
    integer(pntrsize), pointer :: volum0
    integer(pntrsize), pointer :: volum1
    integer(pntrsize), pointer :: vortic
    integer(pntrsize), pointer :: vsus
    integer(pntrsize), pointer :: w1
    integer(pntrsize), pointer :: w10mag
    integer(pntrsize), pointer :: wenf
    integer(pntrsize), pointer :: wenfm
    integer(pntrsize), pointer :: wenl
    integer(pntrsize), pointer :: wenlm
    integer(pntrsize), pointer :: windcd
    integer(pntrsize), pointer :: windsu
    integer(pntrsize), pointer :: windsv
    integer(pntrsize), pointer :: windu
    integer(pntrsize), pointer :: windv
    integer(pntrsize), pointer :: wlen
    integer(pntrsize), pointer :: wlcom
    integer(pntrsize), pointer :: wphy
    integer(pntrsize), pointer :: ws
    integer(pntrsize), pointer :: wssus
    integer(pntrsize), pointer :: wstau
    integer(pntrsize), pointer :: wsu
    integer(pntrsize), pointer :: wsucom
    integer(pntrsize), pointer :: wsv
    integer(pntrsize), pointer :: wsvcom
    integer(pntrsize), pointer :: wsbodyu
    integer(pntrsize), pointer :: wsbodyucom
    integer(pntrsize), pointer :: wsbodyv
    integer(pntrsize), pointer :: wsbodyvcom
    integer(pntrsize), pointer :: x2y
    integer(pntrsize), pointer :: x3
    integer(pntrsize), pointer :: xcor
    integer(pntrsize), pointer :: xy2
    integer(pntrsize), pointer :: xydro
    integer(pntrsize), pointer :: xyzsrc
    integer(pntrsize), pointer :: xz
    integer(pntrsize), pointer :: y3
    integer(pntrsize), pointer :: ycor
    integer(pntrsize), pointer :: yz
    integer(pntrsize), pointer :: z0ucur
    integer(pntrsize), pointer :: z0vcur
    integer(pntrsize), pointer :: z0urou
    integer(pntrsize), pointer :: z0vrou
    integer(pntrsize), pointer :: zalfas
    integer(pntrsize), pointer :: zbdsed
    integer(pntrsize), pointer :: zbmnf
    integer(pntrsize), pointer :: zbmnl
    integer(pntrsize), pointer :: zcuru
    integer(pntrsize), pointer :: zcurv
    integer(pntrsize), pointer :: zcurw
    integer(pntrsize), pointer :: zdicww
    integer(pntrsize), pointer :: zdist
    integer(pntrsize), pointer :: zdps
    integer(pntrsize), pointer :: zdpsed
    integer(pntrsize), pointer :: zenst
    integer(pntrsize), pointer :: zetabf
    integer(pntrsize), pointer :: zetabl
    integer(pntrsize), pointer :: zetaif
    integer(pntrsize), pointer :: zetail
    integer(pntrsize), pointer :: zfixfac
    integer(pntrsize), pointer :: zfrac
    integer(pntrsize), pointer :: zhidexp
    integer(pntrsize), pointer :: zmeanf
    integer(pntrsize), pointer :: zmeanl
    integer(pntrsize), pointer :: zmudfrac
    integer(pntrsize), pointer :: zqxk
    integer(pntrsize), pointer :: zqyk
    integer(pntrsize), pointer :: zrca
    integer(pntrsize), pointer :: zrho
    integer(pntrsize), pointer :: zrich
    integer(pntrsize), pointer :: zrsdeq
    integer(pntrsize), pointer :: zsandfrac
    integer(pntrsize), pointer :: zsbu
    integer(pntrsize), pointer :: zsbv
    integer(pntrsize), pointer :: zseddif
    integer(pntrsize), pointer :: zsinkse
    integer(pntrsize), pointer :: zsourse
    integer(pntrsize), pointer :: zssu
    integer(pntrsize), pointer :: zssv
    integer(pntrsize), pointer :: zstep
    integer(pntrsize), pointer :: ztaub
    integer(pntrsize), pointer :: ztauet
    integer(pntrsize), pointer :: ztauks
    integer(pntrsize), pointer :: ztur
    integer(pntrsize), pointer :: zvicww
    integer(pntrsize), pointer :: zvort
    integer(pntrsize), pointer :: zwl
    integer(pntrsize), pointer :: zws
    integer(pntrsize), pointer :: zwndsp
    integer(pntrsize), pointer :: zwndcd
    integer(pntrsize), pointer :: zwnddr
    integer(pntrsize), pointer :: zairp
    integer(pntrsize), pointer :: drhodx
    integer(pntrsize), pointer :: drhody
    integer(pntrsize), pointer :: dzs0
    integer(pntrsize), pointer :: dzs1
    integer(pntrsize), pointer :: dzu0
    integer(pntrsize), pointer :: dzu1
    integer(pntrsize), pointer :: dzv0
    integer(pntrsize), pointer :: dzv1
    integer(pntrsize), pointer :: res
    integer(pntrsize), pointer :: rl
    integer(pntrsize), pointer :: rj
    integer(pntrsize), pointer :: p1
    integer(pntrsize), pointer :: p0
    integer(pntrsize), pointer :: p00
    integer(pntrsize), pointer :: pnhcor
    integer(pntrsize), pointer :: w0
    integer(pntrsize), pointer :: s00
    integer(pntrsize), pointer :: hydprs
    integer(pntrsize), pointer :: guz
    integer(pntrsize), pointer :: gvz
    integer(pntrsize), pointer :: gud
    integer(pntrsize), pointer :: gvd
    integer(pntrsize), pointer :: gsqiu
    integer(pntrsize), pointer :: gsqiv
    integer(pntrsize), pointer :: ibuff
    integer(pntrsize), pointer :: idifu
    integer(pntrsize), pointer :: irocol
    integer(pntrsize), pointer :: iroll
    integer(pntrsize), pointer :: itbcc
    integer(pntrsize), pointer :: itbct
    integer(pntrsize), pointer :: itdis
    integer(pntrsize), pointer :: itdro
    integer(pntrsize), pointer :: kadu
    integer(pntrsize), pointer :: kadv
    integer(pntrsize), pointer :: kcs
    integer(pntrsize), pointer :: kcu
    integer(pntrsize), pointer :: kcv
    integer(pntrsize), pointer :: kfs
    integer(pntrsize), pointer :: kfu
    integer(pntrsize), pointer :: kfv
    integer(pntrsize), pointer :: kspu
    integer(pntrsize), pointer :: kspv
    integer(pntrsize), pointer :: kstp
    integer(pntrsize), pointer :: kzs
    integer(pntrsize), pointer :: kzu
    integer(pntrsize), pointer :: kzv
    integer(pntrsize), pointer :: mnbar
    integer(pntrsize), pointer :: mnbnd
    integer(pntrsize), pointer :: mndro
    integer(pntrsize), pointer :: mnksrc
    integer(pntrsize), pointer :: nob
    integer(pntrsize), pointer :: kcshyd
    integer(pntrsize), pointer :: kfumin
    integer(pntrsize), pointer :: kfvmin
    integer(pntrsize), pointer :: kfsmin
    integer(pntrsize), pointer :: kfumax
    integer(pntrsize), pointer :: kfvmax
    integer(pntrsize), pointer :: kfsmax
    integer(pntrsize), pointer :: kfumx0
    integer(pntrsize), pointer :: kfvmx0
    integer(pntrsize), pointer :: kfsmx0
    integer(pntrsize), pointer :: kfumn0
    integer(pntrsize), pointer :: kfvmn0
    integer(pntrsize), pointer :: kfsmn0
    integer(pntrsize), pointer :: kfsz0
    integer(pntrsize), pointer :: kfsz1
    integer(pntrsize), pointer :: kfuz0
    integer(pntrsize), pointer :: kfuz1
    integer(pntrsize), pointer :: kfvz0
    integer(pntrsize), pointer :: kfvz1
    integer(pntrsize), pointer :: izmodl
    integer(pntrsize), pointer :: kcscut
    integer(pntrsize), pointer :: kcu45
    integer(pntrsize), pointer :: kcv45
    integer(pntrsize), pointer :: disint
    integer(pntrsize), pointer :: dismmt
    integer(pntrsize), pointer :: nambar
    integer(pntrsize), pointer :: nambnd
    integer(pntrsize), pointer :: namcon
    integer(pntrsize), pointer :: namdro
    integer(pntrsize), pointer :: namsrc
    integer(pntrsize), pointer :: tprofc
    integer(pntrsize), pointer :: tprofu
    integer(pntrsize), pointer :: typbnd
    integer(pntrsize), pointer :: zkfs
    integer(pntrsize), pointer :: iwrk1
    integer(pntrsize), pointer :: iwrk2
    integer(pntrsize), pointer :: iwrk3
    integer(pntrsize), pointer :: wrka1
    integer(pntrsize), pointer :: wrka2
    integer(pntrsize), pointer :: wrka3
    integer(pntrsize), pointer :: wrka4
    integer(pntrsize), pointer :: wrka5
    integer(pntrsize), pointer :: wrka6
    integer(pntrsize), pointer :: wrka7
    integer(pntrsize), pointer :: wrka8
    integer(pntrsize), pointer :: wrka9
    integer(pntrsize), pointer :: wrka10
    integer(pntrsize), pointer :: wrka11
    integer(pntrsize), pointer :: wrka12
    integer(pntrsize), pointer :: wrka13
    integer(pntrsize), pointer :: wrka14
    integer(pntrsize), pointer :: wrka15
    integer(pntrsize), pointer :: wrka16
    integer(pntrsize), pointer :: wrkb1
    integer(pntrsize), pointer :: wrkb2
    integer(pntrsize), pointer :: wrkb3
    integer(pntrsize), pointer :: wrkb4
    integer(pntrsize), pointer :: wrkb5
    integer(pntrsize), pointer :: wrkb6
    integer(pntrsize), pointer :: wrkb7
    integer(pntrsize), pointer :: wrkb8
    integer(pntrsize), pointer :: wrkb9
    integer(pntrsize), pointer :: wrkb10
    integer(pntrsize), pointer :: wrkb11
    integer(pntrsize), pointer :: wrkb12
    integer(pntrsize), pointer :: wrkb13
    integer(pntrsize), pointer :: wrkb14
    integer(pntrsize), pointer :: wrkb15
    integer(pntrsize), pointer :: wrkb16
    integer(pntrsize), pointer :: wrkb17
    integer(pntrsize), pointer :: wrkb18
    integer(pntrsize), pointer :: wrkc1
    integer(pntrsize), pointer :: wrkc2
    integer(pntrsize), pointer :: wrkc3
    integer(pntrsize), pointer :: wrkc4
    integer(pntrsize), pointer :: zwork
    integer(pntrsize), pointer :: zevap
    integer(pntrsize), pointer :: zprecp
    integer , pointer :: lmax
    integer , pointer :: kc
    integer , pointer :: ndro
!
! Global variables
!
!
! Local variables
!
    integer(pntrsize), external :: gtcpnt
    integer(pntrsize), external :: gtipnt
    integer(pntrsize), external :: gtrpnt
    integer(pntrsize), external :: gtdpnt
!
!! executable statements -------------------------------------------------------
!
    alfas      => gdp%gdr_i_ch%alfas
    alpha      => gdp%gdr_i_ch%alpha
    ampbc      => gdp%gdr_i_ch%ampbc
    areau      => gdp%gdr_i_ch%areau
    areav      => gdp%gdr_i_ch%areav
    atr        => gdp%gdr_i_ch%atr
    bruvai     => gdp%gdr_i_ch%bruvai
    c          => gdp%gdr_i_ch%c
    cbuv       => gdp%gdr_i_ch%cbuv
    cbuvrt     => gdp%gdr_i_ch%cbuvrt
    cdwlsu     => gdp%gdr_i_ch%cdwlsu
    cdwlsv     => gdp%gdr_i_ch%cdwlsv
    cdwzbu     => gdp%gdr_i_ch%cdwzbu
    cdwzbv     => gdp%gdr_i_ch%cdwzbv
    cdwztu     => gdp%gdr_i_ch%cdwztu
    cdwztv     => gdp%gdr_i_ch%cdwztv
    cfurou     => gdp%gdr_i_ch%cfurou
    cfvrou     => gdp%gdr_i_ch%cfvrou
    cgc        => gdp%gdr_i_ch%cgc
    cgdghf     => gdp%gdr_i_ch%cgdghf
    cgdghl     => gdp%gdr_i_ch%cgdghl
    cvalu0     => gdp%gdr_i_ch%cvalu0
    cvalv0     => gdp%gdr_i_ch%cvalv0
    circ2d     => gdp%gdr_i_ch%circ2d
    circ3d     => gdp%gdr_i_ch%circ3d
    crbc       => gdp%gdr_i_ch%crbc
    ctbf       => gdp%gdr_i_ch%ctbf
    ctbl       => gdp%gdr_i_ch%ctbl
    ctif       => gdp%gdr_i_ch%ctif
    ctil       => gdp%gdr_i_ch%ctil
    ctr        => gdp%gdr_i_ch%ctr
    ctrf       => gdp%gdr_i_ch%ctrf
    ctrl       => gdp%gdr_i_ch%ctrl
    czusus     => gdp%gdr_i_ch%czusus
    czvsus     => gdp%gdr_i_ch%czvsus
    dddeta     => gdp%gdr_i_ch%dddeta
    dddksi     => gdp%gdr_i_ch%dddksi
    disch0     => gdp%gdr_i_ch%disch0
    disch1     => gdp%gdr_i_ch%disch1
    decay      => gdp%gdr_i_ch%decay
    deltau     => gdp%gdr_i_ch%deltau
    deltav     => gdp%gdr_i_ch%deltav
    depchg     => gdp%gdr_i_ch%depchg
    dfu        => gdp%gdr_i_ch%dfu
    dfv        => gdp%gdr_i_ch%dfv
    diapl      => gdp%gdr_i_ch%diapl
    dicuv      => gdp%gdr_i_ch%dicuv
    dicww      => gdp%gdr_i_ch%dicww
    dircom     => gdp%gdr_i_ch%dircom
    dircos     => gdp%gdr_i_ch%dircos
    dirsin     => gdp%gdr_i_ch%dirsin
    dis        => gdp%gdr_i_ch%dis
    df         => gdp%gdr_i_ch%df
    disch      => gdp%gdr_i_ch%disch
    disinp     => gdp%gdr_i_ch%disinp
    discom     => gdp%gdr_i_ch%discom
    discum     => gdp%gdr_i_ch%discum
    dldeta     => gdp%gdr_i_ch%dldeta
    dldksi     => gdp%gdr_i_ch%dldksi
    dp         => gdp%gdr_i_ch%dp
    dpc        => gdp%gdr_i_ch%dpc
    dpdeta     => gdp%gdr_i_ch%dpdeta
    dpdksi     => gdp%gdr_i_ch%dpdksi
    dps        => gdp%gdr_i_ch%dps
    dpsed      => gdp%gdr_i_ch%dpsed
    dpu        => gdp%gdr_i_ch%dpu
    dpv        => gdp%gdr_i_ch%dpv
    drodep     => gdp%gdr_i_ch%drodep
    rint0      => gdp%gdr_i_ch%rint0
    rint1      => gdp%gdr_i_ch%rint1
    dsdeta     => gdp%gdr_i_ch%dsdeta
    dsdksi     => gdp%gdr_i_ch%dsdksi
    dtdeta     => gdp%gdr_i_ch%dtdeta
    dtdksi     => gdp%gdr_i_ch%dtdksi
    dteu       => gdp%gdr_i_ch%dteu
    dtev       => gdp%gdr_i_ch%dtev
    dtr        => gdp%gdr_i_ch%dtr
    dudz       => gdp%gdr_i_ch%dudz
    umdis0     => gdp%gdr_i_ch%umdis0
    umdis1     => gdp%gdr_i_ch%umdis1
    dvdz       => gdp%gdr_i_ch%dvdz
    vmdis0     => gdp%gdr_i_ch%vmdis0
    vmdis1     => gdp%gdr_i_ch%vmdis1
    dxydro     => gdp%gdr_i_ch%dxydro
    dzdeta     => gdp%gdr_i_ch%dzdeta
    dzdksi     => gdp%gdr_i_ch%dzdksi
    encgf      => gdp%gdr_i_ch%encgf
    encgl      => gdp%gdr_i_ch%encgl
    enstro     => gdp%gdr_i_ch%enstro
    entr       => gdp%gdr_i_ch%entr
    eroll0     => gdp%gdr_i_ch%eroll0
    eroll1     => gdp%gdr_i_ch%eroll1
    evap       => gdp%gdr_i_ch%evap
    ewabr0     => gdp%gdr_i_ch%ewabr0
    ewabr1     => gdp%gdr_i_ch%ewabr1
    ewave0     => gdp%gdr_i_ch%ewave0
    ewave1     => gdp%gdr_i_ch%ewave1
    excbed     => gdp%gdr_i_ch%excbed
    fcorio     => gdp%gdr_i_ch%fcorio
    fltr       => gdp%gdr_i_ch%fltr
    fuiwe      => gdp%gdr_i_ch%fuiwe
    fviwe      => gdp%gdr_i_ch%fviwe
    fxw        => gdp%gdr_i_ch%fxw
    fyw        => gdp%gdr_i_ch%fyw
    grmasu     => gdp%gdr_i_ch%grmasu
    grmasv     => gdp%gdr_i_ch%grmasv
    grmsur     => gdp%gdr_i_ch%grmsur
    grmsvr     => gdp%gdr_i_ch%grmsvr
    grfacu     => gdp%gdr_i_ch%grfacu
    grfacv     => gdp%gdr_i_ch%grfacv
    gro        => gdp%gdr_i_ch%gro
    gsqd       => gdp%gdr_i_ch%gsqd
    gsqs       => gdp%gdr_i_ch%gsqs
    guu        => gdp%gdr_i_ch%guu
    guv        => gdp%gdr_i_ch%guv
    gvu        => gdp%gdr_i_ch%gvu
    gvv        => gdp%gdr_i_ch%gvv
    hkru       => gdp%gdr_i_ch%hkru
    hkrv       => gdp%gdr_i_ch%hkrv
    hrmcom     => gdp%gdr_i_ch%hrmcom
    hrms       => gdp%gdr_i_ch%hrms
    hu         => gdp%gdr_i_ch%hu
    hu0        => gdp%gdr_i_ch%hu0
    huvw       => gdp%gdr_i_ch%huvw
    hv         => gdp%gdr_i_ch%hv
    hv0        => gdp%gdr_i_ch%hv0
    hydrbc     => gdp%gdr_i_ch%hydrbc
    msucom     => gdp%gdr_i_ch%msucom
    msvcom     => gdp%gdr_i_ch%msvcom
    ombc       => gdp%gdr_i_ch%ombc
    omega      => gdp%gdr_i_ch%omega
    patm       => gdp%gdr_i_ch%patm
    phibc      => gdp%gdr_i_ch%phibc
    porosu     => gdp%gdr_i_ch%porosu
    porosv     => gdp%gdr_i_ch%porosv
    precip     => gdp%gdr_i_ch%precip
    procbc     => gdp%gdr_i_ch%procbc
    pship      => gdp%gdr_i_ch%pship
    qsrcrt     => gdp%gdr_i_ch%qsrcrt
    qtfrac     => gdp%gdr_i_ch%qtfrac
    qtfrct     => gdp%gdr_i_ch%qtfrct
    qtfrt2     => gdp%gdr_i_ch%qtfrt2
    qu         => gdp%gdr_i_ch%qu
    qv         => gdp%gdr_i_ch%qv
    qxk        => gdp%gdr_i_ch%qxk
    qxkr       => gdp%gdr_i_ch%qxkr
    qxkw       => gdp%gdr_i_ch%qxkw
    qyk        => gdp%gdr_i_ch%qyk
    qykr       => gdp%gdr_i_ch%qykr
    qykw       => gdp%gdr_i_ch%qykw
    qzk        => gdp%gdr_i_ch%qzk
    r0         => gdp%gdr_i_ch%r0
    r1         => gdp%gdr_i_ch%r1
    rbnd       => gdp%gdr_i_ch%rbnd
    rbuff      => gdp%gdr_i_ch%rbuff
    rettim     => gdp%gdr_i_ch%rettim
    rho        => gdp%gdr_i_ch%rho
    rhowat     => gdp%gdr_i_ch%rhowat
    rich       => gdp%gdr_i_ch%rich
    rint       => gdp%gdr_i_ch%rint
    rintsm     => gdp%gdr_i_ch%rintsm
    rlabda     => gdp%gdr_i_ch%rlabda
    rmneg      => gdp%gdr_i_ch%rmneg
    rnpl       => gdp%gdr_i_ch%rnpl
    rob        => gdp%gdr_i_ch%rob
    rsed       => gdp%gdr_i_ch%rsed
    rthbnd     => gdp%gdr_i_ch%rthbnd
    rtu2d0     => gdp%gdr_i_ch%rtu2d0
    rtu2d1     => gdp%gdr_i_ch%rtu2d1
    rtubnd     => gdp%gdr_i_ch%rtubnd
    rtur0      => gdp%gdr_i_ch%rtur0
    rtur1      => gdp%gdr_i_ch%rtur1
    rxx        => gdp%gdr_i_ch%rxx
    rxy        => gdp%gdr_i_ch%rxy
    ryy        => gdp%gdr_i_ch%ryy
    rxz        => gdp%gdr_i_ch%rxz
    ryz        => gdp%gdr_i_ch%ryz
    s0         => gdp%gdr_i_ch%s0
    s1         => gdp%gdr_i_ch%s1
    sbtr       => gdp%gdr_i_ch%sbtr
    sbtrc      => gdp%gdr_i_ch%sbtrc
    sbuu       => gdp%gdr_i_ch%sbuu
    sbvv       => gdp%gdr_i_ch%sbvv
    seddif     => gdp%gdr_i_ch%seddif
    sepsus     => gdp%gdr_i_ch%sepsus
    sig        => gdp%gdr_i_ch%sig
    sigdif     => gdp%gdr_i_ch%sigdif
    sigmol     => gdp%gdr_i_ch%sigmol
    sink       => gdp%gdr_i_ch%sink
    sinkr      => gdp%gdr_i_ch%sinkr
    sinkw      => gdp%gdr_i_ch%sinkw
    soumud     => gdp%gdr_i_ch%soumud
    sour       => gdp%gdr_i_ch%sour
    sourr      => gdp%gdr_i_ch%sourr
    sourw      => gdp%gdr_i_ch%sourw
    sstr       => gdp%gdr_i_ch%sstr
    sstrc      => gdp%gdr_i_ch%sstrc
    stbf       => gdp%gdr_i_ch%stbf
    stbl       => gdp%gdr_i_ch%stbl
    stif       => gdp%gdr_i_ch%stif
    stil       => gdp%gdr_i_ch%stil
    sumrho     => gdp%gdr_i_ch%sumrho
    taubmx     => gdp%gdr_i_ch%taubmx
    taubpu     => gdp%gdr_i_ch%taubpu
    taubpv     => gdp%gdr_i_ch%taubpv
    taubsu     => gdp%gdr_i_ch%taubsu
    taubsv     => gdp%gdr_i_ch%taubsv
    teta       => gdp%gdr_i_ch%teta
    tgarkt     => gdp%gdr_i_ch%tgarkt
    tgarkx     => gdp%gdr_i_ch%tgarkx
    tgarnp     => gdp%gdr_i_ch%tgarnp
    thetbc     => gdp%gdr_i_ch%thetbc
    thick      => gdp%gdr_i_ch%thick
    thtim      => gdp%gdr_i_ch%thtim
    tkedis     => gdp%gdr_i_ch%tkedis
    tkepro     => gdp%gdr_i_ch%tkepro
    tp         => gdp%gdr_i_ch%tp
    tpcom      => gdp%gdr_i_ch%tpcom
    u0         => gdp%gdr_i_ch%u0
    u1         => gdp%gdr_i_ch%u1
    ubrlsu     => gdp%gdr_i_ch%ubrlsu
    ubrlsv     => gdp%gdr_i_ch%ubrlsv
    umdis      => gdp%gdr_i_ch%umdis
    umean      => gdp%gdr_i_ch%umean
    umeanf     => gdp%gdr_i_ch%umeanf
    umeanl     => gdp%gdr_i_ch%umeanl
    umnflc     => gdp%gdr_i_ch%umnflc
    umnldf     => gdp%gdr_i_ch%umnldf
    uorb       => gdp%gdr_i_ch%uorb
    ubot       => gdp%gdr_i_ch%ubot
    ubcom      => gdp%gdr_i_ch%ubcom
    usus       => gdp%gdr_i_ch%usus
    uvdist     => gdp%gdr_i_ch%uvdist
    uwtypu     => gdp%gdr_i_ch%uwtypu
    uwtypv     => gdp%gdr_i_ch%uwtypv
    v0         => gdp%gdr_i_ch%v0
    v1         => gdp%gdr_i_ch%v1
    vicuv      => gdp%gdr_i_ch%vicuv
    vicww      => gdp%gdr_i_ch%vicww
    vmdis      => gdp%gdr_i_ch%vmdis
    vmean      => gdp%gdr_i_ch%vmean
    vmnflc     => gdp%gdr_i_ch%vmnflc
    vmnldf     => gdp%gdr_i_ch%vmnldf
    vnu2d      => gdp%gdr_i_ch%vnu2d
    vnu3d      => gdp%gdr_i_ch%vnu3d
    voldis     => gdp%gdr_i_ch%voldis
    volum0     => gdp%gdr_i_ch%volum0
    volum1     => gdp%gdr_i_ch%volum1
    vortic     => gdp%gdr_i_ch%vortic
    vsus       => gdp%gdr_i_ch%vsus
    w1         => gdp%gdr_i_ch%w1
    w10mag     => gdp%gdr_i_ch%w10mag
    wenf       => gdp%gdr_i_ch%wenf
    wenfm      => gdp%gdr_i_ch%wenfm
    wenl       => gdp%gdr_i_ch%wenl
    wenlm      => gdp%gdr_i_ch%wenlm
    windsu     => gdp%gdr_i_ch%windsu
    windsv     => gdp%gdr_i_ch%windsv
    windcd     => gdp%gdr_i_ch%windcd
    windu      => gdp%gdr_i_ch%windu
    windv      => gdp%gdr_i_ch%windv
    wlen       => gdp%gdr_i_ch%wlen
    wlcom      => gdp%gdr_i_ch%wlcom
    wphy       => gdp%gdr_i_ch%wphy
    ws         => gdp%gdr_i_ch%ws
    wssus      => gdp%gdr_i_ch%wssus
    wstau      => gdp%gdr_i_ch%wstau
    wsu        => gdp%gdr_i_ch%wsu
    wsucom     => gdp%gdr_i_ch%wsucom
    wsv        => gdp%gdr_i_ch%wsv
    wsvcom     => gdp%gdr_i_ch%wsvcom
    wsbodyu    => gdp%gdr_i_ch%wsbodyu
    wsbodyucom => gdp%gdr_i_ch%wsbodyucom
    wsbodyv    => gdp%gdr_i_ch%wsbodyv
    wsbodyvcom => gdp%gdr_i_ch%wsbodyvcom
    x2y        => gdp%gdr_i_ch%x2y
    x3         => gdp%gdr_i_ch%x3
    xcor       => gdp%gdr_i_ch%xcor
    xy2        => gdp%gdr_i_ch%xy2
    xydro      => gdp%gdr_i_ch%xydro
    xyzsrc     => gdp%gdr_i_ch%xyzsrc
    xz         => gdp%gdr_i_ch%xz
    y3         => gdp%gdr_i_ch%y3
    ycor       => gdp%gdr_i_ch%ycor
    yz         => gdp%gdr_i_ch%yz
    z0ucur     => gdp%gdr_i_ch%z0ucur
    z0vcur     => gdp%gdr_i_ch%z0vcur
    z0urou     => gdp%gdr_i_ch%z0urou
    z0vrou     => gdp%gdr_i_ch%z0vrou
    zalfas     => gdp%gdr_i_ch%zalfas
    zbdsed     => gdp%gdr_i_ch%zbdsed
    zbmnf      => gdp%gdr_i_ch%zbmnf
    zbmnl      => gdp%gdr_i_ch%zbmnl
    zcuru      => gdp%gdr_i_ch%zcuru
    zcurv      => gdp%gdr_i_ch%zcurv
    zcurw      => gdp%gdr_i_ch%zcurw
    zdicww     => gdp%gdr_i_ch%zdicww
    zdist      => gdp%gdr_i_ch%zdist
    zdps       => gdp%gdr_i_ch%zdps
    zdpsed     => gdp%gdr_i_ch%zdpsed
    zenst      => gdp%gdr_i_ch%zenst
    zetabf     => gdp%gdr_i_ch%zetabf
    zetabl     => gdp%gdr_i_ch%zetabl
    zetaif     => gdp%gdr_i_ch%zetaif
    zetail     => gdp%gdr_i_ch%zetail
    zfixfac    => gdp%gdr_i_ch%zfixfac
    zfrac      => gdp%gdr_i_ch%zfrac
    zhidexp    => gdp%gdr_i_ch%zhidexp
    zmeanf     => gdp%gdr_i_ch%zmeanf
    zmeanl     => gdp%gdr_i_ch%zmeanl
    zmudfrac   => gdp%gdr_i_ch%zmudfrac
    zqxk       => gdp%gdr_i_ch%zqxk
    zqyk       => gdp%gdr_i_ch%zqyk
    zrca       => gdp%gdr_i_ch%zrca
    zrho       => gdp%gdr_i_ch%zrho
    zrich      => gdp%gdr_i_ch%zrich
    zrsdeq     => gdp%gdr_i_ch%zrsdeq
    zsandfrac  => gdp%gdr_i_ch%zsandfrac
    zsbu       => gdp%gdr_i_ch%zsbu
    zsbv       => gdp%gdr_i_ch%zsbv
    zseddif    => gdp%gdr_i_ch%zseddif
    zsinkse    => gdp%gdr_i_ch%zsinkse
    zsourse    => gdp%gdr_i_ch%zsourse
    zssu       => gdp%gdr_i_ch%zssu
    zssv       => gdp%gdr_i_ch%zssv
    zstep      => gdp%gdr_i_ch%zstep
    ztaub      => gdp%gdr_i_ch%ztaub
    ztauet     => gdp%gdr_i_ch%ztauet
    ztauks     => gdp%gdr_i_ch%ztauks
    ztur       => gdp%gdr_i_ch%ztur
    zvicww     => gdp%gdr_i_ch%zvicww
    zvort      => gdp%gdr_i_ch%zvort
    zwl        => gdp%gdr_i_ch%zwl
    zws        => gdp%gdr_i_ch%zws
    zwndsp     => gdp%gdr_i_ch%zwndsp
    zwndcd     => gdp%gdr_i_ch%zwndcd
    zwnddr     => gdp%gdr_i_ch%zwnddr
    zairp      => gdp%gdr_i_ch%zairp
    zprecp     => gdp%gdr_i_ch%zprecp
    zevap      => gdp%gdr_i_ch%zevap
    drhodx     => gdp%gdr_i_ch%drhodx
    drhody     => gdp%gdr_i_ch%drhody
    dzs0       => gdp%gdr_i_ch%dzs0
    dzs1       => gdp%gdr_i_ch%dzs1
    dzu0       => gdp%gdr_i_ch%dzu0
    dzu1       => gdp%gdr_i_ch%dzu1
    dzv0       => gdp%gdr_i_ch%dzv0
    dzv1       => gdp%gdr_i_ch%dzv1
    res        => gdp%gdr_i_ch%res
    rl         => gdp%gdr_i_ch%rl
    rj         => gdp%gdr_i_ch%rj
    p1         => gdp%gdr_i_ch%p1
    p0         => gdp%gdr_i_ch%p0
    p00        => gdp%gdr_i_ch%p00
    pnhcor     => gdp%gdr_i_ch%pnhcor
    w0         => gdp%gdr_i_ch%w0
    s00        => gdp%gdr_i_ch%s00
    hydprs     => gdp%gdr_i_ch%hydprs
    guz        => gdp%gdr_i_ch%guz
    gvz        => gdp%gdr_i_ch%gvz
    gud        => gdp%gdr_i_ch%gud
    gvd        => gdp%gdr_i_ch%gvd
    gsqiu      => gdp%gdr_i_ch%gsqiu
    gsqiv      => gdp%gdr_i_ch%gsqiv
    ibuff      => gdp%gdr_i_ch%ibuff
    idifu      => gdp%gdr_i_ch%idifu
    irocol     => gdp%gdr_i_ch%irocol
    iroll      => gdp%gdr_i_ch%iroll
    itbcc      => gdp%gdr_i_ch%itbcc
    itbct      => gdp%gdr_i_ch%itbct
    itdis      => gdp%gdr_i_ch%itdis
    itdro      => gdp%gdr_i_ch%itdro
    kadu       => gdp%gdr_i_ch%kadu
    kadv       => gdp%gdr_i_ch%kadv
    kcs        => gdp%gdr_i_ch%kcs
    kcu        => gdp%gdr_i_ch%kcu
    kcv        => gdp%gdr_i_ch%kcv
    kfs        => gdp%gdr_i_ch%kfs
    kfu        => gdp%gdr_i_ch%kfu
    kfv        => gdp%gdr_i_ch%kfv
    kspu       => gdp%gdr_i_ch%kspu
    kspv       => gdp%gdr_i_ch%kspv
    kstp       => gdp%gdr_i_ch%kstp
    kzs        => gdp%gdr_i_ch%kzs
    kzu        => gdp%gdr_i_ch%kzu
    kzv        => gdp%gdr_i_ch%kzv
    mnbar      => gdp%gdr_i_ch%mnbar
    mnbnd      => gdp%gdr_i_ch%mnbnd
    mndro      => gdp%gdr_i_ch%mndro
    mnksrc     => gdp%gdr_i_ch%mnksrc
    nob        => gdp%gdr_i_ch%nob
    kcshyd     => gdp%gdr_i_ch%kcshyd
    kfumin     => gdp%gdr_i_ch%kfumin
    kfvmin     => gdp%gdr_i_ch%kfvmin
    kfsmin     => gdp%gdr_i_ch%kfsmin
    kfumax     => gdp%gdr_i_ch%kfumax
    kfvmax     => gdp%gdr_i_ch%kfvmax
    kfsmax     => gdp%gdr_i_ch%kfsmax
    kfumx0     => gdp%gdr_i_ch%kfumx0
    kfvmx0     => gdp%gdr_i_ch%kfvmx0
    kfsmx0     => gdp%gdr_i_ch%kfsmx0
    kfumn0     => gdp%gdr_i_ch%kfumn0
    kfvmn0     => gdp%gdr_i_ch%kfvmn0
    kfsmn0     => gdp%gdr_i_ch%kfsmn0
    kfsz0      => gdp%gdr_i_ch%kfsz0
    kfsz1      => gdp%gdr_i_ch%kfsz1
    kfuz0      => gdp%gdr_i_ch%kfuz0
    kfuz1      => gdp%gdr_i_ch%kfuz1
    kfvz0      => gdp%gdr_i_ch%kfvz0
    kfvz1      => gdp%gdr_i_ch%kfvz1
    izmodl     => gdp%gdr_i_ch%izmodl
    kcscut     => gdp%gdr_i_ch%kcscut
    kcu45      => gdp%gdr_i_ch%kcu45
    kcv45      => gdp%gdr_i_ch%kcv45
    disint     => gdp%gdr_i_ch%disint
    dismmt     => gdp%gdr_i_ch%dismmt
    nambar     => gdp%gdr_i_ch%nambar
    nambnd     => gdp%gdr_i_ch%nambnd
    namcon     => gdp%gdr_i_ch%namcon
    namdro     => gdp%gdr_i_ch%namdro
    namsrc     => gdp%gdr_i_ch%namsrc
    tprofc     => gdp%gdr_i_ch%tprofc
    tprofu     => gdp%gdr_i_ch%tprofu
    typbnd     => gdp%gdr_i_ch%typbnd
    zkfs       => gdp%gdr_i_ch%zkfs
    iwrk1      => gdp%gdaddress%iwrk1
    iwrk2      => gdp%gdaddress%iwrk2
    iwrk3      => gdp%gdaddress%iwrk3
    wrka1      => gdp%gdaddress%wrka1
    wrka2      => gdp%gdaddress%wrka2
    wrka3      => gdp%gdaddress%wrka3
    wrka4      => gdp%gdaddress%wrka4
    wrka5      => gdp%gdaddress%wrka5
    wrka6      => gdp%gdaddress%wrka6
    wrka7      => gdp%gdaddress%wrka7
    wrka8      => gdp%gdaddress%wrka8
    wrka9      => gdp%gdaddress%wrka9
    wrka10     => gdp%gdaddress%wrka10
    wrka11     => gdp%gdaddress%wrka11
    wrka12     => gdp%gdaddress%wrka12
    wrka13     => gdp%gdaddress%wrka13
    wrka14     => gdp%gdaddress%wrka14
    wrka15     => gdp%gdaddress%wrka15
    wrka16     => gdp%gdaddress%wrka16
    wrkb1      => gdp%gdaddress%wrkb1
    wrkb2      => gdp%gdaddress%wrkb2
    wrkb3      => gdp%gdaddress%wrkb3
    wrkb4      => gdp%gdaddress%wrkb4
    wrkb5      => gdp%gdaddress%wrkb5
    wrkb6      => gdp%gdaddress%wrkb6
    wrkb7      => gdp%gdaddress%wrkb7
    wrkb8      => gdp%gdaddress%wrkb8
    wrkb9      => gdp%gdaddress%wrkb9
    wrkb10     => gdp%gdaddress%wrkb10
    wrkb11     => gdp%gdaddress%wrkb11
    wrkb12     => gdp%gdaddress%wrkb12
    wrkb13     => gdp%gdaddress%wrkb13
    wrkb14     => gdp%gdaddress%wrkb14
    wrkb15     => gdp%gdaddress%wrkb15
    wrkb16     => gdp%gdaddress%wrkb16
    wrkb17     => gdp%gdaddress%wrkb17
    wrkb18     => gdp%gdaddress%wrkb18
    wrkc1      => gdp%gdaddress%wrkc1
    wrkc2      => gdp%gdaddress%wrkc2
    wrkc3      => gdp%gdaddress%wrkc3
    wrkc4      => gdp%gdaddress%wrkc4
    zwork      => gdp%gdaddress%zwork
    lmax       => gdp%d%lmax
    kc         => gdp%d%kc
    ndro       => gdp%d%ndro
!
! CHARACTER POOL ARRAYS
!
    disint     = gtcpnt('disint', gdp)
    dismmt     = gtcpnt('dismmt', gdp)
    nambar     = gtcpnt('nambar', gdp)
    nambnd     = gtcpnt('nambnd', gdp)
    namcon     = gtcpnt('namcon', gdp)
    namdro     = gtcpnt('namdro', gdp)
    namsrc     = gtcpnt('NAMSRC', gdp)
    tprofc     = gtcpnt('tprofc', gdp)
    tprofu     = gtcpnt('tprofu', gdp)
    typbnd     = gtcpnt('typbnd', gdp)
!
! INTEGER POOL ARRAY
!
    ibuff      = gtipnt('ibuff' , gdp)
    idifu      = gtipnt('idifu' , gdp)
    irocol     = gtipnt('IROCOL', gdp)
    iroll      = gtipnt('iroll' , gdp)
    itbcc      = gtipnt('itbcc' , gdp)
    itbct      = gtipnt('itbct' , gdp)
    itdis      = gtipnt('itdis' , gdp)
    itdro      = gtipnt('itdro' , gdp)
    izmodl     = gtipnt('izmodl', gdp)
    kadu       = gtipnt('kadu'  , gdp)
    kadv       = gtipnt('kadv'  , gdp)
    kcs        = gtipnt('kcs'   , gdp)
    kcscut     = gtipnt('kcscut', gdp)
    kcshyd     = gtipnt('kcshyd', gdp)
    kcu        = gtipnt('kcu'   , gdp)
    kcu45      = gtipnt('kcu45' , gdp)
    kcv        = gtipnt('kcv'   , gdp)
    kcv45      = gtipnt('kcv45' , gdp)
    kfs        = gtipnt('kfs'   , gdp)
    kfsmax     = gtipnt('kfsmax', gdp)
    kfsmin     = gtipnt('kfsmin', gdp)
    kfsmx0     = gtipnt('kfsmx0', gdp)
    kfsmn0     = gtipnt('kfsmn0', gdp)
    kfsz0      = gtipnt('kfsz0' , gdp)
    kfsz1      = gtipnt('kfsz1' , gdp)
    kfu        = gtipnt('kfu'   , gdp)
    kfumax     = gtipnt('kfumax', gdp)
    kfumin     = gtipnt('kfumin', gdp)
    kfumx0     = gtipnt('kfumx0', gdp)
    kfumn0     = gtipnt('kfumn0', gdp)
    kfuz0      = gtipnt('kfuz0' , gdp)
    kfuz1      = gtipnt('kfuz1' , gdp)
    kfv        = gtipnt('kfv'   , gdp)
    kfvmax     = gtipnt('kfvmax', gdp)
    kfvmin     = gtipnt('kfvmin', gdp)
    kfvmx0     = gtipnt('kfvmx0', gdp)
    kfvmn0     = gtipnt('kfvmn0', gdp)
    kfvz0      = gtipnt('kfvz0' , gdp)
    kfvz1      = gtipnt('kfvz1' , gdp)
    kspu       = gtipnt('kspu'  , gdp)
    kspv       = gtipnt('kspv'  , gdp)
    kstp       = gtipnt('kstp'  , gdp)
    kzs        = gtipnt('kzs'   , gdp)
    kzu        = gtipnt('kzu'   , gdp)
    kzv        = gtipnt('kzv'   , gdp)
    mnbar      = gtipnt('mnbar' , gdp)
    mnbnd      = gtipnt('MNBND' , gdp)
    mndro      = gtipnt('mndro' , gdp)
    mnksrc     = gtipnt('MNKSRC', gdp)
    nob        = gtipnt('NOB'   , gdp)
    zkfs       = gtipnt('zkfs'  , gdp)
!
! REAL POOL ARRAY
!
    alfas      = gtrpnt('alfas' , gdp)
    alpha      = gtrpnt('alpha' , gdp)
    ampbc      = gtrpnt('ampbc' , gdp)
    areau      = gtrpnt('areau' , gdp)
    areav      = gtrpnt('areav' , gdp)
    atr        = gtrpnt('atr'   , gdp)
    bruvai     = gtrpnt('bruvai', gdp)
    c          = gtrpnt('c'     , gdp)
    cbuv       = gtrpnt('cbuv'  , gdp)
    cbuvrt     = gtrpnt('cbuvrt', gdp)
    cdwlsu     = gtrpnt('cdwlsu', gdp)
    cdwlsv     = gtrpnt('cdwlsv', gdp)
    cdwzbu     = gtrpnt('cdwzbu', gdp)
    cdwzbv     = gtrpnt('cdwzbv', gdp)
    cdwztu     = gtrpnt('cdwztu', gdp)
    cdwztv     = gtrpnt('cdwztv', gdp)
    cfurou     = gtrpnt('cfurou', gdp)
    cfvrou     = gtrpnt('cfvrou', gdp)
    cgc        = gtrpnt('cgc'   , gdp)
    cgdghf     = gtrpnt('cgdghf', gdp)
    cgdghl     = gtrpnt('cgdghl', gdp)
    circ2d     = gtrpnt('circ2d', gdp)
    circ3d     = gtrpnt('circ3d', gdp)
    crbc       = gtrpnt('crbc'  , gdp)
    ctbf       = gtrpnt('ctbf'  , gdp)
    ctbl       = gtrpnt('ctbl'  , gdp)
    ctif       = gtrpnt('ctif'  , gdp)
    ctil       = gtrpnt('ctil'  , gdp)
    ctr        = gtrpnt('ctr'   , gdp)
    ctrf       = gtrpnt('ctrf'  , gdp)
    ctrl       = gtrpnt('ctrl'  , gdp)
    cvalu0     = gtrpnt('cvalu0', gdp)
    cvalv0     = gtrpnt('cvalv0', gdp)
    czusus     = gtrpnt('czusus', gdp)
    czvsus     = gtrpnt('czvsus', gdp)
    dddeta     = gtrpnt('dddeta', gdp)
    dddksi     = gtrpnt('dddksi', gdp)
    disch0     = gtrpnt('disch0', gdp)
    disch1     = gtrpnt('disch1', gdp)
    decay      = gtrpnt('decay' , gdp)
    deltau     = gtrpnt('deltau', gdp)
    deltav     = gtrpnt('deltav', gdp)
    depchg     = gtrpnt('depchg', gdp)
    dfu        = gtrpnt('dfu'   , gdp)
    dfv        = gtrpnt('dfv'   , gdp)
    dicuv      = gtrpnt('dicuv' , gdp)
    dicww      = gtrpnt('dicww' , gdp)
    dircom     = gtrpnt('dircom', gdp)
    dircos     = gtrpnt('dircos', gdp)
    dirsin     = gtrpnt('dirsin', gdp)
    dis        = gtrpnt('dis'   , gdp)
    df         = gtrpnt('df'    , gdp)
    disch      = gtrpnt('disch' , gdp)
    discom     = gtrpnt('discom', gdp)
    discum     = gtrpnt('discum', gdp)
    disinp     = gtrpnt('disinp', gdp)
    dldeta     = gtrpnt('dldeta', gdp)
    dldksi     = gtrpnt('dldksi', gdp)
    dp         = gtrpnt('dp'    , gdp)
    dpc        = gtrpnt('dpc'   , gdp)
    dpdeta     = gtrpnt('dpdeta', gdp)
    dpdksi     = gtrpnt('dpdksi', gdp)
    if (prec == sp) then
       dps     = gtrpnt('dps'   , gdp)
    elseif (prec == hp) then
       dps     = gtdpnt('dps'   , gdp)
    else
       ! catched in esm_alloc_real.f90
    endif
    dpu        = gtrpnt('dpu'   , gdp)
    dpv        = gtrpnt('dpv'   , gdp)
    drhodx     = gtrpnt('drhodx', gdp)
    drhody     = gtrpnt('drhody', gdp)
    rint0      = gtrpnt('rint0' , gdp)
    rint1      = gtrpnt('rint1' , gdp)
    drodep     = gtrpnt('drodep', gdp)
    dsdeta     = gtrpnt('dsdeta', gdp)
    dsdksi     = gtrpnt('dsdksi', gdp)
    dtdeta     = gtrpnt('dtdeta', gdp)
    dtdksi     = gtrpnt('dtdksi', gdp)
    dteu       = gtrpnt('dteu'  , gdp)
    dtev       = gtrpnt('dtev'  , gdp)
    dtr        = gtrpnt('dtr'   , gdp)
    dudz       = gtrpnt('dudz'  , gdp)
    umdis0     = gtrpnt('umdis0', gdp)
    umdis1     = gtrpnt('umdis1', gdp)
    dvdz       = gtrpnt('dvdz'  , gdp)
    vmdis0     = gtrpnt('vmdis0', gdp)
    vmdis1     = gtrpnt('vmdis1', gdp)
    dxydro     = gtrpnt('dxydro', gdp)
    dzdeta     = gtrpnt('dzdeta', gdp)
    dzdksi     = gtrpnt('dzdksi', gdp)
    dzs0       = gtrpnt('dzs0'  , gdp)
    dzs1       = gtrpnt('dzs1'  , gdp)
    dzu0       = gtrpnt('dzu0'  , gdp)
    dzu1       = gtrpnt('dzu1'  , gdp)
    dzv0       = gtrpnt('dzv0'  , gdp)
    dzv1       = gtrpnt('dzv1'  , gdp)
    encgf      = gtrpnt('encgf' , gdp)
    encgl      = gtrpnt('encgl' , gdp)
    entr       = gtrpnt('entr'  , gdp)
    enstro     = gtrpnt('enstro', gdp)
    eroll0     = gtrpnt('eroll0', gdp)
    eroll1     = gtrpnt('eroll1', gdp)
    evap       = gtrpnt('evap'  , gdp)
    ewabr0     = gtrpnt('ewabr0', gdp)
    ewabr1     = gtrpnt('ewabr1', gdp)
    ewave0     = gtrpnt('ewave0', gdp)
    ewave1     = gtrpnt('ewave1', gdp)
    excbed     = gtrpnt('excbed', gdp)
    fcorio     = gtrpnt('fcorio', gdp)
    fltr       = gtrpnt('fltr'  , gdp)
    fuiwe      = gtrpnt('fuiwe' , gdp)
    fviwe      = gtrpnt('fviwe' , gdp)
    fxw        = gtrpnt('fxw'   , gdp)
    fyw        = gtrpnt('fyw'   , gdp)
    grmasu     = gtrpnt('grmasu', gdp)
    grmasv     = gtrpnt('grmasv', gdp)
    grmsur     = gtrpnt('grmsur', gdp)
    grmsvr     = gtrpnt('grmsvr', gdp)
    grfacu     = gtrpnt('grfacu', gdp)
    grfacv     = gtrpnt('grfacv', gdp)
    gro        = gtrpnt('gro'   , gdp)
    gsqd       = gtrpnt('gsqd'  , gdp)
    gsqiu      = gtrpnt('gsqiu' , gdp)
    gsqiv      = gtrpnt('gsqiv' , gdp)
    gsqs       = gtrpnt('gsqs'  , gdp)
    gud        = gtrpnt('gud'   , gdp)
    guu        = gtrpnt('guu'   , gdp)
    guv        = gtrpnt('guv'   , gdp)
    guz        = gtrpnt('guz'   , gdp)
    gvd        = gtrpnt('gvd'   , gdp)
    gvz        = gtrpnt('gvz'   , gdp)
    gvu        = gtrpnt('gvu'   , gdp)
    gvv        = gtrpnt('gvv'   , gdp)
    hkru       = gtrpnt('hkru'  , gdp)
    hkrv       = gtrpnt('hkrv'  , gdp)
    hrmcom     = gtrpnt('hrmcom', gdp)
    hrms       = gtrpnt('hrms'  , gdp)
    hu         = gtrpnt('hu'    , gdp)
    hu0        = gtrpnt('hu0'   , gdp)
    huvw       = gtrpnt('huvw'  , gdp)
    hv         = gtrpnt('hv'    , gdp)
    hv0        = gtrpnt('hv0'   , gdp)
    hydprs     = gtrpnt('hydprs', gdp)
    hydrbc     = gtrpnt('hydrbc', gdp)
    msucom     = gtrpnt('msucom', gdp)
    msvcom     = gtrpnt('msvcom', gdp)
    ombc       = gtrpnt('ombc'  , gdp)
    omega      = gtrpnt('omega' , gdp)
    patm       = gtrpnt('patm'  , gdp)
    phibc      = gtrpnt('phibc' , gdp)
    porosu     = gtrpnt('porosu', gdp)
    porosv     = gtrpnt('porosv', gdp)
    precip     = gtrpnt('precip', gdp)
    procbc     = gtrpnt('procbc', gdp)
    pship      = gtrpnt('pship' , gdp)
    qsrcrt     = gtrpnt('qsrcrt', gdp)
    qtfrac     = gtrpnt('qtfrac', gdp)
    qtfrct     = gtrpnt('qtfrct', gdp)
    qtfrt2     = gtrpnt('qtfrt2', gdp)
    qu         = gtrpnt('qu'    , gdp)
    qv         = gtrpnt('qv'    , gdp)
    qxk        = gtrpnt('qxk'   , gdp)
    qxkr       = gtrpnt('qxkr'  , gdp)
    qxkw       = gtrpnt('qxkw'  , gdp)
    qyk        = gtrpnt('qyk'   , gdp)
    qykr       = gtrpnt('qykr'  , gdp)
    qykw       = gtrpnt('qykw'  , gdp)
    qzk        = gtrpnt('qzk'   , gdp)
    r0         = gtrpnt('r0'    , gdp)
    r1         = gtrpnt('r1'    , gdp)
    rbnd       = gtrpnt('rbnd'  , gdp)
    rettim     = gtrpnt('rettim', gdp)
    rho        = gtrpnt('rho'   , gdp)
    rhowat     = gtrpnt('rhowat', gdp)
    rich       = gtrpnt('rich'  , gdp)
    rint       = gtrpnt('rint'  , gdp)
    rintsm     = gtrpnt('rintsm', gdp)
    rlabda     = gtrpnt('rlabda', gdp)
    rmneg      = gtrpnt('rmneg' , gdp)
    rob        = gtrpnt('rob'   , gdp)
    rsed       = gtrpnt('rsed'  , gdp)
    rthbnd     = gtrpnt('rthbnd', gdp)
    rtu2d0     = gtrpnt('rtu2d0', gdp)
    rtu2d1     = gtrpnt('rtu2d1', gdp)
    rtubnd     = gtrpnt('rtubnd', gdp)
    rtur0      = gtrpnt('rtur0' , gdp)
    rtur1      = gtrpnt('rtur1' , gdp)
    rxx        = gtrpnt('rxx'   , gdp)
    rxy        = gtrpnt('rxy'   , gdp)
    ryy        = gtrpnt('ryy'   , gdp)
    rxz        = gtrpnt('rxz'   , gdp)
    ryz        = gtrpnt('ryz'   , gdp)
    s0         = gtrpnt('s0'    , gdp)
    s1         = gtrpnt('s1'    , gdp)
    sbtr       = gtrpnt('sbtr'  , gdp)
    sbtrc      = gtrpnt('sbtrc' , gdp)
    sbuu       = gtrpnt('sbuu'  , gdp)
    sbvv       = gtrpnt('sbvv'  , gdp)
    seddif     = gtrpnt('seddif', gdp)
    sepsus     = gtrpnt('sepsus', gdp)
    sig        = gtrpnt('sig'   , gdp)
    sigdif     = gtrpnt('sigdif', gdp)
    sigmol     = gtrpnt('sigmol', gdp)
    sink       = gtrpnt('sink'  , gdp)
    sinkr      = gtrpnt('sinkr' , gdp)
    sinkw      = gtrpnt('sinkw' , gdp)
    soumud     = gtrpnt('soumud', gdp)
    sour       = gtrpnt('sour'  , gdp)
    sourr      = gtrpnt('sourr' , gdp)
    sourw      = gtrpnt('sourw' , gdp)
    sstr       = gtrpnt('sstr'  , gdp)
    sstrc      = gtrpnt('sstrc' , gdp)
    stbf       = gtrpnt('stbf'  , gdp)
    stbl       = gtrpnt('stbl'  , gdp)
    stif       = gtrpnt('stif'  , gdp)
    stil       = gtrpnt('stil'  , gdp)
    sumrho     = gtrpnt('sumrho', gdp)
    taubmx     = gtrpnt('taubmx', gdp)
    taubpu     = gtrpnt('taubpu', gdp)
    taubpv     = gtrpnt('taubpv', gdp)
    taubsu     = gtrpnt('taubsu', gdp)
    taubsv     = gtrpnt('taubsv', gdp)
    teta       = gtrpnt('teta'  , gdp)
    thetbc     = gtrpnt('thetbc', gdp)
    tgarkt     = gtrpnt('tgarkt', gdp)
    tgarkx     = gtrpnt('tgarkx', gdp)
    tgarnp     = gtrpnt('tgarnp', gdp)
    thick      = gtrpnt('thick' , gdp)
    thtim      = gtrpnt('thtim' , gdp)
    tkedis     = gtrpnt('tkedis', gdp)
    tkepro     = gtrpnt('tkepro', gdp)
    tp         = gtrpnt('tp'    , gdp)
    tpcom      = gtrpnt('tpcom' , gdp)
    u0         = gtrpnt('u0'    , gdp)
    u1         = gtrpnt('u1'    , gdp)
    ubrlsu     = gtrpnt('ubrlsu', gdp)
    ubrlsv     = gtrpnt('ubrlsv', gdp)
    umdis      = gtrpnt('umdis' , gdp)
    umean      = gtrpnt('umean' , gdp)
    umeanf     = gtrpnt('umeanf', gdp)
    umeanl     = gtrpnt('umeanl', gdp)
    uorb       = gtrpnt('uorb'  , gdp)
    ubot       = gtrpnt('ubot'  , gdp)
    ubcom      = gtrpnt('ubcom' , gdp)
    usus       = gtrpnt('usus  ', gdp)
    uvdist     = gtrpnt('uvdist', gdp)
    uwtypu     = gtrpnt('uwtypu', gdp)
    uwtypv     = gtrpnt('uwtypv', gdp)
    v0         = gtrpnt('v0'    , gdp)
    v1         = gtrpnt('v1'    , gdp)
    vicuv      = gtrpnt('vicuv' , gdp)
    vicww      = gtrpnt('vicww' , gdp)
    vmdis      = gtrpnt('vmdis' , gdp)
    vmean      = gtrpnt('vmean' , gdp)
    vnu2d      = gtrpnt('vnu2d' , gdp)
    vnu3d      = gtrpnt('vnu3d' , gdp)
    voldis     = gtrpnt('voldis', gdp)
    volum0     = gtrpnt('volum0', gdp)
    volum1     = gtrpnt('volum1', gdp)
    vortic     = gtrpnt('vortic', gdp)
    vsus       = gtrpnt('vsus'  , gdp)
    w1         = gtrpnt('w1'    , gdp)
    w10mag     = gtrpnt('w10mag', gdp)
    wenf       = gtrpnt('wenf'  , gdp)
    wenfm      = gtrpnt('wenfm' , gdp)
    wenl       = gtrpnt('wenl'  , gdp)
    wenlm      = gtrpnt('wenlm' , gdp)
    windsu     = gtrpnt('windsu', gdp)
    windsv     = gtrpnt('windsv', gdp)
    windcd     = gtrpnt('windcd' , gdp)
    windu      = gtrpnt('windu' , gdp)
    windv      = gtrpnt('windv' , gdp)
    wlen       = gtrpnt('wlen'  , gdp)
    wlcom      = gtrpnt('wlcom' , gdp)
    wphy       = gtrpnt('wphy'  , gdp)
    ws         = gtrpnt('ws'    , gdp)
    wssus      = gtrpnt('wssus' , gdp)
    wstau      = gtrpnt('wstau' , gdp)
    wsu        = gtrpnt('wsu'   , gdp)
    wsucom     = gtrpnt('wsucom', gdp)
    wsv        = gtrpnt('wsv'   , gdp)
    wsvcom     = gtrpnt('wsvcom', gdp)
    wsbodyu    = gtrpnt('wsbu' , gdp)
    wsbodyucom = gtrpnt('wsbuc', gdp)
    wsbodyv    = gtrpnt('wsbv' , gdp)
    wsbodyvcom = gtrpnt('wsbvc', gdp)
    x2y        = gtrpnt('x2y'   , gdp)
    x3         = gtrpnt('x3'    , gdp)
    xcor       = gtrpnt('xcor'  , gdp)
    xy2        = gtrpnt('xy2'   , gdp)
    xydro      = gtrpnt('xydro' , gdp)
    xyzsrc     = gtrpnt('XYZSRC', gdp)
    xz         = gtrpnt('xz'    , gdp)
    y3         = gtrpnt('y3'    , gdp)
    ycor       = gtrpnt('ycor'  , gdp)
    yz         = gtrpnt('yz'    , gdp)
    z0ucur     = gtrpnt('z0ucur', gdp)
    z0vcur     = gtrpnt('z0vcur', gdp)
    z0urou     = gtrpnt('z0urou', gdp)
    z0vrou     = gtrpnt('z0vrou', gdp)
    zalfas     = gtrpnt('zalfas', gdp)
    zbdsed     = gtrpnt('zbdsed', gdp)
    zbmnf      = gtrpnt('zbmnf' , gdp)
    zbmnl      = gtrpnt('zbmnl' , gdp)
    zcuru      = gtrpnt('zcuru' , gdp)
    zcurv      = gtrpnt('zcurv' , gdp)
    zcurw      = gtrpnt('zcurw' , gdp)
    zdicww     = gtrpnt('zdicww', gdp)
    zdist      = gtrpnt('zdist ', gdp)
    zdps       = gtrpnt('zdps'  , gdp)
    zdpsed     = gtrpnt('zdpsed', gdp)
    zenst      = gtrpnt('zenst' , gdp)
    zetabf     = gtrpnt('zetabf', gdp)
    zetabl     = gtrpnt('zetabl', gdp)
    zetaif     = gtrpnt('zetaif', gdp)
    zetail     = gtrpnt('zetail', gdp)
    zfixfac    = gtrpnt('zfixfc', gdp)
    zfrac      = gtrpnt('zfrac' , gdp)
    zhidexp    = gtrpnt('zhidex', gdp)
    zmeanf     = gtrpnt('zmeanf', gdp)
    zmeanl     = gtrpnt('zmeanl', gdp)
    zmudfrac   = gtrpnt('zmudfr', gdp)
    zqxk       = gtrpnt('zqxk'  , gdp)
    zqyk       = gtrpnt('zqyk'  , gdp)
    zrca       = gtrpnt('zrca'  , gdp)
    zrho       = gtrpnt('zrho'  , gdp)
    zrich      = gtrpnt('zrich' , gdp)
    zrsdeq     = gtrpnt('zrsdeq', gdp)
    zsandfrac  = gtrpnt('zsandf', gdp)
    zsbu       = gtrpnt('zsbu'  , gdp)
    zsbv       = gtrpnt('zsbv'  , gdp)
    zseddif    = gtrpnt('zseddf', gdp)
    zsinkse    = gtrpnt('zsinks', gdp)
    zsourse    = gtrpnt('zsours', gdp)
    zssu       = gtrpnt('zssu'  , gdp)
    zssv       = gtrpnt('zssv'  , gdp)
    zstep      = gtrpnt('zstep' , gdp)
    ztaub      = gtrpnt('ztaub' , gdp)
    ztauet     = gtrpnt('ztauet', gdp)
    ztauks     = gtrpnt('ztauks', gdp)
    ztur       = gtrpnt('ztur'  , gdp)
    zvicww     = gtrpnt('zvicww', gdp)
    zvort      = gtrpnt('zvort' , gdp)
    zwl        = gtrpnt('zwl'   , gdp)
    zws        = gtrpnt('zws'   , gdp)
    zwndsp     = gtrpnt('zwndsp', gdp)
    zwndcd     = gtrpnt('zwndcd', gdp)
    zwnddr     = gtrpnt('zwnddr', gdp)
    zairp      = gtrpnt('zairp' , gdp)
    zprecp     = gtrpnt('zprecp', gdp)    
    zevap      = gtrpnt('zevap' , gdp)
    !
    ! Define pointers for sub-grid viscosity
    !
    umnldf     = gtrpnt('umnldf', gdp)
    umnflc     = gtrpnt('umnflc', gdp)
    vmnldf     = gtrpnt('vmnldf', gdp)
    vmnflc     = gtrpnt('vmnflc', gdp)
    !
    ! Define pointers for Non-hydrostatic pressure
    !
    p1         = gtrpnt('p1'    , gdp)
    p0         = gtrpnt('p0'    , gdp)
    p00        = gtrpnt('p00'   , gdp)
    pnhcor     = gtrpnt('pnhcor', gdp)
    w0         = gtrpnt('w0'    , gdp)
    s00        = gtrpnt('s00'   , gdp)
    !
    ! Define pointers for (Rigid) 3D Vegetation Model
    !
    diapl      = gtrpnt('diapl' ,gdp)
    rnpl       = gtrpnt('rnpl'  ,gdp)
    !
    ! definition of work array pointers in include file 'ADDRESS.INC'
    !
    iwrk1      = gtipnt('iwrk1' , gdp)
    iwrk2      = gtipnt('iwrk2' , gdp)
    iwrk3      = gtipnt('iwrk3' , gdp)
    !
    wrka1      = gtrpnt('wrka1' , gdp)
    wrka2      = gtrpnt('wrka2' , gdp)
    wrka3      = gtrpnt('wrka3' , gdp)
    wrka4      = gtrpnt('wrka4' , gdp)
    wrka5      = gtrpnt('wrka5' , gdp)
    wrka6      = gtrpnt('wrka6' , gdp)
    wrka7      = gtrpnt('wrka7' , gdp)
    wrka8      = gtrpnt('wrka8' , gdp)
    wrka9      = gtrpnt('wrka9' , gdp)
    wrka10     = gtrpnt('wrka10', gdp)
    wrka11     = gtrpnt('wrka11', gdp)
    wrka12     = gtrpnt('wrka12', gdp)
    wrka13     = gtrpnt('wrka13', gdp)
    wrka14     = gtrpnt('wrka14', gdp)
    wrka15     = gtrpnt('wrka15', gdp)
    wrka16     = gtrpnt('wrka16', gdp)
    !
    wrkb1      = gtrpnt('wrkb1' , gdp)
    wrkb2      = gtrpnt('wrkb2' , gdp)
    wrkb3      = gtrpnt('wrkb3' , gdp)
    wrkb4      = gtrpnt('wrkb4' , gdp)
    wrkb5      = gtrpnt('wrkb5' , gdp)
    wrkb6      = gtrpnt('wrkb6' , gdp)
    wrkb7      = gtrpnt('wrkb7' , gdp)
    wrkb8      = gtrpnt('wrkb8' , gdp)
    wrkb9      = gtrpnt('wrkb9' , gdp)
    wrkb10     = gtrpnt('wrkb10', gdp)
    wrkb11     = gtrpnt('wrkb11', gdp)
    wrkb12     = gtrpnt('wrkb12', gdp)
    wrkb13     = gtrpnt('wrkb13', gdp)
    wrkb14     = gtrpnt('wrkb14', gdp)
    wrkb15     = gtrpnt('wrkb15', gdp)
    wrkb16     = gtrpnt('wrkb16', gdp)
    wrkb17     = gtrpnt('wrkb17', gdp)
    wrkb18     = gtrpnt('wrkb18', gdp)
    !
    wrkc1      = gtrpnt('wrkc1' , gdp)
    wrkc2      = gtrpnt('wrkc2' , gdp)
    wrkc3      = gtrpnt('wrkc3' , gdp)
    wrkc4      = gtrpnt('wrkc4' , gdp)
    !
    zwork      = gtrpnt('zwork' , gdp)
    !
    ! RBUFF is used only outside TRISOL. So we use a work array, which
    ! is initialized each timestep in TRISOL, here WRKB1 or WRKC1
    ! The maximum needed storage space for RBUFF is defined in esm_alloc_real
    ! and taken in consideration in the declaration of WRKB1 and WRKC1
    ! Because RBUFF must fit in NMAXUS * MMAX * KMAX * 2 the turn-
    ! over point lies for LMAX > 1 (NMAXUS * MMAX * (KMAX+1) * LMAX)
    !
    if (lmax<=1) then
       rbuff = wrkb1
    else
       rbuff = wrkc1
    endif
end subroutine gtptrs
