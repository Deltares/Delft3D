function getpointer(pntnam, gdp)
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
! ESM function gtcpnt/gtipnt/gtrpnt can only be used during initialization phase.
! See subroutine gtptrs
! If used after initialization, they point to the latest initialized sub domain.
! If needed after initialization, use this function getpointer instead.
! See subroutine getfpt
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer(pntrsize) , pointer :: iwrk1
    integer(pntrsize) , pointer :: iwrk2
    integer(pntrsize) , pointer :: iwrk3
    integer(pntrsize) , pointer :: wrka1
    integer(pntrsize) , pointer :: wrka2
    integer(pntrsize) , pointer :: wrka3
    integer(pntrsize) , pointer :: wrka4
    integer(pntrsize) , pointer :: wrka5
    integer(pntrsize) , pointer :: wrka6
    integer(pntrsize) , pointer :: wrka7
    integer(pntrsize) , pointer :: wrka8
    integer(pntrsize) , pointer :: wrka9
    integer(pntrsize) , pointer :: wrka10
    integer(pntrsize) , pointer :: wrka11
    integer(pntrsize) , pointer :: wrka12
    integer(pntrsize) , pointer :: wrka13
    integer(pntrsize) , pointer :: wrka14
    integer(pntrsize) , pointer :: wrka15
    integer(pntrsize) , pointer :: wrka16
    integer(pntrsize) , pointer :: wrkb1
    integer(pntrsize) , pointer :: wrkb2
    integer(pntrsize) , pointer :: wrkb3
    integer(pntrsize) , pointer :: wrkb4
    integer(pntrsize) , pointer :: wrkb5
    integer(pntrsize) , pointer :: wrkb6
    integer(pntrsize) , pointer :: wrkb7
    integer(pntrsize) , pointer :: wrkb8
    integer(pntrsize) , pointer :: wrkb9
    integer(pntrsize) , pointer :: wrkb10
    integer(pntrsize) , pointer :: wrkb11
    integer(pntrsize) , pointer :: wrkb12
    integer(pntrsize) , pointer :: wrkb13
    integer(pntrsize) , pointer :: wrkb14
    integer(pntrsize) , pointer :: wrkb15
    integer(pntrsize) , pointer :: wrkb16
    integer(pntrsize) , pointer :: wrkb17
    integer(pntrsize) , pointer :: wrkb18
    integer(pntrsize) , pointer :: wrkc1
    integer(pntrsize) , pointer :: wrkc2
    integer(pntrsize) , pointer :: wrkc3
    integer(pntrsize) , pointer :: wrkc4
    integer(pntrsize) , pointer :: zwork
    integer(pntrsize) , pointer :: alfas
    integer(pntrsize) , pointer :: alpha
    integer(pntrsize) , pointer :: areau
    integer(pntrsize) , pointer :: areav
    integer(pntrsize) , pointer :: atr
    integer(pntrsize) , pointer :: bruvai
    integer(pntrsize) , pointer :: c
    integer(pntrsize) , pointer :: cbuv
    integer(pntrsize) , pointer :: cbuvrt
    integer(pntrsize) , pointer :: cdwlsu
    integer(pntrsize) , pointer :: cdwlsv
    integer(pntrsize) , pointer :: cdwzbu
    integer(pntrsize) , pointer :: cdwzbv
    integer(pntrsize) , pointer :: cdwztu
    integer(pntrsize) , pointer :: cdwztv
    integer(pntrsize) , pointer :: cfurou
    integer(pntrsize) , pointer :: cfvrou
    integer(pntrsize) , pointer :: cvalu0
    integer(pntrsize) , pointer :: cvalv0
    integer(pntrsize) , pointer :: circ2d
    integer(pntrsize) , pointer :: circ3d
    integer(pntrsize) , pointer :: ctr
    integer(pntrsize) , pointer :: czusus
    integer(pntrsize) , pointer :: czvsus
    integer(pntrsize) , pointer :: dddeta
    integer(pntrsize) , pointer :: dddksi
    integer(pntrsize) , pointer :: disch0
    integer(pntrsize) , pointer :: disch1
    integer(pntrsize) , pointer :: ddpf
    integer(pntrsize) , pointer :: decay
    integer(pntrsize) , pointer :: deltau
    integer(pntrsize) , pointer :: deltav
    integer(pntrsize) , pointer :: depchg
    integer(pntrsize) , pointer :: dfu
    integer(pntrsize) , pointer :: dfv
    integer(pntrsize) , pointer :: diapl
    integer(pntrsize) , pointer :: dicuv
    integer(pntrsize) , pointer :: dicww
    integer(pntrsize) , pointer :: dircom
    integer(pntrsize) , pointer :: dis
    integer(pntrsize) , pointer :: df
    integer(pntrsize) , pointer :: disch
    integer(pntrsize) , pointer :: discom
    integer(pntrsize) , pointer :: discum
    integer(pntrsize) , pointer :: disinp
    integer(pntrsize) , pointer :: disnf
    integer(pntrsize) , pointer :: dldeta
    integer(pntrsize) , pointer :: dldksi
    integer(pntrsize) , pointer :: dpd
    integer(pntrsize) , pointer :: dpc
    integer(pntrsize) , pointer :: dpdeta
    integer(pntrsize) , pointer :: dpdksi
    integer(pntrsize) , pointer :: dps
    integer(pntrsize) , pointer :: dpsed
    integer(pntrsize) , pointer :: dpu
    integer(pntrsize) , pointer :: dpv
    integer(pntrsize) , pointer :: rint0
    integer(pntrsize) , pointer :: rint1
    integer(pntrsize) , pointer :: dsdeta
    integer(pntrsize) , pointer :: dsdksi
    integer(pntrsize) , pointer :: dtdeta
    integer(pntrsize) , pointer :: dtdksi
    integer(pntrsize) , pointer :: dteu
    integer(pntrsize) , pointer :: dtev
    integer(pntrsize) , pointer :: dtr
    integer(pntrsize) , pointer :: dudz
    integer(pntrsize) , pointer :: umdis0
    integer(pntrsize) , pointer :: umdis1
    integer(pntrsize) , pointer :: dvdz
    integer(pntrsize) , pointer :: vmdis0
    integer(pntrsize) , pointer :: vmdis1
    integer(pntrsize) , pointer :: dxydro
    integer(pntrsize) , pointer :: dzdeta
    integer(pntrsize) , pointer :: dzdksi
    integer(pntrsize) , pointer :: enstro
    integer(pntrsize) , pointer :: entr
    integer(pntrsize) , pointer :: evap
    integer(pntrsize) , pointer :: excbed
    integer(pntrsize) , pointer :: fcorio
    integer(pntrsize) , pointer :: fltr
    integer(pntrsize) , pointer :: fuiwe
    integer(pntrsize) , pointer :: fviwe
    integer(pntrsize) , pointer :: grmasu
    integer(pntrsize) , pointer :: grmasv
    integer(pntrsize) , pointer :: gro
    integer(pntrsize) , pointer :: gsqd
    integer(pntrsize) , pointer :: gsqs
    integer(pntrsize) , pointer :: guu
    integer(pntrsize) , pointer :: guv
    integer(pntrsize) , pointer :: gvu
    integer(pntrsize) , pointer :: gvv
    integer(pntrsize) , pointer :: hkru
    integer(pntrsize) , pointer :: hkrv
    integer(pntrsize) , pointer :: hrmcom
    integer(pntrsize) , pointer :: hrms
    integer(pntrsize) , pointer :: hu
    integer(pntrsize) , pointer :: hu0
    integer(pntrsize) , pointer :: huvw
    integer(pntrsize) , pointer :: hv
    integer(pntrsize) , pointer :: hv0
    integer(pntrsize) , pointer :: hydrbc
    integer(pntrsize) , pointer :: msucom
    integer(pntrsize) , pointer :: msvcom
    integer(pntrsize) , pointer :: omega
    integer(pntrsize) , pointer :: patm
    integer(pntrsize) , pointer :: porosu
    integer(pntrsize) , pointer :: porosv
    integer(pntrsize) , pointer :: procbc
    integer(pntrsize) , pointer :: pship
    integer(pntrsize) , pointer :: qtfrac
    integer(pntrsize) , pointer :: qtfrct
    integer(pntrsize) , pointer :: qtfrt2
    integer(pntrsize) , pointer :: qu
    integer(pntrsize) , pointer :: qv
    integer(pntrsize) , pointer :: qxk
    integer(pntrsize) , pointer :: qyk
    integer(pntrsize) , pointer :: qzk
    integer(pntrsize) , pointer :: r0
    integer(pntrsize) , pointer :: r1
    integer(pntrsize) , pointer :: rbnd
    integer(pntrsize) , pointer :: rbuff
    integer(pntrsize) , pointer :: rettim
    integer(pntrsize) , pointer :: rho
    integer(pntrsize) , pointer :: rhowat
    integer(pntrsize) , pointer :: rich
    integer(pntrsize) , pointer :: rint
    integer(pntrsize) , pointer :: rintsm
    integer(pntrsize) , pointer :: rlabda
    integer(pntrsize) , pointer :: rmneg
    integer(pntrsize) , pointer :: rnpl
    integer(pntrsize) , pointer :: rob
    integer(pntrsize) , pointer :: rsed
    integer(pntrsize) , pointer :: rthbnd
    integer(pntrsize) , pointer :: rtu2d0
    integer(pntrsize) , pointer :: rtu2d1
    integer(pntrsize) , pointer :: rtubnd
    integer(pntrsize) , pointer :: rtur0
    integer(pntrsize) , pointer :: rtur1
    integer(pntrsize) , pointer :: rxx
    integer(pntrsize) , pointer :: rxy
    integer(pntrsize) , pointer :: ryy
    integer(pntrsize) , pointer :: s0
    integer(pntrsize) , pointer :: s1
    integer(pntrsize) , pointer :: sbuu
    integer(pntrsize) , pointer :: sbvv
    integer(pntrsize) , pointer :: seddif
    integer(pntrsize) , pointer :: sepsus
    integer(pntrsize) , pointer :: sig
    integer(pntrsize) , pointer :: sigdif
    integer(pntrsize) , pointer :: sigmol
    integer(pntrsize) , pointer :: sink
    integer(pntrsize) , pointer :: soumud
    integer(pntrsize) , pointer :: sour
    integer(pntrsize) , pointer :: sournf
    integer(pntrsize) , pointer :: sumrho
    integer(pntrsize) , pointer :: taubmx
    integer(pntrsize) , pointer :: taubpu
    integer(pntrsize) , pointer :: taubpv
    integer(pntrsize) , pointer :: taubsu
    integer(pntrsize) , pointer :: taubsv
    integer(pntrsize) , pointer :: teta
    integer(pntrsize) , pointer :: tgarkt
    integer(pntrsize) , pointer :: tgarkx
    integer(pntrsize) , pointer :: tgarnp
    integer(pntrsize) , pointer :: thick
    integer(pntrsize) , pointer :: thtim
    integer(pntrsize) , pointer :: tkedis
    integer(pntrsize) , pointer :: tkepro
    integer(pntrsize) , pointer :: tp
    integer(pntrsize) , pointer :: tpcom
    integer(pntrsize) , pointer :: u0
    integer(pntrsize) , pointer :: u1
    integer(pntrsize) , pointer :: ubrlsu
    integer(pntrsize) , pointer :: ubrlsv
    integer(pntrsize) , pointer :: umdis
    integer(pntrsize) , pointer :: umean
    integer(pntrsize) , pointer :: umnflc
    integer(pntrsize) , pointer :: umnldf
    integer(pntrsize) , pointer :: uorb
    integer(pntrsize) , pointer :: ubot
    integer(pntrsize) , pointer :: ubcom
    integer(pntrsize) , pointer :: usus
    integer(pntrsize) , pointer :: uvdist
    integer(pntrsize) , pointer :: uwtypu
    integer(pntrsize) , pointer :: uwtypv
    integer(pntrsize) , pointer :: v0
    integer(pntrsize) , pointer :: v1
    integer(pntrsize) , pointer :: vicuv
    integer(pntrsize) , pointer :: vicww
    integer(pntrsize) , pointer :: vmdis
    integer(pntrsize) , pointer :: vmean
    integer(pntrsize) , pointer :: vmnflc
    integer(pntrsize) , pointer :: vmnldf
    integer(pntrsize) , pointer :: vnu2d
    integer(pntrsize) , pointer :: vnu3d
    integer(pntrsize) , pointer :: volum0
    integer(pntrsize) , pointer :: volum1
    integer(pntrsize) , pointer :: vortic
    integer(pntrsize) , pointer :: vsus
    integer(pntrsize) , pointer :: w1
    integer(pntrsize) , pointer :: w10mag
    integer(pntrsize) , pointer :: windcd
    integer(pntrsize) , pointer :: windsu
    integer(pntrsize) , pointer :: windsv
    integer(pntrsize) , pointer :: windu
    integer(pntrsize) , pointer :: windv
    integer(pntrsize) , pointer :: wlen
    integer(pntrsize) , pointer :: wlcom
    integer(pntrsize) , pointer :: wphy
    integer(pntrsize) , pointer :: ws
    integer(pntrsize) , pointer :: wssus
    integer(pntrsize) , pointer :: wstau
    integer(pntrsize) , pointer :: wsu
    integer(pntrsize) , pointer :: wsucom
    integer(pntrsize) , pointer :: wsv
    integer(pntrsize) , pointer :: wsvcom
    integer(pntrsize) , pointer :: wsbodyu
    integer(pntrsize) , pointer :: wsbodyucom
    integer(pntrsize) , pointer :: wsbodyv
    integer(pntrsize) , pointer :: wsbodyvcom
    integer(pntrsize) , pointer :: x2y
    integer(pntrsize) , pointer :: x3
    integer(pntrsize) , pointer :: xcor
    integer(pntrsize) , pointer :: xy2
    integer(pntrsize) , pointer :: xydro
    integer(pntrsize) , pointer :: xyzsrc
    integer(pntrsize) , pointer :: xz
    integer(pntrsize) , pointer :: y3
    integer(pntrsize) , pointer :: ycor
    integer(pntrsize) , pointer :: yz
    integer(pntrsize) , pointer :: z0ucur
    integer(pntrsize) , pointer :: z0vcur
    integer(pntrsize) , pointer :: z0urou
    integer(pntrsize) , pointer :: z0vrou
    integer(pntrsize) , pointer :: zalfas
    integer(pntrsize) , pointer :: zbdsed
    integer(pntrsize) , pointer :: zcuru
    integer(pntrsize) , pointer :: zcurv
    integer(pntrsize) , pointer :: zcurw
    integer(pntrsize) , pointer :: zdicww
    integer(pntrsize) , pointer :: zdist
    integer(pntrsize) , pointer :: zdps
    integer(pntrsize) , pointer :: zdpsed
    integer(pntrsize) , pointer :: zqxk
    integer(pntrsize) , pointer :: zqyk
    integer(pntrsize) , pointer :: zrho
    integer(pntrsize) , pointer :: zrich
    integer(pntrsize) , pointer :: zrsdeq
    integer(pntrsize) , pointer :: zstep
    integer(pntrsize) , pointer :: ztauet
    integer(pntrsize) , pointer :: ztauks
    integer(pntrsize) , pointer :: ztur
    integer(pntrsize) , pointer :: zvicww
    integer(pntrsize) , pointer :: zwl
    integer(pntrsize) , pointer :: zws
    integer(pntrsize) , pointer :: zwndsp
    integer(pntrsize) , pointer :: zwndcd
    integer(pntrsize) , pointer :: zwnddr
    integer(pntrsize) , pointer :: zairp
    integer(pntrsize) , pointer :: zevap
    integer(pntrsize) , pointer :: zprecp
    integer(pntrsize) , pointer :: drhodx
    integer(pntrsize) , pointer :: drhody
    integer(pntrsize) , pointer :: dzs0
    integer(pntrsize) , pointer :: dzs1
    integer(pntrsize) , pointer :: dzu0
    integer(pntrsize) , pointer :: dzu1
    integer(pntrsize) , pointer :: dzv0
    integer(pntrsize) , pointer :: dzv1
    integer(pntrsize) , pointer :: rl
    integer(pntrsize) , pointer :: p1
    integer(pntrsize) , pointer :: p0
    integer(pntrsize) , pointer :: pnhcor
    integer(pntrsize) , pointer :: w0
    integer(pntrsize) , pointer :: s00
    integer(pntrsize) , pointer :: hydprs
    integer(pntrsize) , pointer :: guz
    integer(pntrsize) , pointer :: gvz
    integer(pntrsize) , pointer :: gud
    integer(pntrsize) , pointer :: gvd
    integer(pntrsize) , pointer :: gsqiu
    integer(pntrsize) , pointer :: gsqiv
    integer(pntrsize) , pointer :: ibuff
    integer(pntrsize) , pointer :: idifu
    integer(pntrsize) , pointer :: irocol
    integer(pntrsize) , pointer :: itbcc
    integer(pntrsize) , pointer :: itbct
    integer(pntrsize) , pointer :: itdis
    integer(pntrsize) , pointer :: itdro
    integer(pntrsize) , pointer :: kadu
    integer(pntrsize) , pointer :: kadv
    integer(pntrsize) , pointer :: kcs
    integer(pntrsize) , pointer :: kcu
    integer(pntrsize) , pointer :: kcv
    integer(pntrsize) , pointer :: kfs
    integer(pntrsize) , pointer :: kfu
    integer(pntrsize) , pointer :: kfv
    integer(pntrsize) , pointer :: kspu
    integer(pntrsize) , pointer :: kspv
    integer(pntrsize) , pointer :: kstp
    integer(pntrsize) , pointer :: kzs
    integer(pntrsize) , pointer :: kzu
    integer(pntrsize) , pointer :: kzv
    integer(pntrsize) , pointer :: mnbar
    integer(pntrsize) , pointer :: mnbnd
    integer(pntrsize) , pointer :: mndro
    integer(pntrsize) , pointer :: mnksrc
    integer(pntrsize) , pointer :: nob
    integer(pntrsize) , pointer :: kfumin
    integer(pntrsize) , pointer :: kfvmin
    integer(pntrsize) , pointer :: kfsmin
    integer(pntrsize) , pointer :: kfumax
    integer(pntrsize) , pointer :: kfvmax
    integer(pntrsize) , pointer :: kfsmax
    integer(pntrsize) , pointer :: kfumx0
    integer(pntrsize) , pointer :: kfvmx0
    integer(pntrsize) , pointer :: kfsmx0
    integer(pntrsize) , pointer :: kfumn0
    integer(pntrsize) , pointer :: kfvmn0
    integer(pntrsize) , pointer :: kfsmn0
    integer(pntrsize) , pointer :: kfsz0
    integer(pntrsize) , pointer :: kfsz1
    integer(pntrsize) , pointer :: kfuz0
    integer(pntrsize) , pointer :: kfuz1
    integer(pntrsize) , pointer :: kfvz0
    integer(pntrsize) , pointer :: kfvz1
    integer(pntrsize) , pointer :: kcscut
    integer(pntrsize) , pointer :: kcu45
    integer(pntrsize) , pointer :: kcv45
    integer(pntrsize) , pointer :: disint
    integer(pntrsize) , pointer :: dismmt
    integer(pntrsize) , pointer :: nambar
    integer(pntrsize) , pointer :: nambnd
    integer(pntrsize) , pointer :: namcon
    integer(pntrsize) , pointer :: namdro
    integer(pntrsize) , pointer :: namsrc
    integer(pntrsize) , pointer :: tprofc
    integer(pntrsize) , pointer :: tprofu
    integer(pntrsize) , pointer :: typbnd
    integer(pntrsize) , pointer :: zkfs
!
! Result
!
    integer(pntrsize) :: getpointer
!
! Global variables
!
    character(*), intent(in) :: pntnam
!
! Local variables
!
    integer(pntrsize) :: returnval
!
!! executable statements -------------------------------------------------------
!
    alfas      => gdp%gdr_i_ch%alfas
    alpha      => gdp%gdr_i_ch%alpha
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
    cvalu0     => gdp%gdr_i_ch%cvalu0
    cvalv0     => gdp%gdr_i_ch%cvalv0
    circ2d     => gdp%gdr_i_ch%circ2d
    circ3d     => gdp%gdr_i_ch%circ3d
    ctr        => gdp%gdr_i_ch%ctr
    czusus     => gdp%gdr_i_ch%czusus
    czvsus     => gdp%gdr_i_ch%czvsus
    dddeta     => gdp%gdr_i_ch%dddeta
    dddksi     => gdp%gdr_i_ch%dddksi
    disch0     => gdp%gdr_i_ch%disch0
    disch1     => gdp%gdr_i_ch%disch1
    ddpf       => gdp%gdr_i_ch%ddpf
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
    dis        => gdp%gdr_i_ch%dis
    df         => gdp%gdr_i_ch%df
    disch      => gdp%gdr_i_ch%disch
    discom     => gdp%gdr_i_ch%discom
    discum     => gdp%gdr_i_ch%discum
    disinp     => gdp%gdr_i_ch%disinp
    disnf      => gdp%gdr_i_ch%disnf
    dldeta     => gdp%gdr_i_ch%dldeta
    dldksi     => gdp%gdr_i_ch%dldksi
    dpd        => gdp%gdr_i_ch%dpd
    dpc        => gdp%gdr_i_ch%dpc
    dpdeta     => gdp%gdr_i_ch%dpdeta
    dpdksi     => gdp%gdr_i_ch%dpdksi
    dps        => gdp%gdr_i_ch%dps
    dpsed      => gdp%gdr_i_ch%dpsed
    dpu        => gdp%gdr_i_ch%dpu
    dpv        => gdp%gdr_i_ch%dpv
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
    enstro     => gdp%gdr_i_ch%enstro
    entr       => gdp%gdr_i_ch%entr
    evap       => gdp%gdr_i_ch%evap
    excbed     => gdp%gdr_i_ch%excbed
    fcorio     => gdp%gdr_i_ch%fcorio
    fltr       => gdp%gdr_i_ch%fltr
    fuiwe      => gdp%gdr_i_ch%fuiwe
    fviwe      => gdp%gdr_i_ch%fviwe
    grmasu     => gdp%gdr_i_ch%grmasu
    grmasv     => gdp%gdr_i_ch%grmasv
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
    omega      => gdp%gdr_i_ch%omega
    patm       => gdp%gdr_i_ch%patm
    porosu     => gdp%gdr_i_ch%porosu
    porosv     => gdp%gdr_i_ch%porosv
    procbc     => gdp%gdr_i_ch%procbc
    pship      => gdp%gdr_i_ch%pship
    qtfrac     => gdp%gdr_i_ch%qtfrac
    qtfrct     => gdp%gdr_i_ch%qtfrct
    qtfrt2     => gdp%gdr_i_ch%qtfrt2
    qu         => gdp%gdr_i_ch%qu
    qv         => gdp%gdr_i_ch%qv
    qxk        => gdp%gdr_i_ch%qxk
    qyk        => gdp%gdr_i_ch%qyk
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
    s0         => gdp%gdr_i_ch%s0
    s1         => gdp%gdr_i_ch%s1
    sbuu       => gdp%gdr_i_ch%sbuu
    sbvv       => gdp%gdr_i_ch%sbvv
    seddif     => gdp%gdr_i_ch%seddif
    sepsus     => gdp%gdr_i_ch%sepsus
    sig        => gdp%gdr_i_ch%sig
    sigdif     => gdp%gdr_i_ch%sigdif
    sigmol     => gdp%gdr_i_ch%sigmol
    sink       => gdp%gdr_i_ch%sink
    soumud     => gdp%gdr_i_ch%soumud
    sour       => gdp%gdr_i_ch%sour
    sournf     => gdp%gdr_i_ch%sournf
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
    volum0     => gdp%gdr_i_ch%volum0
    volum1     => gdp%gdr_i_ch%volum1
    vortic     => gdp%gdr_i_ch%vortic
    vsus       => gdp%gdr_i_ch%vsus
    w1         => gdp%gdr_i_ch%w1
    w10mag     => gdp%gdr_i_ch%w10mag
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
    zcuru      => gdp%gdr_i_ch%zcuru
    zcurv      => gdp%gdr_i_ch%zcurv
    zcurw      => gdp%gdr_i_ch%zcurw
    zdicww     => gdp%gdr_i_ch%zdicww
    zdist      => gdp%gdr_i_ch%zdist
    zdps       => gdp%gdr_i_ch%zdps
    zdpsed     => gdp%gdr_i_ch%zdpsed
    zqxk       => gdp%gdr_i_ch%zqxk
    zqyk       => gdp%gdr_i_ch%zqyk
    zrho       => gdp%gdr_i_ch%zrho
    zrich      => gdp%gdr_i_ch%zrich
    zrsdeq     => gdp%gdr_i_ch%zrsdeq
    zstep      => gdp%gdr_i_ch%zstep
    ztauet     => gdp%gdr_i_ch%ztauet
    ztauks     => gdp%gdr_i_ch%ztauks
    ztur       => gdp%gdr_i_ch%ztur
    zvicww     => gdp%gdr_i_ch%zvicww
    zwl        => gdp%gdr_i_ch%zwl
    zws        => gdp%gdr_i_ch%zws
    zwndsp     => gdp%gdr_i_ch%zwndsp
    zwndcd     => gdp%gdr_i_ch%zwndcd
    zwnddr     => gdp%gdr_i_ch%zwnddr
    zairp      => gdp%gdr_i_ch%zairp
    zevap      => gdp%gdr_i_ch%zevap
    zprecp     => gdp%gdr_i_ch%zprecp
    drhodx     => gdp%gdr_i_ch%drhodx
    drhody     => gdp%gdr_i_ch%drhody
    dzs0       => gdp%gdr_i_ch%dzs0
    dzs1       => gdp%gdr_i_ch%dzs1
    dzu0       => gdp%gdr_i_ch%dzu0
    dzu1       => gdp%gdr_i_ch%dzu1
    dzv0       => gdp%gdr_i_ch%dzv0
    dzv1       => gdp%gdr_i_ch%dzv1
    rl         => gdp%gdr_i_ch%rl
    p1         => gdp%gdr_i_ch%p1
    p0         => gdp%gdr_i_ch%p0
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
!
    arrayname:select case (pntnam)
    case ('alfas')
       returnval = alfas
    case ('alpha')
       returnval = alpha
    case ('areau')
       returnval = areau
    case ('areav')
       returnval = areav
    case ('atr')
       returnval = atr
    case ('bruvai')
       returnval = bruvai
    case ('cbuv')
       returnval = cbuv
    case ('cbuvrt')
       returnval = cbuvrt
    case ('cdwlsu')
       returnval = cdwlsu
    case ('cdwlsv')
       returnval = cdwlsv
    case ('cdwztu')
       returnval = cdwztu
    case ('cdwztv')
       returnval = cdwztv
    case ('cdwzbu')
       returnval = cdwzbu
    case ('cdwzbv')
       returnval = cdwzbv
    case ('cfurou')
       returnval = cfurou
    case ('cfvrou')
       returnval = cfvrou
    case ('circ2d')
       returnval = circ2d
    case ('circ3d')
       returnval = circ3d
    case ('ctr')
       returnval = ctr
    case ('cvalu0')
       returnval = cvalu0
    case ('cvalv0')
       returnval = cvalv0
    case ('czusus')
       returnval = czusus
    case ('czvsus')
       returnval = czvsus
    case ('dddeta')
       returnval = dddeta
    case ('dddksi')
       returnval = dddksi
    case ('disch0')
       returnval = disch0
    case ('disch1')
       returnval = disch1
    case ('decay')
       returnval = decay
    case ('deltau')
       returnval = deltau
    case ('deltav')
       returnval = deltav
    case ('depchg')
       returnval = depchg
    case ('dfu')
       returnval = dfu
    case ('dfv')
       returnval = dfv
    case ('diapl')
       returnval  = diapl
    case ('dicuv')
       returnval = dicuv
    case ('dicww')
       returnval = dicww
    case ('dircom')
       returnval = dircom
    case ('dis')
       returnval = dis
    case ('df')
       returnval = df
    case ('disch')
       returnval = disch
    case ('disinp')
       returnval = disinp
    case ('discom')
       returnval = discom
    case ('discum')
       returnval = discum
    case ('disnf')
       returnval = disnf
    case ('dldksi')
       returnval = dldksi
    case ('dldeta')
       returnval = dldeta
    case ('dpd')
       returnval = dpd
    case ('dpc')
       returnval = dpc
    case ('dpdeta')
       returnval = dpdeta
    case ('dpdksi')
       returnval = dpdksi
    case ('ddpf')
       returnval = ddpf
    case ('dps')
       returnval = dps
    case ('dpu')
       returnval = dpu
    case ('dpv')
       returnval = dpv
    case ('rint0')
       returnval = rint0
    case ('rint1')
       returnval = rint1
    case ('dsdeta')
       returnval = dsdeta
    case ('dsdksi')
       returnval = dsdksi
    case ('dtdeta')
       returnval = dtdeta
    case ('dtdksi')
       returnval = dtdksi
    case ('dteu')
       returnval = dteu
    case ('dtev')
       returnval = dtev
    case ('dtr')
       returnval = dtr
    case ('dudz')
       returnval = dudz
    case ('umdis0')
       returnval = umdis0
    case ('umdis1')
       returnval = umdis1
    case ('dvdz')
       returnval = dvdz
    case ('vmdis0')
       returnval = vmdis0
    case ('vmdis1')
       returnval = vmdis1
    case ('dxydro')
       returnval = dxydro
    case ('dzdeta')
       returnval = dzdeta
    case ('dzdksi')
       returnval = dzdksi
    case ('enstro')
       returnval = enstro
    case ('entr')
       returnval = entr
    case ('evap')
       returnval = evap
    case ('excbed')
       returnval = excbed
    case ('fcorio')
       returnval = fcorio
    case ('fltr')
       returnval = fltr
    case ('fuiwe')
       returnval = fuiwe
    case ('fviwe')
       returnval = fviwe
    case ('grmasu')
       returnval = grmasu
    case ('grmasv')
       returnval = grmasv
    case ('gro')
       returnval = gro
    case ('gsqd')
       returnval = gsqd
    case ('gsqiu')
       returnval = gsqiu
    case ('gsqiv')
       returnval = gsqiv
    case ('gsqs')
       returnval = gsqs
    case ('gud')
       returnval = gud
    case ('guu')
       returnval = guu
    case ('guv')
       returnval = guv
    case ('guz')
       returnval = guz
    case ('gvd')
       returnval = gvd
    case ('gvu')
       returnval = gvu
    case ('gvv')
       returnval = gvv
    case ('gvz')
       returnval = gvz
    case ('hkru')
       returnval = hkru
    case ('hkrv')
       returnval = hkrv
    case ('hrmcom')
       returnval = hrmcom
    case ('hu')
       returnval = hu
    case ('hu0')
       returnval = hu0
    case ('huvw')
       returnval = huvw
    case ('hv')
       returnval = hv
    case ('hydrbc')
       returnval = hydrbc
    case ('hv0')
       returnval = hv0
    case ('hrms')
       returnval = hrms
    case ('msucom')
       returnval = msucom
    case ('msvcom')
       returnval = msvcom
    case ('omega')
       returnval = omega
    case ('patm')
       returnval = patm
    case ('porosu')
       returnval = porosu
    case ('porosv')
       returnval = porosv
    case ('procbc')
       returnval = procbc
    case ('pship')
       returnval = pship
    case ('qtfrac')
       returnval = qtfrac
    case ('qtfrct')
       returnval = qtfrct
    case ('qtfrt2')
       returnval = qtfrt2
    case ('qu')
       returnval = qu
    case ('qv')
       returnval = qv
    case ('qxk')
       returnval = qxk
    case ('qyk')
       returnval = qyk
    case ('qzk')
       returnval = qzk
    case ('r0')
       returnval = r0
    case ('r1')
       returnval = r1
    case ('rbnd')
       returnval = rbnd
    case ('rbuff')
       returnval = rbuff
    case ('rettim')
       returnval = rettim
    case ('rho')
       returnval = rho
    case ('rhowat')
       returnval = rhowat
    case ('rich')
       returnval = rich
    case ('rint')
       returnval = rint
    case ('rintsm')
       returnval = rintsm
    case ('rlabda')
       returnval = rlabda
    case ('rmneg')
       returnval = rmneg
    case ('rnpl')
       returnval = rnpl
    case ('rob')
       returnval = rob
    case ('rsed')
       returnval = rsed
    case ('rthbnd')
       returnval = rthbnd
    case ('rtur0')
       returnval = rtur0
    case ('rtur1')
       returnval = rtur1
    case ('rtu2d0')
       returnval = rtu2d0
    case ('rtu2d1')
       returnval = rtu2d1
    case ('rtubnd')
       returnval = rtubnd
    case ('rxx')
       returnval = rxx
    case ('rxy')
       returnval = rxy
    case ('ryy')
       returnval = ryy
    case ('s0')
       returnval = s0
    case ('s1')
       returnval = s1
    case ('sbuu')
       returnval = sbuu
    case ('sbvv')
       returnval = sbvv
    case ('seddif')
       returnval = seddif
    case ('sepsus')
       returnval = sepsus
    case ('sig')
       returnval = sig
    case ('sigdif')
       returnval = sigdif
    case ('sigmol')
       returnval = sigmol
    case ('sink')
       returnval = sink
    case ('sour')
       returnval = sour
    case ('sournf')
       returnval = sournf
    case ('sumrho')
       returnval = sumrho
    case ('soumud')
       returnval = soumud
    case ('taubmx')
       returnval = taubmx
    case ('taubpu')
       returnval = taubpu
    case ('taubpv')
       returnval = taubpv
    case ('taubsu')
       returnval = taubsu
    case ('taubsv')
       returnval = taubsv
    case ('teta')
       returnval = teta
    case ('tgarkt')
       returnval = tgarkt
    case ('tgarkx')
       returnval = tgarkx
    case ('tgarnp')
       returnval = tgarnp
    case ('thick')
       returnval = thick
    case ('thtim')
       returnval = thtim
    case ('tkedis')
       returnval = tkedis
    case ('tkepro')
       returnval = tkepro
    case ('tp')
       returnval = tp
    case ('tpcom')
       returnval = tpcom
    case ('u0')
       returnval = u0
    case ('u1')
       returnval = u1
    case ('ubrlsu')
       returnval = ubrlsu
    case ('ubrlsv')
       returnval = ubrlsv
    case ('umdis')
       returnval = umdis
    case ('umean')
       returnval = umean
    case ('uorb')
       returnval = uorb
    case ('ubot')
       returnval = ubot
    case ('ubcom')
       returnval = ubcom
    case ('uvdist')
       returnval = uvdist
    case ('uwtypu')
       returnval = uwtypu
    case ('uwtypv')
       returnval = uwtypv
    case ('umnldf')
       returnval = umnldf
    case ('umnflc')
       returnval = umnflc
    case ('usus')
       returnval = usus
    case ('v0')
       returnval = v0
    case ('v1')
       returnval = v1
    case ('vicuv')
       returnval = vicuv
    case ('vicww')
       returnval = vicww
    case ('vmdis')
       returnval = vmdis
    case ('vmean')
       returnval = vmean
    case ('vmnldf')
       returnval = vmnldf
    case ('vmnflc')
       returnval = vmnflc
    case ('vnu2d')
       returnval = vnu2d
    case ('vnu3d')
       returnval = vnu3d
    case ('volum0')
       returnval = volum0
    case ('volum1')
       returnval = volum1
    case ('vortic')
       returnval = vortic
    case ('vsus')
       returnval = vsus
    case ('w1')
       returnval = w1
    case ('w10mag')
       returnval = w10mag
    case ('windsu')
       returnval = windsu
    case ('windsv')
       returnval = windsv
    case ('windcd')
       returnval = windcd
    case ('windu')
       returnval = windu
    case ('windv')
       returnval = windv
    case ('wlen')
       returnval = wlen
    case ('wlcom')
       returnval = wlcom
    case ('wphy')
       returnval = wphy
    case ('ws')
       returnval = ws
    case ('wssus')
       returnval = wssus
    case ('wstau')
       returnval = wstau
    case ('wsu')
       returnval = wsu
    case ('wsucom')
       returnval = wsucom
    case ('wsv')
       returnval = wsv
    case ('wsvcom')
       returnval = wsvcom
    case ('wsbu')
       returnval = wsbodyu
    case ('wsbuc')
       returnval = wsbodyucom
    case ('wsbv')
       returnval = wsbodyv
    case ('wsbvc')
       returnval = wsbodyvcom
    case ('x2y')
       returnval = x2y
    case ('x3')
       returnval = x3
    case ('xcor')
       returnval = xcor
    case ('xy2')
       returnval = xy2
    case ('xydro')
       returnval = xydro
    case ('xyzsrc')
       returnval = xyzsrc
    case ('xz')
       returnval = xz
    case ('y3')
       returnval = y3
    case ('ycor')
       returnval = ycor
    case ('yz')
       returnval = yz
    case ('z0ucur')
       returnval = z0ucur
    case ('z0vcur')
       returnval = z0vcur
    case ('z0urou')
       returnval = z0urou
    case ('z0vrou')
       returnval = z0vrou
    case ('zalfas')
       returnval = zalfas
    case ('zbdsed')
       returnval = zbdsed
    case ('zcuru')
       returnval = zcuru
    case ('zcurv')
       returnval = zcurv
    case ('zcurw')
       returnval = zcurw
    case ('zdicww')
       returnval = zdicww
    case ('zdist')
       returnval = zdist
    case ('zdpsed')
       returnval = zdpsed
    case ('zdps')
       returnval = zdps
    case ('zqxk')
       returnval = zqxk
    case ('zqyk')
       returnval = zqyk
    case ('zrho')
       returnval = zrho
    case ('zrich')
       returnval = zrich
    case ('zrsdeq')
       returnval = zrsdeq
    case ('zstep')
       returnval = zstep
    case ('ztauet')
       returnval = ztauet
    case ('ztauks')
       returnval = ztauks
    case ('ztur')
       returnval = ztur
    case ('zvicww')
       returnval = zvicww
    case ('zwl')
       returnval = zwl
    case ('zws')
       returnval = zws
    case ('zwndsp')
       returnval = zwndsp
    case ('zwndcd')
       returnval = zwndcd
    case ('zwnddr')
       returnval = zwnddr
    case ('zairp')
       returnval = zairp
    case ('zprecp')
       returnval = zprecp
    case ('zevap')
       returnval = zevap
    case ('drhodx')
       returnval = drhodx
    case ('drhody')
       returnval = drhody
    case ('dzs0')
       returnval = dzs0
    case ('dzs1')
       returnval = dzs1
    case ('dzu0')
       returnval = dzu0
    case ('dzu1')
       returnval = dzu1
    case ('dzv0')
       returnval = dzv0
    case ('dzv1')
       returnval = dzv1
    case ('p1')
       returnval = p1
    case ('p0')
       returnval = p0
    case ('pnhcor')
       returnval = pnhcor
    case ('w0')
       returnval = w0
    case ('s00')
       returnval = s00
    case ('hydprs')
       returnval = hydprs
    case ('ibuff')
       returnval = ibuff
    case ('idifu')
       returnval = idifu
    case ('irocol')
       returnval = irocol
    case ('itbcc')
       returnval = itbcc
    case ('itbct')
       returnval = itbct
    case ('itdis')
       returnval = itdis
    case ('itdro')
       returnval = itdro
    case ('kadu')
       returnval = kadu
    case ('kadv')
       returnval = kadv
    case ('kcs')
       returnval = kcs
    case ('kcscut')
       returnval = kcscut
    case ('kcu')
       returnval = kcu
    case ('kcu45')
       returnval = kcu45
    case ('kcv')
       returnval = kcv
    case ('kcv45')
       returnval = kcv45
    case ('kfs')
       returnval = kfs
    case ('kfu')
       returnval = kfu
    case ('kfv')
       returnval = kfv
    case ('kspu')
       returnval = kspu
    case ('kspv')
       returnval = kspv
    case ('kstp')
       returnval = kstp
    case ('kzs')
       returnval = kzs
    case ('kzu')
       returnval = kzu
    case ('kzv')
       returnval = kzv
    case ('mnbar')
       returnval = mnbar
    case ('mnbnd')
       returnval = mnbnd
    case ('mndro')
       returnval = mndro
    case ('mnksrc')
       returnval = mnksrc
    case ('nob')
       returnval = nob
    case ('kfumin')
       returnval = kfumin
    case ('kfvmin')
       returnval = kfvmin
    case ('kfsmin')
       returnval = kfsmin
    case ('kfumax')
       returnval = kfumax
    case ('kfvmax')
       returnval = kfvmax
    case ('kfsmax')
       returnval = kfsmax
    case ('kfumx0')
       returnval = kfumx0
    case ('kfvmx0')
       returnval = kfvmx0
    case ('kfsmx0')
       returnval = kfsmx0
    case ('kfumn0')
       returnval = kfumn0
    case ('kfvmn0')
       returnval = kfvmn0
    case ('kfsmn0')
       returnval = kfsmn0
    case ('kfsz0')
       returnval = kfsz0
    case ('kfsz1')
       returnval = kfsz1
    case ('kfuz0')
       returnval = kfuz0
    case ('kfuz1')
       returnval = kfuz1
    case ('kfvz0')
       returnval = kfvz0
    case ('kfvz1')
       returnval = kfvz1
    case ('disint')
       returnval = disint
    case ('dismmt')
       returnval = dismmt
    case ('nambar')
       returnval = nambar
    case ('nambnd')
       returnval = nambnd
    case ('namcon')
       returnval = namcon
    case ('namdro')
       returnval = namdro
    case ('namsrc')
       returnval = namsrc
    case ('tprofc')
       returnval = tprofc
    case ('tprofu')
       returnval = tprofu
    case ('typbnd')
       returnval = typbnd
    case ('zkfs')
       returnval = zkfs
!
    case ('iwrk1')
       returnval = iwrk1
    case ('iwrk2')
       returnval = iwrk2
    case ('iwrk3')
       returnval = iwrk3
    case ('wrka1')
       returnval = wrka1
    case ('wrka2')
       returnval = wrka2
    case ('wrka3')
       returnval = wrka3
    case ('wrka4')
       returnval = wrka4
    case ('wrka5')
       returnval = wrka5
    case ('wrka6')
       returnval = wrka6
    case ('wrka7')
       returnval = wrka7
    case ('wrka8')
       returnval = wrka8
    case ('wrka9')
       returnval = wrka9
    case ('wrka10')
       returnval = wrka10
    case ('wrka11')
       returnval = wrka11
    case ('wrka12')
       returnval = wrka12
    case ('wrka13')
       returnval = wrka13
    case ('wrka14')
       returnval = wrka14
    case ('wrka15')
       returnval = wrka15
    case ('wrka16')
       returnval = wrka16
    case ('wrkb1')
       returnval = wrkb1
    case ('wrkb2')
       returnval = wrkb2
    case ('wrkb3')
       returnval = wrkb3
    case ('wrkb4')
       returnval = wrkb4
    case ('wrkb5')
       returnval = wrkb5
    case ('wrkb6')
       returnval = wrkb6
    case ('wrkb7')
       returnval = wrkb7
    case ('wrkb8')
       returnval = wrkb8
    case ('wrkb9')
       returnval = wrkb9
    case ('wrkb10')
       returnval = wrkb10
    case ('wrkb11')
       returnval = wrkb11
    case ('wrkb12')
       returnval = wrkb12
    case ('wrkb13')
       returnval = wrkb13
    case ('wrkb14')
       returnval = wrkb14
    case ('wrkb15')
       returnval = wrkb15
    case ('wrkb16')
       returnval = wrkb16
    case ('wrkb17')
       returnval = wrkb17
    case ('wrkb18')
       returnval = wrkb18
    case ('wrkc1')
       returnval = wrkc1
    case ('wrkc2')
       returnval = wrkc2
    case ('wrkc3')
       returnval = wrkc3
    case ('wrkc4')
       returnval = wrkc4
    case ('zwork')
       returnval = zwork
    case default
       write(*,*) '*** ERROR parameter ',pntnam,' not found; using water level instead.'
       returnval = s1
    end select arrayname
    getpointer = returnval
end function getpointer
