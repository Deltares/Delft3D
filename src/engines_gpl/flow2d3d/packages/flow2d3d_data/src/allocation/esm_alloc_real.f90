subroutine esm_alloc_real(lundia, error, gdp)
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
!    Function: Determines memory requirements for the
!              REAL ARRAY. In this subroutine the start indices
!              of all real arrays are calculated by using the
!              memory management functions MKRPNT, MKDPNT and MKFPNT.
!              The start adress of an array can be found by using
!              the function GTRPNT.
!              Function mk*pnt will when errors occure call an
!              errorroutine (ERRPNT). The function mk*pnt will
!              return with value 1 or for memory already
!              declared with correct length with value -1
!              Because the Delft3D-FLOW module can use static
!              array declaration the error messages will stay
!              at the end of the routine
! Method used:
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
    integer                 , pointer :: ncmax
    integer                 , pointer :: nmax
    integer                 , pointer :: mmax
    integer                 , pointer :: nlb
    integer                 , pointer :: nub
    integer                 , pointer :: mlb
    integer                 , pointer :: mub
    integer                 , pointer :: ddbound
    integer                 , pointer :: nmaxus
    integer                 , pointer :: kmax
    integer                 , pointer :: nmaxd
    integer                 , pointer :: mmaxd
    integer                 , pointer :: lmax
    integer                 , pointer :: lsts
    integer                 , pointer :: lstsc
    integer                 , pointer :: lstsci
    integer                 , pointer :: lsed
    integer                 , pointer :: lsedtot
    integer                 , pointer :: ltur
    integer                 , pointer :: kmxdt
    integer                 , pointer :: npiwe
    integer                 , pointer :: nbub
    integer                 , pointer :: nlcest
    integer                 , pointer :: nto
    integer                 , pointer :: kc
    integer                 , pointer :: kcd
    integer                 , pointer :: nopest
    integer                 , pointer :: nsrc
    integer                 , pointer :: nostat
    integer                 , pointer :: ntruv
    integer                 , pointer :: ntru
    integer                 , pointer :: nofou
    integer                 , pointer :: ndro
    integer                 , pointer :: nsluv
    integer                 , pointer :: nrpntr
    logical                 , pointer :: wind
    logical                 , pointer :: salin
    logical                 , pointer :: temp
    logical                 , pointer :: const
    logical                 , pointer :: drogue
    logical                 , pointer :: wave
    logical                 , pointer :: struct
    logical                 , pointer :: cdwstruct
    logical                 , pointer :: sedim
    logical                 , pointer :: zmodel
    logical                 , pointer :: roller
    logical                 , pointer :: veg3d
    logical                 , pointer :: bubble
    real(fp), dimension(:,:), pointer :: ustokes
    real(fp), dimension(:,:), pointer :: vstokes
!
! Global variables
!
    integer                   :: lundia !  Description and declaration in inout.igs
    logical    , intent(out)  :: error  !  TRUE if an error is encountered
!
! Local variables
!
    integer           :: ierr     ! Errorflag 
    integer           :: istat
    integer           :: kfacrl   ! Multiplication factor; 1 if ROLLER='Y', else 0 
    integer           :: kfaccdw  ! Multiplication factor; 1 if CDWSTRUCT='Y', else 0 
    integer           :: kfacvg3d ! Multiplication factor; 1 if VEG3D='Y', else 0 
    integer           :: kfacwv   ! Multiplication factor; 1 if WAVE=TRUE, else 0
    integer           :: kfacz    ! Multiplication factor; 1 if ZMODEL=TRUE 1, else 0
    integer           :: laak     ! Length of aak array in combination with length of rbuff array, lmax <= 1 
    integer           :: laakl    ! Length of wrkc1 array in combination with length of rbuff array, lmax > 1 
    integer           :: lmaxsed  ! max(lmax,lsedtot)
    integer           :: lrbuff   ! Length of rbuff array for writing to nefis files maximum value of rbuff nmaxus * mmax * kmax * 2 or nmaxus * mmax * kmax * lmax or 2 * nostat or 4 * ntruv (the last 2 are only theoretical the largest) 
    integer           :: mmax3d
    integer           :: mmaxddb
    integer           :: nmax3d
    integer           :: nmaxddb
    integer, external :: mkdpnt
    integer, external :: mkfpnt
    integer, external :: mkrpnt
    character(6)      :: pntnam   ! Pointername
    character(256)    :: message
!
!! executable statements -------------------------------------------------------
!
    wind       => gdp%gdprocs%wind
    salin      => gdp%gdprocs%salin
    temp       => gdp%gdprocs%temp
    const      => gdp%gdprocs%const
    drogue     => gdp%gdprocs%drogue
    wave       => gdp%gdprocs%wave
    struct     => gdp%gdprocs%struct
    cdwstruct  => gdp%gdprocs%cdwstruct
    sedim      => gdp%gdprocs%sedim
    zmodel     => gdp%gdprocs%zmodel
    roller     => gdp%gdprocs%roller
    veg3d      => gdp%gdprocs%veg3d
    bubble     => gdp%gdprocs%bubble
    nrpntr     => gdp%gdpointrs%nrpntr
    ncmax      => gdp%d%ncmax
    nmax       => gdp%d%nmax
    mmax       => gdp%d%mmax
    nlb        => gdp%d%nlb
    nub        => gdp%d%nub
    mlb        => gdp%d%mlb
    mub        => gdp%d%mub
    ddbound    => gdp%d%ddbound
    nmaxus     => gdp%d%nmaxus
    kmax       => gdp%d%kmax
    nmaxd      => gdp%d%nmaxd
    mmaxd      => gdp%d%mmaxd
    lmax       => gdp%d%lmax
    lsts       => gdp%d%lsts
    lstsc      => gdp%d%lstsc
    lstsci     => gdp%d%lstsci
    lsed       => gdp%d%lsed
    lsedtot    => gdp%d%lsedtot
    ltur       => gdp%d%ltur
    kmxdt      => gdp%d%kmxdt
    npiwe      => gdp%d%npiwe
    nbub       => gdp%d%nbub
    nlcest     => gdp%d%nlcest
    nto        => gdp%d%nto
    kc         => gdp%d%kc
    kcd        => gdp%d%kcd
    nopest     => gdp%d%nopest
    nsrc       => gdp%d%nsrc
    nostat     => gdp%d%nostat
    ntruv      => gdp%d%ntruv
    ntru       => gdp%d%ntru
    nofou      => gdp%d%nofou
    ndro       => gdp%d%ndro
    nsluv      => gdp%d%nsluv
    !
    ! initialize array bounderies
    !
    nmaxddb = gdp%d%nub - gdp%d%nlb + 1
    mmaxddb = gdp%d%mub - gdp%d%mlb + 1
    nmax3d  = nmaxd + 2*gdp%d%ddbound
    mmax3d  = mmaxd + 2*gdp%d%ddbound
    !
    ! set multiplication factors
    !
    kfacwv = 0
    if (wave) then
       kfacwv = 1
    endif
    !
    kfacz = 0
    if (zmodel) then
       kfacz = 1
    endif
    !
    kfacrl = 0
    if (roller) kfacrl = 1
    !
    kfaccdw = 0
    if (cdwstruct) then
       kfaccdw = 1
    endif
    !
    kfacvg3d = 0
    if (veg3d) then
       kfacvg3d = 1
    endif
    !
    ! check 32bit/64bit:
    ! pntrsize = 4 : 32bit
    ! pntrsize = 8 : 64bit
    !
    write (message,'(a,i2,a)') 'Executable for ', 8*pntrsize, '-bits platform.'
    call prterr(lundia, 'G051', trim(message))
    !
    ! check precision:
    ! fp = sp : single precision
    ! fp = hp : double precision
    ! else    : error
    !
    if (fp == hp) then
       write (message,'(a,i4)') 'Double precision computation using reals of kind ',fp
       call prterr(lundia, 'G051', trim(message))
    elseif (fp == sp) then
       write (message,'(a,i4)') 'Single precision computation using reals of kind ',fp
       call prterr(lundia, 'G051', trim(message))
    else
       write (message,'(a,i4)') 'Unable to allocate for reals of kind ',fp
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    ! check precision:
    ! prec = sp : single precision
    ! prec = hp : double precision
    ! else     : error
    ! create messages in case prec /= fp
    !
    if (prec == sp) then
       if (prec /= fp) then
          write (message,'(a)') 'Array DPS is in single precision'
          call prterr(lundia, 'G051', trim(message))
       endif
    elseif (prec == hp) then
       if (prec /= fp) then
          write (message,'(a)') 'Array DPS is in double precision'
          call prterr(lundia, 'G051', trim(message))
          write (message,'(a)') 'DPS can not be viewed with Online Visualisation'
          call prterr(lundia, 'Z013', trim(message))
          write (*,'(a,a)') '*** WARNING ',trim(message)
       endif
    else
       write (message,'(a,i4)') 'Unable to allocate for reals of kind ',prec
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    ! arrays for: 3d information
    !
    !                           thick   (kmax                )
    !                           sig     (kmax                ) in sigma model
    !                           sig     (0:kmax              ) = zk() in z model
    !                           zwork   (kmax*5              )
    !
    pntnam = 'thick'         !  Relative layer thickness
    ierr = mkfpnt(pntnam, kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sig'           !  Sigma [-1,0] levels of layer centers (in sigma model): kmax
                             !  or Z levels of layer interfaces (in z model): kmax+1
    ierr = mkfpnt(pntnam, kmax+1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zwork'         !  Work array for Z layer model
    ierr = mkfpnt(pntnam, kmax*5, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----array for: x,y,z position of sources
    !
    !                           xyzsrc  (3     ,nsrc         )
    !                           qsrcrt  (2     ,nsrc         )
    !
    pntnam = 'XYZSRC'        !  Coordinates for discharge points
    ierr = mkfpnt(pntnam, 3*nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qsrcrt'        !  RTC-controlled discharge data:
                             !  QSRCRT(1,*) = Return status from RTC
                             !              > 0 : OK
                             !              < 0 : Not OK/Found
                             !  QSRCRT(2,*) = Value from RTC
    ierr = mkfpnt(pntnam, 2*nsrc, gdp)
    if (ierr <= -9) goto 9999

    !
    !-----arrays for: boundaries
    !
    !                           alpha   (nto                 )
    !                           omega   (kc                  )
    !                           hydrbc  (4     ,nto   ,kcd   )
    !                           procbc  (4     ,nto   ,kmax  ,lstsc )
    !                           rob     (kmax  ,nopest       )
    !                           zstep   (2     ,nto   ,lstsc )
    !                           circ3d  (kmax  ,2     ,nlcest)
    !                           circ2d  (4     ,nlcest       )
    !                           rbnd    (kmax  ,lstsc ,2     ,nlcest)
    !                           rthbnd  (kmax  ,lstsc ,2     ,nlcest)
    !                           thtim   (kmax  ,lstsc ,2     ,nlcest)
    !                           rettim  (nto   ,lstc  ,2     )
    !                           qtfrac  (nopest)
    !                           qtfrct  (nto   )
    !                           qtfrt2  (nto   )
    !
    pntnam = 'alpha'         !  HLES parameter; slope of the energy
                             !  density spectrum
                             !  Weakly reflective coefficient
    ierr = mkfpnt(pntnam, nto, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'omega'         !  Frequencies of the Hydrod. forcings
    ierr = mkfpnt(pntnam, kc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'hydrbc'        !  Hydrodynamic bound. val. at MNBND sections.
                             !  The array is completely filled when the
                             !  H-type boundary is considered.
                             !  When time series is considered, the array is
                             !  only relevant for index K=1.
                             !               H-signal  t-signal
                             !               --------  --------
                             !  HYDRBC(1,N,K)= AmplA  AmplA
                             !                        at curr. time
                             !  HYDRBC(2,N,K)= AmplB  AmplB
                             !                        at curr. time
                             !  HYDRBC(3,N,K)= FaseA  AmplA incr. at
                             !                        subeseq. time
                             !  HYDRBC(4,N,K)= FaseB  AmplB incr. at
                             !                        subeseq. time
                             !  K = 1,.,KC & N = 1,.,NTOF (H)
                             !  K = 1,.,KMAX & N = NTOF+1,.,NTO (T)
    ierr = mkfpnt(pntnam, 4*nto*kcd, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'procbc'        !  Boundary val. for constituents at
                             !  opening sections (time series).
                             !  1,N,K,L = value at a
                             !  2,N,K,L = value at b
                             !  3,N,K,L = time incr. at a
                             !  4,N,K,L = time incr. at b
                             !  N=1,.,NTO; K=1,.,KMAX & L=1,.,LSTSC
    ierr = mkfpnt(pntnam, 4*nto*kmax*lstsc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rob'           !  Help array
    ierr = mkfpnt(pntnam, kmax*nopest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zstep'         !  Time varying location of disconti-
                             !  nuity for the 3D BC for constituents
                             !  old and new time in index 1 and 2
    ierr = mkfpnt(pntnam, 2*nto*lstsc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'circ3d'        !  Array with 3D Boundary Conditions
    ierr = mkfpnt(pntnam, kmax*2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'circ2d'        !  Array with Boundary Conditions and
                             !  Reflection coefficients hydrodynamics
    ierr = mkfpnt(pntnam, 4*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    !    MAX(lstsc,1) is applied to avoid undefined rbnd pointer in tritra when calling difu*, when lstsc is 0.
    pntnam = 'rbnd'          !  Return time/Boundary values for constituents
                             !  at each open boundary point (after in-
                             !  terpolation in space has been
                             !  carried out). 2-nd and 3-rd index in
                             !  the array points to IROCOL).
                             !  RBND(K,L,I,N) = AMPL
                             !  L=1,.,LSTSC & I=1/2 & N=1,.,NOROCO
    ierr = mkfpnt(pntnam, kmax*max(lstsc, 1)*2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rthbnd'        !  Last outflow (Th.Harleman) value of Boundary
                             !  Conditions for constituents
    ierr = mkfpnt(pntnam, kmax*lstsc*2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'thtim'         !  Actual Thatcher Harleman time
    ierr = mkfpnt(pntnam, kmax*lstsc*2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rettim'        !  User defined delay for Thatcher Harleman return time
    ierr = mkfpnt(pntnam, 2*nto*lstsc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qtfrac'        !  no description (yet)
    ierr = mkfpnt(pntnam, nopest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qtfrct'        !  no description (yet)
    ierr = mkfpnt(pntnam, nto, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qtfrt2'        !  no description (yet)
    ierr = mkfpnt(pntnam, nto, gdp)
    if (ierr <= -9) goto 9999
    !
    ! for Riemann-Van Dongeren-Svendsen left bc incoming signal (begin)
    !
    ! left boundary
    !
    pntnam = 'ctif'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'stif'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zetabf'        !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ctbf'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'stbf'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'encgf'         !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cgdghf'        !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wenf'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wenfm'         !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zbmnf'         !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zetaif'        !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ctrf'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'umeanf'        !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zmeanf'        !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    ! right boundary
    !
    pntnam = 'ctil'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'stil'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zetabl'        !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ctbl'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'stbl'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'encgl'         !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cgdghl'        !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wenl'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wenlm'         !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zbmnl'         !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zetail'        !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ctrl'          !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'umeanl'        !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zmeanl'        !  no description (yet)
    ierr = mkfpnt(pntnam, 2*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'crbc'          !  no description (yet)
    ierr = mkfpnt(pntnam, 24*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    ! for Riemann-Van Dongeren-Svendsen bc incoming signal (end)
    !
    !
    !-----arrays for: depths
    !
    !                           dp    (nmaxddb  ,mmaxddb)
    !                           dpu   (nmaxddb  ,mmaxddb)
    !                           dpv   (nmaxddb  ,mmaxddb)
    !                           dps   (nmaxddb  ,mmaxddb)
    !                           hkru  (nmaxddb  ,mmaxddb)
    !                           hkrv  (nmaxddb  ,mmaxddb)
    !                           hu    (nmaxddb  ,mmaxddb)
    !                           hu0   (nmaxddb  ,mmaxddb)
    !                           hv    (nmaxddb  ,mmaxddb)
    !                           hv0   (nmaxddb  ,mmaxddb)
    !
    pntnam = 'dp'            !  Depth value at depth points
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dpu'           !  Depth value at u-points incl. crest
                             !  height (only for general 3D weir)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dpv'           !  Depth value at V-points incl. crest
                             !  height (only for general 3D weir)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dps'           !  Depth at waterlevel points
    if (prec == sp) then
       ierr = mkrpnt(pntnam, nmaxddb*mmaxddb, gdp)
    elseif (prec == hp) then
       ierr = mkdpnt(pntnam, nmaxddb*mmaxddb, gdp)
    else
       ! caught at top of esm_alloc_real.f90
    endif
    if (ierr <= -9) goto 9999
    !
    pntnam = 'hkru'          !  Vertical position crest of U-weir
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'hkrv'          !  Vertical position crest of V-weir
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'hu'            !  Total water depth in u-point [m]
                             !  Upwind approach in
                             !  a. points with HU below 5*dryflc
                             !  b. barrier points.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'hu0'           !  Total water depth in u-point [m] in previous time step
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'hv'            !  Total water depth in v-point [m]
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'hv0'           !  Total water depth in v-point [m] in previous time step
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: initial field values
    !
    !                           s1    (nmaxddb ,mmaxddb)
    !                           s0    (nmaxddb ,mmaxddb)
    !                           u1    (nmaxddb ,mmaxddb,kmax  )
    !                           u0    (nmaxddb ,mmaxddb,kmax  )
    !                           umean (nmaxddb ,mmaxddb)
    !                           v1    (nmaxddb ,mmaxddb,kmax  )
    !                           v0    (nmaxddb ,mmaxddb,kmax  )
    !                           vmean (nmaxddb ,mmaxddb)
    !                           w1    (nmax3d,-1:mmax3d+2,0:kmax)
    !                           r1    (nmaxddb ,mmaxddb,kmax ,lstsci)
    !                           r0    (nmaxddb ,mmaxddb,kmax ,lstsci)
    !                           rtur1 (nmaxddb ,mmaxddb,0:kmax,ltur )
    !                           rtur0 (nmaxddb ,mmaxddb,0:kmax,ltur )
    !                           decay (lstsc )
    !
    pntnam = 's1'            !  Water level (zeta) at NEW time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 's0'            !  Water level (zeta) at old time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'u1'            !  U-velocities at new time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'u0'            !  U-velocities at old time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'umean'         !  Depth-average of u-velocity component
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'v1'            !  V-velocities at new time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'v0'            !  V-velocities at old time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'vmean'         !  Depth-average of v-velocity component
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'w1'            !  Omega-velocity at new time level
                             !  (velocity relative to sigma-plane)
    ierr = mkfpnt(pntnam, nmax3d*(mmax3d + 4)*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'r1'            !  Concentrations at new time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lstsci, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'r0'            !  Concentrations at old time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lstsci, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rtur1'         !  Turbulent kinetic energy (0:kmax,1)
                             !  and its dissipation rate (0:kmax,2)
                             !  at new time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1)*ltur, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'rtur0'         !  Turbulent kinetic energy (0:kmax,1)
                             !  and its dissipation rate (0:kmax,2)
                             !  at old time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1)*ltur, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'decay'         !  Decay rates per day as input
                             !  divided by 86400. in RDIC
    ierr = mkfpnt(pntnam, lstsc, gdp)
    if (ierr <= -9) goto 9999
    !
    !
    !-----arrays for: subgrid viscosity model
    !
    !                           umnldf(nmaxddb  ,mmaxddb)
    !                           umnflc(nmaxddb  ,mmaxddb)
    !                           vmnldf(nmaxddb  ,mmaxddb)
    !                           vmnflc(nmaxddb  ,mmaxddb)
    !                           vortic(nmaxddb  ,mmaxddb,kmax)
    !                           enstro(nmaxddb  ,mmaxddb,kmax)
    !
    pntnam = 'umnldf'        !  Depth-averaged low-pass filtered
                             !  U-velocities
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'umnflc'        !  Depth-averaged fluctuating
                             !  U-velocities
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'vmnldf'        !  Depth-averaged low-pass filtered
                             !  V-velocities
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'vmnflc'        !  Depth-averaged fluctuating
                             !  V-velocities
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    !
    if (ierr <= -9) goto 9999
    !
    pntnam = 'vortic'        !  Vorticity
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'enstro'        !  Enstrophy
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax), gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: roughness, viscosity and diffusity field values
    !
    !                           cfurou(nmaxddb  ,mmaxddb,3)
    !                           cfvrou(nmaxddb  ,mmaxddb,3)
    !                           cvalu0(nmaxddb  ,mmaxddb)
    !                           cvalv0(nmaxddb  ,mmaxddb)
    !                           vicuv (nmaxddb  ,mmaxddb,kmax+2)
    !                           vicww (nmax3d ,-1:mmax3d+2,0:kmax(>1))
    !                           dicuv (nmaxddb  ,mmaxddb,kmax+2)
    !                           dicww (nmax3d ,-1:mmax3d+2,0:kmax(>1))
    !
    pntnam = 'cfurou'        !  1: ratio magnitude velocity/ustar
                             !  in U-point (Chezy/sqrt(g))
                             !  2: U-roughness expressed in User
                             !  RouMet unit (includes trachytopes)
                             !  3: default U-roughness expressed in User
                             !  RouMet unit (excludes trachytopes)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*3, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cfvrou'        !  1: ratio magnitude velocity/ustar
                             !  in V-point (Chezy/sqrt(g))
                             !  2: V-roughness expressed in User
                             !  RouMet unit (includes trachytopes)
                             !  3: default V-roughness expressed in User
                             !  RouMet unit (excludes trachytopes)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*3, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cvalu0'        !  Chezy value for roughness in U-direction
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cvalv0'        !  Chezy value for roughness in V-direction
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'vicuv'         !  Horizontal eddy visc. coefficient [m2/s] (in density point)
                             !  Background value will be set in layer kmax+1
                             !  Horizontal eddy value (HLES or roller) will be set in layer kmax+2
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 2), gdp)
  if (ierr <= -9) goto 9999
    !
    pntnam = 'vicww'         !  Vertial eddy viscosity coefficient [m2/s] (at interface between layer k and k+1)
    ierr = mkfpnt(pntnam, nmax3d*(mmax3d + 4)*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dicuv'         !  Horizontal Diffusion coefficient [m2/s]
                             !  Background value will be set in layer kmax+1
                             !  Horizontal eddy value (HLES or roller) will be set in layer kmax+2
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 2), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dicww'         !  Vertical   Diffusion coefficient [m2/s]
                             !  at layer interfaces
    ierr = mkfpnt(pntnam, nmax3d*(mmax3d + 4)*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: grid information
    !
    !                           guu   (nmaxddb  ,mmaxddb)
    !                           gvv   (nmaxddb  ,mmaxddb)
    !                           gsqs  (nmaxddb  ,mmaxddb)
    !                           gsqd  (nmaxddb  ,mmaxddb)
    !                           alfas (nmaxddb  ,mmaxddb)
    !                           guv   (nmaxddb  ,mmaxddb)
    !                           gvu   (nmaxddb  ,mmaxddb)
    !                           xcor  (nmaxddb  ,mmaxddb)
    !                           ycor  (nmaxddb  ,mmaxddb)
    !                           xz    (nmaxddb  ,mmaxddb)
    !                           yz    (nmaxddb  ,mmaxddb)
    !                           wphy  (nmaxddb  ,mmaxddb,kmax    )
    !                           guz   (nmaxddb  ,mmaxddb)
    !                           gvz   (nmaxddb  ,mmaxddb)
    !                           gud   (nmaxddb  ,mmaxddb)
    !                           gvd   (nmaxddb  ,mmaxddb)
    !                           gsqiu (nmaxddb  ,mmaxddb)
    !                           gsqiv (nmaxddb  ,mmaxddb)
    !
    pntnam = 'guu'           !  Grid distance in the ETA-/Y-direction
                             !  at U-velocity point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'gvv'           !  Grid distance in the KSI-/X-direction
                             !  at V-velocity point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'gsqs'          !  Area of computational cell defined at
                             !  zeta point. Reduced in case of rigid lid
                             !  approximation.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'gsqd'          !  Area of a cell defined at the depth point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'alfas'         !  Orientation of each computational cell
                             !  defined as the angle formed by the line
                             !  spanned by the u-velocity points
                             !  around the zeta point and the x-axis
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'guv'           !  Grid distance in the eta-/y-direction at v-velocity point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'gvu'           !  Grid distance in the ksi-/x-direction at u-velocity point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'xcor'          !  X-coord. of the depth point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ycor'          !  Y-coord. of the depth point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'xz'            !  X-coord. of the zeta/water elevation pnt.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'yz'            !  Y-coord. of the zeta/water elevation pnt.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wphy'          !  Physical w-velocity at new time level (in fixed coordinate system)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'guz'           !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'gvz'           !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'gud'           !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'gvd'           !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'gsqiu'         !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'gsqiv'         !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: flows
    !
    !                           qxk   (nmaxddb  ,mmaxddb ,kmax  )
    !                           qyk   (nmaxddb  ,mmaxddb ,kmax  )
    !                           qzk   (nmax3d ,-1:mmax3d+2,0:kmax)
    !
    pntnam = 'qxk'           !  Discharges for layer k in the X-dir.
                             !  in U-velocity point at following time
                             !  integration continuity equation
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qyk'           !  Discharges for layer k in the Y-dir.
                             !  in V-velocity point at following time
                             !  integration continuity equation
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qzk'           !  Discharges at layer interface k+1/2
                             !  in the Z-direction following time in-
                             !  tegration continuity equation
    ierr = mkfpnt(pntnam, nmax3d*(mmax3d + 4)*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: Coriolis
    !
    !                           fcorio(nmaxddb  ,mmaxddb)
    !
    pntnam = 'fcorio'        !  Coriolis Force
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for drogues; dxydro and xydro
    !
    !                           dxydro(2,ndro  )
    !                           xydro (2,ndro  )
    !
    pntnam = 'dxydro'        !  Delta X(1)Y(2)-coordinate correspon-
                             !  ding to drogue starting point
                             !  0. := left/lower cell boundary
                             !  1. := right/upper cell boundary
    ierr = mkfpnt(pntnam, 2*ndro, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'xydro'         !  X(1)Y(2)-coordinate corresponding to
                             !  drogue starting point if
                             !  track is calculated else 999.999
    ierr = mkfpnt(pntnam, 2*ndro, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'drodep'        !  Depth of each drogue under water level
    ierr = mkfpnt(pntnam, ndro, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for barriers;
    !
    !                           cbuv  (4,nsluv   )
    !                           cbuvrt(2,nsluv   )
    !
    pntnam = 'cbuv'          !  CBUV(1,*) = Initial gate height
                             !  CBUV(2,*) = old (1.0) or new (2.0) format of barrier specification
                             !  CBUV(3,*) = Loss coefficient
                             !  CBUV(4,*) = ??? used in new format only
    ierr = mkfpnt(pntnam, 4*nsluv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cbuvrt'        !  RTC-controlled barrier data:
                             !  CBUVRT(1,*) = Return status from RTC
                             !              > 0 : OK
                             !              < 0 : Not OK/Found
                             !  CBUVRT(2,*) = Value from RTC
    ierr = mkfpnt(pntnam, 2*nsluv, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: structure information
    !
    !                           pship    (nmaxddb  ,mmaxddb)
    !                           ubrlsu   (nmaxddb  ,mmaxddb,kmax)
    !                           ubrlsv   (nmaxddb  ,mmaxddb,kmax)
    !                           uwtypu   (nmaxddb  ,mmaxddb)
    !                           uwtypv   (nmaxddb  ,mmaxddb)
    !                           dteu     (nmaxddb  ,mmaxddb)
    !                           dtev     (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwlsu   (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwlsv   (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwztu   (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwzbu   (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwztv   (nmaxddb  ,mmaxddb)
    !         *kfaccdw          cdwzbv   (nmaxddb  ,mmaxddb)
    !                           porosu   (nmaxddb  ,mmaxddb, kmax)
    !                           porosv   (nmaxddb  ,mmaxddb, kmax)
    !                           areau    (nmaxddb  ,mmaxddb, kmax)
    !                           areav    (nmaxddb  ,mmaxddb, kmax)
    !
    pntnam = 'pship'         !  Depth value floating structure
                             !  Later re-defined as pressure
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ubrlsu'        !  Roughness coefficient in x-dir.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ubrlsv'        !  Roughness coefficient in y-dir.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'uwtypu'        !  Type of weir in x-dir.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'uwtypv'        !  Type of weir in y-dir.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dteu'          !  Subgrid energy loss due to 2D weir in U-points
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dtev'          !  Subgrid energy loss due to 2D weir in V-points
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cdwztu'          !  vertical position [m] of top of the fixed gate/cdw in U-point
                               !  Only used in cells with KSPU(NM,0) = 10
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cdwzbu'          !  vertical position [m] of bottom of the fixed gate/cdw in U-point
                               !  Only used in cells with KSPU(NM,0) = 10
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cdwztv'          !  vertical position [m] of top of the fixed gate/cdw in V-point
                               !  Only used in cells with KSPV(NM,0) = 10
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cdwzbv'          !  vertical position [m] of bottom of the fixed gate/cdw in V-point
                               !  Only used in cells with KSPV(NM,0) = 10
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cdwlsu'          !  loss coefficient [???] of the fixed gate/cdw in U-point
                               !  Only used in cells with KSPU(NM,0) = 10
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cdwlsv'          !  loss coefficient [???] of the fixed gate/cdw in V-point
                               !  Only used in cells with KSPV(NM,0) = 10
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfaccdw, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'porosu'          !  porosity [-] in U-point, value in range [0.0 - 1.0]
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'porosv'          !  porosity [-] in V-point, value in range [0.0 - 1.0]
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'areau'           !  flow area of cell in U-point [m2]
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'areav'           !  flow area of cell in V-point [m2]
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----additional arrays for sediment transport
    !
    !                           ws    (nmaxddb  ,mmaxddb,kmax+1,lsed  )
    !                           rhowat(nmaxddb  ,mmaxddb,kmax)
    !                           depchg(nmaxddb  ,mmaxddb)
    !                           sbu   (nmaxddb  ,mmaxddb,lsedtot)
    !                           sbv   (nmaxddb  ,mmaxddb,lsedtot)
    !                           epscur(0:kmax)
    !                           epswav(0:kmax)
    !                          sbuu  (nmaxddb  ,mmaxddb,lsedtot)
    !                          sbvv  (nmaxddb  ,mmaxddb,lsedtot)
    !
    pntnam = 'ws'            !  Settling/fall velocity
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1)*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rhowat'        !  Water density [kg/m3]
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'seddif'        !  Vertical sediment diffusion coeff.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1)*lmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'depchg'        !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'epscur'        !  no description (yet)
    ierr = mkfpnt(pntnam, kmax + 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'epswav'        !  dissipation at surface (breaking waves)
    ierr = mkfpnt(pntnam, kmax + 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sbuu'          !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*lsedtot, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sbvv'          !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*lsedtot, gdp)
    if (ierr <= -9) goto 9999
    !
    !     Arrays for: fluid mud; driving forces for fluid mud layer
    !                   usus, vsus, czusus, czvsus, wssus, entr, rsed
    !                   excbed and soumud
    !
    !
    pntnam = 'usus'          !  U-velocities at old time level
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     usus_mud = u0(kmax)_water
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'vsus'          !  V-velocities at old time level
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     vsus_mud = v0(kmax)_water
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'czusus'        !  U-chezy coefficients
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     czusus_mud = cfurou(2)_water
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'czvsus'        !  V-chezy coefficients
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     czvsus_mud = cfvrou(2)_water
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wssus'         !  Fall velocity
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     wssus_mud = wstau_water
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wstau'         !  Fall velocity at k=kmax
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'entr'          !  Entrainment mud --> suspension layer
                             !  Only used in water fraction of fluid mud calculation
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rsed'          !  Concentration of sediment(1) at old time level
                             !  at k=kmax (suspension layer), water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     rsed_mud = r0(kmax,l1)_water
                             !  WARNING: only the first sediment type is communicated!!!
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'soumud'        !  Growth speed mud layer (m/s)
                             !  >0 source(>0) or sink(<0) for mud layer
                             !  source term for combined effect of
                             !  erosion/dewater (exchange) and settling/entrainment
                             !  Only used in mud fraction of fluid mud calculation
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'excbed'        !  Exchange bed / mud layer
                             !  Only used in mud fraction of fluid mud calculation
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sepsus'        !  Water level  at old time level of water fraction
                             !  Only used in mud fraction of fluid mud calculation:
                             !     sepsus_mud = s0_water
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: time dependent discharges
    !
    !                        disch (nsrc  )
    !                        disch0(nsrc  )
    !                        disch1(nsrc  )
    !                        rint   (lstsc ,nsrc      )
    !                        rintsm (lstsc ,nsrc      )
    !                        rint0  (lstsc ,nsrc      )
    !                        rint1  (lstsc ,nsrc      )
    !                        umdis (nsrc  )
    !                        umdis0(nsrc  )
    !                        umdis1(nsrc  )
    !                        vmdis (nsrc  )
    !                        vmdis0(nsrc  )
    !                        vmdis1(nsrc  )
    !                        sink  (nmaxddb  ,mmaxddb,kmax ,lstsci)
    !                        sour  (nmaxddb  ,mmaxddb,kmax ,lstsci)
    !
    pntnam = 'disch'         !  Time varying discharges [m3/sec]
                             !  at discharge points
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'disinp'        !  Copy of array DISCH
                             !  Contains input values (required for bubble screens)
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'voldis'        !  Total volume discharged by discharge [m3]
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999    
    !
    pntnam = 'disch0'        !  Old discharge value
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'disch1'        !  New discharge value
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rint'          !  Concentration at discharge points (as obtained from BCC file)
    ierr = mkfpnt(pntnam, lstsc*nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rintsm'        !  Concentration at discharge points (actual values as used in simulation)
    ierr = mkfpnt(pntnam, lstsc*nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rint0'         !  Old concentration value at discharge points (as obtained from BCC file)
    ierr = mkfpnt(pntnam, lstsc*nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rint1'         !  New concentration value at discharge points (as obtained from BCC file)
    ierr = mkfpnt(pntnam, lstsc*nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'umdis'         !  Velocity for discharges (in the x-dir)
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'umdis0'        !  Old time velocity for discharges (in the x-dir)
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'umdis1'        !  New time velocity for discharges (in the x-dir)
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'vmdis'         !  Velocity for discharges (in the y-dir)
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'vmdis0'        !  Old time velocity for discharges (in the y-dir)
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'vmdis1'        !  New time velocity for discharges (in the y-dir)
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sink'          !  Sink terms advection-diffusion equation (added to main diagonal BBKL)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lstsci, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sour'          !  Source terms advection-diffusion equation (added to Right Hand Side DDKL)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lstsci, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: time dependent wind fields
    !
    !                        windu (nmaxddb  ,mmaxddb)
    !                        windsu(nmaxddb  ,mmaxddb)
    !                        windv (nmaxddb  ,mmaxddb)
    !                        windsv(nmaxddb  ,mmaxddb)
    !                        patm  (nmaxddb  ,mmaxddb)
    !                        w10mag(nmaxddb  ,mmaxddb)
    !                        evap  (nmaxddb  ,mmaxddb)
    !                        windcd(nmaxddb  ,mmaxddb)
    !
    pntnam = 'windu'         !  Current wind velocity at 10 m in the ksi-dir.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'windsu'        !  Wind stress in the ksi-dir.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'windv'         !  Current wind velocity at 10 m in the eta-dir.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'windsv'        !  Wind stress in the eta-dir.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'patm'          !  Current atmosferic pressure in N/m2
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'w10mag'        !  Magnitude of Wind at 10 m
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'evap'          !  Evaporation in m/s
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999    
    !
    pntnam = 'zevap'         !  Instantaneous evaporation at monitoring stations
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'precip'        !  Precipitation in m/s
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zprecp'       !  Instantaneous precipitation at monitoring stations
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'windcd'        !  Time and space varying wind drag coef.
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    !
    !
    !-----arrays for: diffusion, concentrations
    !
    !                        sigdif(lstsci )
    !                        sigmol(lstsci )
    !                        rmneg (lstsci )
    !
    pntnam = 'sigdif'        !  Prandtl/schmidt-numbers for const.
    ierr = mkfpnt(pntnam, lstsci, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sigmol'        !  Moleculair Prandtl numbers for const.
    ierr = mkfpnt(pntnam, lstsci, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rmneg'         !  Criterion for conc. above which filter procedure is applied
    ierr = mkfpnt(pntnam, lstsci, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: diffusion, concentrations
    !
    !                        rho   (nmaxddb  ,mmaxddb,kmax  )
    !                        sumrho(nmaxddb  ,mmaxddb,kmax  )
    !
    pntnam = 'rho'           !  Mud-water densities [kg/m3]
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sumrho'        !  Integral of density from layer k until free surface [kg/m3]
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: tau
    !
    !                        taubpu(nmaxddb  ,mmaxddb)
    !                        taubpv(nmaxddb  ,mmaxddb)
    !                        taubsu(nmaxddb  ,mmaxddb)
    !                        taubsv(nmaxddb  ,mmaxddb)
    !                        taubmx(nmaxddb  ,mmaxddb)
    !                        rxx   (nmaxddb  ,mmaxddb,kmax  )
    !                        rxy   (nmaxddb  ,mmaxddb,kmax  )
    !                        ryy   (nmaxddb  ,mmaxddb,kmax  )
    !                        rxz   (nmaxddb  ,mmaxddb,kmax  )
    !                        ryz   (nmaxddb  ,mmaxddb,kmax  )
    !
    pntnam = 'taubpu'        !  Primary   bottom friction term in the
                             !  x-dir. (velocity dependent)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'taubpv'        !  Primary   bottom friction term in the
                             !  y-dir. (velocity dependent)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'taubsu'        !  Secondary bottom friction term in the
                             !  x-dir. (velocity dependent)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'taubsv'        !  Secondary bottom friction term in the
                             !  y-dir. (velocity dependent)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'taubmx'        !  Primary maximal bottom friction term
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rxx'           !  Turbulent stresses
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rxy'           !  Turbulent stresses
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ryy'           !  Turbulent stresses
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rxz'           !  Turbulent stresses
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ryz'           !  Turbulent stresses
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: waves
    !
    !                        dis   (nmaxddb  ,mmaxddb) * kfacwv * 4
    !                        discom(nmaxus,mmax   ,2) * kfacwv
    !                        rlabda(nmaxddb  ,mmaxddb) * kfacwv
    !                        teta  (nmaxddb  ,mmaxddb) * kfacwv
    !                        dircom(nmaxus,mmax   ,2) * kfacwv
    !                        dircos(nmaxus,mmax   ,2) * kfacwv
    !                        dirsin(nmaxus,mmax   ,2) * kfacwv
    !                        uorb  (nmaxddb  ,mmaxddb) * kfacwv
    !                        ubot  (nmaxddb  ,mmaxddb) * kfacwv
    !                        ubcom (nmaxddb  ,mmaxddb) * kfacwv
    !                        hrms  (nmaxddb  ,mmaxddb) * kfacwv
    !                        hrmcom(nmaxus,mmax   ,2) * kfacwv
    !                        tp    (nmaxddb  ,mmaxddb) * kfacwv
    !                        tpcom (nmaxus,mmax   ,2) * kfacwv
    !                        grmasu(nmaxddb  ,mmaxddb)
    !                        msucom(nmaxus,mmax   ,2) * kfacwv
    !                        grmasv(nmaxddb  ,mmaxddb)
    !                        msvcom(nmaxus,mmax   ,2) * kfacwv
    !                        wsu   (nmaxddb  ,mmaxddb)
    !                        wsucom(nmaxus,mmax   ,2) * kfacwv
    !                        wsv   (nmaxddb  ,mmaxddb)
    !                        wsvcom(nmaxus,mmax   ,2) * kfacwv
    !                        wsbu  (nmaxddb  ,mmaxddb)
    !                        wsbuc (nmaxus,mmax   ,2) * kfacwv
    !                        wsbv  (nmaxddb  ,mmaxddb)
    !                        wsbvc (nmaxus,mmax   ,2) * kfacwv
    !                        wlen  (nmaxus,mmax   ,2) * kfacwv
    !                        wlcom (nmaxus,mmax   ,2) * kfacwv
    !                        qxkw  (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        qykw  (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        cgc   (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        c     (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        qxkr  (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        qykr  (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        ewabr0(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        ewabr1(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        ewave0(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        ewave1(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        eroll0(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        eroll1(nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        sinkw (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        sourw (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        sinkr (nmaxddb  ,mmaxddb) * kfacwv *kfacrl
    !                        sourr (nmaxddb  ,mmaxddb) * kfacwv
    !                        fxw   (nmaxddb  ,mmaxddb) * kfacwv
    !                        fyw   (nmaxddb  ,mmaxddb) * kfacwv
    !                        dfu   (nmaxddb  ,mmaxddb)
    !                        dfv   (nmaxddb  ,mmaxddb)
    !                        deltau(nmaxddb  ,mmaxddb)
    !                        deltav(nmaxddb  ,mmaxddb)
    !
    pntnam = 'dis'           !  Dissipation waves
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*4, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'df'            !  Dissipation waves in wave boundary layer
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'discom'        !  Help array to interpolate between to consecutive timesteps for dissipation waves
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rlabda'        !  Wavelength
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'teta'          !  Angle between wave direction and local grid orientation
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dircom'        !  Help array to interpolate between to consecutive timesteps for angle waves
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dircos'        !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dirsin'        !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'uorb'          !  Orbital velocity at the bottom layer
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ubot'          !  Peak orbital velocity at the bottom layer
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ubcom'         !  Help array to interpolate between to consecutive timesteps for peak orbital velocity at the bottom layer
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'hrms'          !  RMS Wave Height
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'hrmcom'        !  Help array to interpolate between to consecutive timesteps for RMS wave height
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'tp'            !  Period waves
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'tpcom'         !  Help array to interpolate between to consecutive timesteps for period waves
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'grmasu'        !  Mass flux waves (in the x direction)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'grmasv'        !  Mass flux waves (in the y direction)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'grmsur'        !  Mass flux Roller (in the x direction)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'grmsvr'        !  Mass flux Roller (in the y direction)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'grfacu'        !  Breaker Delay Adjustment (in the x direction)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'grfacv'        !  Breaker Delay Adjustment (in the y direction)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'msucom'        !  Help array to interpolate between to consecutive timesteps for mass flux waves (in the x-dir.)
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'msvcom'        !  Help array to interpolate between to consecutive timesteps for mass flux waves (in the y-dir.)
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wsu'           !  Local x-component of the flow-driving force due to wave breaking (:= WSU)
                             !  Wave stresses (in the x-direction)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wsucom'        !  Help array to interpolate between to consecutive timesteps for wave stresses (in the x-direction)
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wsv'           !  Local y-component of the flow-driving force due to wave breaking (:= WSV)
                             !  Wave stresses (in the y-direction)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wsvcom'        !  Help array to interpolate between to consecutive timesteps for wave stresses (in the y-direction)
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wsbu'          !  Local x-component of the flow-driving force due to waves in the water column (:= WSBODYU)
                             !  Wave stresses in the water column (in the x-direction)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
                             ! 
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wsbuc'         !  Help array to interpolate between to consecutive timesteps for wave stresses in the water column (in the x-direction)
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wsbv'          !  Local y-component of the flow-driving force due to waves in the water column (:= WSBODYV)
                             !  Wave stresses in the water column (in the y-direction)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wsbvc'         !  Help array to interpolate between to consecutive timesteps for wave stresses in the water column (in the y-direction)
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wlen'          !  Mean wave length
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wlcom'        !  Help array to interpolate between to consecutive timesteps for mean wave length
    ierr = mkfpnt(pntnam, nmaxus*mmax*2*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qxkw'          !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qykw'          !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'cgc'           !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'c'             !  Internal work array, tridiagonal matrix water levels upper diagonal
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qxkr'          !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qykr'          !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ewabr0'        !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ewabr1'        !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ewave0'        !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ewave1'        !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'eroll0'        !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'eroll1'        !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sinkw'         !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sourw'         !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sinkr'         !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sourr'         !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'fxw'           !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'fyw'           !  no description (yet)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kfacwv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ampbc'         !  no description (yet)
    ierr = mkfpnt(pntnam, ncmax*kfacwv*kfacrl, gdp)
    !
    if (ierr <= -9) goto 9999
    pntnam = 'ombc'          !  no description (yet)
    ierr = mkfpnt(pntnam, ncmax*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'phibc'         !  no description (yet)
    ierr = mkfpnt(pntnam, ncmax*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'thetbc'        !  no description (yet)
    ierr = mkfpnt(pntnam, ncmax*kfacwv*kfacrl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dfu'           !  Bottom wave dissipation in u-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dfv'           !  Bottom wave dissipation in v-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'deltau'        !  Boundary layer thickness in u-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'deltav'        !  Boundary layer thickness in v-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: derivatives
    !
    !                        dddksi(nmaxddb  ,mmaxddb)
    !                        dddeta(nmaxddb  ,mmaxddb)
    !                        dzdksi(nmaxddb  ,mmaxddb)
    !                        dzdeta(nmaxddb  ,mmaxddb)
    !
    pntnam = 'dddksi'        !  Ddepth/deta in u-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dddeta'        !  Ddepth/dksi in v-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dzdeta'        !  Dzeta /deta in u-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dzdksi'        !  Dzeta /dksi in v-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    !      Export HDT to shared mem
    !
    pntnam = 'hdt'           !  Half Integration time step [seconds]
    ierr = mkfpnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    ! overview workarrays for version v240:
    ! 16 2D arrays (r 80-88 + 88a-88e)
    ! 17 3D arrays (r 89-109)
    !  4 3D arrays (r 94-97)
    !
    !                wrka1    (nmaxddb  ,mmaxddb) or (nmaxwind*mmaxwind)
    !                wrka2    (nmaxddb  ,mmaxddb) or (nmaxwind*mmaxwind)
    !                wrka3    (nmaxddb  ,mmaxddb) or (nmaxwind*mmaxwind)
    !                wrka4    (nmaxddb  ,mmaxddb)
    !                wrka5    (nmaxddb  ,mmaxddb)
    !                wrka6    (nmaxddb  ,mmaxddb)
    !                wrka7    (nmaxddb  ,mmaxddb)
    !                wrka8    (nmaxddb  ,mmaxddb)
    !                wrka9    (nmaxddb  ,mmaxddb)
    !                wrka12   (nmaxddb  ,mmaxddb)
    !                wrka13   (nmaxddb  ,mmaxddb)
    !                wrka14   (nmaxddb  ,mmaxddb)
    !                wrka15   (nmaxddb  ,mmaxddb)
    !                wrka16   (nmaxddb  ,mmaxddb)
    !
    pntnam = 'wrka1'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrka2'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrka3'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrka4'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrka5'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrka6'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrka7'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrka8'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrka9'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrka10'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'wrka11'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr == -1) goto 9999    
    !
    pntnam = 'wrka12'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'wrka13'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'wrka14'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'wrka15'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'wrka16'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr == -1) goto 9999
    !
    !-----arrays for: coefficient matrices (double kmax dimension)
    !     for the array rbuff we use wrkb1 or wrkc1 dependent on the value
    !     of lmax
    ! NB. arrays wrkb1, wrkb2, wrkb3, wrkb4, wrkb5 and wrkb6 have 1 extra layer
    ! due to new turbulence model and fully non-hydrostatic module
    !
    !                        wrkb1   (nmaxddb  ,mmaxddb,0:kmax  )
    !                      := rbuff max ((nmaxus,mmax  ,0:kmax,lmax  ),
    !                                    (nmaxus,mmax  ,kmax  ,2     ))
    !                              for lmax > 1 declared with wrkc1
    !                        wrkb2   (nmaxddb  ,mmaxddb,0:kmax  )
    !                        wrkb3   (nmaxddb  ,mmaxddb,0:kmax  )
    !                        wrkb4   (nmaxddb  ,mmaxddb,0:kmax  )
    !                        wrkb5   (nmaxddb  ,mmaxddb,0:kmax  )
    !                        wrkb6   (nmaxddb  ,mmaxddb,0:kmax  )
    !                        wrkb7   (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb8   (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb9   (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb10  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb11  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb12  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb13  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb14  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb15  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb16  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb17  (nmaxddb  ,mmaxddb,kmax  )
    !                        wrkb18  (nmaxddb  ,mmaxddb,kmax  )
    !
    lrbuff = nmaxus*mmax*kmax*2
    if (lmax>1) lrbuff = 0
    laak = max(nmaxddb*mmaxddb*(kmax + 1), lrbuff)
    !
    pntnam = 'wrkb1'         !  Internal work array
    ierr = mkfpnt(pntnam, laak, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb2'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb3'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb4'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb5'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb6'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax+1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb7'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb8'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb9'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb10'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb11'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb12'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb13'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb14'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb15'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb16'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb17'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkb18'        !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for: coefficient matrices (double kmax dimension)
    !
    !                        wrkc1  (nmaxddb  ,mmaxddb,kmax,lmax)
    !                      := rbuff max ((nmaxus,mmax  ,0:kmax,lmax  ),
    !                                    (nmaxus,mmax  ,kmax  ,2     ))
    !                              for lmax > 1 it fits always in wrkc1
    !                              for lmax <= 1 declared in wrkb1
    !                        wrkc2  (nmaxddb  ,mmaxddb,kmax,lmax)
    !                        wrkc3  (nmaxddb  ,mmaxddb,kmax,lmax)
    !                        wrkc4  (nmaxddb  ,mmaxddb,kmax,lmax)
    !
    lmaxsed = max(lmax,lsedtot)
    lrbuff  = nmaxus*mmax*(kmax + 1)*lmaxsed
    if (lmaxsed<=1) lrbuff = 0
    laakl = max(nmaxddb*mmaxddb*kmax*lmaxsed, lrbuff)
    !
    pntnam = 'wrkc1'         !  Internal work array
    ierr = mkfpnt(pntnam, laakl, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkc2'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*max(1, lmax), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkc3'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*lmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'wrkc4'         !  Internal work array
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*max(1, lmax), gdp)
    if (ierr <= -9) goto 9999
    !
    ! RBUFF is used only outside TRISOL. So we use a work array, which
    ! is initialized each timestep in TRISOL, here WRKB1 or WRKC1
    ! The maximum needed storage space for RBUFF is defined in esm_alloc_real
    ! and taken in consideration in the declaration of WRKB1 and WRKC1
    !
    ! As a result RBUFF does not need to be allocated
    ! RBUFF is going to point to WRKB1 or WRKC1, depending on their size
    ! See subroutine gtptrs.f90
    !
    !-----arrays for: curvature coefficients of streakline
    !
    !                        x3    (nmaxddb  ,mmaxddb)
    !                        x2y   (nmaxddb  ,mmaxddb)
    !                        xy2   (nmaxddb  ,mmaxddb)
    !                        y3    (nmaxddb  ,mmaxddb)
    !
    pntnam = 'x3'            !  Coef. of u**3 used for curvature
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'x2y'           !  Coef. of u**2*v used for curvature
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'xy2'           !  Coef. of u*v**2 used for curvature
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'y3'            !  Coef. of v**3  used for curvature
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for anti creep algorithm
    !
    !                        dpdksi(nmaxddb  ,mmaxddb,1:kmax )
    !                        dpdeta(nmaxddb  ,mmaxddb,1:kmax )
    !                        dsdksi(nmaxddb  ,mmaxddb,1:kmax )
    !                        dsdeta(nmaxddb  ,mmaxddb,1:kmax )
    !                        dtdksi(nmaxddb  ,mmaxddb,1:kmax )
    !                        dtdeta(nmaxddb  ,mmaxddb,1:kmax )
    !                        dldksi(nmaxddb  ,mmaxddb,1:kmax )
    !                        dldeta(nmaxddb  ,mmaxddb,1:kmax )
    !
    pntnam = 'dpdksi'        !  Strictly horizontal gradient density (baroclinic part) in pressure term in ksi-direction
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'dpdeta'        !  Strictly horizontal gradient density (baroclinic part) in pressure term in eta-direction
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'dsdksi'        !  Strictly horizontal gradient salinity in ksi-direction
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'dsdeta'        !  Strictly horizontal gradient salinity in eta-direction
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'dtdksi'        !  Strictly horizontal gradient temperature in ksi-direction
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'dtdeta'        !  Strictly horizontal gradient temperature in eta-direction
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'dldksi'        !  Horizontal gradient sediment, strictly horizontal in ksi-direction
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'dldeta'        !  Horizontal gradient sediment, strictly horizontal in eta-direction
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr == -1) goto 9999
    !
    !-----arrays for: matrices for turbulence model
    !
    !                        bruvai(nmaxddb  ,mmaxddb,0:kmax )
    !                        rich  (nmaxddb  ,mmaxddb,0:kmax )
    !                        dudz  (nmaxddb  ,mmaxddb,0:kmax )
    !                        dvdz  (nmaxddb  ,mmaxddb,0:kmax )
    !
    pntnam = 'bruvai'        !  Bouyancy frequency, squared - Brunt Vaisly frequency
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'rich'          !  Gradient Richardson number at layer interfaces
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'dudz'          !  Vertical gradient of u-velocity component at layer interface
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'dvdz'          !  Vertical gradient of v-velocity component at layer interface
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)
    if (ierr == -1) goto 9999
    !
    !-----arrays for DELWAQ
    !
    !                        volum0(nmaxddb  ,mmaxddb,kmax  )
    !                        volum1(nmaxddb  ,mmaxddb,kmax  )
    !                        z0ucur(nmaxddb  ,mmaxddb       )
    !                        z0vcur(nmaxddb  ,mmaxddb       )
    !                        z0urou(nmaxddb  ,mmaxddb       )
    !                        z0vrou(nmaxddb  ,mmaxddb       )
    !                        qu    (nmaxddb  ,mmaxddb,kmax  )
    !                        qv    (nmaxddb  ,mmaxddb,kmax  )
    !
    pntnam = 'volum0'        !  Volume of a cell at old time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'volum1'        !  Volume of a cell at new time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'z0ucur'        !  Z0 roughness related to currents in U-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'z0vcur'        !  Z0 roughness related to currents in V-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr == -1) goto 9999
    !
    pntnam = 'z0urou'        !  Wave enhanced Z0 roughness in U-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'z0vrou'        !  Wave enhanced Z0 roughness in V-point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qu'            !  Cummulative discharge for layer k in the X-dir. in U-velocity point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'qv'            !  Cummulative discharge for layer k in the Y-dir. in V-velocity point
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for post processing HIS file
    !
    !                        zwl   (nostat       )
    !                        zalfas(nostat       )
    !                        zcuru (nostat,kmax  )
    !                        zcurv (nostat,kmax  )
    !                        zcurw (nostat,kmax  )
    !                        zqxk  (nostat,kmax  )
    !                        zqyk  (nostat,kmax  )
    !                        ztauks(nostat       )
    !                        ztauet(nostat       )
    !                        zvicww(nostat,0:kmax)
    !                        zdicww(nostat,0:kmax)
    !                        zrich (nostat,0:kmax)
    !                        zrho  (nostat,kmax  )
    !                        fltr  (ntruv        )
    !                        ctr   (ntruv        )
    !                        gro   (nostat,kmax  ,lstsci)
    !                        ztur  (nostat,0:kmax,ltur  )
    !                        atr   (ntruv ,lstsci)
    !                        dtr   (ntruv ,lstsci)
    !                      a sbtr  (ntruv ,lsedtot  )
    !                      b sstr  (ntruv ,lsed  )
    !                      c sbtrc (ntruv ,lsedtot  )
    !                      d sstrc (ntruv ,lsed  )
    !                        discum(nsrc         )
    !                        zws   (nostat,0:kmax,lsed  )
    !                        zrsdeq(nostat,lsed  )
    !                        zbdsed(nostat       ,lsedtot  )
    !                        zdpsed(nostat       )
    !                        zdps  (nostat       )
    !                      d zsbu  (nostat       ,lsedtot  )
    !                      e zsbv  (nostat       ,lsedtot  )
    !                      f zssu  (nostat       ,lsed  )
    !                      g zssv  (nostat       ,lsed  )
    !                        zvort (nostat,kmax  )
    !                        zenst (nostat,kmax  )
    !                       hydprs(nostat,kmax  )
    !
    pntnam = 'zwl'           !  Water elevation at the monitoring stations
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zalfas'        !  Orientation at the monitoring stations (defined at the water elevation point) as the angle formed by
                             !  the line spanned by the u-velocity points around the zeta point and the x-axis
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zcuru'         !  U-velocity at the monitoring stations (defined at the water elevation point)
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zcurv'         !  V-velocity at the monitoring stations (defined at the water elevation point)
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zcurw'         !  W-velocity at the monitoring stations (defined at the water elevation point)
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zqxk'          !  Flow in the x-dir. at the monitoring stations (defined at the water elevation point)
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zqyk'          !  Flow in the y-dir. at the monitoring stations (defined at the water elevation point)
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ztauks'        !  Bottom friction in KSI direction at the monitoring stations defined at the water elevation point
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ztauet'        !  Bottom friction in ETA direction at the monitoring stations defined at the water elevation point
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zvicww'        !  Vertical Eddy viscosity-3D at the monitoring stations (defined at the water elevation point)
    ierr = mkfpnt(pntnam, nostat*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zdicww'        !  Vertical Eddy diffusivity-3D at the monitoring stations (defined at the water elevation point)
    ierr = mkfpnt(pntnam, nostat*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zrich'         !  Richardson numbers at the monitoring stations (defined at the water elevation point)
    ierr = mkfpnt(pntnam, nostat*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zrho'          !  Density at the monitoring stations (defined at the water elevation point)
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zwndsp'        !  Wind speed at the monitoring stations
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zwndcd'        !  Wind drag coef at the monitoring stations
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zwnddr'        !  Wind direction at the monitoring stations
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zairp'         !  Air pressure at the monitoring stations
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'fltr'          !  Cumulative volume transport for water through the cross section.
    ierr = mkfpnt(pntnam, ntruv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ctr'           !  Total flow through the cross section.
    ierr = mkfpnt(pntnam, ntruv, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'gro'           !  Concentration values at the monitoring stations
    ierr = mkfpnt(pntnam, nostat*kmax*lstsci, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ztur'          !  Concentrations turbulent energy and dissipation at the monitoring station (defined at the water elevation point)
    ierr = mkfpnt(pntnam, nostat*(kmax + 1)*ltur, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'atr'           !  Cumulative advective transport for constituents through the cross section.
    ierr = mkfpnt(pntnam, ntruv*lstsci, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dtr'           !  Cumulative diffusive transport for constituents through the cross section.
    ierr = mkfpnt(pntnam, ntruv*lstsci, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sbtr'          !  no description (yet)
    ierr = mkfpnt(pntnam, ntruv*lsedtot, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sstr'          !  no description (yet)
    ierr = mkfpnt(pntnam, ntruv*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sbtrc'         !  no description (yet)
    ierr = mkfpnt(pntnam, ntruv*lsedtot, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'sstrc'         !  no description (yet)
    ierr = mkfpnt(pntnam, ntruv*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'discum'        !  Discharge values [m3/sec] cumulative in time
    ierr = mkfpnt(pntnam, nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zws'           !  Fall velocity (dependent on sediment type)
    ierr = mkfpnt(pntnam, nostat*(kmax + 1)*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zrsdeq'        !  Equilibrium sediment concentration
    ierr = mkfpnt(pntnam, nostat*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zbdsed'        !  Total sediment in bed: kg /m2
    ierr = mkfpnt(pntnam, nostat*lsedtot, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zdpsed'        !  Total thickness of sediment layer
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zdps'          !  Depth at monitoring station
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zsbu'          !  Bed load transport in u-direction at station
    ierr = mkfpnt(pntnam, nostat*lsedtot, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zsbv'          !  Bed load transport in v-direction at station
    ierr = mkfpnt(pntnam, nostat*lsedtot, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zssu'          !  Susp. load transport in u-direction at station
    ierr = mkfpnt(pntnam, nostat*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zssv'          !  Susp. load transport in v-direction at station
    ierr = mkfpnt(pntnam, nostat*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zrca'          !  Near-bed reference concentration of sediment at station
    ierr = mkfpnt(pntnam, nostat*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zseddf'         !  Veritcal sediment diffusion at station
    ierr = mkfpnt(pntnam, nostat*(kmax + 1)*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zsours'         !  Suspended sediment source term at station
    ierr = mkfpnt(pntnam, nostat*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zsinks'         !  Suspended sediment sink term at station
    ierr = mkfpnt(pntnam, nostat*lsed, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zfrac'          !  Sediment fraction at station
    ierr = mkfpnt(pntnam, nostat*lsedtot, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zmudfr'         !  Total mud fraction at station
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zsandf'         !  Total sand fraction at station
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zfixfc'         !  Reduction factor for limited sediment availability at station
    ierr = mkfpnt(pntnam, nostat*lsedtot, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zhidex'         !  Hiding and exposure factor at station
    ierr = mkfpnt(pntnam, nostat*lsedtot, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ztaub'          !  Bed shear stress for morphology at station
    ierr = mkfpnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zvort'         !  Vorticity at station
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zenst'         !  Enstrophy at station
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'hydprs'        !  Non-hydrostatic pressure at station
    ierr = mkfpnt(pntnam, nostat*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for Internal wave energy
    !
    !                        tgarkx(15   * (kmax +1))
    !                        tgarkt(24   * (kmxdt+1))
    !                        tgarnp(4    * (npiwe+1))
    !                        tkepro(nmaxddb  ,mmaxddb,kmax  )
    !                        tkedis(nmaxddb  ,mmaxddb,kmax  )
    !                        fuiwe (nmaxddb  ,mmaxddb,kmax  )
    !                        fviwe (nmaxddb  ,mmaxddb,kmax  )
    !
    pntnam = 'tgarkx'        !  Delft3D flow arrays for point (N,M)
                             !  for all layers  0:KMAX
    ierr = mkfpnt(pntnam, 15*(kmax + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'tgarkt'        !  All work arrays for IWE for point
                             !  (N,M) for all KMXT layers
    ierr = mkfpnt(pntnam, 24*(kmxdt + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'tgarnp'        !  All work arrays for IWE for point
                             !  (N,M) for all NFREQS frequencies
    ierr = mkfpnt(pntnam, 4*(npiwe + 1), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'tkepro'        !  Turbulence production term due to IWE
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'tkedis'        !  Turbulence dissipation term due to IWE
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'fuiwe'         !  Force/momentum flux in u-direction due to critical-layer formation, on hydrodynamic grid (not yet installed)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'fviwe'         !  Force/momentum flux in v-direction due to critical-layer formation, on hydrodynamic grid (not yet installed)
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    !-----arrays for 2D Turbulence model
    !
    !                        vnu2d (nmaxddb  ,mmaxddb       )
    !                        vnu3d (nmaxddb  ,mmaxddb       )
    !                        rtu2d0(nmaxddb  ,mmaxddb,   2  )
    !                        rtu2d1(nmaxddb  ,mmaxddb,   2  )
    !                        rtubnd(nmaxddb  ,mmaxddb,   2  )
    !
    pntnam = 'vnu2d'         !  Horizontal eddy visc. coefficient [m2/s] (in depth point), due to 2D-turbulence
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'vnu3d'         !  Depth-averaged vertical eddy visc. coefficient [m2/s] (in depth point), due to 3D-turbulence
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rtu2d0'        !  Concentrations turbulent energy and dissipation/enstrophy at old time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*2, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rtu2d1'        !  Concentrations turbulent energy and dissipation/enstrophy at new time level
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*2, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rtubnd'        !  Boundary conditions for turbulent quantities
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*2, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: vertical grid information for Online Visualisation
    !
    !                        uvdist(max(nmax,mmax),kmax+2)
    !                        huvw  (max(nmax,mmax),kmax+2)
    !                        zdist (max(nmax,mmax),kmax+2)
    !                        dpc   (max(nmax,mmax),kmax+2)
    !
    pntnam = 'uvdist'        !  no description (yet)
    ierr = mkfpnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'huvw'          !  no description (yet)
    ierr = mkfpnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'zdist'         !  no description (yet)
    ierr = mkfpnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dpc'           !  no description (yet)
    ierr = mkfpnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
    if (ierr <= -9) goto 9999
    !
    ! BEGIN arrays for: z-model (fixed layer)
    !
    !
    ! arrays for: vertical grid
    !
    !            * kfacz     dzs0  (nmaxddb,mmaxddb,kmax)
    !            * kfacz     dzs1  (nmaxddb,mmaxddb,kmax)
    !            * kfacz     dzu0  (nmaxddb,mmaxddb,kmax)
    !            * kfacz     dzu1  (nmaxddb,mmaxddb,kmax)
    !            * kfacz     dzv0  (nmaxddb,mmaxddb,kmax)
    !            * kfacz     dzv1  (nmaxddb,mmaxddb,kmax)
    !
    pntnam = 'dzs0'          !  Layer thickness at Z-points (old time level)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dzs1'          !  Layer thickness at Z-points (new time level)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dzu0'          !  Layer thickness at U-points (old time level)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dzu1'          !  Layer thickness at U-points (new time level)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dzv0'          !  Layer thickness at V-points (old time level)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dzv1'          !  Layer thickness at V-points (new time level)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: baroclinic pressure gradient
    !
    !                        drhodx(nmaxddb,mmaxddb,kmax+2)
    !                        drhody(nmaxddb,mmaxddb,kmax+2)
    !
    pntnam = 'drhodx'        !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'drhody'        !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    !
    ! END arrays for: z-model (fixed layer)
    !
    !
    ! BEGIN arrays for: non-hydrostatic pressure
    !
    !
    pntnam = 'p1'            !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'p0'            !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'p00'           !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'pnhcor'        !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'w0'            !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*(kmax + 1)*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 's00'           !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dzs00'         !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dzu00'         !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'dzv00'         !  no description (yet)
    ierr = mkfpnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    ! END arrays for: non-hydrostatic pressure
    !
    !
    ! BEGIN arrays for: (Rigid) 3D Vegetation Model
    !
    !
    pntnam = 'diapl'         !  plant stem diameter [m]
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*kfacvg3d, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'rnpl'          !  number of stems per horizontal unit area [1/m2]
    ierr = mkfpnt(pntnam, nmaxddb*mmaxddb*kmax*kfacvg3d, gdp)
    if (ierr <= -9) goto 9999
    !
    ! END arrays for: (Rigid) 3D Vegetation Model
    !
    !
    ! The following list of scalar reals have to be placed in shared memory,
    ! because then the mapper can read them.
    ! BE CAREFUL:
    !    These reals are allocated TWICE inside FLOW:
    !    in esm_alloc_real.f90 as part of the shared memory block, allocated via esm/fsm, and
    !    in *.igs-files as part of the GDP structure (e.g. ag) or
    !    locally on a high level (see scalar integers in esm_alloc_int.f90)
    !
    !    FLOW uses the instance in the GDP-structure (or the
    !    local definition respectively) and place a copy of these parameters in
    !    the shared memory block. The mapper uses this copy and is assumed not to
    !    change the values.
    !
    ! TO DO: Clean implementation:
    !    These parameters should be allocated in shared memory only (since the
    !    mapper needs them). The GDP-structure and local allocation variants
    !    must be replaced by pointers to the shared memory instance.
    !
    pntnam = 'AG'            !  Gravity acceleration [m2/s]
    ierr = mkfpnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'RHOW'          !  Density of water [kg/m3]
    ierr = mkfpnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'DT'            !  Integration time step in tunits (tunit is 60.0 sec by default)
    ierr = mkfpnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'TSCALE'        !  Integration time step in seconds
                             !  TSCALE = dt*tunit
    ierr = mkfpnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    ! GDP arrays
    !
    if (.not. associated(gdp%gdtrisol%ustokes)) then
       allocate (gdp%gdtrisol%ustokes(gdp%d%nmlb:gdp%d%nmub,kmax), stat = istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'esm_alloc_real: memory alloc error')
          call d3stop(1, gdp)
       endif
       gdp%gdtrisol%ustokes = 0.0_fp
    endif
    if (.not. associated(gdp%gdtrisol%vstokes)) then
       allocate (gdp%gdtrisol%vstokes(gdp%d%nmlb:gdp%d%nmub,kmax), stat = istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'esm_alloc_real: memory alloc error')
          call d3stop(1, gdp)
       endif
       gdp%gdtrisol%vstokes = 0.0_fp
    endif
    !
    ! Test if pointer declaration outside declaration in POINTRS.INC
    !
    if (ierr== - 3) then
       error = .true.
       call prterr(lundia    ,'G005'    ,' '       )
       write (lundia, *) '         Parameter MXRPNT to small, add ',            &
                       & nrpntr - mxrpnt
    endif
    !
    ! Test exit code which are not allowed (in theory not possible)
    !
 9999 continue
    if (ierr <= -9) then
       error = .true.
       call prterr(lundia    ,'G920'    ,'esm_alloc_real'   )
    endif
end subroutine esm_alloc_real
