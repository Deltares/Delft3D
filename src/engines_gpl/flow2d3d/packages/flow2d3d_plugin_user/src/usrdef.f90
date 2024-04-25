subroutine usrdef(lundia    ,error     ,grdang    ,secflo    ,gdp       )
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
!    Function: - Initialisation of array's used by user defined
!                subroutines
!              - Reading of input from user defined files
!              - Initialisation of array's using user defined
!                constants
!              - For "rigid sheet" the old files (<3.00) can be
!                read and are adjusted
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'fsm.i'
    include 'tri-dyn.igd'
    integer                        , pointer :: nmax
    integer                        , pointer :: mmax
    integer                        , pointer :: nmaxus
    integer                        , pointer :: kmax
    integer                        , pointer :: ltur
    integer                        , pointer :: nto
    integer                        , pointer :: kc
    integer                        , pointer :: nrob
    real(fp)                       , pointer :: tstop
    real(fp)                       , pointer :: dt
    integer                        , pointer :: itstrt
    integer                        , pointer :: itfinish
    integer                        , pointer :: itdiag
    real(fp)                       , pointer :: zwi
    real(fp)                       , pointer :: ck
    integer                        , pointer :: inpzw
    integer                        , pointer :: nprocs
    integer      , dimension(:)    , pointer :: nread
    integer(pntrsize), dimension(:, :) , pointer :: nprptr
    integer      , dimension(:, :) , pointer :: nprinp
    real(fp)     , dimension(:)    , pointer :: rcousr
    character*256, dimension(:)    , pointer :: filusr
    character*20 , dimension(:)    , pointer :: procs
!
! Global variables
!
    integer               :: lundia  !  Description and declaration in inout.igs
    logical               :: error   !!  Flag=TRUE if an error is encountered
    logical  , intent(in) :: secflo  !  Description and declaration in procs.igs
    real(fp) , intent(in) :: grdang  !  Description and declaration in tricom.igs
!
! Local variables
!
    integer(pntrsize) :: kcs    ! Pointer of array KCS 
    integer(pntrsize) :: kspu   ! Pointer of array KSPU 
    integer(pntrsize) :: kspv   ! Pointer of array KSPV 
    integer(pntrsize) :: nob    ! Pointer of array NOB 
    integer(pntrsize) :: ubnd   ! Pointer of array UBND 
    integer           :: it
    integer           :: length
    integer           :: ltest1
    integer           :: ltest2
    integer           :: ltest3
    integer           :: ltest4
    integer           :: ltest6
    integer           :: n
    integer           :: nfil
    integer           :: nreal
    integer           :: ubrlsu
    integer           :: ubrlsv
    integer(pntrsize), external :: gtipnt
    integer(pntrsize), external :: gtrpnt
    logical           :: dtn
    real(fp)          :: anglen
    real(fp)          :: t
    real(fp)          :: tdiagm
    real(fp)          :: windd  ! Wind direction read from file given as wind from north 
    real(fp)          :: windft
    real(fp)          :: windsp ! Wind speed read from file 
    real(fp)          :: windxt
    real(fp)          :: windyt
!
!! executable statements -------------------------------------------------------
!
    nmax        => gdp%d%nmax
    mmax        => gdp%d%mmax
    nmaxus      => gdp%d%nmaxus
    kmax        => gdp%d%kmax
    ltur        => gdp%d%ltur
    nto         => gdp%d%nto
    kc          => gdp%d%kc
    nrob        => gdp%d%nrob
    tstop       => gdp%gdexttim%tstop
    dt          => gdp%gdexttim%dt
    itstrt      => gdp%gdinttim%itstrt
    itfinish    => gdp%gdinttim%itfinish
    itdiag      => gdp%gdinttim%itdiag
    zwi         => gdp%gdturcoe%zwi
    ck          => gdp%gdturcoe%ck
    inpzw       => gdp%gdturcoe%inpzw
    nprocs      => gdp%gdusrpar%nprocs
    nread       => gdp%gdusrpar%nread
    nprptr      => gdp%gdusrpar%nprptr
    nprinp      => gdp%gdusrpar%nprinp
    rcousr      => gdp%gdusrpar%rcousr
    filusr      => gdp%gdusrpar%filusr
    procs       => gdp%gdusrpar%procs
    !
    inpzw = 0
    zwi = 0.0
    !
    itdiag = itfinish + 1
    !
    !-----User Defined Function ? <YES/NO>
    !
    if (nprocs==0) goto 600
    !
    ltest1 = 0
    ltest2 = 0
    ltest3 = 0
    ltest4 = 0
    ltest6 = 0
    do n = 1, nprocs
       if (procs(n)=='bc turbulence model ') then
          ltest1 = n
       endif
       if (procs(n)=='rigid sheets        ') then
          ltest2 = n
       endif
       if (procs(n)=='diagnostic mode     ') then
          ltest3 = n
       endif
       if (procs(n)=='particle wind factor') then
          ltest4 = n
       endif
       if (procs(n)=='z_wave              ') then
          ltest6 = n
       endif
    enddo
    !
    !-----User Defined Function: BC for turbulence model defined (YES) ?
    !
    if (ltest1==0) goto 200
    if (nread(ltest1)/=0) then
       !
       !--------Initialize user defined array's
       !
       call prterr(lundia    ,'V200'    ,'bc turbulence model')
       !
       length = 2*ltur*(kmax + 1)*2*nto
       call usrptr(lundia    ,error     ,'ubnd'    ,'real     '          ,length    , &
                 & ubnd      ,gdp       )
       !
       if (error) goto 600
       nprptr(1, ltest1) = ubnd
       !
       !--------Retrieve array entry for FLOW array's
       !
       if (ltur>0 .and. nto>0) then
          nob = gtipnt('NOB', gdp)
          !
          !-----------Define file number
          !
          nfil = 0
          do n = 1, nread(ltest1) - 1
             nfil = nfil + nprinp(1, n)
          enddo
          nfil = nfil + 1
          !
          !-----------Read user defined open boundary condition input in array
          !           UBND from user defined file
          !
          call urdbcc(lundia    ,error     ,filusr(nfil)         ,ltur      ,kmax      , &
                    & nto       ,r(ubnd)   ,gdp       )
          !
          if (error) goto 600
          !
          !-----------Test direction boundary versus contents of UBND array
          !
          call uckbcc(ltur      ,kmax      ,nto       ,nrob      ,i(nob)    , &
                    & r(ubnd)   )
       endif
    endif
    !
    !-----User Defined Function: Rigid sheets defined (YES) ?
    !     "rigid sheets" is defined by keyword Filrgs from v3.03 on.
    !     For clients with permission to use the UDF they should be able
    !     to run as before
    !     For "local weir loss" the changes in the input are obstructing
    !     upward competiblity.
    !
  200 continue
    if (ltest2==0) goto 300
    if (nread(ltest2)/=0) then
       !
       !--------Warning to diagnostic file
       !
       call prterr(lundia    ,'V200'    ,'rigid sheets'       )
       !
       !
       !--------Retrieve array entry for Delft3D-FLOW arrays
       !
       kcs = gtipnt('kcs', gdp)
       kspu = gtipnt('kspu', gdp)
       kspv = gtipnt('kspv', gdp)
       ubrlsu = gtrpnt('ubrlsu', gdp)
       ubrlsv = gtrpnt('ubrlsv', gdp)
       !
       !--------Define file number
       !
       nfil = 0
       do n = 1, nread(ltest2) - 1
          nfil = nfil + nprinp(1, n)
       enddo
       nfil = nfil + 1
       !
       !--------Read user defined input in arrays from user defined file
       !
       call urdrgs(lundia    ,error     ,filusr(nfil)         ,nmax      ,mmax      , &
                 & kmax      ,nmaxus    ,i(kcs)    ,i(kspu)   ,i(kspv)   , &
                 & r(ubrlsu) ,r(ubrlsv) ,gdp       )
       !
       if (error) goto 600
    endif
    !
    !-----User Defined Function: Diagnostic mode (YES) ?
    !
  300 continue
    if (ltest3==0) goto 400
    if (nread(ltest3)/=0) then
       !
       !--------Initialize user defined array's
       !
       call prterr(lundia    ,'V200'    ,'diagnostic mode'    )
       !
       !
       !--------Test for secondary flow in combination with diagnostic mode
       !        not allowed
       !
       if (secflo) then
          call prterr(lundia    ,'V240'    ,' '       )
          !
          error = .true.
          goto 600
       endif
       !
       !--------Define real parameter
       !
       nreal = 0
       do n = 1, nread(ltest3) - 1
          nreal = nreal + nprinp(2, n)
       enddo
       nreal = nreal + 1
       !
       !--------Read user defined diagnostic mode
       !        from user defined function: real parameter
       !
       tdiagm = rcousr(nreal)
       !
       !--------caluculate integer multiples of dt and test calculated values
       !
       itdiag = nint(tdiagm/dt)
       if (dtn(itdiag, tdiagm, dt)) then
          error = .true.
          call prterr(lundia    ,'U044'    ,'Diagnostic mode time'          )
          !
          goto 600
       endif
       !
       !--------test value inside time frame
       !        For ITDIAG = 0 diagnostic mode for computation is presumed
       !
       if (itdiag>itfinish) then
          call prterr(lundia    ,'V241'    ,' '       )
       !
       endif
       !
       if (itdiag<=itstrt) then
          call prterr(lundia    ,'V242'    ,' '       )
       !
       endif
    endif
    !
    !-----User Defined Function: Particle wind factor (YES) ?
    !
  400 continue
    if (ltest4==0) goto 500
    if (nread(ltest4)/=0) then
       !
       !--------Initialize user defined array's
       !
       call prterr(lundia    ,'V200'    ,'particle wind factor'          )
       !
       !
       !--------Define real parameter
       !
       nreal = 0
       do n = 1, nread(ltest4) - 1
          nreal = nreal + nprinp(2, n)
       enddo
       nreal = nreal + 1
       !
       !--------Read user defined particle wind factor
       !        from user defined function: three real parameters
       !
       windsp = rcousr(nreal)
       windd = rcousr(nreal + 1)
       windft = rcousr(nreal + 2)
       !
       !--------test wind factor
       !
       if (windft>1.00) then
          call prterr(lundia    ,'V243'    ,' '       )
          !
          windft = 0.
       endif
       !
       !--------direction is given relative to the north, positive clockwise
       !        the wind is coming from that direction.
       !        angle to east-axis
       !          alpha = 90-windd
       !        wind blows to direction
       !          beta  = 90-windd+180 = 270-windd
       !        extra angle between North and Y ax
       !          angle = 270-windd+grdang
       !
       anglen = (270. - windd + grdang)*degrad
       windxt = windft*windsp*cos(anglen)
       windyt = windft*windsp*sin(anglen)
       !
       !--------reset rcousr and set pointer nreal in nprptr(1,n)
       !
       nprptr(1, ltest4) = nreal
       rcousr(nreal) = windxt
       rcousr(nreal + 1) = windyt
       rcousr(nreal + 2) = windft
    endif
    !
    !-----User Defined Function: z_wave (YES) ?
    !
  500 continue
    if (ltest6==0) goto 600
    if (nread(ltest6)/=0) then
       !
       !--------Initialize user defined array's
       !
       call prterr(lundia    ,'V200'    ,'z_wave'  )
       !
       !
       !--------Define real parameter
       !
       nreal = 0
       do n = 1, nread(ltest6) - 1
          nreal = nreal + nprinp(2, n)
       enddo
       nreal = nreal + 1
       !
       !--------Read user defined z_wave coefficient and define input flag
       !        from user defined function: one real parameters
       !
       zwi = rcousr(nreal)
       inpzw = 1
       !
       !--------test z_wave value
       !
       if (zwi<=0.00) then
          call prterr(lundia    ,'U021'    ,'z_wave will be calculated !'   )
          !
          inpzw = 0
       endif
    endif
    !
    !
  600 continue
end subroutine usrdef
