subroutine inibct(lundia    ,error     ,runid     , &
                & itbct     ,nto       ,ntoftoq   , &
                & kmax      ,kcd       ,nambnd    ,typbnd    ,tprofu    , &
                & hydrbc    ,bubble    ,gdp       )
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
!    Function: Reads the time dependent data from file for the
!              first time
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'pardef.igd'
    integer      , pointer :: itdate
    integer      , pointer :: lunbct
    real(fp)     , pointer :: timscl
    logical      , pointer :: bndneu
!
! Global variables
!
    integer                                            :: kcd     !  Description and declaration in dimens.igs
    integer                                            :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                            :: lundia  !  Description and declaration in inout.igs
    integer                                            :: nto     !  Description and declaration in esm_alloc_int.f90
    integer                              , intent(in)  :: ntoftoq ! Number of open boundary sections before the time series type:
                                                                  ! ntoftoq = ntof + ntoq
    integer      , dimension(5, nto)                   :: itbct   !  Description and declaration in esm_alloc_int.f90
    logical                              , intent(in)  :: bubble  !  Description and declaration in procs.igs
    logical                                            :: error   !!  Flag=TRUE if an error is encountered
    real(fp)     , dimension(4, nto, kcd)              :: hydrbc  !  Description and declaration in esm_alloc_real.f90
    character(*)                                       :: runid
    character(1) , dimension(nto)                      :: typbnd  !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto)                      :: nambnd  !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto)                      :: tprofu  !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                  :: iend
    integer                                  :: ifound
    integer                                  :: iocond
    integer                                  :: irecrd ! Counter of records if input file is a direct access file 
    integer, dimension(nto)                  :: irecs
    integer                                  :: istart
    integer                                  :: itfac  ! Interpolation factor 
    integer                                  :: ito    ! Index number of open boundary loc. 
    integer                                  :: k
    integer                                  :: lrec   ! Record length of direct access file 
    integer                                  :: lrid   ! Length of character string runid 
    integer                                  :: np
    integer                                  :: npara  ! Number of parameter records in time dependent direct access file 
    integer                                  :: nparrd ! NR. of parameter records actual read 
    integer                                  :: ntimrd
    integer                                  :: ntyp   ! Index number of open boundary type and PARNAM 
    integer, external                        :: newlun
    logical                                  :: access ! Flag to read file as direct access or sequential    logical                                  :: first  ! Help var. It is always set to TRUE before calling the relevant routines for the time dependent data, because they are activated here for the first time
    logical                                  :: opend  ! Help flag = TRUE when file is still open (Delft3D) and 
    character(1)                             :: dumchr ! Dummy character (#) in first record of direct access file 
    character(20)                            :: chlp20
    character(20)                            :: cntent ! String with <contence> input
    character(20)                            :: namhlp
    character(256)                           :: filnam ! Help var. for file name 
    character(36)  , dimension(1 + 2*kmax)   :: parnam ! Number of parameter records in time dependent direct access files for BCT 
    character(36)  , dimension(6)            :: defpar ! Default parameter 
    character(6)                             :: typtst ! Data string to test type of boundary 
    character(kmax*24*2 + 48)                :: record ! Record for BCT file 
    !
    data typtst/'ZCQRTN'/
!
!! executable statements -------------------------------------------------------
!
    timscl  => gdp%gdinibct%timscl
    lunbct  => gdp%gdluntmp%lunbct
    itdate  => gdp%gdexttim%itdate
    bndneu  => gdp%gdnumeco%bndneu
    !
    ! Initialize DEFPAR
    !
    defpar(1) = 'water elevation (z)                 '
    defpar(2) = 'current         (c)                 '
    defpar(3) = 'flux/discharge  (q)                 '
    defpar(4) = 'riemann         (r)                 '
    defpar(5) = 'total discharge (t)                 '
    defpar(6) = 'neumann         (n)                 '
    !
    ! Define length of RUNID
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    filnam = 'TMP_' // runid(:lrid) // '.bct'
    !
    ! Test if file is already opened (multi entry Delft3D)
    !
    inquire (file = filnam(:8 + lrid), opened = opend)
    if (.not.opend) then
       open (newunit=lunbct, file = filnam(:8 + lrid), form = 'formatted',              &
            & status = 'old')
       read (lunbct, '(a1,i5)', iostat = iocond) dumchr, lrec
       !
       ! Error or EOF (IOCOND <> 0), not allowed
       !
       if (iocond /= 0) then
          call prterr(lundia    ,'G007'    ,filnam(:8 + lrid)    )
          !
          error = .true.
          goto 9999
       endif
       close (lunbct)
       !
       ! for parallel runs, find locations of open boundaries in current subdomain
       !
       if (parll) then
          !
          irecrd = 0
          !
          ! file not open as direct access!
          !
          open (newunit=lunbct, file = filnam(:8 + lrid), form = 'formatted')
   10     continue
          irecrd = irecrd + 1
          read (lunbct, '(a)', end=20) record(:lrec - 1)
          iend   = len(record)
          istart = 1
          call srckey(record    ,istart    ,iend      ,ifound    ,gdp       )
          if (ifound==3) then
             !
             ! location keyword found
             !
             call keyinp(record(istart:iend)  ,chlp20   )
             call small(chlp20    ,len(chlp20)          )
             do ito = ntoftoq+1, nto
                namhlp = nambnd(ito)
                call small(namhlp    ,len(namhlp)          )
                if ( chlp20 == namhlp ) then
                   irecs(ito) = irecrd - 2    ! correpond to record table-name
                   exit
                endif
             enddo
          endif
          goto 10
   20     close (lunbct)
       endif
       !
       ! Open file as direct access
       !
       open (newunit=lunbct, file = filnam(:8 + lrid), form = 'formatted',              &
            & access = 'direct', recl = lrec)
       !
       ! Initialize ITBCT array
       !
       irecrd = 2
       access = .true.
       !
       do ito = 1, nto
          itbct(1, ito) = -1
          itbct(2, ito) = -1
          itbct(3, ito) = -1
          itbct(4, ito) = -1
          itbct(5, ito) = -1
       enddo
    endif
    !
    ! Loop over ntoftoq+1>NTO open boundary points
    !
    do ito = ntoftoq + 1, nto
       !
       if (parll) irecrd = irecs(ito)
       !
       ! Always read hydrbc values from file
       !
       itbct(1, ito) = -1
       itbct(2, ito) = -1
       if (itbct(3, ito) == -1) then
          !
          ! Read initial values from file
          ! IRECRD = 2 as input and start of time varying data as
          ! output
          !
          read (lunbct, '(a)', rec = irecrd) record(:lrec - 1)
          npara = 1 + 2*kmax
          dumchr = 'Y'
          call flhnew(lunbct    ,lundia    ,error     ,record(:lrec - 1)    ,access    , &
                    & irecrd    ,nambnd(ito)          ,cntent    ,dumchr    ,itdate    , &
                    & timscl    ,ntimrd    ,parnam    ,npara     ,nparrd    , &
                    & bubble    ,gdp       )
          if (error) then
             exit
          endif
          !
          ! Check TPROFU
          !
          if (cntent /= tprofu(ito)) then
             error = .true.
             call prterr(lundia    ,'V096'    ,cntent    )
             exit
          endif
          !
          ! Test number of parameters read incombination with defined
          !
          if (tprofu(ito)(:10) == '3d-profile') then
             if (nparrd /= 1+2*kmax) then
                call prterr(lundia    ,'V097'    ,' '       )
                error = .true.
                exit
             endif
          elseif (nparrd /= 3) then
             call prterr(lundia    ,'V097'    ,' '       )
             error = .true.
             exit
          else
          endif
          !
          ! Test name of parameter def. conform read parameter name
          ! Only first 20 characters are of significance
          !
          ntyp = index(typtst, typbnd(ito))
          do np = 2, nparrd
             if (parnam(np)(:20) /= defpar(ntyp)(:20)) then
                call prterr(lundia    ,'V096'    ,parnam(np)(:20)      )
                error = .true.
                goto 9999
             endif
          enddo
          if (typbnd(ito) == 'R' .and. bndneu) then
             typbnd(ito) = 'N'
          endif
          !
          ! Define ITBCT (3/4,ITO) record values
          !
          itbct(3, ito) = irecrd
          itbct(4, ito) = irecrd + ntimrd - 1
          !
          ! Define start record for next information records
          !
          irecrd = irecrd + ntimrd
       endif
       itbct(5, ito) = itbct(3, ito) - 1
    enddo
    !
    ! lunbct is not used anymore after this point
    ! The creation and usage of TMP_<runid>.bct should be removed
    ! Basicly it is replaced using the table module
    ! Now it is still only used for some minor things
    !
    close(lunbct)
    !
    ! Read first time dep. input
    !
    if (nto > ntoftoq) then
       call updbct(lundia, filnam, ntoftoq, nto, kcd, kmax, hydrbc, tprofu, error, gdp)
    endif
 9999 continue
end subroutine inibct
