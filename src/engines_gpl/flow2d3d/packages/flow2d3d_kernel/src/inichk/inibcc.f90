subroutine inibcc(lundia    ,error     ,runid     ,timnow    , &
                & itbcc     ,itstrt    ,itfinish  ,nto       ,lstsc     , &
                & kmax      ,nambnd    ,namcon    ,tprofc    ,procbc    , &
                & zstep     ,bubble    ,gdp       )
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
    integer  , pointer :: itdate
    real(fp) , pointer :: tstop
    integer  , pointer :: lunbcc
    real(fp) , pointer :: timscl
!
! Global variables
!
    integer                                       :: itfinish !  Description and declaration in inttim.igs
    integer                                       :: itstrt   !  Description and declaration in inttim.igs
    integer                                       :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                                       :: lstsc    !  Description and declaration in dimens.igs
    integer                                       :: lundia   !  Description and declaration in inout.igs
    integer                                       :: nto      !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(5, nto, lstsc)       :: itbcc    !  Description and declaration in esm_alloc_int.f90
    logical                         , intent(in)  :: bubble   !  Description and declaration in procs.igs
    logical                                       :: error    !!  Flag=TRUE if an error is encountered
    real(fp)                                      :: timnow   !!  Current timestep (ITSTRT * dt)
    real(fp)     , dimension(2, nto, lstsc)       :: zstep    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(4, nto, kmax, lstsc) :: procbc   !  Description and declaration in esm_alloc_real.f90
    character(*)                                  :: runid
    character(10), dimension(nto, lstsc)          :: tprofc   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(lstsc) , intent(in)  :: namcon   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto)                 :: nambnd   !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer, dimension(nto)                :: inambnd  ! to check if boundary name has already been found in parallel code
    integer                                :: iend
    integer                                :: ifound
    integer                                :: iocond
    integer                                :: irecrd  ! Counter of records if input file is a direct access file 
    integer, dimension(nto)                :: irecs
    integer                                :: istsc   ! Index number of constituent 
    integer                                :: istart
    integer                                :: itfac   ! Interpolation factor 
    integer                                :: ito     ! Index number of open boundary loc. 
    integer                                :: k       ! Loop counter over KMAX 
    integer                                :: lrec    ! Record length of direct access file 
    integer                                :: lrid    ! Length of character string runid 
    integer                                :: newlun
    integer                                :: np
    integer                                :: npara   ! Number of parameter records in time dependent direct access file 
    integer                                :: nparrd  ! NR. of parameter records actual read 
    integer                                :: npconc  ! NR. of parameter records actual read for constituents only 
    integer                                :: ntimrd
    logical                                :: access  ! Flag to read file as direct access or sequential 
    logical                                :: first   ! Help var. It is always set to TRUE before calling the relevant routines for the time dependent data, because they are activated here for the first time 
    logical                                :: opend   ! Help flag = TRUE when file is still open (Delft3D) and 
    character(1)                           :: dumchr  ! Dummy character (#) in first record of direct access file 
    character(20)                          :: chlp20
    character(20)                          :: namhlp  ! Name of NAMCON(ISTSC) in small characters 
    character(256)                         :: filnam  ! Help var. for file name 
    character(36), dimension(1 + 2*kmax)   :: parnam  ! Number of parameter records in time dependent direct access files for BCC 
    character(kmax*24*2+48)                :: record  ! Record for BCC file (maximum length kmax*24*2+48, see also rdtdcn.f90)
!
!! executable statements -------------------------------------------------------
!
    timscl  => gdp%gdinibcc%timscl
    lunbcc  => gdp%gdluntmp%lunbcc
    itdate  => gdp%gdexttim%itdate
    tstop   => gdp%gdexttim%tstop
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    filnam = 'TMP_' // runid(:lrid) // '.bcc'
    !
    ! Test if file is already opened (multi entry Delft3D)
    !
    inquire (file = filnam(:8 + lrid), opened = opend)
    if (.not.opend) then
       open (newunit=lunbcc, file = filnam(:8 + lrid), form = 'formatted',              &
            & status = 'old')
       read (lunbcc, '(a1,i5)', iostat = iocond) dumchr, lrec
       !
       ! error or EOF (IOCOND <> 0), not allowed
       !
       if (iocond/=0) then
          call prterr(lundia    ,'G007'    ,filnam(:8 + lrid)    )
          !
          error = .true.
          goto 9999
       endif
       close (lunbcc)
       !
       ! for parallel runs, find locations of open boundaries in current subdomain
       !
       if (parll) then
          !
          irecrd  = 0
          inambnd = 0
          !
          ! file not open as direct access!
          !
          open (newunit=lunbcc, file = filnam(:8 + lrid), form = 'formatted')
   10     continue
          irecrd = irecrd + 1
          read (lunbcc, '(a)', end=20) record(:lrec - 1)
          iend   = len(record)
          istart = 1
          call srckey(record    ,istart    ,iend      ,ifound    ,gdp       )
          !
          if (ifound==3) then
             !
             ! location keyword found
             !
             call keyinp(record(istart:iend)  ,chlp20   )
             call small(chlp20    ,len(chlp20)          )
             do ito = 1, nto
                namhlp = nambnd(ito)
                call small(namhlp    ,len(namhlp)          )
                if ( (chlp20 == namhlp) .and. (inambnd(ito) /= 1) ) then
                   !
                   ! correpond to record table-name
                   !
                   irecs(ito) = irecrd - 2
                   inambnd(ito) = 1
                   exit
                endif
             enddo
          endif
          goto 10
   20     close (lunbcc)
       endif
       !
       ! Open file as direct access
       !
       open (newunit=lunbcc, file = filnam(:8 + lrid), form = 'formatted',              &
            & access = 'direct', recl = lrec)
       !
       ! Initialize ITBCC array
       !
       irecrd = 2
       access = .true.
       !
       do ito = 1, nto
          do istsc = 1, lstsc
             itbcc(1, ito, istsc) = -1
             itbcc(2, ito, istsc) = -1
             itbcc(3, ito, istsc) = -1
             itbcc(4, ito, istsc) = -1
             itbcc(5, ito, istsc) = -1
          enddo
       enddo
    endif
    !
    ! Loop over NTO open boundaries and LSTSC constituents
    !
    do ito = 1, nto
       !
       if (parll) then
          irecrd = irecs(ito)
       endif
       do istsc = 1, lstsc
          !
          ! Always read procbc values from file
          !
          itbcc(1, ito, istsc) = -1
          itbcc(2, ito, istsc) = -1
          if (itbcc(3, ito, istsc)== - 1) then
             !
             ! Read initial values from file
             ! IRECRD = 2 as input and start of time varying data as output
             !
             read (lunbcc, '(a)', rec = irecrd) record(:lrec - 1)
             npara = max(6, 1 + 2*kmax)
             dumchr = 'Y'
             !
             call flhnew(lunbcc    ,lundia    ,error     ,record(:lrec - 1)    ,access    , &
                       & irecrd    ,nambnd(ito)          ,tprofc(ito, istsc)   ,dumchr    ,itdate    , &
                       & timscl    ,ntimrd    ,parnam    ,npara     ,nparrd    , &
                       & bubble    ,gdp       )
             if (error) goto 9999
             !
             ! Test number of parameters read incombination with defined
             !
             if (tprofc(ito, istsc)(:7)=='uniform') then
                if (nparrd/=3) then
                   call prterr(lundia    ,'V097'    ,' '       )
                   error = .true.
                   goto 9999
                endif
             elseif (tprofc(ito, istsc)(:6)=='linear') then
                if (nparrd/=5) then
                   call prterr(lundia    ,'V097'    ,' '       )
                   error = .true.
                   goto 9999
                endif
             elseif (tprofc(ito, istsc)(:4)=='step') then
                if (nparrd/=6) then
                   call prterr(lundia    ,'V097'    ,' '       )
                   error = .true.
                   goto 9999
                endif
             elseif (tprofc(ito, istsc)=='3d-profile') then
                if (nparrd/=1 + 2*kmax) then
                   call prterr(lundia    ,'V097'    ,' '       )
                   error = .true.
                   goto 9999
                endif
             else
             endif
             !
             ! Test name of constituent conform read parameter name
             ! Only for the constituent names, so for step last parameter
             ! should be tested for 'dicontinuity' and only first 20 char.
             ! for constituents are of significance
             !
             npconc = nparrd
             if (tprofc(ito, istsc)(:4)=='step') npconc = nparrd - 1
             namhlp = namcon(istsc)
             call small(namhlp    ,20        )
             do np = 2, npconc
                if (parnam(np)(:20)/=namhlp) then
                   call prterr(lundia    ,'V096'    ,parnam(np)(:20)      )
                   !
                   error = .true.
                   goto 9999
                endif
             enddo
             if (tprofc(ito, istsc)(:4)=='step') then
                if (parnam(nparrd)(:13)/='discontinuity') then
                   call prterr(lundia    ,'V096'    ,parnam(nparrd)       )
                   !
                   error = .true.
                   goto 9999
                endif
             endif
             !
             ! Define ITBCC (3/4,ITO,ISTSC) record values
             !
             itbcc(3, ito, istsc) = irecrd
             itbcc(4, ito, istsc) = irecrd + ntimrd - 1
             !
             ! Define start record for next information records
             !
             irecrd = irecrd + ntimrd
          endif
          itbcc(5, ito, istsc) = itbcc(3, ito, istsc) - 1
          !
          ! Read first time dep. input
          !
          first = .true.
          call updbcc(lunbcc    ,lundia    ,first     ,itbcc     ,ito       , &
                    & istsc     ,timnow    ,itfinish  ,timscl    , &
                    & nto       ,kmax      ,lstsc     ,procbc    ,tprofc    , &
                    & zstep     ,gdp       )
          !
          ! Interpolate between ITBCC (1,ITO,ISTSC) and TIMNOW
          !
          itfac = (int(timnow) - itbcc(1, ito, istsc))*2
          do k = 1, kmax
             procbc(1, ito, k, istsc) = procbc(1, ito, k, istsc)                &
                                      & + procbc(3, ito, k, istsc)*itfac
             procbc(2, ito, k, istsc) = procbc(2, ito, k, istsc)                &
                                      & + procbc(4, ito, k, istsc)*itfac
          enddo
       enddo
    enddo
 9999 continue
end subroutine inibcc
