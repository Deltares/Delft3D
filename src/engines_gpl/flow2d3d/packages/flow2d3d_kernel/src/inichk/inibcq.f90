subroutine inibcq(lundia    ,error     ,runid     ,itbct     ,nto       , &
                & ntof      ,ntoq      ,kcd       ,nambnd    ,hydrbc    , &
                & bubble    ,kmax      ,gdp       )
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
!    Function: Reads the QH relations from file for the
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
    integer , pointer :: lunbcq
!
! Global variables
!
    integer                              , intent(in)  :: kcd    !  Description and declaration in dimens.igs
    integer                                            :: lundia !  Description and declaration in inout.igs
    integer                              , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                              , intent(in)  :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                              , intent(in)  :: ntof   !  Description and declaration in dimens.igs
    integer                              , intent(in)  :: ntoq   !  Description and declaration in dimens.igs
    integer      , dimension(5, nto)     , intent(out) :: itbct  !  Description and declaration in esm_alloc_int.f90
    logical                              , intent(in)  :: bubble !  Description and declaration in procs.igs    
    logical                                            :: error
    real(fp)     , dimension(4, nto, kcd), intent(out) :: hydrbc !  Description and declaration in esm_alloc_real.f90
    character(*)                                       :: runid
    character(20), dimension(nto)                      :: nambnd !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                :: idummy
    integer                                :: iend
    integer                                :: ifound
    integer                                :: iocond
    integer                                :: irecrd  ! Counter of records if input file is a direct access file 
    integer, dimension(nto)                :: irecs
    integer                                :: istart
    integer                                :: ito     ! Index number of open boundary loc. 
    integer                                :: lrec    ! Record length of direct access file 
    integer                                :: lrid    ! Length of character string runid 
    integer                                :: np
    integer                                :: npara   ! Number of parameter records in time dependent direct access file 
    integer                                :: nparrd  ! NR. of parameter records actual read 
    integer                                :: nqhrd
    integer, external                      :: newlun
    logical                                :: access  ! Flag to read file as direct access or sequential 
    logical                                :: opend   ! Help flag = TRUE when file is still open (Delft3D) and 
    real(fp)                               :: tdummy
    character(1)                           :: dumchr  ! Dummy character (#) in first record of direct access file 
    character(20)                          :: cntent  ! String with <contents> input 
    character(20)                          :: chlp20
    character(20)                          :: namhlp
    character(256)                         :: filnam  ! Help var. for file name 
    character(36), dimension(1 + 2*kmax)   :: parnam  ! Number of parameter records in time dependent direct access files for BCT 
    character(36), dimension(2)            :: defpar  ! Default parameter 
    character(500)                         :: record  ! Record for BCT file 
!
!! executable statements -------------------------------------------------------
!
    lunbcq  => gdp%gdluntmp%lunbcq
    !
    defpar(1) = 'total discharge (t)                 '
    defpar(2) = 'water elevation (z)                 '
    !
    ! Define length of RUNID
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    filnam = 'TMP_' // runid(:lrid) // '.bcq'
    !
    ! Test if file is already opened (multi entry Delft3D)
    !
    inquire (file = filnam(:8 + lrid), opened = opend)
    if (.not.opend) then
       open (newunit=lunbcq, file = filnam(:8 + lrid), form = 'formatted',              &
            & status = 'old')
       read (lunbcq, '(a1,i5)', iostat = iocond) dumchr, lrec
       !
       ! error or EOF (IOCOND <> 0), not allowed
       !
       if (iocond/=0) then
          call prterr(lundia    ,'G007'    ,filnam(:8 + lrid)    )
          !
          error = .true.
          goto 9999
       endif
       close (lunbcq)
       !
       ! for parallel runs, find locations of open boundaries in current subdomain
       !
       if (parll) then
          !
          irecrd = 0
          !
          ! file not open as direct access!
          !
          open (newunit=lunbcq, file = filnam(:8 + lrid), form = 'formatted')
   10     continue
          irecrd = irecrd + 1
          read (lunbcq, '(a)', end=20) record(:lrec - 1)
          iend   = len(record)
          istart = 1
          call srckey(record    ,istart    ,iend      ,ifound    ,gdp       )
          if (ifound==3) then
             !
             ! location keyword found
             !
             call keyinp(record(istart:iend)  ,chlp20   )
             call small(chlp20    ,len(chlp20)          )
             do ito = ntof + 1, ntof + ntoq
                namhlp = nambnd(ito)
                call small(namhlp    ,len(namhlp)          )
                if ( chlp20 == namhlp ) then
                   irecs(ito) = irecrd - 2    ! correpond to record table-name
                   exit
                endif
             enddo
          endif
          goto 10
   20     close (lunbcq)
       endif
       !
       ! Open file as direct access
       !
       open (newunit=lunbcq, file = filnam(:8 + lrid), form = 'formatted',              &
            & access = 'direct', recl = lrec)
       !
       ! Initialize ITBCT array
       !
       irecrd = 2
       access = .true.
    endif
    !
    ! Loop over NTOF+1>NTOF+NTOQ open boundaries qith QH rel.
    !
    do ito = ntof + 1, ntof + ntoq
       !
       ! Read initial values from file
       ! IRECRD = 2 as input and start of time varying data as
       ! output
       !
       read (lunbcq, '(a)', rec = irecrd) record(:lrec - 1)
       npara = 2
       dumchr = 'Y'
       !
       call flhnew(lunbcq    ,lundia    ,error     ,record(:lrec - 1)    ,access    , &
                 & irecrd    ,nambnd(ito)          ,cntent    ,dumchr    ,idummy    , &
                 & tdummy    ,nqhrd     ,parnam    ,npara     ,nparrd    , &
                 & bubble    ,gdp       )
       if (error) then
          exit
       endif
       !
       ! Test number of parameters read
       !
       if (nparrd/=npara) then
          call prterr(lundia    ,'V097'    ,' '       )
          error = .true.
          exit
       endif
       !
       ! Test name of parameter def. conform read parameter name
       ! Only first 20 characters are of significance
       !
       do np = 1, nparrd
          if (parnam(np)(1:20)/=defpar(np)(1:20)) then
             call prterr(lundia    ,'V096'    ,parnam(np)(1:20)     )
             error = .true.
             goto 9999
          endif
       enddo
       !
       ! Define ITBCT (:,ITO) record values
       !
       itbct(1, ito) = 0
       itbct(2, ito) = 0
       itbct(3, ito) = irecrd
       itbct(4, ito) = irecrd + nqhrd - 1
       itbct(5, ito) = -1
       hydrbc(1, ito, 1) = 0
       hydrbc(2, ito, 1) = 0
       hydrbc(3, ito, 1) = 0
       hydrbc(4, ito, 1) = 0
       !
       ! Define start record for next information records
       !
       irecrd = irecrd + nqhrd
    enddo
 9999 continue
end subroutine inibcq
