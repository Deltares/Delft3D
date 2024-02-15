subroutine rdspec(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & yestdd    ,filsrc    ,fmtsrc    ,nsrc      ,mmax      , &
                & nmax      ,nmaxus    ,mnksrc    ,namsrc    ,disint    , &
                & upwsrc    ,gdp       )
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
!    Function: Reads the records from the MD-file related to dis- 
!              charge param. (only if NSRC>0) : MNKDIS & DISINT 
! Method used: 
! 
!!--pseudo code and references-------------------------------------------------- 
! NONE 
!!--declarations---------------------------------------------------------------- 
    use precision 
    use globaldata 
    use dfparall 
    ! 
    implicit none 
    ! 
    type(globdat),target :: gdp 
    ! 
    ! The following list of pointer parameters is used to point inside the gdp structure 
    ! 
    integer, pointer       :: itis 
    integer, pointer       :: mfg 
    integer, pointer       :: nfg 
    integer, pointer       :: mlg 
    integer, pointer       :: nlg 
! 
! Global variables 
! 
    integer                             , intent(in)  :: lundia !  Description and declaration in inout.igs 
    integer                             , intent(in)  :: lunmd  !  Description and declaration in inout.igs 
    integer                             , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90 
    integer                             , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90 
    integer                             , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90 
    integer                                           :: nrrec  !  Pointer to the record number in the MD-file 
    integer                                           :: nsrc   !  Description and declaration in esm_alloc_int.f90 
    integer                             , intent(out) :: upwsrc !  Description and declaration in esm_alloc_int.f90 
    integer      , dimension(7, nsrc)   , intent(out) :: mnksrc !  Description and declaration in esm_alloc_int.f90 
    logical                             , intent(out) :: error  !  Flag=TRUE if an error is encountered 
    logical                             , intent(in)  :: yestdd !  Flag for call from TDATOM (.true.) for time varying data 
    character(*)                        , intent(out) :: filsrc !  File name for the discharge location definition file 
    character(*)                                      :: mdfrec !  Standard rec. length in MD-file (300) 
    character(1) , dimension(nsrc)                    :: disint !  Description and declaration in esm_alloc_char.f90 
    character(2)                        , intent(out) :: fmtsrc !  File format for the discharge location definition file 
    character(20), dimension(nsrc)      , intent(out) :: namsrc !  Description and declaration in esm_alloc_char.f90 
! 
! 
! Local variables 
! 
    integer                        :: i 
    integer                        :: idef     ! Help var. containing default va- lue(s) for integer variable  
    integer                        :: j 
    integer                        :: lenc     ! Help var. (length of var. cvar to be looked for in the MD-file)  
    integer                        :: lkw 
    integer                        :: m1       ! Help var.  
    integer                        :: m2       ! m-index of outfall point 
    integer                        :: m        ! Help var. 
    integer                        :: md 
    integer                        :: mfl      ! first m-index of this local partition, excluding the halo
    integer                        :: mll      ! last  m-index of this local partition, excluding the halo
    integer                        :: nfl      ! first n-index of this local partition, excluding the halo
    integer                        :: nll      ! last  n-index of this local partition, excluding the halo
    integer                        :: n1       ! Help var.  
    integer                        :: n2       ! n-index of outfall point 
    integer                        :: n        ! Help var. 
    integer                        :: nd 
    integer                        :: nlook    ! Help var.: nr. of data to look for in the MD-file  
    integer                        :: nr       ! Loop var. for NSRC  
    integer                        :: ntrec    ! Help. var to keep track of NRREC  
    integer       , dimension(3)   :: ival     ! Help array (int.) where the data, recently read from the MD-file, are stored temporarily  
    logical                        :: defaul   ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook)  
    logical                        :: found    ! FOUND=TRUE if KEYW in the MD-file was found  
    logical                        :: lerror   ! Flag=TRUE if a local error is encountered  
    logical                        :: newkw    ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line  
    logical                        :: nodef    ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook)  
    character(12)                  :: fildef   ! Default file name (usually = blank)  
    character(20)                  :: cdef     ! Help var.  
    character(20)                  :: chulp    ! Help var.  
    character(6)                   :: keyw     ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM)  
    character(100)                 :: txtput 
    character(300)                 :: message
! 
!! executable statements ------------------------------------------------------- 
! 
    itis  => gdp%gdrdpara%itis 
    ! 
    mfg   => gdp%gdparall%mfg 
    mlg   => gdp%gdparall%mlg 
    nfg   => gdp%gdparall%nfg 
    nlg   => gdp%gdparall%nlg 
    ! 
    lerror = .false. 
    newkw  = .true. 
    defaul = .true. 
    nodef  = .not.defaul 
    fildef = ' ' 
    idef   = 0 
    ! 
    ival   = 0 
    ! 
    ! Initialize parameters that are to be read 
    ! 
    filsrc = ' ' 
    fmtsrc = 'FR' 
    do n = 1, nsrc 
       do j = 1, 7 
          mnksrc(j, n) = 0 
       enddo 
       disint(n) = 'Y' 
       namsrc(n) = ' ' 
    enddo 
    upwsrc = 0 
    ! 
    ! Read info of discharge locations from attribute file or md-file 
    ! 
    keyw  = 'Filsrc' 
    ntrec = nrrec 
    nlook = 1 
    lkw   = 6 
    lenc  = 12 
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , & 
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , & 
              & 'NO'      ) 
    ! 
    ! Keyword in md-file (FOUND) then read (default value allowed) 
    ! 
    if (found) then 
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , & 
                 & mdfrec    ,filsrc    ,fildef    ,lenc      ,nrrec     , & 
                 & ntrec     ,lundia    ,gdp       ) 
       ! 
       if (lerror) then 
          error = .true. 
          goto 9999 
       endif 
    else 
       filsrc = fildef 
    endif 
    ! 
    ! Discharge location definitions in file? <YES> 
    ! 
    if (filsrc /= fildef) then 
       fmtsrc = 'FR' 
       ! 
       ! Read flag for Upwind 
       ! 
       keyw  = 'Upwsrc' 
       ntrec = nrrec 
       nlook = 1 
       lkw   = 6 
       lenc  = 1 
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , & 
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , & 
                 & 'NO'      ) 
       ! 
       ! Keyword in md-file (FOUND) then read (default = Momentum only) 
       ! 
       txtput = 'Upwind advection scheme only near momentum discharges' 
       if (found) then 
          cdef  = ' ' 
          chulp = cdef 
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , & 
                    & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , & 
                    & ntrec     ,lundia    ,gdp       ) 
          ! 
          if (lerror) then 
             error = .true. 
             goto 9999 
          else              
             call small(chulp ,20         ) 
             if (chulp(:1) == 'y') then 
                upwsrc =  1 
                txtput = 'Upwind advection scheme near all discharges' 
             elseif (chulp(:1) == 'n') then 
                upwsrc = -1 
                txtput = 'No upwind advection scheme for all discharges' 
             elseif (chulp(:13) == 'momentum only') then 
                upwsrc = 0 
             else 
                write(message,'(a,a)') 'UPWSRC should be [Y/N/Momentum only], but found: ', trim(chulp) 
                call prterr(lundia    ,'P004'    ,trim(message)      )
                error = .true. 
                goto 9999 
             endif 
              
          endif 
       endif 
       call prterr(lundia    ,'G051'    ,txtput      )
       ! 
       ! Read discharge location definitions from file.
       ! Stop if reading error occurred or file did not exist (error  = .true.) 
       ! 
       call srcfil(lundia    ,filsrc    ,error     ,nsrc      ,mnksrc    , & 
                 & namsrc    ,disint    ,gdp       ) 
       if (error) goto 9999 

    elseif (nsrc > 0) then 
       ! 
       ! Discharge location definitions in file? <NO> and NSRC > 0 
       ! Start from top and read a record first, because NEWKW = .true. 
       ! 
       rewind (lunmd) 
       read (lunmd, '(a300)') mdfrec 
       ! 
       nrrec = 1 
       ntrec = nrrec 
       ! 
       do n = 1, nsrc 
          ! 
          ! Locate 'Namdis' record for name of discharge source 
          ! Read namdis from record there must be a name defined 
          ! 
          keyw  = 'Namdis' 
          nlook = 1 
          cdef  = ' ' 
          chulp = cdef 
          lenc  = 20 
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , & 
                    & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , & 
                    & ntrec     ,lundia    ,gdp       ) 
          ! 
          ! Reading error? 
          ! 
          if (lerror) then 
             error = .true. 
             lerror = .false. 
          else 
             namsrc(n) = chulp 
             if (namsrc(n) == cdef) then 
                error = .true. 
                call prterr(lundia, 'V012', ' ')
             endif 
          endif 
          ! 
          ! Read 'Disint' record for interpolation option Y/N 
          ! If error (no # found) then old md-file => default Y 
          ! 
          keyw  = 'Disint' 
          nlook = 0 
          cdef  = 'Y' 
          chulp = cdef 
          lenc  = 1 
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , & 
                    & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , & 
                    & ntrec     ,lundia    ,gdp       ) 
          ! 
          ! Reading error? 
          ! 
          if (lerror) then 
             disint(n) = cdef(:1) 
             lerror = .false. 
             exit 
          else 
             disint(n) = chulp(:1) 
          endif 
          ! 
          ! Test for interpolation option is 'Y' 
          ! 
          if (disint(n) == 'n') disint(n) = 'N' 
          if (disint(n) /= 'N') disint(n) = cdef(:1) 
          ! 
          ! Locate and read 'MNKdis' record for coordinates of discharge 
          ! Default value not allowed => nodef 
          ! 
          keyw  = 'MNKdis' 
          nlook = 3 
          call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , & 
                    & mdfrec    ,ival      ,idef      ,nodef     ,nrrec     , & 
                    & ntrec     ,lundia    ,gdp       ) 
          ! 
          ! Reading error?  
          ! 
          if (lerror) then 
             lerror = .false. 
             exit 
          else 
             mnksrc(1, n) = ival(1) 
             mnksrc(2, n) = ival(2) 
             mnksrc(3, n) = ival(3) 
             mnksrc(4, n) = ival(1) 
             mnksrc(5, n) = ival(2) 
             mnksrc(6, n) = ival(3) 
          endif 
       enddo 
       ! 
       ! Stop reading 
       !
 
    else 
    endif 
    ! 
    ! Not twice the same name 
    ! 
    do nr = 1, nsrc 
       do n = 1, nr - 1 
          if (namsrc(n) == namsrc(nr)) then 
             error = .true. 
             call prterr(lundia, 'U173', namsrc(nr))
          endif 
       enddo 
    enddo 
    ! 
    ! for parallel runs, determine which discharge points are inside subdomain (excluding the halo) and store them 
    ! Note this routine is also called from tdatom, so subdomains are not yet defined in that case. Therefore check on mfg 
    ! also.
    !
    if ( parll .and. mfg .gt. 0 ) then
       if (idir == 1) then
          !
          ! n direction is split
          !
          mfl = 1
          mll = gdp%d%mmax
          if (nfg == 1) then
             !
             ! first part; no halo in front of nfl
             !
             nfl = 1
          else
             !
             ! exclude halo in front of nfl
             !
             nfl = 1  + ihalon
          endif
          if (nlg == gdp%gdparall%nmaxgl) then
             !
             ! last part; no halo behind nll
             !
             nll = gdp%d%nmaxus
          else
             !
             ! exclude halo behind nll
             !
             nll = gdp%d%nmaxus - ihalon
          endif
       elseif (idir == 2) then
          !
          ! m direction is split
          !
          nfl = 1
          nll = gdp%d%nmaxus
          if (mfg == 1) then
             !
             ! first part; no halo in front of mfl
             !
             mfl = 1
          else
             !
             ! exclude halo in front of mfl
             !
             mfl = 1 + ihalom
          endif
          if (mlg == gdp%gdparall%mmaxgl) then
             !
             ! last part; no halo behind mll
             !
             mll = gdp%d%mmax
          else
             !
             ! exclude halo behind mll
             !
             mll = gdp%d%mmax - ihalom
          endif
       endif


       do n = 1, nsrc 
          m1 = mnksrc(1, n) -mfg +1 
          n1 = mnksrc(2, n) -nfg +1 
          m2 = mnksrc(4, n) -mfg +1 
          n2 = mnksrc(5, n) -nfg +1 

          mnksrc(1, n) = m1 
          mnksrc(2, n) = n1 
          mnksrc(4, n) = m2 
          mnksrc(5, n) = n2 

          ! 
          ! if inlet is inside and outfall is outside partition (or the other way around): stop with an errormessage
          !
          if (      (     (mfl<=m1 .and. m1<=mll .and. nfl<=n1 .and. n1<=nll)       &
                     .and.(m2<mfl  .or.  mll<m2  .or.  n2<nfl  .or.  nll<n2 )  )    &
               .or. (     (mfl<=m2 .and. m2<=mll .and. nfl<=n2 .and. n2<=nll)       &
                     .and.(m1<mfl  .or.  mll<m1  .or.  n1<nfl  .or.  nll<n1 )  ) ) then 
             write (message,'(a,a,a)') 'Inlet and outfall of discharge "',trim(namsrc(n)),'" are in different partitions' 
             call prterr( lundia, 'P004', trim(message))
             error = .true.
             goto 9999
          elseif ( min(m1,m2) < mfl .or. min(n1,n2) < nfl .or. max(m1,m2) > mll .or. max(n1,n2) > nll ) then 
             ! 
             ! if inlet or outfall is outside partition, then they are both outside this partition:
             ! remove it from this partition by setting k to -1 en continue
             ! 
             write(message,'(a,3(i0,a))') 'Discharge (m,n,k)=(', mnksrc(1, n) +mfg -1, &
                ',', mnksrc(2, n)+nfg-1, ',', mnksrc(3, n), &
                ') is disabled: inlet and/or outfall not in this partition'
             call prterr( lundia, 'U190', trim(message))
             mnksrc(3,n) = -1
             mnksrc(6,n) = -1
          endif
          if (mnksrc(7,n) == 1) then
             !
             ! parallel and walking discharge disabled: what if the discharge walks outside partition?
             !
             write (message,'(a,a,a)') 'Discharge "',trim(namsrc(n)),'" is a walking discharge and is not supported when running parallel.' 
             call prterr( lundia, 'P004', trim(message))
             error = .true.
             goto 9999
          endif
          if (mnksrc(7,n)==3 .or. mnksrc(7,n)==4 .or. mnksrc(7,n)==5 .or. mnksrc(7,n)==7) then
             !
             ! parallel and culvert disabled: rdcul and culver must be adapted to support a culvert with inlet and outfall in the same partition
             !
             write (message,'(a,a,a)') 'Discharge "',trim(namsrc(n)),'" is a culvert and is not supported when running parallel.' 
             call prterr( lundia, 'P004', trim(message))
             error = .true.
             goto 9999
          endif
       enddo 
    endif 
    if (error) goto 9999 
    ! 
    ! Test and fill KSPU/V(nm,0) array is moved to INIDIS 
    ! 
 9999 continue 
end subroutine rdspec 
