subroutine decarr(lunmd     ,lundia    ,error     ,runid     , &
                & prgnm     ,gdp       )
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
!    Function: - Initialises parameters/variables
!              - Reads the processes from the MD-file
!              - Reads various dimensions from MD-file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character(256)   , pointer :: nflmod
    integer          , pointer :: nmax
    integer          , pointer :: mmax
    integer          , pointer :: kmax
    integer          , pointer :: nmaxd
    integer          , pointer :: mmaxd
    integer          , pointer :: lmax
    integer          , pointer :: lmaxd
    integer          , pointer :: nlcest
    integer          , pointer :: kc
    integer          , pointer :: kcd
    integer          , pointer :: nopest
    logical          , pointer :: wind
    logical          , pointer :: salin
    logical          , pointer :: temp
    logical          , pointer :: const
    logical          , pointer :: culvert
    logical          , pointer :: dredge
    logical          , pointer :: drogue
    logical          , pointer :: wave
    integer          , pointer :: waveol
    logical          , pointer :: threed
    logical          , pointer :: secflo
    logical          , pointer :: iweflg
    logical          , pointer :: struct
    logical          , pointer :: cdwstruct
    logical          , pointer :: sedim
    logical          , pointer :: htur2d
    logical          , pointer :: flmd2l
    logical          , pointer :: mudlay
    logical          , pointer :: zmodel
    logical          , pointer :: nonhyd
    logical          , pointer :: roller
    logical          , pointer :: wavcmp
    logical          , pointer :: cnstwv
    logical          , pointer :: veg3d
    logical          , pointer :: snelli
    logical          , pointer :: lrdamp
    logical          , pointer :: sbkol
    logical          , pointer :: bubble
    logical          , pointer :: nfl
    logical          , pointer :: lfsdu
    logical          , pointer :: lfsdus1
!
! Global variables
!
    integer         :: lundia      !  Description and declaration in inout.igs
    integer         :: lunmd       !  Description and declaration in inout.igs
    logical         :: error       !!  Flag=TRUE if an error is encountered
    character(*)    :: runid       !!  Run identification code for the cur-
                                   !!  rent simulation (used to determine
                                   !!  the names of the in- /output files
                                   !!  used by the system)
    character(6)    :: prgnm       !!  Help var. determining the prog. name
                                   !!  currently active
!
! Local variables
!
    integer      :: nrver  ! Integer representative of versio 
    logical      :: lerror ! Flag=TRUE if an error is encountered local in this subroutine to test ipsize, rpsize and cpsize in one effort 
!
!! executable statements -------------------------------------------------------
!
    nflmod     => gdp%gdnfl%nflmod
    wind       => gdp%gdprocs%wind
    salin      => gdp%gdprocs%salin
    temp       => gdp%gdprocs%temp
    const      => gdp%gdprocs%const
    culvert    => gdp%gdprocs%culvert
    dredge     => gdp%gdprocs%dredge
    drogue     => gdp%gdprocs%drogue
    lfsdu      => gdp%gdprocs%lfsdu
    lfsdus1    => gdp%gdprocs%lfsdus1
    wave       => gdp%gdprocs%wave
    waveol     => gdp%gdprocs%waveol
    threed     => gdp%gdprocs%threed
    secflo     => gdp%gdprocs%secflo
    iweflg     => gdp%gdprocs%iweflg
    struct     => gdp%gdprocs%struct
    cdwstruct  => gdp%gdprocs%cdwstruct
    sedim      => gdp%gdprocs%sedim
    htur2d     => gdp%gdprocs%htur2d
    flmd2l     => gdp%gdprocs%flmd2l
    mudlay     => gdp%gdprocs%mudlay
    zmodel     => gdp%gdprocs%zmodel
    nonhyd     => gdp%gdprocs%nonhyd
    roller     => gdp%gdprocs%roller
    wavcmp     => gdp%gdprocs%wavcmp
    cnstwv     => gdp%gdprocs%cnstwv
    veg3d      => gdp%gdprocs%veg3d
    snelli     => gdp%gdprocs%snelli
    lrdamp     => gdp%gdprocs%lrdamp
    sbkol      => gdp%gdprocs%sbkol
    bubble     => gdp%gdprocs%bubble
    nfl        => gdp%gdprocs%nfl
    nmax       => gdp%d%nmax
    mmax       => gdp%d%mmax
    kmax       => gdp%d%kmax
    nmaxd      => gdp%d%nmaxd
    mmaxd      => gdp%d%mmaxd
    lmax       => gdp%d%lmax
    lmaxd      => gdp%d%lmaxd
    nlcest     => gdp%d%nlcest
    kc         => gdp%d%kc
    kcd        => gdp%d%kcd
    nopest     => gdp%d%nopest
    !
    nopest    = 0
    nmaxd     = 1
    mmaxd     = 1
    nflmod    = ' '
    cdwstruct = .false.
    const     = .false.
    culvert   = .false.
    flmd2l    = .false.
    veg3d     = .false.
    dredge    = .false.
    drogue    = .false.
    htur2d    = .false.
    iweflg    = .false.
    mudlay    = .false.
    roller    = .false.
    salin     = .false.
    secflo    = .false.
    sedim     = .false.
    temp      = .false.
    threed    = .false.
    wavcmp    = .false.
    wave      = .false.
    wind      = .false.
    snelli    = .false.
    cnstwv    = .false.
    waveol    = 0
    sbkol     = .false.
    bubble    = .false.
    nfl       = .false.
    lfsdu     = .false.    
    lfsdus1   = .false.    
    !
    ! read dimensions out of md-file
    !
    call dimrd(lunmd     ,lundia    ,error     ,runid     ,nrver     , &
             & prgnm     ,wind      ,salin     ,temp      ,sedim     , &
             & const     ,secflo    ,drogue    ,wave      ,iweflg    , &
             & htur2d    ,mudlay    , &
             & flmd2l    ,zmodel    ,nonhyd    ,roller    ,wavcmp    , &
             & culvert   ,dredge    ,cdwstruct ,snelli    ,cnstwv    , &
             & veg3d     ,waveol    ,lrdamp    ,sbkol     ,bubble    , &
             & nfl       ,nflmod    ,lfsdu     ,lfsdus1   ,gdp       )
    if (error) goto 9999
    !
    ! carry out domain decomposition based on load balancing
    ! using grid enclosure meant for parallel runs according
    ! to distributed-memory approach
    !
    call dfdecomp(lunmd, lundia, error, runid, gdp)
    if (error) goto 9999
    !
    ! calculate NOPEST and NLCEST and all NMAX MMAX combinations
    ! which depends on the type of computation (for DROGUE and TEMP no special
    ! arrays declared yet; for WAVE not yet implemented)
    !
    nopest = 4 * (nmax+mmax)
    if (nlcest == 0) then
       !
       ! nlcest not specified (overwritten) by the user
       ! define an estimation:
       !
       nlcest = 7 * (nmax+mmax)
    endif
    if (kmax > 1) then
       threed = .true.
       nmaxd  = nmax
       mmaxd  = mmax
    endif
    lmaxd = max(1 , lmax)
    kcd   = max(kc, kmax)
    !
    ! set grid dimensions (changed with the introduction of DDbounds)
    !
    call griddims(gdp)
    !
    ! calculate indices of real arrays
    !
    lerror = .false.
    call esm_alloc_real(lundia    ,lerror    ,gdp       )
    if (lerror) error = .true.
    !
    ! calculate indices of integer arrays
    !
    lerror = .false.
    call esm_alloc_int(lundia    ,lerror    ,zmodel    ,gdp       )
    if (lerror) error = .true.
    !
    ! calculate indices of character arrays
    !
    lerror = .false.
    call esm_alloc_char(lundia    ,lerror    ,gdp       )
    if (lerror) error = .true.
    !
    ! Allocate arrays in GDP structure using read dimensions
    ! and initialise arrays
    !
    call gdp_alloc_arrays(gdp)
    call initarrays(gdp)
    !
 9999 continue
end subroutine decarr
