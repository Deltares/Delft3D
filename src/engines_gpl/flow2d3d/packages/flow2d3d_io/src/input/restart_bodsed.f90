subroutine restart_bodsed (error     ,restid    ,i_restart ,bodsed    , &
                         & lsedtot   ,nmaxus    ,mmax      ,success   , &
                         & elmnam    ,lundia    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: restart_bodsed.f90 5990 2016-04-11 13:28:37Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/input/restart_bodsed.f90 $
!!--description-----------------------------------------------------------------
! Reads initial field condition records from an
! NEFIS flow output map file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision 
    use globaldata
    use string_module
    use nan_check_module
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                                                               :: i_restart
    integer                                                                               :: lundia
    integer                                                                               :: lsedtot
    integer                                                                               :: nmaxus
    integer                                                                               :: mmax
    logical                                                                               :: error
    logical                                                                 , intent(out) :: success
    real(prec), dimension(lsedtot, gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: bodsed
    character(*)                                                                          :: restid
    character(*)                                                                          :: elmnam
!
! Local variables
!
    integer                                   :: i
    integer                                   :: j
    integer                                   :: l
    integer                                   :: lrid        ! character variables for files Help var., length of restid
    integer                      , external   :: crenef
    integer                      , external   :: getelt
    integer                      , external   :: clsnef
    integer                      , external   :: neferr
    integer                                   :: rst_lsed
    integer                                   :: rst_lsedbl
    integer                                   :: rst_lsedtot
    integer                                   :: ierror
    integer                                   :: fds
    integer                      , pointer    :: mfg
    integer                      , pointer    :: mlg
    integer                      , pointer    :: nfg
    integer                      , pointer    :: nlg
    integer                      , pointer    :: nmaxgl
    integer                      , pointer    :: mmaxgl
    integer , dimension(3,5)                  :: cuindex
    integer , dimension(3,5)                  :: uindex
    real(sp), dimension(:,:,:)   , pointer    :: rst_bodsed
    character(len=256)                        :: dat_file
    character(len=256)                        :: def_file
    character(len=1024)                       :: errmsg
!
!! executable statements -------------------------------------------------------
!
    mfg                 => gdp%gdparall%mfg
    mlg                 => gdp%gdparall%mlg
    nfg                 => gdp%gdparall%nfg
    nlg                 => gdp%gdparall%nlg
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl 
    nullify(rst_bodsed)
    error        = .false.
    success      = .false.
    call remove_leading_spaces(restid    ,lrid      )
    !
    ! open NEFIS trim-<restid> file
    !
    dat_file = restid(1:lrid)//'.dat'
    def_file = restid(1:lrid)//'.def'
    if (inode == master) then    
       ierror   = crenef(fds, dat_file, def_file, ' ', 'r')
    endif
    call dfbroadc_gdp ( ierror, 1, dfint, gdp )
    if (ierror/= 0) then
       error = .true.
       goto 9999
    endif
    !
    ! initialize group index constant data
    !
    cuindex (3,1) = 1 ! increment in time
    cuindex (1,1) = 1
    cuindex (2,1) = 1
    !
    ! initialize group index time dependent data
    !
    uindex (3,1) = 1 ! increment in time
    uindex (1,1) = i_restart
    uindex (2,1) = i_restart
    !
    ! allocate global versions of msed and thlyr
    !
    allocate(rst_bodsed(nmaxgl, mmaxgl, lsedtot), stat=ierror)
    !   
    ! the master opens and reads the file 
    ! 
    if ( inode /= master ) goto 50 
    !
    ! determine number of suspended sediment fractions
    !
    ierror = getelt(fds, 'map-const', 'LSED'  , cuindex, 1, 4, rst_lsed)
    if (ierror /= 0) then
       !
       ! if LSED has not been written to the map-file then LSED=0
       ! remove the error message from NEFIS error stack
       !
       rst_lsed   = 0
       ierror     = neferr(0,errmsg)
    endif
    !
    ! determine number of bedload sediment fractions
    !
    ierror = getelt(fds, 'map-const', 'LSEDBL', cuindex, 1, 4, rst_lsedbl)
    if (ierror /= 0) then
       !
       ! if LSEDBL has not been written to the map-file then LSEDBL=0
       ! remove the error message from NEFIS error stack
       !
       rst_lsedbl = 0
       ierror     = neferr(0,errmsg)
    endif
    rst_lsedtot = rst_lsed + rst_lsedbl
    if (rst_lsedtot /= lsedtot) then
       ierror = 1
       goto 50
    endif
    !
    ierror = getelt(fds , 'map-sed-series', elmnam, uindex, 1, &
                 & mmaxgl*nmaxgl*rst_lsedtot*4, rst_bodsed )
    if (ierror/= 0) goto 50
    !    end of master part
    ! 
    ! scatter information to all nodes     
 50 continue     
    ! 
    ! scatter array bodsed to all nodes.
    ! Note: the broadc must use 'dfreal' since the array is 'sp'
    ! 
    call dfsync ( gdp) 
    call dfbroadc_gdp ( ierror, 1, dfint, gdp )
    if (ierror/=0) goto 9999
    !
    call dfbroadc_gdp ( rst_bodsed, nmaxgl*mmaxgl*lsedtot, dfreal, gdp ) 
    ! 
    ! extract relevant part of bodsed for each subdomain 
    ! 
    call dfsync ( gdp ) 
    do j = mfg, mlg 
       do i = nfg, nlg
          do l = 1, lsedtot
             bodsed(l,i-nfg+1,j-mfg+1) = rst_bodsed(i,j,l)
          enddo
       enddo 
    enddo 
    if (.not. nan_check(bodsed, elmnam//' (restart-file)', lundia)) call d3stop(1, gdp)
    deallocate(rst_bodsed)
    success = .true.
    !
9999 continue
    if (inode == master) then
       ierror = clsnef(fds)
    endif
end subroutine restart_bodsed
