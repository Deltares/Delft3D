subroutine restart_bdf_from_trim(lundia   ,nmaxus   ,mmax     ,bdfh     , &
                              &  bdfhread ,bdfl     ,bdflread ,gdp      )
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
!  $Id: restart_bdf_from_trim.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/input/restart_bdf_from_trim.f90 $
!!--description-----------------------------------------------------------------
! Reads initial field condition records from a NEFIS flow output map file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! Global variables
    !
    integer                                                                    , intent(in)  :: lundia
    integer                                                                    , intent(in)  :: mmax
    integer                                                                    , intent(in)  :: nmaxus
    logical                                                                    , intent(out) :: bdfhread
    logical                                                                    , intent(out) :: bdflread
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: bdfh
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: bdfl
    !
    ! Local variables
    !
    integer                               :: lrid        ! character variables for files Help var., length of restid
    integer, external                     :: crenef
    integer, external                     :: getelt
    integer, external                     :: clsnef
    integer                               :: error
    integer                               :: fds
    integer, dimension(3,5)               :: cuindex
    integer, dimension(3,5)               :: uindex
    real(sp), dimension(:,:,:,:), pointer :: sbuff
    character(len=256)                    :: dat_file
    character(len=256)                    :: def_file
    !
    integer                             , pointer :: i_restart
    character(256)                      , pointer :: restid
    !
    !! executable statements -------------------------------------------------------
    !
    i_restart          => gdp%gdrestart%i_restart
    restid             => gdp%gdrestart%restid
    !
    nullify(sbuff)
    bdfhread = .false.
    bdflread = .false.
    call remove_leading_spaces(restid    ,lrid      )
    !
    ! open NEFIS trim-<restid> file
    !
    dat_file = restid(1:lrid)//'.dat'
    def_file = restid(1:lrid)//'.def'
    error    = crenef(fds, dat_file, def_file, ' ', 'r')
    if (error /= 0) goto 9999
    !
    ! initialize group index time dependent data
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
    allocate(sbuff(nmaxus, mmax, 1, 1))
    !
    ! Read DUNEHEIGHT
    !
    error = getelt( fds , 'map-sed-series', 'DUNEHEIGHT', uindex, 1, mmax*nmaxus*4, sbuff )
    if (error /= 0) then
       !
       ! In the research version, DUNEHEIGHT was stored in the map-series group.
       ! For the time being remain compatible with that version.
       !
       error = getelt( fds , 'map-series', 'DUNEHEIGHT', uindex, 1, mmax*nmaxus*4, sbuff )
    endif
    if (error == 0) then
       write(lundia, '(a)') 'Bed form height read from restart file.'
       bdfh(1:nmaxus,1:mmax) = real(sbuff(1:nmaxus,1:mmax,1,1),fp)
       bdfhread = .true.
    endif
    !
    ! Read DUNELENGTH
    !
    error = getelt( fds , 'map-sed-series', 'DUNELENGTH', uindex, 1, mmax*nmaxus*4, sbuff )
    if (error /= 0) then
       !
       ! In the research version, DUNELENGTH was stored in the map-series group.
       ! For the time being remain compatible with that version.
       !
       error = getelt( fds , 'map-series', 'DUNELENGTH', uindex, 1, mmax*nmaxus*4, sbuff )
    endif
    if (error == 0) then
       write(lundia, '(a)') 'Bed form length read from restart file.'
       bdfl(1:nmaxus,1:mmax) = real(sbuff(1:nmaxus,1:mmax,1,1),fp)
       bdflread = .true.
    endif
9999 continue
    if (associated(sbuff)) deallocate (sbuff)
    error = clsnef(fds) 
end subroutine restart_bdf_from_trim
