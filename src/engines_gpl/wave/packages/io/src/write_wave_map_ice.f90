subroutine write_wave_map_ice (sg, sif, n_swan_grids, wavedata, casl)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use wave_data
    use swan_flow_grid_maps
    !
    implicit none
!
! Local parameters
!
    integer, parameter :: nelmx = 3
!
! Global variables
!
    integer              , intent(in)  :: n_swan_grids ! number of swan grids
    character(*)         , intent(in)  :: casl         ! runid
    type (grid)          , intent(in)  :: sg           ! swan grid
    type (input_fields)  , intent(in)  :: sif          ! input fields defined on swan grid
    type (wave_data_type), intent(in)  :: wavedata
!
! Local variables
!
    integer                         :: celidt
    integer                         :: error
    integer                         :: ind
    integer, dimension(1)           :: idummy ! Help array to read/write Nefis files 
    integer, dimension(6, nelmx)    :: elmdms
    integer, dimension(nelmx)       :: nbytsg
    logical                         :: wrswch
    character(10), dimension(nelmx) :: elmunt
    character(16)                   :: grpnam
    character(16), dimension(nelmx) :: elmnms
    character(16), dimension(nelmx) :: elmqty
    character(16), dimension(nelmx) :: elmtps
    character(256)                  :: filnam
    character(64), dimension(nelmx) :: elmdes
    character(256)                  :: gridnam
    !
    !     Define data structure; element dimensions are required only
    !     in write-mode.
    !
    data grpnam/'ICE'/
    data elmnms/'TIME', 'ICEFRAC', 'FLOEDIA'/
    data elmdes/'time',                                                     &
        & 'Area fraction covered by ice                                  ', &
        & 'Ice floe diameter                                             '/
    data elmqty/nelmx*' '/
    data elmunt/'[TSCALE] ', '[  -  ]  ', '[  M  ]  '/
    data elmtps/'INTEGER', 2*'REAL'/
    data nbytsg/nelmx*4/
!
!! executable statements -------------------------------------------------------
!
    wrswch = .true.
    if (n_swan_grids == 1) then
       write(filnam,'(2a)')'wavm-',trim(casl)
    else
       gridnam = sg%grid_name
       ind = index(gridnam, '/', back = .true.)
       if (ind > 0) gridnam = gridnam(ind+1:)
       ind = index(gridnam, '\', back = .true.)
       if (ind > 0) gridnam = gridnam(ind+1:)
       ind = index(gridnam, '.', back = .true.)
       if (ind > 0) gridnam = gridnam(:ind-1)
       write(filnam,'(4a)')'wavm-',trim(casl),'-',trim(gridnam)
    endif
    !
    call filldm(elmdms, 1, 1, 1       , 0       , 0, 0, 0)
    call filldm(elmdms, 2, 2, sif%mmax, sif%nmax, 0, 0, 0)
    call filldm(elmdms, 3, 2, sif%mmax, sif%nmax, 0, 0, 0)
    !
    !        Write all elements to file; all
    !        definition and creation of files, data groups, cells and
    !        elements is handled by PUTGET
    !
    celidt=wavedata%output%count
    idummy(1) = wavedata%time%calctimtscale
    call putgti(filnam   , grpnam, nelmx , elmnms, elmdms    , &
              & elmqty   , elmunt, elmdes, elmtps, nbytsg    , &
              & elmnms(1), celidt, wrswch, error , idummy(1) )
    call putgtr(filnam   , grpnam, nelmx , elmnms, elmdms   , &
              & elmqty   , elmunt, elmdes, elmtps, nbytsg   , &
              & elmnms(2), celidt, wrswch, error , sif%ice_frac)

    call putgtr(filnam   , grpnam, nelmx , elmnms, elmdms   , &
              & elmqty   , elmunt, elmdes, elmtps, nbytsg   , &
              & elmnms(3), celidt, wrswch, error , sif%floe_dia)
              
end subroutine write_wave_map_ice
