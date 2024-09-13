module polygon_module
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
!  $Id: polygon_module.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/input/polygon_module.f90 $
!!--module description----------------------------------------------------------
!
! This module provides support function for processing polygon data.
!
!!--module declarations---------------------------------------------------------


contains


subroutine register_polygon(name, pol_ptr, idcount, totpoints, &
                          & areatp, mustbeunique, gdp)
!!--description-----------------------------------------------------------------
!
! Search for a polygon in a tree, given it's name. Stop with an error message
!    if it is not found.
! If the polygon does not have an id, add a unique one.
! If the polygon already has an id, and the usage of the polygon should be
!    unique, stop with an error message.
!
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: lundia
!
! Global variables
!
    type(tree_data), pointer              :: pol_ptr
    integer                               :: idcount
    integer                               :: totpoints
    character(len=*)        , intent(in)  :: name
    character(len=*)        , intent(in)  :: areatp
    logical                 , intent(in)  :: mustbeunique
!
! Local variables
!
    integer                                     :: id
    integer           , dimension(1:1)          :: values
    character(len=1)  , dimension(:)  , pointer :: data_ptr
    character(len=10)                           :: idstring
    character(len=200)                          :: message
    character(len=80)                           :: node_type
    type(tree_data)                   , pointer :: polygon_ptr
    type(tree_data)                   , pointer :: node_ptr
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    !
    ! Initialize
    !
    values = 0
    !
    ! Get corresponding polygon node
    !
    call tree_get_node_by_name(pol_ptr, name, polygon_ptr )
    if ( .not. associated(polygon_ptr) ) then
       write (message,'(3a)') 'polygon ', trim(name),' is missing'
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    ! Create the polygon's ID
    !
    id = 0
    call prop_get_integer(polygon_ptr, '*', trim(areatp)//'id', id)
    if (id == 0) then
       idcount = idcount + 1
       !
       ! This is a new area ... add the ID
       !
       call tree_create_node( polygon_ptr, trim(areatp)//'id', node_ptr )
       write(idstring,'(i0)') idcount
       call tree_put_data( node_ptr, transfer(trim(idstring),node_value), 'STRING' )
       !
       ! Get number of points in this polygon
       !
       call tree_get_data_ptr( polygon_ptr, data_ptr, node_type )
       values = transfer( data_ptr, values )
    else
       if ( mustbeunique ) then
          write (message,'(3a)') trim(areatp)//' area ', trim(name), &
                                 ' is specified more than once in polygon-file'
          call prterr(lundia, 'U021', trim(message))
          call d3stop(1, gdp)
       endif
    endif
    totpoints = totpoints + values(1)
end subroutine register_polygon


subroutine read_polygon_data(polygon_ptr, idcoord, start, number, &
                           & xcoord, ycoord, areatp, indx, gdp )
!!--description-----------------------------------------------------------------
!
! Read polygon points
!
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: lundia
!
! Global variables
!
    type(tree_data), pointer              :: polygon_ptr
    integer                               :: idcoord
    integer                 , intent(out) :: start
    integer                 , intent(out) :: number
    real(fp), dimension(:)  , intent(out) :: xcoord
    real(fp), dimension(:)  , intent(out) :: ycoord
    character(len=*)        , intent(in)  :: areatp
    integer                 , intent(in)  :: indx
!
! Local variables
!
    integer                                 :: ip
    integer , dimension(1:2)                :: inputivals
    real(fp), dimension(1:2)                :: inputvals
    real(fp)                                :: misvalue
    character(len=1), pointer, dimension(:) :: data_ptr
    character(len=30)                       :: node_type
    character(len=30)                       :: parname
    character(len=200)                      :: message
    type(tree_data), pointer                :: node_ptr
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    !
    ! Record start and number of polygon points
    !
    misvalue = -999.999
    start    = idcoord
    number   = -1
    call tree_get_data_ptr( polygon_ptr, data_ptr, node_type )
    inputivals = transfer( data_ptr, inputivals )
    number     = inputivals(1)
    if (number == -1) then
       write(message,'(a,a,i0)') 'Unable to read the number of points in ', &
            &                    trim(areatp), ' polygon of area',indx
       call prterr(lundia, 'U021', message)
       call d3stop(1, gdp)
    endif
    !write (lundia,'(a,i0,a,i0)') 'Number of points in '//trim(areatp)// &
    !      &                      ' polygon of area ', indx,': ',number
    !
    ! read the polygon points
    !
    do ip = 1, number
       inputvals = misvalue
       write (parname,'(a,i0)')'row_',ip
       call tree_get_node_by_name( polygon_ptr, parname, node_ptr )
       call tree_get_data_ptr( node_ptr, data_ptr, node_type )
       !
       ! inputvals is of type real(fp)
       ! the data to be retrieved is in real(sp)
       ! call transfer with a real(sp) constant as second parameter
       !
       inputvals = transfer( data_ptr, 0., 2 )
       if (  comparereal(inputvals(1),misvalue) == 0 .or. &
           & comparereal(inputvals(2),misvalue) == 0        ) then
          write(message,'(a,i0,a,i0)') 'Unable to read '//trim(areatp)// &
               &                       ' polygon point ', ip,' of area ',indx
          call prterr(lundia, 'U021', message)
          call d3stop(1, gdp)
       endif
       xcoord(start+ip-1) = inputvals(1)
       ycoord(start+ip-1) = inputvals(2)
       !write(lundia,'(a,i3,a,i3,a,f13.5,f13.5)') 'Point ',ip,', &
       !     &   '//trim(areatp)//' area ',indx,':',xcoord(start+ip-1), ycoord(start+ip-1)
       idcoord = idcoord + 1
    enddo
end subroutine read_polygon_data


end module polygon_module
