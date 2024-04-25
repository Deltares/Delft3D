!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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

!> This module provides support function for processing polygon data.
module polygon_module

contains

!> Search for a polygon in a tree, given it's name. Stop with an error message if it is not found.
!! If the polygon does not have an id, add a unique one.
!! If the polygon already has an id, and the usage of the polygon should be unique, stop with an error message.
subroutine register_polygon(name, pol_ptr, idcount, totpoints, &
                          & areatp, mustbeunique, lundia)
    use precision
    use properties
    use message_module
    !
    implicit none
!
! Global variables
!
    type(tree_data), pointer              :: pol_ptr
    integer                               :: idcount
    integer                               :: totpoints
    character(len=*)        , intent(in)  :: name
    character(len=*)        , intent(in)  :: areatp
    logical                 , intent(in)  :: mustbeunique
    integer                               :: lundia
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
       call write_error(message, unit=lundia)
       return ! return error?
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
          call write_error(message, unit=lundia)
          return
       endif
    endif
    totpoints = totpoints + values(1)
end subroutine register_polygon


!> Read polygon points.
subroutine read_polygon_data(polygon_ptr, idcoord, start, number, &
                           & xcoord, ycoord, areatp, indx, lundia)
    use precision
    use properties
    use message_module
    !
    implicit none
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
    integer                               :: lundia
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
       call write_error(trim(message), unit=lundia)
       return
    endif
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
          call write_error(trim(message), unit=lundia)
          return
       endif
       xcoord(start+ip-1) = inputvals(1)
       ycoord(start+ip-1) = inputvals(2)
       idcoord = idcoord + 1
    enddo
end subroutine read_polygon_data

!>  Detect whether point (xp,yp) lies inside polygon (x,y) of n points.
!! Point n+1 is made equal to point 1.
!! inout = -1 :  Outside polygon
!! inout =  0 :  On boundary of polygon
!! inout =  1 :  Inside polygon
!!
!! Method used:
!! - Draw a vertical line through (xp,yp)
!! - Detect the number of crossings with the polygon below yp: nunder
!! - If nunder is even, the point is outside the polygon, else inside
!! - The boundary is handled seperately
!!
!! Original implementation
!! Author: J.A. Roelvink
!! Date  : 22 Dec 1988
subroutine ipon(xpoly, ypoly, n, xp, yp, inout) ! should use pinpok from geometry_module?
    use precision
    !
    implicit none
!
! Global variables
!
    integer               , intent(out) :: inout
    integer               , intent(in)  :: n
    real(fp)              , intent(in)  :: xp
    real(fp)              , intent(in)  :: yp
    real(fp), dimension(*), intent(in)  :: xpoly
    real(fp), dimension(*), intent(in)  :: ypoly
!
! Local variables
!
    integer :: i
    integer :: istat
    integer :: nunder
    real(fp):: ysn
    real(fp):: xprev
    real(fp):: yprev
    real(fp):: xnext
    real(fp):: ynext
!
! executable statements ------------------------------------------------------
!
    xnext = xpoly(n) - xp
    ynext = ypoly(n) - yp
    nunder   = 0
    do i = 1, n
       xprev = xnext
       yprev = ynext
       xnext = xpoly(i) - xp
       ynext = ypoly(i) - yp
       !
       if ((xprev<0. .and. xnext>=0.).or.(xnext<0. .and. xprev>=0.)) then
          if (yprev<0. .and. ynext<0.) then
             nunder = nunder + 1
          elseif ((yprev<=0. .and. ynext>=0.) .or.                       &
                & (ynext<=0. .and. yprev>=0.)) then
             ysn = (yprev*xnext - xprev*ynext)/(xnext - xprev)
             if (ysn<0.) then
                nunder = nunder + 1
             elseif (ysn<=0.) then
                !
                ! boundary
                !
                inout = 0
                goto 100
             else
             endif
          else
          endif
       elseif (abs(xprev)<1.0E-8 .and. abs(xnext)<1.0E-8) then
          if ((yprev<=0. .and. ynext>=0.).or.(ynext<=0..and.yprev>=0.)) &
            & then
             !
             ! boundary
             !
             inout = 0
             goto 100
          endif
       else
       endif
    enddo
    if (mod(nunder, 2)==0) then
       !
       ! outside
       !
       inout = -1
    else
       !
       ! inside
       !
       inout = 1
    endif
  100 continue
end subroutine ipon

end module polygon_module
