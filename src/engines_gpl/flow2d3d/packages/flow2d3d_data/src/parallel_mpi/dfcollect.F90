subroutine dfcollect ( fieldc, field, gdp )
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
!  $Id: dfcollect.F90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfcollect.F90 $
!!--description-----------------------------------------------------------------
!
!   Collects field array from all nodes
!
!!--pseudo code and references--------------------------------------------------
!
!   if not parallel, return
!   gather grid indices of all subdomains
!   determine total length for collecting data of all nodes and allocate array
!   gather data of all subdomains
!   copy gathered data to global array in appropriate manner
!
!
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
    integer, pointer               :: mmaxgl
    integer, pointer               :: nmaxgl
    integer, pointer               :: nfg
    integer, pointer               :: nlg
    integer, pointer               :: mfg
    integer, pointer               :: mlg
!
! Global variables
!
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                          , intent(in ) :: field  ! field array in current subdomain
    real(fp), dimension(1:gdp%gdparall%nmaxgl, -1:gdp%gdparall%mmaxgl+2), intent(out) :: fieldc ! global field array gathered from all nodes
!
! Local variables
!
    integer, dimension(4)               :: iarrl  ! array containing grid indices of each subdomain
    integer, dimension(4,0:nproc-1)     :: iarrc  ! array containing collected grid indices
    integer                             :: indx   ! array index
    integer                             :: ip     ! node number
    integer                             :: istart ! start pointer for each subdomain in array FLDC
    integer                             :: len    ! length of field of current subdomain
    integer                             :: lenc   ! length of field containing collected data
    integer                             :: m      ! loop counter
    integer                             :: mf     ! first index w.r.t. global grid in x-direction
    integer                             :: ml     ! last index w.r.t. global grid in x-direction
    integer                             :: n      ! loop counter
    integer                             :: nf     ! first index w.r.t. global grid in y-direction
    integer                             :: nl     ! last index w.r.t. global grid in y-direction
    integer                             :: nsiz   ! size of present subdomain in y-direction
    !
    real(fp), dimension(:), allocatable :: fldc   ! auxiliary array for collecting data
!
!! executable statements -------------------------------------------------------
!
    mmaxgl    => gdp%gdparall%mmaxgl
    nmaxgl    => gdp%gdparall%nmaxgl
    nfg       => gdp%gdparall%nfg
    nlg       => gdp%gdparall%nlg
    mfg       => gdp%gdparall%mfg
    mlg       => gdp%gdparall%mlg
    !
    ! if not parallel, return
    !
    if (.not.parll) return
    !
    ! gather grid indices of all subdomains
    !
    iarrl(1) = mfg -2
    iarrl(2) = mlg +2
    iarrl(3) = nfg
    iarrl(4) = nlg
    call dfgather_lowlevel ( iarrc, 4*nproc, iarrl, 4, dfint, gdp )
    !
    len = (iarrl(2)-iarrl(1)+1)*(iarrl(4)-iarrl(3)+1)
    !
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate array
       !
       lenc = 0
       do ip = 0, nproc-1
          lenc = lenc + (iarrc(2,ip)-iarrc(1,ip)+1)*(iarrc(4,ip)-iarrc(3,ip)+1)
       enddo
       !
       allocate(fldc(lenc))
       !
    endif
    !
    ! gather data of all subdomains
    !
    call dfgather_lowlevel ( fldc, lenc, field, len, dfloat, gdp )
    !
    ! copy gathered data to global array in appropriate manner
    !
    if (inode == master) then
       !
       istart = 0
       !
       do ip = 0, nproc-1
          !
          if ( iarrc(1,ip) == -1 ) then
             mf = 1
          else
             mf = iarrc(1,ip) + ihalom +2
          endif
          if ( iarrc(2,ip) == mmaxgl+2 ) then
             ml = mmaxgl
          else
             ml = iarrc(2,ip) - ihalom -2
          endif
          if ( iarrc(3,ip) == 1 ) then
             nf = 1
          else
             nf = iarrc(3,ip) + ihalon
          endif
          if ( iarrc(4,ip) == nmaxgl ) then
             nl = nmaxgl
          else
             nl = iarrc(4,ip) - ihalon
          endif
          !
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          !
          do m = mf, ml
             do n = nf, nl
                indx = istart + (m-iarrc(1,ip))*nsiz + n-iarrc(3,ip)+1
                fieldc(n,m) = fldc(indx)
             enddo
          enddo
          !
          istart = istart + (iarrc(2,ip)-iarrc(1,ip)+1)*(iarrc(4,ip)-iarrc(3,ip)+1)
          !
       enddo
       !
    endif
    !
    if (inode == master) deallocate(fldc)
    !
end subroutine dfcollect
