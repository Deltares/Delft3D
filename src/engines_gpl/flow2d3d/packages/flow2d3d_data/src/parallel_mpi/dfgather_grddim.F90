subroutine dfgather_grddim(lundia, nfg, nlg, mfg, mlg, nmaxgl, mmaxgl, &
                         & nf, nl, mf, ml, iarrc, lengl, lenlo, gdp)
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
!    Function: gathers "connectivity" local - global grids
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                                      :: lundia !  Description and declaration in inout.igs
    integer                        , intent(in)  :: mfg
    integer                        , intent(in)  :: mlg
    integer                        , intent(in)  :: nfg
    integer                        , intent(in)  :: nlg
    integer                        , intent(in)  :: mmaxgl
    integer                        , intent(in)  :: nmaxgl
    integer, dimension(4,0:nproc-1), intent(out) :: iarrc  ! array containing collected grid indices 
    integer                        , intent(out) :: lenlo  ! length of field of current subdomain
    integer                        , intent(out) :: lengl  ! length of field containing collected data
    integer, dimension(0:nproc-1)  , intent(out) :: mf     ! first index w.r.t. global grid in x-direction
    integer, dimension(0:nproc-1)  , intent(out) :: ml     ! last index w.r.t. global grid in x-direction
    integer, dimension(0:nproc-1)  , intent(out) :: nf     ! first index w.r.t. global grid in y-direction
    integer, dimension(0:nproc-1)  , intent(out) :: nl     ! last index w.r.t. global grid in y-direction
!
! Local variables
!
    integer                                      :: ip     ! node number 
    integer                                      :: msiz   ! size of present subdomain in x-direction
    integer                                      :: nsiz   ! size of present subdomain in y-direction
    !
    !
    ! gather grid indices of all subdomains
    !
    call dfgather_lowlevel ( iarrc, 4*nproc, (/mfg - 2, mlg + 2, nfg, nlg/), 4, dfint, gdp )
    !
    if (inode == master) then
       !
       do ip = 0, nproc-1
          !
          if ( iarrc(1,ip) == -1 ) then
             mf(ip) = 1
          else
             mf(ip) = iarrc(1,ip) + ihalom +2
          endif
          if ( iarrc(2,ip) == mmaxgl+2 ) then
             ml(ip) = mmaxgl
          else
             ml(ip) = iarrc(2,ip) - ihalom -2
          endif
          if ( iarrc(3,ip) == 1 ) then
             nf(ip) = 1
          else
             nf(ip) = iarrc(3,ip) + ihalon
          endif
          if ( iarrc(4,ip) == nmaxgl ) then
             nl(ip) = nmaxgl
          else
             nl(ip) = iarrc(4,ip) - ihalon
          endif
          !
       enddo
       !
    endif
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz
    !
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
    endif

end subroutine dfgather_grddim
