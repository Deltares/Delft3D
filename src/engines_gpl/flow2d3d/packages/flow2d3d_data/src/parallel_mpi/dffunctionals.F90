#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
module dffunctionals
!
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
!    Function: regroups functionalities for operations spanning all partitions
!              and requiring communications
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   use dfgather_module
   use dfscatter_module
   !
   implicit none
   !
   interface dffind_duplicate
      module procedure dffind_duplicate_C
      module procedure dffind_duplicate_I
   end interface dffind_duplicate
   !
contains
!
!
!
!===============================================================================
subroutine dffind_duplicate_C(lundia, nb, nbto, nbgl, nam, duplicate, gdp)
!!--description-----------------------------------------------------------------
!
!    Function: find duplicated stations/cross-sections/etc. over partitions
!              returns nbto (total), nbgl (global=original number without duplicates)
!                      duplicate array (=0 if no duplicate; =i if duplicate of i)
!    Method used: based on names
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                 :: gdp
integer                                 , intent(in)  :: lundia
integer                                 , intent(in)  :: nb
integer                                 , intent(out) :: nbto
integer                                 , intent(out) :: nbgl
character(20), dimension(nb)            , intent(in)  :: nam
integer      , dimension(:), allocatable              :: duplicate
!
! Local variables
!
integer                                     :: m
integer                                     :: n
integer                                     :: ip
integer                                     :: indx
integer      , dimension(:), allocatable    :: nbarr
character(20), dimension(:), allocatable    :: namto
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) then
       allocate( nbarr( nproc ) )
    else
       allocate( nbarr( 1 ) )
    endif
    call dfgather_lowlevel ( nbarr, nproc, nb, 1, dfint, gdp )
    if (inode == master) then
       nbto = SUM(nbarr)
       allocate( namto(1:nbto) )
    else
       allocate( namto(1) )
    endif
    call dfgather_lowlevel ( namto, 20*nbto, nam, 20*nb, dfchar, gdp )
    if (inode == master) then
       allocate( duplicate(nbto) )
       ip = 1
       indx = nbarr(ip)
       do n = 1, nbto
          duplicate(n) = 0
          m = 1
          do while (m < n .and. duplicate(n) == 0)
             if (namto(m) == namto(n)) duplicate(n) = m
             m = m + 1
          enddo
          if (n >= indx) then
             ip = ip + 1
             indx = indx + nbarr(ip)
          endif
       enddo
       nbgl = count(duplicate == 0)
    endif
    deallocate( namto )
    deallocate( nbarr )
    call dfbroadc_gdp( nbgl, 1, dfint, gdp)
    call dfbroadc_gdp( nbto, 1, dfint, gdp)
end subroutine dffind_duplicate_C
!
!
!
!===============================================================================
subroutine dffind_duplicate_I(lundia, nb, nbto, nbgl, order, gdp)
!!--description-----------------------------------------------------------------
!
!    Function: find duplicated stations/cross-sections/etc. over partitions
!              returns nbgl
!    Method used: based on original global numbering
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
!
! Global variables
!
type(globdat), target                 :: gdp
integer                 , intent(in)  :: lundia
integer                 , intent(in)  :: nb
integer                 , intent(out) :: nbto
integer                 , intent(out) :: nbgl
integer, dimension(nb)  , intent(in)  :: order
!
! Local variables
!
logical                                     :: found
integer                                     :: m
integer                                     :: n
integer, dimension(:), allocatable          :: nbarr
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) then
       allocate( nbarr( nproc ) )
    else
       allocate( nbarr( 1 ) )
    endif
    call dfgather_lowlevel ( nbarr, nproc, nb, 1, dfint, gdp )
    if (inode == master) then
       nbto = SUM(nbarr)
       deallocate( nbarr )
       allocate( nbarr(1:nbto) )
    endif
    call dfgather_lowlevel ( nbarr, nbto, order, nb, dfint, gdp )
    if (inode == master) then
       nbgl = 0
       do m = 1, nbto
          if (nbarr(m) == 0) cycle ! does not count when order==0
          n = 1
          found = .false.
          search_loop: do while (n <= nbto .and. n /= m)
             if (nbarr(m) == nbarr(n)) then
                found = .true.
                exit search_loop
             endif
             n = n + 1
          enddo search_loop
          if (.not.found) nbgl = nbgl + 1
       enddo
    endif
    deallocate( nbarr )
    call dfbroadc_gdp( nbgl, 1, dfint, gdp)
    call dfbroadc_gdp( nbto, 1, dfint, gdp)
end subroutine dffind_duplicate_I

end module dffunctionals
