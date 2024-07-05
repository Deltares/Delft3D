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


module m_domain
    !! module contains everything for the domain discription

    implicit none

    integer, parameter, private :: NAME_SIZE = 256      ! size of descriptive names
    integer, parameter, private :: MAX_NUM = 5      ! allocated per bunch

    integer, parameter :: DD_RIGHT_LEFT = 1      ! direction of dd_boundary
    integer, parameter :: DD_LEFT_RIGHT = 2      ! direction of dd_boundary
    integer, parameter :: DD_TOP_BOTTOM = 3      ! direction of dd_boundary
    integer, parameter :: DD_BOTTOM_TOP = 4      ! direction of dd_boundary

    type t_domain
        character(len = NAME_SIZE) :: name                   ! name of domain
        integer :: mmax                   ! domain size in the m direction
        integer :: nmax                   ! domain size in the n direction
        character(len = NAME_SIZE) :: aggr                   ! aggregation file name
    end type t_domain

    type t_domain_collection
        type(t_domain), pointer :: domain_pnts(:)         ! pointer to the domains
        integer :: maxsize                ! maximum size of the current array
        integer :: current_size                ! filled up to this size
    contains
        procedure :: find => domain_coll_find
        procedure :: add => domain_coll_add
    end type t_domain_collection

    type t_dd_bound
        character(len = NAME_SIZE) :: name1                  ! name of domain 1
        integer :: i_domain1              ! index of domain 1
        integer :: refine1                ! refinement for domain 1 side
        integer :: m_begin1               ! begin m of boundary in domain 1
        integer :: n_begin1               ! begin n of boundary in domain 1
        integer :: m_end1                 ! end m of boundary in domain 1
        integer :: n_end1                 ! end n of boundary in domain 1
        character(len = NAME_SIZE) :: name2                  ! name of domain 2
        integer :: i_domain2         ! index of domain 2
        integer :: refine2           ! refinement for domain 2 side
        integer :: m_begin2          ! begin m of boundary in domain 2
        integer :: n_begin2          ! begin n of boundary in domain 2
        integer :: m_end2            ! end m of boundary in domain 2
        integer :: n_end2            ! end n of boundary in domain 2
        integer :: direction         ! direction can be either DD_RIGHT_LEFT DD_LEFT_RIGHT DD_TOP_BOTTOM DD_BOTTOM_TOP
    end type t_dd_bound

    type t_dd_bound_coll
        type(t_dd_bound), pointer :: dd_bound_pnts(:)       ! pointer to the dd_bound's
        integer :: maxsize                ! maximum size of the current array
        integer :: current_size           ! filled up to this size
    contains
        procedure :: add => dd_bound_coll_add
    end type t_dd_bound_coll

    private :: domain_coll_find, domain_coll_add, dd_bound_coll_add

contains

    function domain_coll_find(self, name) result (iret)
        ! function to find a domain name in a collection, case sensitive at the moment
        class(t_domain_collection) :: self            ! collection of domains
        character(LEN = *) :: name                   ! name of domain to be found
        integer :: iret                   ! result index in collection or 0 if not found

        integer :: i                      ! loop counter

        iret = 0
        do i = 1, self%current_size
            if (self%domain_pnts(i)%name == name) then
                iret = i
                return
            endif
        end do

    end function domain_coll_find

    function domain_coll_add(self, domain) result (current_size)
        ! function to add to a collection of domains (copy)
        class(t_domain_collection) :: self     ! collection of domains
        type(t_domain) :: domain               ! domain to be added
        integer :: current_size                ! return value the new current collection size
        ! and the index of the added domain

        type(t_domain), pointer :: domain_pnts(:)         ! pointer for the resize operation
        integer :: i                      ! loop counter

        if (self%current_size == self%maxsize) then

            ! resize, allocate new array
            allocate (domain_pnts (self%maxsize + MAX_NUM))

            ! copy the domains into the new array
            do i = 1, self%maxsize
                domain_pnts(i) = self%domain_pnts(i)   ! copies the domains
            enddo

            ! deallocate the old array and attach the new array to the collection
            if (self%maxsize /= 0) deallocate (self%domain_pnts)
            self%domain_pnts => domain_pnts
            self%maxsize = self%maxsize + MAX_NUM

        endif

        self%current_size = self%current_size + 1
        self%domain_pnts(self%current_size) = domain
        current_size = self%current_size

    end function domain_coll_add

    function dd_bound_coll_add(self, dd_bound) result (current_size)
        ! function to add to a collection of dd_bound (copy)
        class(t_dd_bound_coll) :: self          ! collection of dd_bounds
        type(t_dd_bound) :: dd_bound               ! dd_bound to be added
        integer :: current_size                ! return value the new current collection size
        ! and the index of the added dd_bound

        type(t_dd_bound), pointer :: dd_bound_pnts(:)       ! pointer for the resize operation
        integer :: i                      ! loop counter

        if (self%current_size == self%maxsize) then

            ! resize, allocate new array
            allocate (dd_bound_pnts (self%maxsize + MAX_NUM))

            ! copy the dd_bounds into the new array
            do i = 1, self%maxsize
                dd_bound_pnts(i) = self%dd_bound_pnts(i)   ! copies the dd_bounds
            enddo

            ! deallocate the old array and attach the new array to the collection
            if (self%maxsize /= 0) deallocate (self%dd_bound_pnts)
            self%dd_bound_pnts => dd_bound_pnts
            self%maxsize = self%maxsize + MAX_NUM

        endif

        self%current_size = self%current_size + 1
        self%dd_bound_pnts(self%current_size) = dd_bound
        current_size = self%current_size

    end function dd_bound_coll_add

end module m_domain
