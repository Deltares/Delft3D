!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

 !> remove netlinks to improve orthogonality
 subroutine del_badortholinks()
    use network_data
    use m_flowgeom, only : xz, yz
    use m_netstore
    use m_missing
    use stdlib_sorting, only: sort_index
    use gridoperations
    implicit none

    double precision, dimension(2)              :: xz_st,  yz_st

    double precision, dimension(:), allocatable :: dortho

    integer,          dimension(:), allocatable :: linkmask
    integer,          dimension(:), allocatable :: iperm

    double precision                            :: dorthosum, dorthosumnew
    double precision                            :: dmaxorthop
    double precision                            :: zz

    integer                                     :: LL, L, L1
    integer                                     :: k1, k2
    integer                                     :: kk, k
    integer                                     :: i, num, numnew

    integer                                     :: jatek

    double precision, external                  :: cosphiunet

    integer,          parameter                 :: P=2

    jatek = 1

    dmaxorthop = cosphiutrsh**P

    call savenet()

    if ( netstat.ne.NETSTAT_OK ) then
       call findcells(0)
    end if

!   take dry cells into account (after findcells)
    call delete_dry_points_and_areas()

    call makenetnodescoding() ! need it for allocation nb

!   allocate
    allocate(linkmask(numL))
    allocate(dortho(numL))
    allocate(iperm(numL))

    linkmask = 0

!   compute orthogonality
    do L=1,numL
       zz = cosphiunet(L)
       if ( zz.ne.DMISS ) then
          dortho(L) = abs(zz)
       end if
    end do

!   get permutation array, increasing orthogonality
    call sort_index(dortho, iperm)

    do i=numL,1,-1   ! decreasing order of orthogonality
       L = iperm(i)
       if ( lnn(L).eq.2 ) then   ! internal links only
          k1 = lne(1,L)
          k2 = lne(2,L)

!         check dimension of merged cell
          if ( netcell(k1)%N + netcell(k2)%N-1 .gt. maxnodespercell ) then
             cycle
          end if

          dorthosum = 0d0
          num = 0
!         compute current orthogonality
          do kk=1,2
             k = lne(kk,L)
             do LL=1,netcell(k)%N
                L1 = netcell(k)%lin(LL)
!               check with mask if this link already contributed
                if ( linkmask(L1).ne.L ) then
                   zz = cosphiunet(L1)
                   if ( zz.ne.DMISS ) then
                      dorthosum = dorthosum + abs(zz)**P
                      num = num+1
                   end if
                   linkmask(L1) = L ! used for link L
                end if
             end do
          end do

          if ( dorthosum.gt.dmaxorthop ) then
!            store neighboring cell administration
             call local_netstore( (/ k1, k2 /) )
             xz_st = (/ xz(k1), xz(k2) /)
             yz_st = (/ yz(k1), yz(k2) /)

!            merge cells
             call mergecells(k1,k2,jatek)

!            compute new circumcenters
             call getcellcircumcenter(k1, xz(k1), yz(k1), zz)
             call getcellcircumcenter(k2, xz(k2), yz(k2), zz)

!            compute new orthogonality
             numnew = 0
             dorthosumnew = 0d0
             do kk=1,2
                k = lne(kk,L)
                do LL=1,netcell(k)%N
                   L1 = netcell(k)%lin(LL)
!                  check with mask if this link already contributed
                   if ( linkmask(L1).ne.-L ) then
                      zz = cosphiunet(L1)
                      if ( zz.ne.DMISS ) then
                         dorthosumnew = dorthosumnew + abs(zz)**P
                         numnew = numnew+1
                      end if
                      linkmask(L1) = -L ! used for link L
                   end if
                end do
             end do

!            restore if orthogonality increased
             if ( dorthosumnew.ge.dorthosum ) then
!               restore
                call local_netrestore()
                xz(k1) = xz_st(1)
                yz(k1) = yz_st(1)

                xz(k2) = xz_st(2)
                yz(k2) = yz_st(2)
             end if
          end if
       end if
    end do

1234 continue

!   deallocate
    call local_netdealloc()

    if ( allocated(linkmask) ) deallocate(linkmask)
    if ( allocated(dortho)   ) deallocate(dortho)
    if ( allocated(iperm)    ) deallocate(iperm)

    return
 end subroutine del_badortholinks
