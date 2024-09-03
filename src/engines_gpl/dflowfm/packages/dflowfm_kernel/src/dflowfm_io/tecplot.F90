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
!> for Tecplot output
module m_tecplot
   implicit none
#ifdef HAVE_TECPLOT
   include 'tecio.f90'

! tecpolyface142 is missing in tecio.f90
   interface
      integer function tecpolyface142 &
         (numfaces, ifacenodecounts, ifacenodes, ifaceleftelems, ifacerightelems)
         !MS$ATTRIBUTES STDCALL :: tecpolyface142
         !MS$ATTRIBUTES REFERENCE :: numfaces
         !MS$ATTRIBUTES REFERENCE :: ifacenodecounts
         !MS$ATTRIBUTES REFERENCE :: ifacenodes
         !MS$ATTRIBUTES REFERENCE :: ifaceleftelems
         !MS$ATTRIBUTES REFERENCE :: ifacerightelems
         integer :: numfaces
         integer :: ifacenodecounts(*)
         integer :: ifacenodes(*)
         integer :: ifaceleftelems(*)
         integer :: ifacerightelems(*)
      end function tecpolyface142
   end interface

   character :: NULLCHR

   integer, pointer, dimension(:)  :: Nulli => null()

   integer :: ifileformat = 0 ! 0: .plt, 1: .szplt
   integer :: ifiletype = 1 ! 0: full, 1: grid, 2: solution
   integer :: jadebug = 1
   integer :: jadouble = 0 ! write single precision for now

   integer :: izonetype = 6 ! FEPOLYGON
   integer :: icellmax = 0; ! not used
   integer :: jcellmax = 0; ! not used
   integer :: kcellmax = 0; ! not used
   double precision :: soltime = 0d0
   integer :: istrandid = 0 ! static zone
   integer :: iparentzn = 0 ! no parent zone
   integer :: jablock = 1 ! block
   integer :: nfconns = 0
   integer :: ifnmode = 0

   integer :: numfaces = 0 ! will be filled by wrinet_tecplot
   integer :: numnodes = 0 ! will be filled by wrinet_tecplot

   integer :: numfacenodes = 0
   integer :: numbfaces = 0
   integer :: numbconnections = 0
   integer :: ishrconn = 0

   integer, dimension(:), allocatable :: ifacenodes, ifaceleftelems, ifacerightelems
   integer, dimension(:), allocatable :: kmask, Lmask ! node- and linkmask

   integer, parameter :: ITECSTAT_OK = 0, ITECSTAT_EMPTY = 1, ITECSTAT_ERROR = 2
   integer :: itecstat = ITECSTAT_EMPTY

contains

!  write array to Tecplot file
   subroutine tecdat(num, var, ierr, kmask, miss)
      use m_missing
      implicit none

      integer, intent(in) :: num ! data size
      double precision, dimension(num), intent(in) :: var ! data to be written
      integer, intent(out) :: ierr ! error (1) or not (0)
      integer, dimension(num), optional, intent(in) :: kmask ! mask
      double precision, optional, intent(in) :: miss ! missing value

      real, dimension(:), allocatable :: xx

      logical :: Lhasmiss
      logical :: Lhasmask

      integer :: i, k

      allocate (xx(num))

      Lhasmiss = present(miss)
      Lhasmask = present(kmask)

      i = 0
      do k = 1, num
         if (Lhasmask) then
            if (kmask(k) == 0) then
               cycle
            end if
         end if

         i = i + 1
         xx(i) = var(k)
         if (Lhasmiss .and. (xx(i) == DMISS)) then
            xx(i) = miss
         end if
      end do
      ierr = tecdat142(i, xx, 0)

      if (allocated(xx)) deallocate (xx)

      return
   end subroutine tecdat
#endif
end module m_tecplot

!>  write net to Tecplot file
subroutine wrinet_tecplot(FNAM)
   use m_tecplot
   use network_data
   use unstruc_messages
   use m_partitioninfo
   use m_qnerror

   implicit none

   character(len=*), intent(in) :: FNAM

#ifdef HAVE_TECPLOT

   double precision, dimension(:), allocatable :: dum

   integer, dimension(4) :: ValueLocation

   integer :: ierr
   integer :: k

   ierr = 1

   ishrconn = 0
   ifiletype = 1 ! grid

   if (itecstat /= ITECSTAT_OK) then
      call ini_tecplot()
   end if

   if (itecstat /= ITECSTAT_OK) then
      goto 1234
   end if

!     open file
   if (jampi /= 1) then
      ierr = tecini142('test'//NULLCHR, 'x y z'//NULLCHR, trim(FNAM)//NULLCHR, &
                       '.'//NULLCHR, ifileformat, ifiletype, jadebug, jadouble)

      if (ierr /= 0) goto 1234

      ierr = teczne142('polygonal zone'//NULLCHR, izonetype, numnodes, nump, numfaces, &
                       icellmax, jcellmax, kcellmax, soltime, istrandid, iparentzn, jablock, &
                       nfconns, ifnmode, numfacenodes, numbfaces, numbconnections, &
                       NULLi, NULLi, NULLi, ishrconn)
   else
      ValueLocation = (/1, 1, 1, 0/)
      ierr = tecini142('test'//NULLCHR, 'x y z imask'//NULLCHR, trim(FNAM)//NULLCHR, &
                       '.'//NULLCHR, ifileformat, ifiletype, jadebug, jadouble)

      if (ierr /= 0) goto 1234

      ierr = teczne142('polygonal zone'//NULLCHR, izonetype, numnodes, nump, numfaces, &
                       icellmax, jcellmax, kcellmax, soltime, istrandid, iparentzn, jablock, &
                       nfconns, ifnmode, numfacenodes, numbfaces, numbconnections, &
                       NULLi, ValueLocation, NULLi, ishrconn)
   end if

   if (ierr /= 0) goto 1234

!     write nodal data
   call tecdat(numk, xk, ierr, kmask=kmask)
   call tecdat(numk, yk, ierr, kmask=kmask)
   call tecdat(numk, zk, ierr, kmask=kmask, miss=zkuni)

   if (jampi == 1) then
      allocate (dum(nump))
      dum = 1d0
      do k = 1, nump
         if (idomain(k) /= my_rank) then
            dum(k) = 0d0
         end if
      end do
      call tecdat(nump, dum, ierr)
      deallocate (dum)
   end if

!     write connectivity
   ierr = tecpolyface142(numfaces, NULLi, ifacenodes, ifaceleftelems, ifacerightelems)

   if (ierr /= 0) goto 1234

!     close file
   ierr = tecend142()

!     deallocate
   if (allocated(ifacenodes)) deallocate (ifacenodes)
   if (allocated(ifaceleftelems)) deallocate (ifaceleftelems)
   if (allocated(ifacerightelems)) deallocate (ifacerightelems)

1234 continue
#else
   call qnerror('Tecplot output not available', ' ', ' ')

#endif

   return
end subroutine wrinet_tecplot

!>  write flow solution as cell-centered data to Tecplot file
!>    it is assumed that the mesh and connectivity are written in the net file
!>    it is also assumed that the flow-node numbering and netcell numbering are the same
subroutine wrimap_tecplot(FNAM)
   use m_tecplot
   use m_flow, only: s1, ucx, ucy, vih
   use m_flowgeom
   use network_data, only: nump
   use m_flowtimes, only: time1
   use unstruc_messages
   use gridoperations

   implicit none

   character(len=*), intent(in) :: FNAM

#ifdef HAVE_TECPLOT

   integer, dimension(:), allocatable :: cellmask

   integer :: k
   integer :: ierr

   ierr = 1

   ifiletype = 2 ! solution

!     check if Ndxi equals nump
   if (Ndxi /= nump) then
      goto 1234
   end if

   if (itecstat /= ITECSTAT_OK) then
      call ini_tecplot()
   end if

!     make mask
   allocate (cellmask(Ndx))

   cellmask = 0
   do k = 1, Ndxi
      cellmask(k) = 1
   end do

!     open file
   ierr = tecini142('test'//NULLCHR, 's1 ucx ucy bl vih'//NULLCHR, trim(FNAM)//NULLCHR, &
                    '.'//NULLCHR, ifileformat, ifiletype, jadebug, jadouble)

   if (ierr /= 0) goto 1234

   numfacenodes = 0
   istrandid = 1
   ishrconn = 0
   soltime = time1

   ierr = teczne142('polygonal zone'//NULLCHR, izonetype, numnodes, nump, numfaces, &
                    icellmax, jcellmax, kcellmax, soltime, istrandid, iparentzn, jablock, &
                    nfconns, ifnmode, numfacenodes, numbfaces, numbconnections, &
                    NULLi, (/0, 0, 0, 0, 0/), NULLi, ishrconn)

!     write cell-centered data
   call tecdat(Ndx, s1, ierr, kmask=cellmask)
   call tecdat(Ndx, ucx, ierr, kmask=cellmask)
   call tecdat(Ndx, ucy, ierr, kmask=cellmask)
   call tecdat(Ndx, bl, ierr, kmask=cellmask)
   call tecdat(Ndx, vih, ierr, kmask=cellmask)

   continue

!     close file
   ierr = tecend142()

1234 continue

   if (allocated(cellmask)) deallocate (cellmask)
#else
   call mess(LEVEL_ERROR, 'Tecplot output not available')
#endif

   return
end subroutine wrimap_tecplot

subroutine ini_tecplot()
   use m_tecplot
   use network_data
   use gridoperations
   implicit none

#ifdef HAVE_TECPLOT

   integer :: i, k, L

   NULLCHR = char(0)

   ifileformat = 0 ! 0: .plt, 1: .szplt
   ifiletype = 1 ! 0: full, 1: grid, 2: solution
   jadebug = 1
   jadouble = 0 ! write single precision for now

   izonetype = 6 ! FEPOLYGON
   icellmax = 0; ! not used
   jcellmax = 0; ! not used
   kcellmax = 0; ! not used
   soltime = 0d0
   istrandid = 0 ! static zone
   iparentzn = 0 ! no parent zone
   jablock = 1 ! block
   nfconns = 0
   ifnmode = 0

   numfaces = 0 ! will be filled by wrinet_tecplot
   numnodes = 0 ! will be filled by wrinet_tecplot

   numfacenodes = 0
   numbfaces = 0
   numbconnections = 0
   ishrconn = 0

   itecstat = ITECSTAT_EMPTY

!     prepare connectivity

   if (netstat /= NETSTAT_OK) then
      call findcells(0)
   end if

   if (nump < 1 .or. numk < 1 .or. numL < 1) goto 1234

!     deallocate
   if (allocated(kmask)) deallocate (kmask)
   if (allocated(Lmask)) deallocate (Lmask)
   if (allocated(cellmask)) deallocate (cellmask)
   if (allocated(ifacenodes)) deallocate (ifacenodes)
   if (allocated(ifaceleftelems)) deallocate (ifaceleftelems)
   if (allocated(ifacerightelems)) deallocate (ifacerightelems)

!     allocate
   allocate (kmask(numk))
   allocate (Lmask(numL))
   allocate (cellmask(nump))

!     count number of active faces and make masks
   numfaces = 0
   kmask = 0
   Lmask = 0

   do L = 1, numL
      if (kn(3, L) /= 0 .and. lne(1, L) /= 0) then
         numfaces = numfaces + 1
         Lmask(L) = numfaces
         kmask(kn(1, L)) = 1
         kmask(kn(2, L)) = 1
      end if
   end do

!     count active nodes and get new numbers
   numnodes = 0
   do k = 1, numk
      if (kmask(k) == 1) then
         numnodes = numnodes + 1
         kmask(k) = numnodes ! store new node number
      end if
   end do

   numfacenodes = 2 * numfaces

!     compose connectivety
   allocate (ifacenodes(numfacenodes))
   allocate (ifaceleftelems(numfaces))
   allocate (ifacerightelems(numfaces))

   i = 0
   do L = 1, numL
      if (Lmask(L) > 0) then
         i = i + 1
         ifacenodes(2 * i - 1) = kmask(kn(1, L)) ! new node number
         ifacenodes(2 * i) = kmask(kn(2, L)) ! new node number
         ifaceleftelems(i) = lne(1, L)
         ifacerightelems(i) = lne(2, L)
      end if
   end do

   itecstat = ITECSTAT_OK

1234 continue

#endif

   return
end subroutine ini_tecplot

