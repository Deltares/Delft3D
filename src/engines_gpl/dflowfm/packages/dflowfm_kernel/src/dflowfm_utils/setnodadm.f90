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
module m_set_nod_adm
   implicit none
contains
!> Toplevel setnodadm routine wraps:
!! * original setnodadm(), for network_data administration.
!! * update_flow1d_admin(), to remove any net links from
!!   the flow1d::network administration, if they were also
!!   removed from network_data in the first step.
subroutine setnodadm(jacrosscheck_)
   use gridoperations
   use m_network
   use m_save_ugrid_state, only: contactnlinks, contactnetlinks, netlink2contact, hashlist_contactids
   use unstruc_channel_flow, only: network
   use network_data, only: lc, numl1d, Lperminv, nlinkremoved
   use m_alloc
   use messagehandling, only: msgbuf

   integer, intent(in) :: jacrosscheck_ !< Whether or not to remove any crossing netlinks.

   integer :: L, LL, Lnew, Ltoberemoved, Linc

   call setnodadm_grd_op(10 + jacrosscheck_)

   ! Update netlink numbers for all 1d2d contacts, after netlinks may have been permuted:
   ! Also, contruct now the complete inverse mapping from net links to contacts
   call realloc(netlink2contact, numl1d, keepExisting=.false., fill=0)
   if (contactnlinks > 0) then
      Ltoberemoved = 1 ! used later, for checking if the length is 0
      do LL = 1, contactnlinks
         L = contactnetlinks(LL)
         Lnew = Lperminv(L)
         contactnetlinks(LL) = Lnew

         netlink2contact(Lnew) = LL

         ! Check if this link's length is 0.
         ! If L is in array LC (filled in subroutine setnodadm_grd_op), then this link has 0 length and we write a warning message.
         do Linc = Ltoberemoved, nlinkremoved
            if (LC(Linc) > L) then ! Link numbers in LC are ascending, so link L will not be present in the remaining LC values.
               exit
            else if (LC(Linc) == L) then ! 1D2D link #LL was removed by setnodadm_grd_op()
               write (msgbuf, '(a,a,a)') '1D2D contact link ''', trim(hashlist_contactids%id_list(LL)), ''' has length of 0, this link will be ignored (removed).'
               call warn_flush()

               Ltoberemoved = Linc + 1 ! remember for next LL loop.
               exit
            end if
         end do ! Linc
      end do ! LL
   end if

   if (nlinkremoved > 0) then
      call update_flow1d_admin(network, lc)
   end if

end subroutine setnodadm
end module m_set_nod_adm
