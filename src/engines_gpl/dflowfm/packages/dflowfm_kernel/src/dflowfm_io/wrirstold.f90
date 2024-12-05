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

module m_wrirstold

   implicit none

   private

   public :: WRIRSTold

contains

   subroutine WRIRSTold(MOUT)
      use M_FLOWTIMES
      use M_FLOW
      use M_FLOWGEOM
      use unstruc_model
      use m_sediment, only: jaceneqtr
      use unstruc_netcdf, only: unc_write_net
      use m_transport
      use m_get_kbot_ktop
      implicit none
      integer :: MOUT, k, kk, kb, kt

      ! WRITE(MOUT,'(a,2x,F25.14,2i10,a)') REFDAT, TIME1,  NDX, LNX, ' (refdat, timsec, ndx, lnx)'

      if (jagrw < 2) then
         !    WRITE(MOUT,'(A,I10)') 'S1 ', NDX, ' 3'
         do K = 1, NDX
            write (MOUT, *) XZ(K), YZ(K), S1(K)
         end do
      else
         !   WRITE(MOUT,'(A,I10)') 'S1 ', NDX, ' 4'
         do K = 1, NDX
            write (MOUT, *) XZ(K), YZ(K), S1(K), SGRW1(K)
         end do
      end if

      ! WRITE(MOUT,'(A,I10)') 'U1 ', LNX

      ! DO L = 1,LNX
      !   WRITE(MOUT,*) U1(L)
      ! ENDDO

      call doclose(mout)

      if (jasal > 0) then
         call newfil(mout, trim(getoutputdir())//trim(md_ident)//'_'//'_salbot.xyz')
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            write (mout, *) xz(kk), yz(kk), constituents(isalt, kb)
         end do
         call doclose(mout)

         if (kmx > 1) then
            call newfil(mout, trim(getoutputdir())//trim(md_ident)//'_'//'_saltop.xyz')
            do kk = 1, ndxi
               call getkbotktop(kk, kb, kt)
               write (mout, *) xz(kk), yz(kk), constituents(isalt, kt)
            end do
            call doclose(mout)
         end if
      end if

      if (jased > 0) then
         if (jaceneqtr /= 1) then
            call unc_write_net(trim(getoutputdir())//trim(md_ident)//'_'//'_new_net.nc') ! write resulting bathymetry
         end if
      end if

   end subroutine WRIRSTold

end module m_wrirstold
