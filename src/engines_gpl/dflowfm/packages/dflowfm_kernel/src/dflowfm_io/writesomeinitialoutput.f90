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

module m_writesomeinitialoutput

   implicit none

   private

   public writesomeinitialoutput

contains

   subroutine writesomeinitialoutput()
      use precision, only: dp
      use m_flow
      use m_flowgeom
      use m_flowtimes, only: rundat0
      use messagehandling, only: msgbuf, msg_flush
      use m_partitioninfo
      use m_samples, only: ns, xs, ys, zs
      use unstruc_model, only: md_ident
      use gridoperations, only: network_dimensions_message
      use m_reasam
      use m_datum
      use m_dbdistance_hk

      implicit none
      integer :: k, mbalat, mwrong, L, msam, n, nf, jacheckba = 0
      real(kind=dp) :: batotown(1), batot(1), voltotown(1), volto(1), dist, dismin

      batotown = 0; voltotown = 0

      ! Stop for initialisation timer and start timer for actual computation time
      call datum(rundat0)

      write (msgbuf, '(a,a)') 'Modelinit finished   at: ', rundat0; call msg_flush()

      msgbuf = ' '; call msg_flush()
      msgbuf = ' '; call msg_flush()

      call network_dimensions_message()
      call flowgeom_dimensions_message()

      write (msgbuf, '(a,I25)') 'nr of closed walls     ( )  :', mxwalls; call msg_flush()

      if (kmx > 0) then
         write (msgbuf, '(a,I25)') 'max nr of layers       ( )  :', kmx; call msg_flush()
         write (msgbuf, '(a,I25)') 'nr of 3D cells         ( )  :', ndkx - 2 * ndx; call msg_flush()
         write (msgbuf, '(a,I25)') 'nr of 3D links         ( )  :', Lnkx - 2 * lnx; call msg_flush()
      end if

      msgbuf = ' '; call msg_flush()
      msgbuf = ' '; call msg_flush()

      if (jacheckba == 1) then
         if (jampi == 1) then
            L = index(md_ident, '_0') - 1
            call oldfil(msam, 'ba_'//trim(md_ident(1:L))//'.xyz')
            call reasam(msam, 0)
            call newfil(mwrong, 'bawrong_'//trim(md_ident)//'.xyz')
         end if
         call newfil(mbalat, 'ba_'//trim(md_ident)//'.xyz')
      end if

      do K = 1, NDXI
         if (jampi == 1) then
            if (idomain(k) == my_rank) then
               batotown(1) = batotown(1) + ba(k)
               voltotown(1) = voltotown(1) + vol1(k)
               if (jacheckba == 1) then
                  write (mbalat, *) xz(k), yz(k), ba(k)
                  if (ns > 0) then
                     dismin = 1d9; nf = 0
                     do n = 1, ns
                        call dbdistancehk(xz(k), yz(k), xs(n), ys(n), dist)
                        if (dist < dismin) then
                           dismin = dist
                           nf = n
                        end if
                     end do
                     if (nf > 0 .and. dismin < 1d0) then
                        if (abs(ba(k) - zs(nf)) > 1d-4) then
                           write (mwrong, '(4F20.5)') xz(k), yz(k), ba(k), zs(nf)
                        end if
                     end if
                  end if
               end if
            end if
         else
            batotown(1) = batotown(1) + ba(k)
            voltotown(1) = voltotown(1) + vol1(k)
            if (jacheckba == 1) then
               write (mbalat, *) xz(k), yz(k), ba(k)
            end if
         end if
      end do

      if (jacheckba == 1) then
         call doclose(mbalat)
         if (jampi == 1) then
            call doclose(mwrong)
         end if
      end if

      write (msgbuf, '(a,E25.10)') 'my model area          (m2) :', batotown; call msg_flush()
      if (jampi == 1) then
         k = 1
         call reduce_double_sum(k, batotown, batot)
         write (msgbuf, '(a,E25.10)') 'total model area       (m2) :', batot; call msg_flush()
      end if

      write (msgbuf, '(a,E25.10)') 'my model volume        (m3) :', voltotown; call msg_flush()
      if (jampi == 1) then
         k = 1
         call reduce_double_sum(k, voltotown, volto)
         write (msgbuf, '(a,E25.10)') 'total model volume     (m3) :', volto; call msg_flush()
      end if

      msgbuf = ' '; call msg_flush()
      msgbuf = ' '; call msg_flush()

   end subroutine writesomeinitialoutput

   subroutine flowgeom_dimensions_message()
      use MessageHandling
      use m_flowgeom
      implicit none

      if (ndx > 0) then
         write (msgbuf, '(a,I25)') 'nr of flownodes              ( )  :', ndx; call msg_flush()
         write (msgbuf, '(a,I25)') 'nr of internal flownodes     ( )  :', ndxi; call msg_flush()
         write (msgbuf, '(a,I25)') 'nr of 2D internal flownodes  ( )  :', ndx2D; call msg_flush()
         write (msgbuf, '(a,I25)') 'nr of 1D internal flownodes  ( )  :', ndxi - ndx2D; call msg_flush()
         msgbuf = ' '; call msg_flush()

         write (msgbuf, '(a,I25)') 'nr of openbnd cells       ( )     :', ndx - ndxi; call msg_flush()
         write (msgbuf, '(a,I25)') 'nr of 2D boundaries       ( )     :', ndx - ndx1Db; call msg_flush()
         write (msgbuf, '(a,I25)') 'nr of 1D boundaries       ( )     :', ndx1Db - ndxi; call msg_flush()
         write (msgbuf, '(a,I25)') 'nr of flowlinks           ( )     :', Lnx; call msg_flush()
         write (msgbuf, '(a,I25)') 'nr of internal links      ( )     :', Lnxi; call msg_flush()
         write (msgbuf, '(a,I25)') 'nr of 1D links            ( )     :', Lnx1D; call msg_flush()
         msgbuf = ' '; call msg_flush()
      end if

   end subroutine flowgeom_dimensions_message

end module m_writesomeinitialoutput
