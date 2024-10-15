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
module m_reasam
   implicit none
contains
   subroutine REASAM(MSAM, JADOORLADEN)
      use M_MISSING
      use M_SAMPLES
      use m_alloc
      use ieee_arithmetic, only: ieee_is_nan
      use m_drawthis
      use m_pharosflow
      use m_readyy
      use m_qnerror
      use m_get_samples_boundingbox

      integer, intent(inout) :: msam !< already opened file pointer to sample file
      integer, intent(in) :: jadoorladen !< whether to append to global set (1) or start empty (0)
      integer :: ierr
      integer :: nkol
      integer :: nrow
      integer :: nsm
      integer :: num
      integer :: K, K0
      double precision :: x, y, z
      double precision :: XX, YY, ZZ, ZZ2
      character REC * 132, TEX * 10
      logical THISISANUMBER

      call SAVESAM()
      NSM = 0
      MXSAM = 0
      MYSAM = 0
      IPSTAT = IPSTAT_NOTOK
      nkol = 0
      call READYY('Counting nr. of Samples ', 0d0)
11    read (MSAM, '()', end=31)
      NSM = NSM + 1
      goto 11
31    NSMAX = 1.2d0 * (NSM + JADOORLADEN * NS)
      if (NSMAX > 100000) NDRAW(32) = 7
      if (NSMAX > 500000) NDRAW(32) = 3
      if (allocated(XS)) deallocate (XS, YS, ZS)
      allocate (XS(NSMAX), YS(NSMAX), ZS(NSMAX), STAT=IERR)
      call AERR('XS(NSMAX),YS(NSMAX),ZS(NSMAX)', IERR, NSMAX)
      if (allocated(ipsam)) deallocate (ipsam)
      allocate (ipsam(NSMAX), stat=ierr)
      call aerr('ipsam(NSMAX)', ierr, NSMAX)
      call READYY(' ', -1d0)

      rewind (MSAM)

      write (TEX, '(I10)') NSM
      call READYY('Reading '//trim(TEX)//' Sample Points', 0d0)
      if (JADOORLADEN == 0) then
         call XMISAR(XS, NSMAX)
         call XMISAR(YS, NSMAX)
         call MISAR(ZS, NSMAX)
         K = 0
      else
         call RESTORESAM()
         K = NS
      end if
      K0 = K

!    check of dit een PHAROS file is
      JFLOW = 1
14    read (MSAM, '(A)', end=30) REC1
      if (rec1(1:1) == '*') goto 14

      if (.not. (THISISANUMBER(REC1))) then
         read (MSAM, '(A)', end=30) REC
         if (THISISANUMBER(REC)) then
            read (REC, *, ERR=16) NROW, NKOL
            goto 15
16          continue
            read (MSAM, '(A)', end=30) REC
            read (REC, *, ERR=15) NUM, X, Y, Z
            JFLOW = 3
         end if
      end if
15    continue

      rewind (MSAM)

      KMOD = max(1, NSM / 100)
10    continue
      read (MSAM, '(A)', end=30) REC
      if (REC(1:1) == '*') goto 10
      if (.not. (THISISANUMBER(REC))) then
!        we nemen aan dat er net een blokcode is gelezen
!        en we lezen meteen de nrow ncol regel, maar checken die regel niet
         read (MSAM, '(A)', end=30) REC
      else

         if (JFLOW == 3) then
            read (REC, *, ERR=40) NUM, XX, YY, ZZ
         else if (NKOL == 4) then
            read (REC, *, ERR=40) XX, YY, ZZ, ZZ2
            if (zz /= -999d0) then
               zz = sqrt(zz * zz + zz2 * zz2)
            end if
         else
            read (REC, *, end=40) XX, YY, ZZ
            read (REC, *, ERR=40) XX, YY, ZZ
         end if

         if (K <= NSMAX - 1 .and. XX /= XYMIS .and. &
             ZZ /= dmiss .and. ZZ /= 999.999d0 .and. &
             .not. (ieee_is_nan(XX) .or. ieee_is_nan(YY) .or. ieee_is_nan(ZZ))) then
            K = K + 1
            NS = K
            XS(K) = XX
            YS(K) = YY
            ZS(K) = ZZ
         end if
         if (mod(K - K0, KMOD) == 0) then
            call READYY(' ', min(1d0, dble(K) / NSM))
         end if
      end if
      goto 10

40    continue
      write (TEX, '(I10)') K
      call QNERROR('ERROR READING SAMPLES FILE LINE NR ', TEX, REC)

30    continue
      if (K > NSMAX) then
         write (TEX, '(I8)') NSMAX
         call QNERROR('ONLY', TEX, 'SAMPLE POINTS CAN BE LOADED')
         write (TEX, '(I8)') K
         call QNERROR('YOU TRIED TO LOAD', TEX, 'SAMPLE POINTS')
      end if
      call READYY(' ', -1d0)
      write (TEX, '(I10)') NS
      call READYY('Sorting '//trim(TEX)//' Samples Points', 0d0)
      if (NS > 1) then
         call TIDYSAMPLES(XS, YS, ZS, IPSAM, NS, MXSAM, MYSAM)
         call get_samples_boundingbox()
         IPSTAT = IPSTAT_OK
      end if
      call READYY(' ', -1d0)
      call doclose(MSAM)
      return
   end
end module m_reasam
