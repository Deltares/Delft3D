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

module m_loadbitmap

   implicit none

contains

   subroutine LOADBITMAP(FILNAM)
      use m_bitmap
      use m_wearelt
      use string_module, only: find_first_letter, find_first_char
      use m_drawthis
      use m_qnerror
      use m_qn_read_error
      use m_qn_eof_error

      integer :: ierr
      integer :: k
      integer :: k1
      integer :: k2
      integer :: l
      integer :: minp
      integer :: num
      integer :: numbersonline
      logical JAWEL
      integer INFO(10)
      character FILNAM * (*), REC * 132

      K1 = find_first_char(FILNAM)
      K2 = len_trim(FILNAM)
      call IGRFILEINFO(FILNAM(K1:K2), info, 3)
      MXP = INFO(2)
      NXP = INFO(3)

      XB = 0
      YB = 0

      NDRAW(26) = 0
      allocate (IPIX(1), STAT=IERR)
      if (MXP >= 1 .and. NXP >= 1) then
         deallocate (IPIX)
         allocate (IPIX(MXP * NXP), STAT=IERR)
!        CALL AERR('IPIX(MXP*NXP)',IERR,MXP*NXP)
         if (IERR /= 0) then
            call QNERROR('BITMAP TOO LARGE', ' ', ' ')
         else
            call IGRLOADIMAGEDATA(FILNAM(K1:K2), IPIX)
         end if

         L = index(FILNAM, '.')
         inquire (FILE=FILNAM(K1:L)//'xyx', EXIST=JAWEL)

         if (JAWEL) then
            call OLDFIL(MINP, FILNAM(K1:L)//'xyx')
            read (MINP, '(A)', end=999) REC
            NUM = NUMBERSONLINE(REC)
            if (NUM == 4) then
               read (REC, *, ERR=888) XP(1), YP(1), XP(3), YP(3)
               XP(2) = XP(3)
               YP(2) = YP(1)
               XP(4) = XP(1)
               YP(4) = YP(3)
            else if (NUM == 3) then
               read (REC, *, ERR=777) XP(1), YP(1), XP(3)
               YP(3) = YP(1) + (XP(3) - XP(1)) * dble(NXP) / dble(MXP)
               XP(2) = XP(3)
               YP(2) = YP(1)
               XP(4) = XP(1)
               YP(4) = YP(3)
            else
               if (FIND_FIRST_LETTER(REC) == 1) then
                  read (MINP, '(A)', end=999) REC
                  do K = 1, 4
                     read (MINP, '(A)', end=999) REC
                     if (NUMBERSONLINE(REC) == 2) then
                        read (REC, *, ERR=666) XP(K), YP(K)
                     else if (NUMBERSONLINE(REC) == 4) then
                        read (REC, *, ERR=555) XP(K), YP(K), XB(K), YB(K)
                        YB(K) = NXP - YB(K) + 1
                     end if
                  end do
               else
                  call QNERROR('Cannot Read *.xyx File', ' ', ' ')
               end if
            end if
            call doclose(MINP)
         else
            XP(1) = 0
            YP(1) = 0
            XP(3) = MXP
            YP(3) = NXP
            XP(2) = XP(3)
            YP(2) = YP(1)
            XP(4) = XP(1)
            YP(4) = YP(3)
         end if
         NDRAW(26) = 1
      end if

      if (XB(1) == 0) XB(1) = -0.5d0
      if (YB(1) == 0) YB(1) = -0.5d0
      if (XB(2) == 0) XB(2) = MXP + 0.5d0
      if (YB(2) == 0) YB(2) = -0.5d0
      if (XB(3) == 0) XB(3) = MXP + 0.5d0
      if (YB(3) == 0) YB(3) = NXP + 0.5d0
      if (XB(4) == 0) XB(4) = -0.5d0
      if (YB(4) == 0) YB(4) = NXP + 0.5d0

      return

999   call QNEOFERROR(MINP)
      call doclose(MINP)
      return

998   call QNREADERROR('Trying to Read X1,Y1,X2,XY2,X3,Y3,X4,Y4 but', 'get:'//REC, MINP)
      call doclose(MINP)
      return

888   call QNREADERROR('Trying to Read X1,Y1,X3,Y3 but Get:', REC, MINP)
      call doclose(MINP)
      return

777   call QNREADERROR('Trying to Read X1,Y1,X3 but Getting', REC, MINP)
      call doclose(MINP)
      return

666   call QNREADERROR('Trying to Read four lines X,Y but Getting', REC, MINP)
      call doclose(MINP)
      return

555   call QNREADERROR('Trying to Read four lines X,Y,MP,NP but Get', REC, MINP)
      call doclose(MINP)
      return
   end

end module m_loadbitmap
