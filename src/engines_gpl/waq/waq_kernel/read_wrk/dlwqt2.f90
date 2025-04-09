!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_dlwqt2
   use m_waq_precision

   implicit none

contains

   subroutine DLWQT2(input_file, LUNOUT, ITIME, result, NTOTAL, LUNTXT, ISFLAG, IFFLAG, ONLINE)
      ! Makes values at ITIME for user supplied binary intermediate files
      !
      !     LOGICAL UNITNUMBERS : input_file  - input unit intermediate file
      !                           LUNOUT - monitor file
      !
      !     SUBROUTINES CALLED  : stop_with_error, stops execution
      !
      !     PARAMETERS          :
      !
      !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     input_file   INTEGER       1     INPUT   unit number intermediate file
      !     LUNOUT  INTEGER       1     INPUT   unit number monitor file
      !     ITIME   INTEGER       1     INPUT   Model timer
      !     RESULT  REAL     NTOTAL     OUTPUT  result array at time ITIME
      !     NTOTAL  INTEGER       1     INPUT   number of items to be filled
      !     LUNTXT  CHAR*(*)      1     INPUT   text concerning unit numbers
      !     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
      !     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
      use m_logger_helper, only: stop_with_error
      use timers

      real(kind=real_wp) :: result(NTOTAL)
      integer(kind=int_wp) :: input_file, LUNOUT, ITIME, NTOTAL, ISFLAG, IFFLAG
      character(len=10) MSGTXT(3)
      character(len=*) LUNTXT
      logical ONLINE
      data MSGTXT/' REWIND   ', ' CONSTANT ', ' ERROR    '/
      logical stream_access ! help variable to detect the type of file access
      character(20) access ! help variable to detect the type of file access

      integer(kind=int_wp) :: ierr, itime1, messge
      integer(kind=int_wp) :: ithandl = 0
      if (timon) call timstrt("dlwqt2", ithandl)

      if (ONLINE) then
         if (input_file == 20) write (*, *) ' Read VOLUME record'
         if (input_file == 24) write (*, *) ' Read FLOW   record'
      end if
      !
      !         is this the first time?
      !         BYPASS FOR ONLINE MODE, TO AVOID APPARENT CONSTANT FUNCTION
      !
      MESSGE = 0
      if (IFFLAG == 1 .and. .not. ONLINE) goto 20
      !
      !         normal time varying read
      !
      read (input_file, end=10, ERR=40) ITIME1, result
      goto 9999 !   RETURN
      !
      !         normal rewind.
      !
10    MESSGE = 1
      if (ONLINE) stop 'REWIND NOT POSSIBLE IN ON-LINE MODE'
      inquire (input_file, access=access)
      stream_access = access == 'STREAM'
      if (stream_access) then
         read (input_file, iostat=ierr, pos=1)
      else
         rewind input_file ! Start at the beginning again
      end if
      read (input_file, end=40, ERR=40) ITIME1, result
      goto 50
      !
      !         This is the first time, check only for nr of records.
      !
20    continue
      read (input_file, end=40, ERR=40) ITIME1, result
      read (input_file, end=30, ERR=40) ITIME1, result
      inquire (input_file, access=access)
      stream_access = access == 'STREAM'
      if (stream_access) then
         read (input_file, iostat=ierr, pos=1)
      else
         rewind input_file ! Start at the beginning again
      end if
      read (input_file, end=30, ERR=40) ITIME1, result
      goto 9999 !   RETURN
      !
      !         file has only one record, array is constant
      !
30    MESSGE = 2
      inquire (input_file, access=access)
      stream_access = access == 'STREAM'
      if (stream_access) then
         read (input_file, iostat=ierr, pos=1)
      else
         rewind input_file ! Start at the beginning again
      end if
      read (input_file, end=40, ERR=40) ITIME1, result
      IFFLAG = -1
      goto 50
      !
      !         error, during read
      !
40    MESSGE = 3
50    if (ISFLAG == 1) then
         write (LUNOUT, 2010) MSGTXT(MESSGE), input_file, LUNTXT, &
            ITIME / 86400, mod(ITIME, 86400) / 3600, &
            mod(ITIME, 3600) / 60, mod(ITIME, 60), &
            ITIME1 / 86400, mod(ITIME1, 86400) / 3600, &
            mod(ITIME1, 3600) / 60, mod(ITIME1, 60)
      elseif (ISFLAG == 2) then
         write (LUNOUT, 2020) MSGTXT(MESSGE), input_file, LUNTXT, &
            ITIME / 31536000, &
            mod(ITIME, 31536000) / 86400, &
            mod(ITIME, 86400) / 3600, &
            mod(ITIME, 3600) / 60, &
            mod(ITIME, 60), &
            ITIME1 / 31536000, &
            mod(ITIME1, 31536000) / 86400, &
            mod(ITIME1, 86400) / 3600, &
            mod(ITIME1, 3600) / 60, &
            mod(ITIME1, 60)
      else
         write (LUNOUT, 2000) MSGTXT(MESSGE), input_file, LUNTXT, &
            ITIME, ITIME1
      end if
      if (MESSGE < 3) goto 9999 !   RETURN
      call stop_with_error()
9999  if (timon) call timstop(ithandl)
      return
      !
2000  format(A10, 'ON UNIT:', I10, ', READING: ', A, / &
             ' SIMULATION TIME :', I10, ' !  TIME IN FILE: ', I10, ' !')
2010  format(A10, 'ON UNIT:', I10, ', READING: ', A, / &
             ' SIMULATION TIME :', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S ! ', / &
             ' TIME IN FILE    :', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S ! ')
2020  format(A10, 'ON UNIT:', I10, ', READING: ', A, / &
             ' SIMULATION TIME :', I2, 'Y ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S .', / &
             ' TIME IN FILE    :', I2, 'Y ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S .')
      !
   end

end module m_dlwqt2
