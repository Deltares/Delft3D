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
module m_getdim
    use m_waq_precision

    implicit none

contains


    SUBROUTINE GETDIM (FNAME, ITYPE, DIM, IPRDEP, ITMDEP, &
            LOCDEP, NDIM, IERROR, OPTION)
        !
        !
        !     Deltares        MARINE & COASTAL MANAGEMENT
        !
        !     CREATED            : May '96  by L. Postma
        !
        !     MODIFIED           :
        !
        !     FUNCTION           : ODS GETDIM routine for DELWAQ HIS-files
        !
        !     SUBROUTINES CALLED :
        !
        !     LOGICAL UNITS      :
        !
        !     PARAMETERS    :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     FNAME   CHAR*256   3        IN/LOC  Complete file name
        !     ITYPE   INTEGER    1        INPUT   File type
        !     DIM     CHAR*3     1        INPUT   Wanted dimension
        !     IPRDEP  INTEGER    1        INPUT   Par code for dimensions
        !     ITMDEP  INTEGER    1        INPUT   Time code for dimensions
        !     LOCDEP  INTEGER    1        INPUT   Loc code for dimensions
        !     NDIM    INTEGER    5        OUTPUT  Wanted dimensions
        !     IERROR  INTEGER    1        OUTPUT  Error code
        !     OPTION  CHAR*256   1        IN/OUT  For future use
        !
        !     NOTE1: FNAME(3) is used as local character space
        !     NOTE2: NDIM is NOT according to ODS specs, it returns:
        !            NDIM(1) = nr of substances in the file
        !            NDIM(2) = nr of locations  in the file
        !            NDIM(3) = nr of time steps in the file
        !
        !
        use m_string_manipulation, only : upper_case
        use m_open_waq_files
        use m_file_path_utils, only : extract_file_extension

        CHARACTER*256 FNAME(3), OPTION
        CHARACTER*3   DIM
        DIMENSION     NDIM(3)
        character*256 :: ext     ! file extension
        integer(kind = int_wp) :: extpos   ! position of extension
        integer(kind = int_wp) :: extlen   ! length of file extension
        logical :: mapfil  ! true if map file extension
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: NODUMP, NOTOT
        integer(kind = int_wp) :: k, NTT, ierror, idummy, notim, ndim, itype
        real(kind = real_wp) :: adummy
        integer(kind = int_wp) :: iprdep, itmdep, locdep

        !
        !         Open the DELWAQ .HIS file
        !
        lun = 10
        CALL open_waq_files (lun, FNAME(1), 24, 2, IERROR)
        IF (IERROR /= 0) RETURN

        ! map or his

        call extract_file_extension(fname(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'MAP') then
            mapfil = .true.
        else
            mapfil = .false.
        endif
        !
        !         Read primary system characteristics
        !
        READ (lun, ERR = 100)   FNAME(3)(1:160)
        READ (lun, ERR = 110)   NOTOT, NODUMP
        READ (lun, ERR = 120) (FNAME(3)(181:200), K = 1, NOTOT)
        if (.not. mapfil) then
            READ (lun, ERR = 130) (IDUMMY, FNAME(3)(221:240), K = 1, NODUMP)
        endif
        !
        !         Read the values at all times
        !
        NTT = NODUMP * NOTOT
        NOTIM = 0
        !     jvb  NTT = 0
        10 READ (lun, ERR = 140, END = 20)   IDUMMY, (ADUMMY, K = 1, NTT)
        NOTIM = NOTIM + 1
        GOTO 10
        !
        !         Supply the desired statistics
        !
        20 NDIM(1) = NOTOT
        NDIM(2) = NODUMP
        NDIM(3) = NOTIM
        GOTO 200
        !
        !         Supply the desired statistics
        !
        100 IERROR = 10
        GOTO 200
        110 IERROR = 11
        GOTO 200
        120 IERROR = 12
        GOTO 200
        130 IERROR = 13
        GOTO 200
        140 IERROR = 14
        !
        200 CLOSE (lun)
        RETURN
        !
    END

end module m_getdim
