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
module m_dlwqi2
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQI2 (LUN, MODID, SYSID, IDUMP, DUMPID, &
            IDPNT, IVPNT, DISP, IBPNT, BNDID, &
            BNDNAM, BNDTYP, INWTYP, IWASTE, iwsknd, &
            WASTID, WSTNAM, WSTTYP, ALENG, CONST, &
            PARAM, NRFTOT, NRHARM, CONAME, PANAME, &
            FUNAME, SFNAME, DINAME, VENAME, IKNMRK, &
            DANAM, IPDMP, IQDMP, ISDMP, RANAM, &
            IORAAI, NQRAAI, IQRAAI, GRDNOS, GRDREF, &
            GRDSEG, GridPs, DMPBAL, dlwqd)

        !     Deltares Software Centre

        !>\file
        !>                          Reads the DelwaQ binary system file
        !>
        !>                          Initialises all fixed conditions and names
        !>                          for the simulation from the binary system
        !>                          file at LUN(2).

        !     CREATED: may -1988 by L. Postma
        !
        !     LOGICAL UNITNUMBERS : LUN( 2) - system intermediate file
        !                           LUN(19) - monitoring output file
        !
        !     SUBROUTINES CALLED  : SRSTOP, stops execution
        !

        use m_srstop
        use timers
        use m_grid_utils_external
        use delwaq2_data
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics


        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     LUN     INTEGER       *     INPUT   logical unitnumbers
        !     MODID   CHAR*40       4     OUTPUT  Model and run-ID
        !     SYSID   CHAR*20   NOTOT     OUTPUT  Systems ID
        !     IDUMP   INTEGER  NODUMP     OUTPUT  Dump segment numbers
        !     DUMPID  CHAR*20  NODUMP     OUTPUT  Dump-segment ID
        !     IDPNT   INTEGER   NOSYS     OUTPUT  Pointers to dispersion array
        !     IVPNT   INTEGER   NOSYS     OUTPUT  Pointers to velocity array
        !     DISP    REAL          3     OUTPUT  dispersion in 3 directions
        !     IBPNT   INTEGER  4*NOBND    OUTPUT  1,* = timelag
        !                                         2,* = flow pointer
        !                                         3,* = segment pointer
        !                                         4,* = time on timelag
        !     BNDID   CHAR*20   NOBND     OUTPUT  Open boundary ID's
        !     BNDNAM  CHAR*40   NOBND     OUTPUT  Open boundary names
        !     BNDTYP  CHAR*20   NOBND     OUTPUT  Open boundary types
        !     INWTYP  INTEGER       *     OUTPUT  Types of items
        !     IWASTE  INTEGER   NOWST     OUTPUT  waste load segment numbers
        integer(kind = int_wp), intent(out) :: iwsknd(*) !  wasteload processing
        !     WASTID  CHAR*20   NOWST     OUTPUT  Waste location ID
        !     WSTNAM  CHAR*40   NOWST     OUTPUT  Waste location names
        !     WSTTYP  CHAR*20   NOWST     OUTPUT  Waste location types
        !     ALENG   REAL        3       OUTPUT  Lengthes in 3 directions
        !     CONST   REAL     NOCONS     OUTPUT  value of constants
        !     PARAM   REAL    NOPA,NOSEG  OUTPUT  value of parameters
        !     NRFTOT  INTEGER  NOITEM     OUTPUT  file lengthes per item
        !     NRHARM  INTEGER  NOITEM     OUTPUT  nr of harmonics per item
        !     CONAME  CHAR*20  NOCONS     OUTPUT  Constant names
        !     PANAME  CHAR*20  NOPA       OUTPUT  Parameter names
        !     FUNAME  CHAR*20  NOFUN      OUTPUT  Function names
        !     SFNAME  CHAR*20  NOSFUN     OUTPUT  Segment function names
        !     DINAME  CHAR*20  NODISP     OUTPUT  Dispersion array names
        !     VENAME  CHAR*20  NOVELO     OUTPUT  Velocity array names
        !     DANAM   CHAR*20  NDMPAR     OUTPUT  Dump-area    ID
        !     IPDMP   INTEGER       *     OUTPUT  pointer structure dump area's
        !     IQDMP   INTEGER       *     OUTPUT  Exchange to dumped exchange pointer
        !     ISDMP   INTEGER       *     OUTPUT  Segment to dumped segment pointer
        !     RANAM   CHAR*20       *     OUTPUT  Raaien names
        !     IORAAI  INTEGER       *     OUTPUT  option output raaien
        !     NQRAAI  INTEGER       *     OUTPUT  number of exch. per raai
        !     IQRAAI  INTEGER       *     OUTPUT  exchange nunbers raaien
        !
        !
        !     IN COMMON BLOCK     :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOSEG   INTEGER       1     INPUT   Number of segments
        !     NOSYS   INTEGER       1     INPUT   Number of active systems
        !     NOTOT   INTEGER       1     INPUT   Number of systems
        !     NODISP  INTEGER       1     INPUT   Number of dispersion array's
        !     NOVELO  INTEGER       1     INPUT   Number of velocity array's
        !     NOQ     INTEGER       1     INPUT   total number of exchanges
        !     NODUMP  INTEGER       1     INPUT   Number of dump segments
        !     NOBND   INTEGER       1     INPUT   Number of open boundaries
        !     NOBTYP  INTEGER       1     INPUT   Number of boundary types
        !     NOWST   INTEGER       1     INPUT   Number of load locations
        !     NOWTYP  INTEGER       1     INPUT   Number of waste load types
        !     NOCONS  INTEGER       1     INPUT   Number of constants used
        !     NOPA    INTEGER       1     INPUT   Number of parameters
        !     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
        !     NOSFUN  INTEGER       1     INPUT   Number of segment functions
        !     NOITEM  INTEGER       1     INPUT   Number possible functions
        !     NDMPAR  INTEGER       1     INPUT   Number of dump area's
        !     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
        !     NTDMPS  INTEGER       1     INPUT   total number segments in dump area
        !     NORAAI  INTEGER       1     INPUT   number of raaien
        !     NTRAAQ  INTEGER       1     INPUT   total number of exch. in raaien
        !
        !

        INTEGER(kind = int_wp) :: IPDMP(*), IQDMP(*), ISDMP (*), IORAAI(*), &
                NQRAAI(*), IQRAAI(*), GRDNOS(*), GRDREF(*), &
                IDUMP (*), IDPNT (*), IVPNT (*), IBPNT (4, *), &
                IWASTE(*), NRFTOT(*), NRHARM(*), LUN   (*), &
                IKNMRK(*), INWTYP(*)
        INTEGER(kind = int_wp) :: GRDSEG(NOSEG + NSEG2, NOGRID)
        character(len=40) MODID (4), BNDNAM(*), WSTNAM(*)
        character(len=20) SYSID (*), DUMPID(*), BNDID (*), BNDTYP(*), &
                WASTID(*), WSTTYP(*), CONAME(*), PANAME(*), &
                FUNAME(*), SFNAME(*), DINAME(*), VENAME(*), &
                DANAM (*), RANAM (*)
        real(kind = real_wp) :: DISP  (*), ALENG (*), CONST (*), PARAM (*)
        character(len=40)  FILLER
        type(GridPointerColl), intent(inout) :: GridPs     !< definitions of the grids
        type(delwaq_data), intent(inout) :: dlwqd      !< derived type for persistent storage
        integer(kind = int_wp) :: dmpbal(*)  !< indicates if dump area is included in the balance
        type(t_grid) :: aGrid      ! a single grid

        integer(kind = int_wp) :: it, noqtt, nosss, k, igrid, iin, iseg, ierror, i_grid
        integer(kind = int_wp) :: isys, ix, i, idummy
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqi2", ithandl)
        !
        !     Local
        !
        IT = 0
        !
        !         read from the system file
        !
        NOQTT = NOQ + NOQ4
        NOSSS = NOSEG + NSEG2
        IIN = LUN(2)
        READ (IIN, END = 40, ERR = 40)  MODID (1), MODID(2)
        READ (IIN, END = 40, ERR = 40)  MODID (3), MODID(4)
        READ (IIN, END = 40, ERR = 40) (SYSID (K), K = 1, NOTOT)
        IF (NODUMP > 0) &
                READ (IIN, END = 40, ERR = 40) (IDUMP(K), DUMPID(K), K = 1, NODUMP)
        IF (NDMPAR > 0) &
                READ (IIN, END = 40, ERR = 40) (DANAM(K), K = 1, NDMPAR)
        IF (NDMPAR > 0) &
                READ (IIN, END = 40, ERR = 40) (DMPBAL(K), K = 1, NDMPAR)
        IF (NORAAI > 0) &
                READ (IIN, END = 40, ERR = 40) (RANAM(K), K = 1, NORAAI)
        !
        !     sub-grid
        !
        DO IGRID = 1, NOGRID
            READ (IIN, END = 40, ERR = 40)  GRDNOS(IGRID), GRDREF(IGRID), &
                    (GRDSEG(ISEG, IGRID), ISEG = 1, NOSSS)
        ENDDO
        !     the grid structures
        DO IGRID = 1, NOGRID
            ierror = aGrid%read(iin, nosss)
            if (ierror /= 0) goto 40
            i_grid = GridPs%add(aGrid)
        ENDDO
        READ (IIN, END = 40, ERR = 40) (IDUMMY, ISYS = 1, NOTOT)
        READ (IIN, END = 40, ERR = 40) (IDUMMY, ISYS = 1, NOTOT)
        READ (IIN, END = 40, ERR = 40) (IKNMRK(K), K = 1, NOSSS)
        IF (NODISP > 0) &
                READ (IIN, END = 40, ERR = 40) (DINAME(K), K = 1, NODISP)
        IF (NOVELO > 0) &
                READ (IIN, END = 40, ERR = 40) (VENAME(K), K = 1, NOVELO)
        READ (IIN, END = 40, ERR = 40) (IDPNT (K), K = 1, NOSYS)
        READ (IIN, END = 40, ERR = 40) (IVPNT (K), K = 1, NOSYS)
        IF (NOBND  > 0) THEN
            READ (IIN, END = 40, ERR = 40) (IBPNT (2, K), K = 1, NOBND)
            READ (IIN, END = 40, ERR = 40) (IBPNT (3, K), K = 1, NOBND)
        ENDIF
        IF (NDMPAR > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (IPDMP(K), K = 1, NDMPAR + NTDMPQ)
            IX = NDMPAR + NTDMPQ
            READ (IIN, END = 40, ERR = 40)  (IPDMP(IX + K), K = 1, NDMPAR + NTDMPS)
        ENDIF
        IF (NORAAI > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (IORAAI(K), K = 1, NORAAI)
            READ (IIN, END = 40, ERR = 40)  (NQRAAI(K), K = 1, NORAAI)
            READ (IIN, END = 40, ERR = 40)  (IQRAAI(K), K = 1, NTRAAQ)
        ENDIF
        IF (NORAAI > 0 .OR. NDMPAR > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (IQDMP(K), K = 1, NOQTT)
        ENDIF
        IF (NDMPAR > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (ISDMP(K), K = 1, NOSSS)
        ENDIF
        READ (IIN, END = 40, ERR = 40) IDUMMY, (DISP  (K), K = 1, 3)
        READ (IIN, END = 40, ERR = 40) IDUMMY, (ALENG (K), K = 1, 3)
        IF (NOBND  > 0) THEN
            DO I = 1, NOBND
                READ (IIN, END = 40, ERR = 40) BNDID(I), BNDNAM(I)
            end do
            READ (IIN, END = 40, ERR = 40) (BNDTYP(K), K = 1, NOBTYP)
            READ (IIN, END = 40, ERR = 40) (INWTYP(K + IT), K = 1, NOBND)
            IT = IT + NOBND
            !          read time lags
            READ (IIN, END = 40, ERR = 40) (IBPNT(1, K), K = 1, NOBND)
        ENDIF
        IF (NOWST  > 0) THEN
            DO I = 1, NOWST
                READ (IIN, END = 40, ERR = 40) IWASTE(I), iwsknd(i), &
                        WASTID(I), WSTNAM(I)
            end do
            READ (IIN, END = 40, ERR = 40) (WSTTYP(K), K = 1, NOWTYP)
            READ (IIN, END = 40, ERR = 40) (INWTYP(K + IT), K = 1, NOWST)
            IT = IT + NOWST
        ENDIF
        IF (NOCONS > 0) THEN
            READ (IIN, END = 40, ERR = 40) (CONAME(K), K = 1, NOCONS)
        ENDIF
        IF (NOPA   > 0) THEN
            READ (IIN, END = 40, ERR = 40) (PANAME(K), K = 1, NOPA)
        ENDIF
        IF (NOFUN  > 0) THEN
            READ (IIN, END = 40, ERR = 40) (FUNAME(K), K = 1, NOFUN)
        ENDIF
        IF (NOSFUN > 0) THEN
            READ (IIN, END = 40, ERR = 40) (SFNAME(K), K = 1, NOSFUN)
        ENDIF
        !
        !     Time function info
        !
        READ (IIN, END = 40, ERR = 40) (NRFTOT(K), K = 1, NOITEM)
        READ (IIN, END = 40, ERR = 40) (NRHARM(K), K = 1, NOITEM)
        !
        !         boundary timings greater then timelag
        !
        DO I = 1, NOBND
            IBPNT(4, I) = IBPNT(1, I) + 1
        end do
        !
        !         extract reference date and time
        !
        CALL MODIFIED_JULIAN(MODID(4))
        dlwqd%otime = otime
        dlwqd%tscale = tscale
        !
        !         completion successful
        !
        WRITE (LUN(19), 2000) (MODID(K), K = 1, 4)
        if (timon) call timstop (ithandl)
        RETURN
        !
        !         unsuccessful read
        !
        40 WRITE (LUN(19), 2010)
        CALL SRSTOP(1)
        !
        !         output formats
        !
        2000 FORMAT (' ', 20X, A40/21X, A40//21X, A40/21X, A40// &
                21X, 'Initialisation from system file completed.')
        2010 FORMAT ('   ERROR reading binary system file !!'/ &
                '   initialisation NOT successful    !!'/ &
                '   simulation impossible            !!')
        !
    CONTAINS
        SUBROUTINE MODIFIED_JULIAN(T0STRING)
            IMPLICIT NONE
            CHARACTER(LEN = *) :: T0STRING

            INTEGER(kind = int_wp) :: IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, ISCALE
            INTEGER(kind = int_wp) :: IERR
            REAL(kind = dp) :: TEMP1, TEMP2

            REAL(kind = dp), PARAMETER :: MODIFICATION_OFFSET = 2400000.5D0

            TSCALE = 1.0d0

            READ(T0STRING, '(4x,i4.4,x,i2.2,x,i2.2,x,i2.2,x,i2.2,x,i2.2,7x,i8)', IOSTAT = IERR) &
                    IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, ISCALE

            IF (IERR /= 0) THEN
                IYEAR = 1900
                IMONTH = 1
                IDAY = 1
                IHOUR = 0
                IMIN = 0
                ISEC = 0
                ISCALE = 1
            ENDIF

            TEMP1 = INT ((IMONTH - 14.0D0) / 12.0D0)
            TEMP2 = IDAY - 32075.0D0 + &
                    INT (1461.0D0 * (IYEAR + 4800.0D0 + TEMP1) / 4.0D0) + &
                    INT (367.0D0 * (IMONTH - 2.0D0 - TEMP1 * 12.0D0) / 12.0D0) - &
                    INT (3.0D0 * INT ((IYEAR + 4900.0D0 + TEMP1) / 100.0D0) / &
                            4.0)
            TEMP1 = FLOAT (IHOUR) * 3600.0 + &
                    FLOAT (IMIN) * 60.0 + &
                    FLOAT (ISEC) - 43200.0
            OTIME = TEMP2 + (TEMP1 / 86400.0) - MODIFICATION_OFFSET
            TSCALE = 86400.0D0 / ISCALE
        END SUBROUTINE MODIFIED_JULIAN

    END

end module m_dlwqi2
