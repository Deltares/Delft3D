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
module m_actloc
    use m_waq_precision

    implicit none

contains


    SUBROUTINE ACTLOC (IOPOIN, NRVAR, NOCONS, NOPA, NOFUN, &
            NOSFUN, NOTOT, NOSEG, NOLOC, NOGRID, &
            NOVAR, VARARR, VARIDX, VARTDA, VARDAG, &
            ARRKND, ARRPOI, ARRDM1, ARRDM2, VGRSET, &
            GRDNOS, GRDSEG, A)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED:            : may 1993 by Jan van Beek
        !
        !     FUNCTION            : Sets all variable from the LOCAL array used
        !                           for output actual for the base grid.
        !                           (ouput always uses the value from base grid)
        !
        !     SUBROUTINES CALLED  : -
        !
        !     FILES               : -
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     IOPOIN  INTEGER       *     INPUT   Pointers to arrays for vars
        !     NRVAR   INTEGER       1     INPUT   Number of output vars
        !     NOCONS  INTEGER       1     INPUT   Number of constants used
        !     NOPA    INTEGER       1     INPUT   Number of parameters
        !     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
        !     NOSFUN  INTEGER       1     INPUT   Number of segment functions
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
        !     NOLOC   INTEGER       1     INPUT   Number of variables in PROLOC
        !     NOGRID  INTEGER       1     INPUT   Number of grids
        !     NOVAR   INTEGER       1     INPUT   Number of variables
        !     VARARR  INTEGER   NOVAR     INPUT   Variable array number
        !     VARIDX  INTEGER   NOVAR     INPUT   Variable index in array
        !     VARTDA  INTEGER   NOVAR     INPUT   Type of disaggregation
        !     VARDAG  INTEGER   NOVAR     INPUT   Variable disaggr. weight var.
        !     ARRKND  INTEGER   NOARR     INPUT   Kind of array
        !     ARRPOI  INTEGER   NOARR     INPUT   Array pointer in A
        !     ARRDM1  INTEGER   NOARR     INPUT   First dimension
        !     ARRDM2  INTEGER   NOARR     INPUT   Second dimension
        !     VGRSET  INTEGER   NOVAR,*   IN/OUT  Actual indication
        !     GRDNOS  INTEGER   NOGRID    INPUT   Number of segments in grid
        !     GRDSEG  INTEGER   NOGRID    INPUT   Segment pointering
        !     A       REAL      *         IN/OUT  Real array work space
        !
        !     Declaration of arguments
        !
        use m_dhgvar
        use m_array_manipulation, only : set_array_parameters
        use timers
        use aggregation, only : resample_v2

        INTEGER(kind = int_wp) :: NRVAR, NOCONS, NOPA, NOFUN, NOSFUN, &
                NOTOT, NOSEG, NOLOC, NOGRID, NOVAR, &
                NOTOTO, NOTOTI, NOSEG2, NOPRED, I, IX_HLP, &
                IV_IDX, IV_HLP, IV_DA, IVAR, ISYSO, ISYSI, &
                NOTOTW, IX_DA, ISYSW, ISYSH, IP_HLP, IP_DA, &
                IPARW, IP_ARR, IP_ARO, IPARI, IOCONS, ILOC, &
                IK_HLP, NOTOTH, ISWCUM, IP_ARW, IP_ARI, IP_ARH, &
                IK_DA, IGRID, IDIM2, IDIM1, IDATYP, ID2_DA, ID2HLP, &
                ID1_DA, ID1HLP, IA_LOC, IA_HLP, IA_DA, IARR, IARKND

        INTEGER(kind = int_wp) :: IOPOIN(NRVAR), VARARR(NOVAR), &
                VARIDX(NOVAR), VARTDA(NOVAR), &
                VARDAG(NOVAR), ARRKND(*), &
                ARRPOI(*), ARRDM1(*), &
                ARRDM2(*), VGRSET(NOVAR, *), &
                GRDNOS(NOGRID), GRDSEG(NOSEG, NOGRID)
        REAL(kind = real_wp) :: A(*)
        !
        !     Local
        !
        PARAMETER (NOPRED = 6)
        INTEGER(kind = int_wp) :: IOPA, IOFUNC, IOSFUN, IOCONC, IOLOC, &
                IODEF, IP
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("actloc", ithandl)
        !
        !     If no locals get out of here
        !
        IF (NOLOC == 0) RETURN
        !
        !     Pointer offsets
        !
        IOCONS = NOPRED + 1
        IOPA = IOCONS + NOCONS
        IOFUNC = IOPA + NOPA
        IOSFUN = IOFUNC + NOFUN
        IOCONC = IOSFUN + NOSFUN
        IOLOC = IOCONC + NOTOT
        IODEF = IOLOC + NOLOC
        !
        IA_LOC = 33
        IX_HLP = 1
        IA_HLP = 33
        CALL DHGVAR(IA_HLP, IX_HLP, IV_HLP)
        IK_HLP = ARRKND(IA_HLP)
        IP_HLP = ARRPOI(IA_HLP)
        ID1HLP = ARRDM1(IA_HLP)
        ID2HLP = ARRDM2(IA_HLP)
        !
        DO I = 1, NRVAR
            IP = IOPOIN(I)
            !
            !        Is it a local value
            !
            IF (IP < IODEF .AND. IP >= IOLOC) THEN
                !
                !           Get variable number
                !
                ILOC = IP - IOLOC + 1
                CALL DHGVAR(IA_LOC, ILOC, IVAR)
                !
                !           Check is variable is active for base grid
                !
                IF (VGRSET(IVAR, 1) == 0) THEN
                    !
                    IARR = IA_LOC
                    IV_IDX = VARIDX(IVAR)
                    IARKND = ARRKND(IARR)
                    IP_ARR = ARRPOI(IARR)
                    IDIM1 = ARRDM1(IARR)
                    IDIM2 = ARRDM2(IARR)
                    !
                    !              Set variable
                    !
                    DO IGRID = 2, NOGRID
                        IF (VGRSET(IVAR, IGRID) == 1) THEN
                            NOSEG2 = GRDNOS(IGRID)
                            !
                            !                    Determine characteristics of variable
                            !
                            CALL set_array_parameters(IVAR, IARR, &
                                    IARKND, IV_IDX, &
                                    IDIM1, IDIM2, &
                                    IP_ARR, IGRID, &
                                    ISYSI, NOTOTI, &
                                    IP_ARI)
                            CALL set_array_parameters(IVAR, IARR, &
                                    IARKND, IV_IDX, &
                                    IDIM1, IDIM2, &
                                    IP_ARR, 1, &
                                    ISYSO, NOTOTO, &
                                    IP_ARO)
                            !
                            !                    Determine characteristics of WEIGHT variable
                            !                    ( Don't mind if this one is actuel ? )
                            !
                            IDATYP = VARTDA(IVAR)
                            IF (IDATYP == 2) THEN
                                IV_DA = VARDAG(IVAR)
                                IA_DA = VARARR(IV_DA)
                                IK_DA = ARRKND(IA_DA)
                                IF (IK_DA == 1) THEN
                                    !
                                    !                          Not variable in space use help var
                                    !
                                    IDATYP = 3
                                    IV_DA = IV_HLP
                                    IA_DA = VARARR(IV_DA)
                                    IK_DA = ARRKND(IA_DA)
                                ENDIF
                                IX_DA = VARIDX(IV_DA)
                                IP_DA = ARRPOI(IA_DA)
                                ID1_DA = ARRDM1(IA_DA)
                                ID2_DA = ARRDM2(IA_DA)
                                CALL set_array_parameters(IV_DA, IA_DA, &
                                        IK_DA, IX_DA, &
                                        ID1_DA, ID2_DA, &
                                        IP_DA, 1, &
                                        ISYSW, NOTOTW, &
                                        IP_ARW)
                                CALL set_array_parameters(IV_HLP, IA_HLP, &
                                        IK_HLP, IX_HLP, &
                                        ID1HLP, ID2HLP, &
                                        IP_HLP, IGRID, &
                                        ISYSH, NOTOTH, &
                                        IP_ARH)
                            ELSEIF (IDATYP == 3) THEN
                                IV_DA = IV_HLP
                                IA_DA = VARARR(IV_DA)
                                IK_DA = ARRKND(IA_DA)
                                IX_DA = VARIDX(IV_DA)
                                IP_DA = ARRPOI(IA_DA)
                                ID1_DA = ARRDM1(IA_DA)
                                ID2_DA = ARRDM2(IA_DA)
                                CALL set_array_parameters(IV_DA, IA_DA, &
                                        IK_DA, IX_DA, &
                                        ID1_DA, ID2_DA, &
                                        IP_DA, 1, &
                                        ISYSW, NOTOTW, &
                                        IP_ARW)
                                CALL set_array_parameters(IV_HLP, IA_HLP, &
                                        IK_HLP, IX_HLP, &
                                        ID1HLP, ID2HLP, &
                                        IP_HLP, IGRID, &
                                        ISYSH, NOTOTH, &
                                        IP_ARH)
                            ELSE
                                !
                                !                       Weight and help array's dummy's
                                !                       so set to the variable itself
                                !
                                ISYSW = ISYSO
                                ISYSH = ISYSI
                                NOTOTW = NOTOTO
                                NOTOTH = NOTOTI
                                IP_ARW = IP_ARO
                                IP_ARH = IP_ARI
                                !
                            ENDIF
                            !
                            ISWCUM = 0
                            CALL resample_v2(NOSEG, NOSEG2, &
                                    NOTOTI, NOTOTW, &
                                    NOTOTH, NOTOTO, &
                                    ISYSI, ISYSW, &
                                    ISYSH, ISYSO, &
                                    GRDSEG(1, IGRID), IDATYP, &
                                    A(IP_ARI), A(IP_ARW), &
                                    ISWCUM, A(IP_ARH), &
                                    A(IP_ARO))
                            VGRSET(IVAR, 1) = 1
                        ENDIF
                    ENDDO
                    !
                ENDIF
                !
            ENDIF
            !
        ENDDO
        !
        if (timon) call timstop (ithandl)
        RETURN
    END

end module m_actloc
