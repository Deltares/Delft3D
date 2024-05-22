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
module m_sobbal
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    SUBROUTINE SOBBAL (NOTOT, ITIME, NOSYS, NOFLUX, NDMPAR, &
            NDMPQ, NTDMPQ, ITSTOP, IMSTRT, IMSTOP, &
            IQDMP, IPDMP, ASMASS, FLXINT, STOCHI, &
            SYNAME, DANAM, MONAME, DMPQ, NOBND, &
            NOBTYP, BNDTYP, INBTYP, NOCONS, CONAME, &
            CONS, NOQ, IPOINT, FLXNAM, INTOPT, &
            VOLUME, SURF, NOSEG, LUNOUT, LCHOUT, &
            INIOUT, DMPBAL, NOWST, NOWTYP, WSTTYP, &
            IWASTE, INWTYP, WSTDMP, ISEGCOL, IMSTEP)
        !
        !     Deltares      SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED             : PROTOTYPE dec. 2001 by Jos van Gils
        !
        !
        !     FUNCTION            : Integrated emissions and processes balance
        !
        !     LOGICAL UNITS       : -
        !
        !     SUBROUTINES CALLED  :
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     ITIME   INTEGER       1     INPUT   Time in system clock units
        !     ITSTOP  INTEGER       1     INPUT   Stop time of the simulation
        !     IMSTRT  INTEGER       1     INPUT   Start time of the output
        !     IMSTOP  INTEGER       1     INPUT   Stop time of the output
        !     MONAME  CHAR*40       4     INPUT   Model and run names
        !     SYNAME  CHAR*20    NOTOT    INPUT   names of substances
        !     NOSYS   INTEGER       1     INPUT   Number of active substances
        !     ASMASS  REAL       NOTOT,*  IN/OUT  Cummulative balance per dump area
        !                                         1   = mass
        !                                         2   = processes
        !                                         3/4 = loads in/out
        !                                         5/6 = transport in/out
        !     NOFLUX  INTEGER       1     INPUT   Number of fluxes
        !     FLXINT  REAL          *     IN/OUT  Integrated fluxes in dump areas
        !     NDMPAR  INTEGER     1       INPUT   Number of dump areas
        !     NTDMPQ  INTEGER     1       INPUT
        !     DANAM   CHAR*20  NDMPAR     INPUT   Dump area names
        !     NDMPQ   INTEGER     1       INPUT   Number of dumped exchanges
        !     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
        !     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
        !     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
        !     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
        !     NOBND   INTEGER     1       INPUT   number of boundaries
        !     NOBTYP  INTEGER     1       INPUT   number of boundaries types
        !     BNDTYP  CHAR*20  NOBTYP     INPUT   boundary types names
        !     INBTYP  INTEGER  NOBND      INPUT   boundary type number (index in BNDTYP)
        !     NOCONS  INTEGER     1       INPUT   number of constants
        !     CONAME  CHAR*20  NOCONS     INPUT   constants names
        !     CONS    REAL     NOCONS     INPUT   constants array
        !     NOQ     INTEGER     1       INPUT   number of exchanges
        !     IPOINT  INTEGER   4,NOQ     INPUT   pointer array
        !     FLXNAM  INTEGER  NOFLUX     INPUT   flux names
        !     INTOPT  INTEGER     1       INPUT   Integration and balance suboptions
        !     VOLUME  REAL     NOSEG      INPUT   Volume
        !     SURF    REAL     NOSEG      INPUT   horizontal surface area
        !     DMPBAL  INTEGER  NDMPAR     INPUT   if dump area is included in balance
        !     NOWST   INTEGER     1       INPUT   number of wasteloads
        !     NOWTYP  INTEGER     1       INPUT   number of wasteload types
        !     WSTTYP  CHAR*20  NOWTYP     INPUT   wasteload types names
        !     IWASTE  INTEGER  NOWST      INPUT   segment number wasteloads
        !     INWTYP  INTEGER  NOWST      INPUT   wasteload type number (index in WSTTYP)
        !     WSTDMP  REAL     NOTOT,NOWST,2  I   accumulated wasteloads 1/2 in and out
        !     ==================================================================
        !
        use m_outhis
        use m_dmpval
        use m_dmpsurf
        use m_logger, only : terminate_execution, get_log_unit_number
        use data_processing, only : extract_value_from_group
        use m_cli_utils, only : retrieve_command_argument
        use m_open_waq_files
        use timers
        INTEGER(kind = int_wp) :: NOTOT, ITIME, NOSYS, NOSEG, LUNOUT, &
                NOFLUX, NDMPAR, NDMPQ, NTDMPQ, &
                NOBND, ITSTOP, IMSTOP, IMSTRT, IMSTEP, &
                NOBTYP, NOCONS, NOQ, INIOUT, INTOPT
        INTEGER(kind = int_wp) :: IQDMP(*), IPDMP(*), &
                INBTYP(NOBND), IPOINT(4, NOQ)
        REAL(kind = real_wp) :: DMPQ(NOSYS, NDMPQ, *), &
                ASMASS(NOTOT, NDMPAR, *), FLXINT(NOFLUX, *), &
                STOCHI(NOTOT, NOFLUX), CONS(NOCONS), &
                VOLUME(*), SURF(*)
        character(len=20)  SYNAME(*), DANAM(*), &
                BNDTYP(NOBTYP), CONAME(NOCONS), &
                FLXNAM(NOFLUX)
        character(len=40)  MONAME(4)
        character(len=255) LCHOUT
        integer(kind = int_wp) :: dmpbal(ndmpar)        ! indicates if dump area is included in the balance
        integer(kind = int_wp) :: nowst                 ! number of wasteloads
        integer(kind = int_wp) :: nowtyp                ! number of wasteload types
        character(len = 20) :: wsttyp(nowtyp)        ! wasteload types names
        integer(kind = int_wp) :: iwaste(nowst)         ! segment numbers of the wasteloads
        integer(kind = int_wp) :: inwtyp(nowst)         ! wasteload type number (index in wsttyp)
        real(kind = real_wp) :: wstdmp(notot, nowst, 2) ! accumulated wasteloads 1/2 in and out
        integer(kind = int_wp), intent(in) :: isegcol(*)            ! pointer from segment to top of column

        !     Local declarations
        !
        !     NAME    KIND     LENGTH     DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOSUM   INTEGER     1       Nr of sum parameters
        !     SFACTO  REAL   NOSUM,NOTOT  Factor for substance in sum parameters
        !     STOCHL  REAL   NOSUM,NOFLUX Local STOCHI for sum parameters
        !     NOOUT   INTEGER     1       Nr of balance terms per dump segment
        !     IMASSA  INTEGER NOTOT+NOSUM Pointer to accumulation term in balance
        !     IEMISS  INTEGER NOTOT+NOSUM Pointer to boundary terms in balance
        !     NEMISS  INTEGER     1       Nr of boundary terms in balance
        !     ITRANS  INTEGER NOTOT+NOSUM Pointer to int. transport terms in balance
        !     IPROCS  INTEGER NOTOT+NOSUM Pointer to processes term(s) in balance
        !     NPROCS  INTEGER NOTOT+NOSUM Nr of processes terms in balance
        !     BALANS  REAL   NOOUT,*      Mass balances for current time
        !     BALTOT  REAL   NOOUT,*      Integrated mass balances
        !     OUNAME  C*20      NOOUT     Names of terms in balances
        !     FL2BAL  INT   NOTOT+NOSUM,* Pointer to relevant fluxes per substance
        !     DANAMP  C*20     NDMPAR+1   Copy of DANAM including sum segment
        !     SYNAMP  C*20  NOTOT+NOSUM   Copy of SYNAME including sum parameters
        !     IBSTRT  INTEGER     1       Proper start time of the balance period
        !     IBSTOP  INTEGER     1       Proper stop time of the balance period

        INTEGER(kind = int_wp) :: IOSOBH, IOBALI, ISYS, IBOUN, NEMISS, IFRAC, &
                IFLUX, IDUMP, IPQ, ISYS2, ITEL, IINIT, &
                ITEL1, IP1, ITEL2, NQC, IQC, IQ, &
                IPOIN, NOOUT, IERR, IOUT, ISUM, NOSUM, &
                NSC, ISC, LUNREP, IBSTRT, IBSTOP
        PARAMETER    (NOSUM = 2)
        real(kind = real_wp), allocatable :: SFACTO(:, :), &
                STOCHL(:, :), &
                FLTRAN(:, :), &
                BALANS(:, :), &
                BALTOT(:, :), &
                DMP_SURF(:), &
                DMP_VOLU(:)
        integer(kind = int_wp), allocatable :: JDUMP(:), &
                FL2BAL(:, :), &
                IMASSA(:), &
                IEMISS(:), &
                ITRANS(:), &
                IPROCS(:), &
                NPROCS(:), &
                SEGDMP(:)
        character(len=20), allocatable :: OUNAME(:), &
                DANAMP(:), &
                SYNAMP(:)
        logical, allocatable :: IWDMP(:, :)

        LOGICAL       LUMPEM, LUMPPR, IFIRST, SUPPFT, ONLYSM, &
                INCLUD, BOUNDA, LUMPTR, B_AREA, B_VOLU
        REAL(kind = real_wp) :: RDUM(1)
        REAL(kind = real_wp) :: ST, TFACTO(NOSUM)
        character(len=20)  C20, SYNAMS(NOSUM)
        character(len=40)  CDUM
        character(len=255) FILNAM
        character(len=2)   c2
        character(len=255) inifil
        logical       lfound
        integer(kind = int_wp) :: idummy, ierr2
        integer(kind = int_wp) :: lunini
        real(kind = real_wp) :: rdummy
        integer(kind = int_wp) :: iseg, iw, idum, ivan, ibal_off, idump_out
        integer(kind = int_wp) :: ndmpar_out, ntrans, indx
        integer(kind = int_wp) :: inaar, itstrt
        real(kind = real_wp) :: tot_surf, tot_volu
        DATA          LUMPEM /.true./
        DATA          LUMPPR /.true./
        DATA          SUPPFT /.true./
        DATA          ONLYSM /.TRUE./
        DATA          LUMPTR /.FALSE./
        DATA          B_AREA /.FALSE./
        DATA          B_VOLU /.FALSE./
        DATA          SYNAMS /'TotN', 'TotP'/
        !     SAVE          IOBALI, BALTOT, SFACTO, SUPPFT, ONLYSM, STOCHL,
        !    J              OUNAME, DANAMP, SYNAMP, JDUMP , LUMPEM, LUMPPR,
        !    J              BALANS, FLTRAN, IMASSA, IEMISS, ITRANS, IPROCS,
        !    J              NPROCS, FL2BAL, LUMPTR, B_AREA, B_VOLU, SEGDMP
        SAVE
        integer(kind = int_wp) :: ithandl = 0

        !     Skip this routine when there are no balance area's
        IF (NDMPAR==0) RETURN

        if (timon) call timstrt ("sobbal", ithandl)
        !**************** INITIALIZATION **************************************

        IF (INIOUT == 1) THEN
            IFIRST = .TRUE.
            CALL get_log_unit_number(LUNREP)

            !         Process flags

            !         from input

            lumppr = .NOT. btest(intopt, 8)
            lumpem = .NOT. btest(intopt, 9)
            lumptr = .NOT. btest(intopt, 10)
            b_area = btest(intopt, 11)
            b_volu = btest(intopt, 12)
            onlysm = .NOT. btest(intopt, 13)
            suppft = .NOT. btest(intopt, 14)

            !         from ini file

            call retrieve_command_argument ('-i', 3, lfound, idummy, rdummy, &
                    inifil, ierr2)
            if (lfound) then
                if (ierr2/= 0) then
                    inifil = ' '
                endif
            else
                inifil = 'delwaq.ini'
            endif
            open (newunit = lunini, file = inifil, status = 'old', err = 123)
            call extract_value_from_group (lunini, 'Balance Options', 'LumpProcessesContributions', c2)
            if (c2 == '-1') lumppr = .true.
            if (c2 == '0') lumppr = .false.
            call extract_value_from_group (lunini, 'Balance Options', 'LumpBoundaryContributions', c2)
            if (c2 == '-1') lumpem = .true.
            if (c2 == '0') lumpem = .false.
            call extract_value_from_group (lunini, 'Balance Options', 'SumOfMonitoringAreas', c2)
            if (c2 == '-1') onlysm = .true.
            if (c2 == '0') onlysm = .false.
            call extract_value_from_group (lunini, 'Balance Options', 'SuppressTimeDependentOutput', c2)
            if (c2 == '-1') suppft = .true.
            if (c2 == '0') suppft = .false.
            close (lunini)
            123     continue

            ! count number of output dump areas

            ndmpar_out = 0
            do idump = 1, ndmpar
                if (dmpbal(idump) == 1) then
                    ndmpar_out = ndmpar_out + 1
                endif
            enddo

            !         Dimension arrays

            if (allocated(fltran)) then
                deallocate(&
                        FLTRAN, &
                        JDUMP, &
                        SFACTO, &
                        DANAMP, &
                        SYNAMP, &
                        IMASSA, &
                        IEMISS, &
                        ITRANS, &
                        IPROCS, &
                        NPROCS, &
                        STOCHL, &
                        FL2BAL)
            endif

            allocate (FLTRAN(2, NOSYS), &
                    JDUMP(NDMPAR_OUT + 1), &
                    SFACTO(NOSUM, NOTOT), &
                    DANAMP(NDMPAR_OUT + 1), &
                    SYNAMP(NOTOT + NOSUM), &
                    IMASSA(NOTOT + NOSUM), &
                    IEMISS(NOTOT + NOSUM), &
                    ITRANS(NOTOT + NOSUM), &
                    IPROCS(NOTOT + NOSUM), &
                    NPROCS(NOTOT + NOSUM), &
                    STOCHL(NOSUM, NOFLUX), &
                    FL2BAL(NOTOT + NOSUM, NOFLUX), &
                    STAT = IERR)
            IF (IERR > 0) GOTO 9000
            IF (.NOT. LUMPTR) THEN

                !             allocate and set SEGDMP, first dump number for each segment (if any)
                if (allocated(segdmp)) then
                    deallocate(segdmp)
                endif
                allocate (SEGDMP(NOSEG), &
                        STAT = IERR)
                IF (IERR > 0) GOTO 9000
                SEGDMP = 0
                ITEL = 0
                IDUMP_OUT = 0
                DO IDUMP = 1, NDMPAR
                    NSC = IPDMP(NDMPAR + NTDMPQ + IDUMP)
                    IF (DMPBAL(IDUMP) == 1) THEN
                        IDUMP_OUT = IDUMP_OUT + 1
                        DO ISC = 1, NSC
                            ITEL = ITEL + 1
                            ISEG = IPDMP(NDMPAR + NTDMPQ + NDMPAR + ITEL)
                            IF (ISEG > 0) THEN
                                IF (SEGDMP(ISEG) == 0) THEN
                                    SEGDMP(ISEG) = IDUMP_OUT
                                ENDIF
                            ENDIF
                        ENDDO
                    ELSE
                        ITEL = ITEL + NSC
                    ENDIF
                ENDDO

            ENDIF
            IF (.NOT. LUMPEM) THEN

                !             allocate and set IWDMP, set to true is wasteload is in dump area
                if (allocated(iwdmp)) then
                    deallocate(iwdmp)
                endif
                allocate (IWDMP(NOWST, NDMPAR), STAT = IERR)
                IF (IERR > 0) GOTO 9000
                IWDMP = .FALSE.
                ITEL = 0
                IDUMP_OUT = 0
                DO IDUMP = 1, NDMPAR
                    NSC = IPDMP(NDMPAR + NTDMPQ + IDUMP)
                    IF (DMPBAL(IDUMP) == 1) THEN
                        IDUMP_OUT = IDUMP_OUT + 1
                        DO ISC = 1, NSC
                            ITEL = ITEL + 1
                            ISEG = IPDMP(NDMPAR + NTDMPQ + NDMPAR + ITEL)
                            IF (ISEG > 0) THEN
                                DO IW = 1, NOWST
                                    IF (IWASTE(IW) == ISEG) THEN
                                        IWDMP(IW, IDUMP_OUT) = .TRUE.
                                    ENDIF
                                ENDDO
                            ENDIF
                        ENDDO
                    ELSE
                        ITEL = ITEL + NSC
                    ENDIF
                ENDDO

            ENDIF

            !         Balances are constructed for all system variables
            !         + total N + total P (IF RELEVANT!)
            !         Find which state variables contribute to what extent

            CALL COMSUM (NOSUM, TFACTO, NOTOT, SYNAME, SFACTO, &
                    NOCONS, CONAME, CONS)
            DO ISYS = 1, NOTOT
                SYNAMP(ISYS) = SYNAME(ISYS)
            ENDDO
            DO ISUM = 1, NOSUM
                SYNAMP(NOTOT + ISUM) = SYNAMS(ISUM)
            ENDDO

            !         Count number of balance terms dep. on flags LUMPEM/LUMPPR

            !         first term   nr of terms    description
            !         ----------   -----------    ---------------------------
            !         IMASSA(ISYS) 1              mass/accumulation
            !         IEMISS(ISYS) NEMISS         inflow/outflow over boundaries
            !                                     if LUMPEM only totals
            !                                     if not    per fraction
            !         ITRANS(ISYS) 2              inflow/outflow other segments
            !         IPROCS(ISYS) NPROCS(ISYS)   contribution from processes
            !                                     if LUMPPR only total
            !                                     if not    per process
            !
            IF (LUMPEM) THEN
                NEMISS = 2
            ELSE

                !             boundary types and loads as seperate term (all in and out)
                NEMISS = 2 * NOBTYP + 2 * NOWTYP
            ENDIF

            IF (LUMPTR) THEN
                NTRANS = 2
            ELSE

                !             internal transport from every dump area possible plus the other term
                NTRANS = 2 * NDMPAR_OUT + 2
            ENDIF

            NOOUT = 0
            DO ISYS = 1, NOTOT + NOSUM
                IF (ISYS > NOTOT) THEN
                    IF (TFACTO(ISYS - NOTOT) > 0.0001) THEN
                        INCLUD = .TRUE.
                    ELSE
                        INCLUD = .FALSE.
                        IMASSA(ISYS) = -1
                    ENDIF
                ELSE
                    INCLUD = .TRUE.
                ENDIF
                IF (INCLUD) THEN
                    IMASSA(ISYS) = NOOUT + 1
                    NOOUT = NOOUT + 1
                    IEMISS(ISYS) = NOOUT + 1
                    NOOUT = NOOUT + NEMISS
                    ITRANS(ISYS) = NOOUT + 1
                    NOOUT = NOOUT + NTRANS
                    IPROCS(ISYS) = NOOUT + 1
                    IF (LUMPPR) THEN
                        NPROCS(ISYS) = 1
                    ELSE
                        !                     Find sum STOCHI coefficients for sum parameters
                        IF (ISYS > NOTOT) THEN
                            ISUM = ISYS - NOTOT
                            DO IFLUX = 1, NOFLUX
                                STOCHL(ISUM, IFLUX) = 0.0
                                DO ISYS2 = 1, NOTOT
                                    STOCHL(ISUM, IFLUX) = &
                                            STOCHL(ISUM, IFLUX) &
                                                    + STOCHI(ISYS2, IFLUX) &
                                                    * SFACTO(ISUM, ISYS2)
                                ENDDO
                            ENDDO
                        ENDIF

                        !                     Make sure that irrelevant fluxes are not included
                        NPROCS(ISYS) = 0
                        DO IFLUX = 1, NOFLUX
                            IF (ISYS <= NOTOT) THEN
                                ST = STOCHI(ISYS, IFLUX)
                            ELSE
                                ST = STOCHL(ISYS - NOTOT, IFLUX)
                            ENDIF
                            IF (ABS(ST) > 1.E-20) THEN
                                NPROCS(ISYS) = NPROCS(ISYS) + 1
                                FL2BAL(ISYS, NPROCS(ISYS)) = IFLUX
                            ENDIF
                        ENDDO
                    ENDIF
                    NOOUT = NOOUT + NPROCS(ISYS)
                ENDIF
            ENDDO

            !         Dimension additional arrays

            if (allocated(balans)) then
                deallocate(balans, baltot, ouname)
            endif

            allocate (BALANS(NOOUT, NDMPAR_OUT + 1), &
                    BALTOT(NOOUT, NDMPAR_OUT + 1), &
                    OUNAME(NOOUT), &
                    stat = ierr)
            if (ierr > 0) goto 9000

            !         Set balance term names

            DO ISYS = 1, NOTOT + NOSUM
                IF (IMASSA(ISYS) > 0) THEN
                    C20 = SYNAMP(ISYS)
                    OUNAME(IMASSA(ISYS)) = C20(1:6) // '_Storage'
                    IF (LUMPEM) THEN
                        OUNAME(IEMISS(ISYS)) = C20(1:6) // '_All Bo+Lo_In'
                        OUNAME(IEMISS(ISYS) + 1) = C20(1:6) // '_All Bo+Lo_Out'
                    ELSE
                        ITEL2 = IEMISS(ISYS) - 1
                        DO IFRAC = 1, NOBTYP
                            ITEL2 = ITEL2 + 1
                            OUNAME(ITEL2) = &
                                    C20(1:6) // '_' // BNDTYP(IFRAC)(1:9) // '_In'
                            ITEL2 = ITEL2 + 1
                            OUNAME(ITEL2) = &
                                    C20(1:6) // '_' // BNDTYP(IFRAC)(1:9) // '_Out'
                        ENDDO
                        DO IFRAC = 1, NOWTYP
                            ITEL2 = ITEL2 + 1
                            OUNAME(ITEL2) = &
                                    C20(1:6) // '_' // WSTTYP(IFRAC)(1:9) // '_In'
                            ITEL2 = ITEL2 + 1
                            OUNAME(ITEL2) = &
                                    C20(1:6) // '_' // WSTTYP(IFRAC)(1:9) // '_Out'
                        ENDDO
                    ENDIF
                    IF (LUMPTR) THEN
                        OUNAME(ITRANS(ISYS)) = C20(1:6) // '_Transport In'
                        OUNAME(ITRANS(ISYS) + 1) = C20(1:6) // '_Transport Out'
                    ELSE
                        ITEL2 = ITRANS(ISYS) - 1
                        ITEL2 = ITEL2 + 1
                        OUNAME(ITEL2) = C20(1:6) // '_' // 'Other    ' // '_In'
                        ITEL2 = ITEL2 + 1
                        OUNAME(ITEL2) = C20(1:6) // '_' // 'Other    ' // '_Out'
                        DO IDUMP = 1, NDMPAR
                            IF (DMPBAL(IDUMP) == 1) THEN
                                ITEL2 = ITEL2 + 1
                                OUNAME(ITEL2) = C20(1:6) // '_' // DANAM(IDUMP)(1:9) // '_In'
                                ITEL2 = ITEL2 + 1
                                OUNAME(ITEL2) = C20(1:6) // '_' // DANAM(IDUMP)(1:9) // '_Out'
                            ENDIF
                        ENDDO
                    ENDIF
                    IF (LUMPPR) THEN
                        OUNAME(IPROCS(ISYS)) = C20(1:6) // '_Processes'
                    ELSE
                        DO ITEL = 1, NPROCS(ISYS)
                            IFLUX = FL2BAL(ISYS, ITEL)
                            OUNAME(IPROCS(ISYS) + ITEL - 1) = &
                                    C20(1:6) // '_' // FLXNAM(IFLUX)(1:13)
                        ENDDO
                    ENDIF
                ENDIF
            ENDDO

            idump_out = 0
            DO IDUMP = 1, NDMPAR
                if (dmpbal(idump) == 1) then
                    idump_out = idump_out + 1
                    DANAMP(IDUMP_out) = DANAM(IDUMP)
                    JDUMP(IDUMP_out) = IDUMP_OUT
                endif
            ENDDO
            DANAMP(NDMPAR_OUT + 1) = 'Sum_of_balance_areas'
            JDUMP(NDMPAR_OUT + 1) = NDMPAR_OUT + 1

            IF (.NOT. SUPPFT) &
                    CALL open_waq_files (LUNOUT, LCHOUT, 21, 1, IDUM)

            !         Zero output matrices
            DO IOUT = 1, NOOUT
                DO IDUMP = 1, NDMPAR_OUT + 1
                    BALTOT(IOUT, IDUMP) = 0.0
                    BALANS(IOUT, IDUMP) = 0.0
                ENDDO
            ENDDO
        ELSE
            IFIRST = .FALSE.
        ENDIF

        !**** END OF **** INITIALIZATION *************************************

        !     Loop over dump areas

        ITEL1 = NDMPAR
        IP1 = NDMPAR + NTDMPQ
        ITEL2 = NDMPAR + NTDMPQ + NDMPAR
        IDUMP_OUT = 0
        DO IDUMP = 1, NDMPAR

            !         ONLY second and following calls !!!!!!
            IF (.NOT.IFIRST) THEN

                IF (DMPBAL(IDUMP) == 1) THEN
                    IDUMP_OUT = IDUMP_OUT + 1

                    !            Mass / accumulation term, previous mass already here
                    !            Subtract current mass
                    CALL UPDBAL (IDUMP_OUT, NOTOT, IMASSA, IMASSA, 0, &
                            BALANS, NOSUM, ASMASS(1, IDUMP, 1), &
                            -1.0, 1, SFACTO, NOOUT, NOTOT)

                    !            Process INFLOW/OUTFLOW terms, both boundaries and internal
                    !            (the IPOINT array is used to distinguish the two)

                    !            Loop over exchanges relevant for current MON AREA
                    NQC = IPDMP(IDUMP)
                    DO IQC = 1, NQC
                        ITEL1 = ITEL1 + 1
                        IQ = IPDMP(ITEL1)

                        !                Is it a boundary? Which type/fraction?
                        IPOIN = ABS(IQ)
                        IVAN = IPOINT(1, IPOIN)
                        INAAR = IPOINT(2, IPOIN)
                        IF (IVAN < 0 .OR. &
                                INAAR < 0) THEN
                            !                    BOUNDARY!!!
                            BOUNDA = .TRUE.
                            IF (IVAN < 0) THEN
                                !                        -I TO +J BOUNDARY!!!
                                IBOUN = -IVAN
                            ELSE
                                !                        +I TO -J BOUNDARY!!!
                                IBOUN = -INAAR
                            ENDIF
                            IFRAC = INBTYP(IBOUN)
                        ELSE
                            !                    INTERNAL
                            BOUNDA = .FALSE.
                            IFRAC = 1
                            IF (.NOT. LUMPTR) THEN
                                IF (IQ < 0) THEN
                                    IF (IVAN > 0) IFRAC = SEGDMP(IVAN) + 1
                                ELSE
                                    IF (INAAR > 0) IFRAC = SEGDMP(INAAR) + 1
                                ENDIF
                            ENDIF
                        ENDIF

                        !                Find fluxes
                        IF (IQ > 0) THEN
                            IPQ = IQDMP(IQ)
                            DO ISYS = 1, NOSYS
                                FLTRAN(1, ISYS) = DMPQ(ISYS, IPQ, 2)
                                FLTRAN(2, ISYS) = -DMPQ(ISYS, IPQ, 1)
                            ENDDO
                        ELSE
                            IPQ = IQDMP(-IQ)
                            DO ISYS = 1, NOSYS
                                FLTRAN(1, ISYS) = DMPQ(ISYS, IPQ, 1)
                                FLTRAN(2, ISYS) = -DMPQ(ISYS, IPQ, 2)
                            ENDDO
                        ENDIF

                        !                Update balances
                        IF (BOUNDA) THEN
                            IF (LUMPEM) THEN
                                CALL UPDBAL (IDUMP_OUT, NOSYS, IMASSA, IEMISS, &
                                        0, BALANS, NOSUM, FLTRAN, &
                                        1.0, 2, SFACTO, NOOUT, &
                                        NOTOT)
                            ELSE
                                CALL UPDBAL (IDUMP_OUT, NOSYS, IMASSA, IEMISS, &
                                        (IFRAC - 1) * 2, BALANS, NOSUM, FLTRAN, &
                                        1.0, 2, SFACTO, NOOUT, &
                                        NOTOT)
                            ENDIF
                        ELSE
                            CALL UPDBAL (IDUMP_OUT, NOSYS, IMASSA, ITRANS, &
                                    (IFRAC - 1) * 2, BALANS, NOSUM, FLTRAN, &
                                    1.0, 2, SFACTO, NOOUT, &
                                    NOTOT)
                        ENDIF
                    ENDDO

                    !            Loads

                    IF (LUMPEM) THEN
                        CALL UPDBAL (IDUMP_OUT, NOTOT, IMASSA, IEMISS, 0, &
                                BALANS, NOSUM, ASMASS(1, IDUMP, 3), 1.0, 1, &
                                SFACTO, NOOUT, NOTOT)
                        CALL UPDBAL (IDUMP_OUT, NOTOT, IMASSA, IEMISS, 1, &
                                BALANS, NOSUM, ASMASS(1, IDUMP, 4), -1.0, 1, &
                                SFACTO, NOOUT, NOTOT)
                    ELSE
                        DO IW = 1, NOWST
                            IF (IWDMP(IW, IDUMP_OUT)) THEN
                                IFRAC = INWTYP(IW)
                                IBAL_OFF = (NOBTYP + IFRAC - 1) * 2
                                CALL UPDBAL (IDUMP_OUT, NOTOT, IMASSA, IEMISS, IBAL_OFF, &
                                        BALANS, NOSUM, WSTDMP(1, IW, 1), 1.0, 1, &
                                        SFACTO, NOOUT, NOTOT)
                                IBAL_OFF = (NOBTYP + IFRAC - 1) * 2 + 1
                                CALL UPDBAL (IDUMP_OUT, NOTOT, IMASSA, IEMISS, IBAL_OFF, &
                                        BALANS, NOSUM, WSTDMP(1, IW, 2), -1.0, 1, &
                                        SFACTO, NOOUT, NOTOT)
                            ENDIF
                        ENDDO
                    ENDIF

                    !            Process sources and sinks

                    IF (LUMPPR) THEN
                        !                Copy term from ASMASS array
                        CALL UPDBAL (IDUMP_OUT, NOTOT, IMASSA, IPROCS, 0, &
                                BALANS, NOSUM, ASMASS(1, IDUMP, 2), &
                                1.0, 1, SFACTO, NOOUT, NOTOT)
                    ELSE

                        !                Loop over substances, including sum parameters
                        DO ISYS = 1, NOTOT + NOSUM
                            IF (IMASSA(ISYS) > 0) THEN
                                !                        Substance (sum parameter) is active
                                !                        Loop over relevant processes
                                DO ITEL = 1, NPROCS(ISYS)
                                    !                            Pointers to balance and fluxes array
                                    IOUT = IPROCS(ISYS) + ITEL - 1
                                    IFLUX = FL2BAL(ISYS, ITEL)
                                    !                            Find stoichiometry constant
                                    IF (ISYS <= NOTOT) THEN
                                        ST = STOCHI(ISYS, IFLUX)
                                    ELSE
                                        ST = STOCHL(ISYS - NOTOT, IFLUX)
                                    ENDIF
                                    !                            Update balance
                                    BALANS(IOUT, IDUMP_OUT) = FLXINT(IFLUX, IDUMP) * ST
                                ENDDO
                            ENDIF
                        ENDDO
                    ENDIF

                ELSE

                    ! dump area excluded from mass balance

                    NQC = IPDMP(IDUMP)
                    ITEL1 = ITEL1 + NQC

                ENDIF

                !         End of actions only if we are not at the first level!
            ENDIF
        ENDDO

        !     Fill balance matrix for sum of areas
        !     zero accumulation term of sum segment first
        DO ISYS = 1, NOTOT + NOSUM
            IOUT = IMASSA(ISYS)
            IF (IOUT>0) &
                    BALANS(IOUT, NDMPAR_OUT + 1) = 0.0
        ENDDO
        DO IDUMP_OUT = 1, NDMPAR_OUT
            DO IOUT = 1, NOOUT
                BALANS(IOUT, NDMPAR_OUT + 1) = BALANS(IOUT, NDMPAR_OUT + 1) &
                        + BALANS(IOUT, IDUMP_OUT)
            ENDDO
        ENDDO

        !     Update integrated balance matrix FOR ALL TERMS EXCEPT ACCUMULATION
        DO IDUMP_OUT = 1, NDMPAR_OUT + 1
            DO ISYS = 1, NOTOT + NOSUM
                IF (IMASSA(ISYS) > 0) THEN
                    ITEL1 = IMASSA(ISYS) + 1
                    ITEL2 = IPROCS(ISYS) + NPROCS(ISYS) - 1
                    DO IOUT = ITEL1, ITEL2
                        BALTOT(IOUT, IDUMP_OUT) = BALTOT(IOUT, IDUMP_OUT) + &
                                BALANS(IOUT, IDUMP_OUT)
                    ENDDO
                ENDIF
            ENDDO
        ENDDO

        !     Optionally scale the balance per volume or area

        IF (B_AREA) THEN

            ALLOCATE(DMP_SURF(NDMPAR), STAT = IERR)
            IF (IERR > 0) GOTO 9000
            CALL DMPSURF(NOSEG, NDMPAR, IPDMP(NDMPAR + NTDMPQ + 1), ISEGCOL, SURF, DMP_SURF)
            IDUMP_OUT = 0
            DO IDUMP = 1, NDMPAR
                IF (DMPBAL(IDUMP) == 1) THEN
                    IDUMP_OUT = IDUMP_OUT + 1
                    DO IOUT = 1, NOOUT
                        IF (DMP_SURF(IDUMP)>1.0E-20) THEN
                            BALANS(IOUT, IDUMP_OUT) = BALANS(IOUT, IDUMP_OUT) / DMP_SURF(IDUMP)
                        ELSE
                            BALANS(IOUT, IDUMP_OUT) = -999.0
                        END IF
                    ENDDO
                    TOT_SURF = TOT_SURF + DMP_SURF(IDUMP)
                ENDIF
            ENDDO
            DO IOUT = 1, NOOUT
                IF (TOT_SURF>1.0E-20) THEN
                    BALANS(IOUT, NDMPAR_OUT + 1) = BALANS(IOUT, NDMPAR_OUT + 1) / TOT_SURF
                ELSE
                    BALANS(IOUT, NDMPAR_OUT + 1) = -999.0
                END IF
            ENDDO
            DEALLOCATE(DMP_SURF)

        ELSEIF (B_VOLU) THEN

            ALLOCATE(DMP_VOLU(NDMPAR), STAT = IERR)
            IF (IERR > 0) GOTO 9000
            CALL DMPVAL(NDMPAR, IPDMP(NDMPAR + NTDMPQ + 1), VOLUME, DMP_VOLU)
            IDUMP_OUT = 0
            DO IDUMP = 1, NDMPAR
                IF (DMPBAL(IDUMP) == 1) THEN
                    IDUMP_OUT = IDUMP_OUT + 1
                    DO IOUT = 1, NOOUT
                        BALANS(IOUT, IDUMP_OUT) = BALANS(IOUT, IDUMP_OUT) / DMP_VOLU(IDUMP)
                    ENDDO
                    TOT_VOLU = TOT_VOLU + DMP_VOLU(IDUMP)
                ENDIF
            ENDDO
            DO IOUT = 1, NOOUT
                BALANS(IOUT, NDMPAR_OUT + 1) = BALANS(IOUT, NDMPAR_OUT + 1) / TOT_VOLU
            ENDDO
            DEALLOCATE(DMP_VOLU)

        ENDIF


        !     Write time dependent output
        IF (IFIRST) THEN
            IINIT = 1
        ELSE
            IINIT = 0
        ENDIF
        IF (.NOT. SUPPFT) THEN
            IF (ONLYSM) THEN
                CALL OUTHIS (LUNOUT, CDUM, ITIME, MONAME, 1, &
                        JDUMP(NDMPAR_OUT + 1), DANAMP(NDMPAR_OUT + 1), &
                        NOOUT, OUNAME, BALANS, &
                        0, CDUM, RDUM, IINIT)
            ELSE
                CALL OUTHIS (LUNOUT, CDUM, ITIME, MONAME, NDMPAR_OUT + 1, &
                        JDUMP, DANAMP, NOOUT, OUNAME, BALANS, &
                        0, CDUM, RDUM, IINIT)
            ENDIF
        ENDIF

        !     Zero output matrix
        DO IOUT = 1, NOOUT
            DO IDUMP = 1, NDMPAR_OUT + 1
                BALANS(IOUT, IDUMP) = 0.0
            ENDDO
        ENDDO

        !     Store current mass in Mass term as a starting point for next step
        IDUMP_OUT = 0
        DO IDUMP = 1, NDMPAR
            IF (DMPBAL(IDUMP) == 1) THEN
                IDUMP_OUT = IDUMP_OUT + 1
                CALL UPDBAL (IDUMP_OUT, NOTOT, IMASSA, IMASSA, 0, &
                        BALANS, NOSUM, ASMASS(1, IDUMP, 1), &
                        1.0, 1, SFACTO, NOOUT, NOTOT)
                !           Sum segment (OBSOLETE??)
                DO ISYS = 1, NOTOT + NOSUM
                    IOUT = IMASSA(ISYS)
                    IF (IOUT > 0) THEN
                        BALANS(IOUT, NDMPAR_OUT + 1) = BALANS(IOUT, NDMPAR_OUT + 1) &
                                + BALANS(IOUT, IDUMP_OUT)
                    ENDIF
                ENDDO
            ENDIF
        ENDDO

        !     Update integrated balance matrix
        !     Mass/accumulation term only FIRST time
        IF (IFIRST) THEN
            DO ISYS = 1, NOTOT + NOSUM
                IOUT = IMASSA(ISYS)
                IF (IOUT > 0) THEN
                    DO IDUMP = 1, NDMPAR_OUT + 1
                        BALTOT(IOUT, IDUMP) = BALANS(IOUT, IDUMP)
                    ENDDO
                ENDIF
            ENDDO
        ENDIF

        !     This is an incorrect statement in case ITIME never reaches
        !     one of the two time levels
        IF (ITIME >= ITSTOP - IMSTEP + 1 .OR. ITIME >= IMSTOP) THEN

            IBSTRT = MAX(ITSTRT, IMSTRT)
            IBSTOP = MIN(ITIME, IMSTOP)

            IF (.NOT. SUPPFT) CLOSE (LUNOUT)
            DO ISYS = 1, NOTOT + NOSUM
                IOUT = IMASSA(ISYS)
                IF (IOUT > 0) THEN
                    DO IDUMP = 1, NDMPAR_OUT + 1
                        BALTOT(IOUT, IDUMP) = BALTOT(IOUT, IDUMP) &
                                - BALANS(IOUT, IDUMP)
                    ENDDO
                ENDIF
            ENDDO

            FILNAM = LCHOUT
            INDX = INDEX(FILNAM, '-bal.his')
            IF (INDX > 0) THEN
                FILNAM(INDX:) = '-bal.prn'
            ELSE
                FILNAM = 'sobwqbal.prn'
            ENDIF
            OPEN (NEWUNIT = IOBALI, FILE = FILNAM)

            !         In mass
            CALL OUTBAI (IOBALI, MONAME, IBSTRT, IBSTOP, NOOUT, &
                    NOTOT, NDMPAR_OUT + 1, DANAMP, OUNAME, SYNAMP, &
                    IMASSA, IEMISS, NEMISS, ITRANS, NTRANS, &
                    IPROCS, NPROCS, BALTOT, ONLYSM, NOSUM, &
                    SFACTO, 0, 1)

            !         In mass/m2
            ALLOCATE(DMP_SURF(NDMPAR), STAT = IERR)
            IF (IERR > 0) GOTO 9000
            CALL DMPSURF(NOSEG, NDMPAR, IPDMP(NDMPAR + NTDMPQ + 1), ISEGCOL, SURF, DMP_SURF)
            IDUMP_OUT = 0
            DO IDUMP = 1, NDMPAR
                IF (DMPBAL(IDUMP) == 1) THEN
                    IDUMP_OUT = IDUMP_OUT + 1
                    DO IOUT = 1, NOOUT
                        IF (DMP_SURF(IDUMP)>1.0E-20) THEN
                            BALTOT(IOUT, IDUMP_OUT) = BALTOT(IOUT, IDUMP_OUT) / DMP_SURF(IDUMP)
                        ELSE
                            BALTOT(IOUT, IDUMP_OUT) = -999.0
                        END IF
                    ENDDO
                    TOT_SURF = TOT_SURF + DMP_SURF(IDUMP)
                ENDIF
            ENDDO
            DO IOUT = 1, NOOUT
                IF (TOT_SURF>1.0E-20) THEN
                    BALTOT(IOUT, NDMPAR_OUT + 1) = BALTOT(IOUT, NDMPAR_OUT + 1) / TOT_SURF
                ELSE
                    BALTOT(IOUT, NDMPAR_OUT + 1) = -999.0
                END IF
            ENDDO
            CALL OUTBAI (IOBALI, MONAME, IBSTRT, IBSTOP, NOOUT, &
                    NOTOT, NDMPAR_OUT + 1, DANAMP, OUNAME, SYNAMP, &
                    IMASSA, IEMISS, NEMISS, ITRANS, NTRANS, &
                    IPROCS, NPROCS, BALTOT, ONLYSM, NOSUM, &
                    SFACTO, 1, 0)

            !         In mass/m3
            ALLOCATE(DMP_VOLU(NDMPAR), STAT = IERR)
            IF (IERR > 0) GOTO 9000
            CALL DMPVAL(NDMPAR, IPDMP(NDMPAR + NTDMPQ + 1), VOLUME, DMP_VOLU)
            IDUMP_OUT = 0
            DO IDUMP = 1, NDMPAR
                IF (DMPBAL(IDUMP) == 1) THEN
                    IDUMP_OUT = IDUMP_OUT + 1
                    DO IOUT = 1, NOOUT
                        BALTOT(IOUT, IDUMP_OUT) = BALTOT(IOUT, IDUMP_OUT) * DMP_SURF(IDUMP) / DMP_VOLU(IDUMP)
                    ENDDO
                    TOT_VOLU = TOT_VOLU + DMP_VOLU(IDUMP)
                ENDIF
            ENDDO
            DO IOUT = 1, NOOUT
                BALTOT(IOUT, NDMPAR_OUT + 1) = BALTOT(IOUT, NDMPAR_OUT + 1) * TOT_SURF / TOT_VOLU
            ENDDO
            CALL OUTBAI (IOBALI, MONAME, IBSTRT, IBSTOP, NOOUT, &
                    NOTOT, NDMPAR_OUT + 1, DANAMP, OUNAME, SYNAMP, &
                    IMASSA, IEMISS, NEMISS, ITRANS, NTRANS, &
                    IPROCS, NPROCS, BALTOT, ONLYSM, NOSUM, &
                    SFACTO, 2, 0)
            DEALLOCATE(DMP_VOLU)
            DEALLOCATE(DMP_SURF)

            CLOSE (IOBALI)
        ENDIF

        INIOUT = 0

        if (timon) call timstop (ithandl)
        return
        9000 write (lunrep, *) 'Error allocating memory'
        write (*, *) 'Error allocating memory'
        call terminate_execution(1)
    end
    SUBROUTINE OUTBAI (IOBALI, MONAME, IBSTRT, IBSTOP, NOOUT, &
            NOTOT, NDMPAR, DANAMP, OUNAME, SYNAME, &
            IMASSA, IEMISS, NEMISS, ITRANS, NTRANS, &
            IPROCS, NPROCS, BALTOT, ONLYSM, NOSUM, &
            SFACTO, IUNIT, INIT)
        use timers

        INTEGER(kind = int_wp) :: IOBALI, IBSTRT, IBSTOP, NOOUT, NOTOT, NDMPAR, &
                IMASSA(*), IEMISS(*), NEMISS, ITRANS(*), NTRANS, &
                IPROCS(*), NPROCS(*), NOSUM, IUNIT, INIT
        character(len=40) MONAME(4)
        character(len=20) DANAMP(NDMPAR), OUNAME(*), SYNAME(*)
        REAL(kind = real_wp) :: BALTOT(NOOUT, NDMPAR), SFACTO(NOSUM, *)
        LOGICAL      ONLYSM

        REAL(kind = real_wp) :: VALUE, VALUE1, VALUE2, SUMPOS, SUMNEG
        INTEGER(kind = int_wp) :: IDUMP, ISYS, ITEL, I, ITEL2, ISUM
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("outbai", ithandl)

        IF (INIT == 1) THEN
            !         Write header
            WRITE (IOBALI, 1000) MONAME(1), MONAME(2), MONAME(3), MONAME(4)

            !         Write timers
            WRITE (IOBALI, 1010) REAL(IBSTRT) / 86400., REAL(IBSTOP) / 86400.

            !         Write sum parameters
            DO ISUM = 1, NOSUM
                IF (IMASSA(NOTOT + ISUM) > 0) THEN
                    WRITE (IOBALI, 1020) SYNAME(NOTOT + ISUM)
                    DO ISYS = 1, NOTOT
                        IF (SFACTO(ISUM, ISYS) >= 0.0001) THEN
                            WRITE (IOBALI, 1030) SYNAME(ISYS), SFACTO(ISUM, ISYS)
                        ENDIF
                    ENDDO
                ENDIF
            ENDDO
        ENDIF

        !     Write the unit of the balance
        IF (IUNIT == 0) THEN
            WRITE (IOBALI, 1040)
        ELSEIF (IUNIT == 1) THEN
            WRITE (IOBALI, 1050)
        ELSEIF (IUNIT == 2) THEN
            WRITE (IOBALI, 1060)
        ENDIF


        !     The balance per area
        DO IDUMP = 1, NDMPAR
            IF (.NOT. ONLYSM .OR. IDUMP == NDMPAR) THEN
                WRITE (IOBALI, 1100) DANAMP(IDUMP)
                DO ISYS = 1, NOTOT + NOSUM
                    ITEL = IMASSA(ISYS)
                    IF (ITEL > 0) THEN
                        WRITE (IOBALI, 1110) SYNAME(ISYS)
                        SUMPOS = 0.0
                        SUMNEG = 0.0

                        !                     Mass term
                        VALUE = BALTOT(ITEL, IDUMP)
                        IF (VALUE > 0.0) THEN
                            VALUE1 = VALUE
                            VALUE2 = 0.0
                        ELSE
                            VALUE1 = 0.0
                            VALUE2 = VALUE
                        ENDIF
                        WRITE (IOBALI, 1120) SYNAME(ISYS)(1:6), &
                                VALUE1, VALUE2
                        SUMPOS = SUMPOS + VALUE1
                        SUMNEG = SUMNEG + VALUE2

                        !                     Inputs from boundaries
                        DO I = 1, NEMISS / 2
                            ITEL2 = (I - 1) * 2 + IEMISS(ISYS)
                            VALUE1 = BALTOT(ITEL2, IDUMP)
                            VALUE2 = BALTOT(ITEL2 + 1, IDUMP)
                            WRITE (IOBALI, 1130) OUNAME(ITEL2)(1:16), &
                                    VALUE1, VALUE2
                            SUMPOS = SUMPOS + VALUE1
                            SUMNEG = SUMNEG + VALUE2
                        ENDDO

                        !                     Internal transport
                        IF (NTRANS == 2) THEN
                            VALUE1 = BALTOT(ITRANS(ISYS), IDUMP)
                            VALUE2 = BALTOT(ITRANS(ISYS) + 1, IDUMP)
                            WRITE (IOBALI, 1140) SYNAME(ISYS)(1:6), &
                                    VALUE1, VALUE2
                            SUMPOS = SUMPOS + VALUE1
                            SUMNEG = SUMNEG + VALUE2
                        ELSE
                            DO I = 1, NTRANS / 2
                                ITEL2 = (I - 1) * 2 + ITRANS(ISYS)
                                VALUE1 = BALTOT(ITEL2, IDUMP)
                                VALUE2 = BALTOT(ITEL2 + 1, IDUMP)
                                WRITE (IOBALI, 1130) OUNAME(ITEL2)(1:16), &
                                        VALUE1, VALUE2
                                SUMPOS = SUMPOS + VALUE1
                                SUMNEG = SUMNEG + VALUE2
                            ENDDO
                        ENDIF

                        !                     Processes
                        DO I = 1, NPROCS(ISYS)
                            ITEL2 = IPROCS(ISYS) - 1 + I
                            VALUE = BALTOT(ITEL2, IDUMP)
                            IF (VALUE >= 0.0) THEN
                                VALUE1 = VALUE
                                VALUE2 = 0.0
                            ELSE
                                VALUE1 = 0.0
                                VALUE2 = VALUE
                            ENDIF
                            WRITE (IOBALI, 1150) OUNAME(ITEL2), &
                                    VALUE1, VALUE2
                            SUMPOS = SUMPOS + VALUE1
                            SUMNEG = SUMNEG + VALUE2
                        ENDDO
                        WRITE (IOBALI, 1160) SUMPOS, SUMNEG
                    ENDIF
                ENDDO
            ENDIF
        ENDDO

        1000 FORMAT ('Mass balances output file'// &
                a40/a40/a40// &
                'All terms in basic mass units,' &
                'for Processes Library always (g)'// &
                'Simulation starts: ', a40)
        1010 FORMAT ('Mass balances output period:'/ &
                'start: ', f9.3, ' days'/ &
                'stop : ', f9.3, ' days')
        1020 FORMAT (/'Balance for sum parameter ', a, ' consists of:'/ &
                'substance           scale factor')
        1030 FORMAT (A20, F10.4)
        1040 FORMAT (// &
                'MASS BALANCE PER DUMPAREA'/ &
                'All terms in basic mass units,' &
                'for Processes Library always (g)')
        1050 FORMAT (// &
                'MASS BALANCE PER SURFACE'/ &
                'All terms in basic mass units/m2,' &
                'for Processes Library always (g)')
        1060 FORMAT (// &
                'MASS BALANCE PER VOLUME'/ &
                'All terms in basic mass units/m3,' &
                'for Processes Library always (g)')
        1100 FORMAT (// &
                '============================================================'/ &
                'Mass balances for ', a/ &
                '============================================================')
        1110 FORMAT (/'Substance ', a20, ' Sources/Inflows Sinks/Outflows'/ &
                '------------------------------------------------------------')
        1120 FORMAT (a6, '_Storage ', 15x, 2e15.5)
        1130 FORMAT (a16, 14x, 2e15.5)
        1140 FORMAT (a6, '_Internal transport', 5x, 2e15.5)
        1150 FORMAT (a20, 10x, 2e15.5)
        1160 FORMAT ('SUM OF ALL TERMS              ', 2e15.5)

        if (timon) call timstop (ithandl)
        RETURN
    END

    subroutine comsum (nosum, tfacto, notot, syname, sfacto, nocons, coname, cons)

        use m_logger, only : terminate_execution, get_log_unit_number
        use timers
        use bloom_data_mass_balance

        implicit none

        integer(kind = int_wp) :: nosum, notot, nocons
        character(len=20)       syname(notot), coname(nocons)
        real(kind = real_wp) :: tfacto(nosum), sfacto(nosum, notot), cons(nocons)

        !      INCLUDE 'cblbal.inc'

        integer(kind = int_wp) :: isum, isys, icons, ires, nres1, nres2, ityp, lunrep
        real(kind = real_wp) :: factor
        parameter           (nres1 = 23, nres2 = 2)
        character(len=20)         resna1(nres1), resna2(nres2)
        character(len=10)         ratna2(2, nres2)
        real(kind = real_wp) :: facres(2, nres1), ratdef(2, nres2)

        data resna1   / 'DetP                ', &
                'OOP                 ', &
                'AlgP                ', &
                'AAP                 ', &
                'PO4                 ', &
                'PAP                 ', &
                'POP1                ', &
                'POP2                ', &
                'POP3                ', &
                'POP4                ', &
                'DOP                 ', &
                'APATP               ', &
                'VIVP                ', &
                'DetN                ', &
                'OON                 ', &
                'AlgN                ', &
                'NO3                 ', &
                'NH4                 ', &
                'PON1                ', &
                'PON2                ', &
                'PON3                ', &
                'PON4                ', &
                'DON                 '/
        data facres   / 0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                0.0, 1.0, &
                1.0, 0.0, &
                1.0, 0.0, &
                1.0, 0.0, &
                1.0, 0.0, &
                1.0, 0.0, &
                1.0, 0.0, &
                1.0, 0.0, &
                1.0, 0.0, &
                1.0, 0.0, &
                1.0, 0.0/
        data resna2   / 'Diat                ', &
                'Green               '/
        data ratna2   / 'NCRatDiat ', 'PCRatDiat ', &
                'NCRatGreen', 'PCRatGreen'/
        data ratdef   / 0.16, 0.02, &
                0.16, 0.02/
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("consum", ithandl)

        !     Compose sum parameters
        !     local functionality nosum = 2, check!!!!!!!!!!!!!

        if (nosum /= 2) then
            call get_log_unit_number(lunrep)
            write (lunrep, *) 'BUG IN COMSUM!'
            write (*, *) 'BUG IN COMSUM!'
            call terminate_execution(1)
        end if

        !     Initialise substance shares in sum parameters as well as totals
        !     (totals are used to find out if sum parameter is active)

        do isum = 1, nosum
            tfacto(isum) = 0.0
            do isys = 1, notot
                sfacto(isum, isys) = 0.0
            enddo
        enddo

        do isys = 1, notot

            !         Reserved substance names, FIXED scale factor
            ires = index_in_array(syname(isys), resna1)
            if (ires > 0) then
                do isum = 1, nosum
                    tfacto(isum) = tfacto(isum) + facres(isum, ires)
                    sfacto(isum, isys) = facres(isum, ires)
                enddo
            endif

            !         Reserved substance names, scale factors from CONS with default
            ires = index_in_array(syname(isys), resna2)
            if (ires > 0) then
                do isum = 1, nosum
                    icons = index_in_array(ratna2(isum, ires), coname)
                    if (icons > 0) then
                        factor = cons(icons)
                    else
                        factor = ratdef(isum, ires)
                    endif
                    tfacto(isum) = tfacto(isum) + factor
                    sfacto(isum, isys) = factor
                enddo
            endif
        enddo

        !     BLOOM algae

        if (ntypa2 > 0) then

            !         BLOOM active!

            do ityp = 1, ntypa2
                isys = iblsub(ityp)
                factor = ncralg(ityp)
                tfacto(1) = tfacto(1) + factor
                sfacto(1, isys) = factor
                factor = pcralg(ityp)
                tfacto(2) = tfacto(2) + factor
                sfacto(2, isys) = factor
            enddo
        endif

        if (timon) call timstop (ithandl)
        return
    end

    SUBROUTINE UPDBAL (IDUMP, NOTOT, IMASSA, ITERMS, IOFFSE, &
            BALANS, NOSUM, DMASSA, FACTOR, NTEL, &
            SFACTO, NOOUT, NOLAST)

        !     IDUMP               index of current monitoring area
        !     NOTOT               nr of substances to be processed
        !     IMASSA              position of accumulation term per substance
        !                         (used as indicator if substance has balance)
        !     ITERMS              position of (first) balance term to be updated
        !     IOFFSE              offset to be added to ITERMS
        !     BALANS              mass balances array to be updated
        !     NOSUM               nr. of sum parameters
        !     DMASSA              fluxes to be added to mass balances
        !     FACTOR              scale factor to be applied
        !     NTEL                nr of terms to be updated
        !     SFACTO              relations between sum parameters and state variables
        !     NOOUT               total nr of mass balance terms
        !     NOLAST              last state variable
        !

        use timers
        INTEGER(kind = int_wp) :: IDUMP, NOTOT, NOSUM, NOOUT, IOFFSE, NTEL, &
                NOLAST
        INTEGER(kind = int_wp) :: IMASSA(*), ITERMS(*)
        REAL(kind = real_wp) :: BALANS(NOOUT, *), DMASSA(NTEL, *), &
                FACTOR, SFACTO(NOSUM, *)

        INTEGER(kind = int_wp) :: ISYS, IOUT, ISYSS, IOUT2, ISUM, &
                ITEST, ITEL, ITEL2
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("updbal", ithandl)

        DO ISYS = 1, NOTOT
            IOUT = ITERMS(ISYS) + IOFFSE
            DO ITEL = 1, NTEL
                ITEL2 = IOUT + (ITEL - 1)
                BALANS(ITEL2, IDUMP) = &
                        BALANS(ITEL2, IDUMP) + DMASSA(ITEL, ISYS) * FACTOR
            ENDDO
            DO ISUM = 1, NOSUM
                !              Bug fix, 5-1-2002
                !              ISYSS = NOTOT+ISUM
                ISYSS = NOLAST + ISUM
                ITEST = IMASSA(ISYSS)
                IF (ITEST > 0) THEN
                    !                 Sum parameter is active
                    IF (SFACTO(ISUM, ISYS) >= 0.0001) THEN
                        !                     Current substance contributes
                        IOUT2 = ITERMS(ISYSS) + IOFFSE
                        DO ITEL = 1, NTEL
                            ITEL2 = IOUT2 + (ITEL - 1)
                            BALANS(ITEL2, IDUMP) = BALANS(ITEL2, IDUMP) &
                                    + DMASSA(ITEL, ISYS) * SFACTO(ISUM, ISYS) &
                                            * FACTOR
                        ENDDO
                    ENDIF
                ENDIF
            ENDDO
        ENDDO

        if (timon) call timstop (ithandl)
        RETURN
    END


end module m_sobbal
