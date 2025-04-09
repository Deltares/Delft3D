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
module m_write_balance_output
   use m_waq_precision
   use m_string_utils

   implicit none

   private
   public :: write_balance_text_output

contains

   subroutine write_balance_text_output(num_substances_total, itime, num_substances_transported, noflux, ndmpar, &
                                        ndmpq, ntdmpq, itstop, imstrt, imstop, &
                                        iqdmp, ipdmp, asmass, flxint, stochi, &
                                        syname, danam, moname, dmpq, num_boundary_conditions, &
                                        num_boundary_types, bndtyp, inbtyp, num_constants, coname, &
                                        cons, num_exchanges, ipoint, flxnam, intopt, &
                                        volume, surf, num_cells, lunout, lchout, &
                                        iniout, dmpbal, num_waste_loads, num_waste_load_types, wsttyp, &
                                        iwaste, inwtyp, wstdmp, isegcol, imstep)

        !! Integrated emissions and processes balance

      !
      !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     num_substances_total   INTEGER       1     INPUT   Total number of substances
      !     ITIME   INTEGER       1     INPUT   Time in system clock units
      !     ITSTOP  INTEGER       1     INPUT   Stop time of the simulation
      !     IMSTRT  INTEGER       1     INPUT   Start time of the output
      !     IMSTOP  INTEGER       1     INPUT   Stop time of the output
      !     MONAME  CHAR*40       4     INPUT   Model and run names
      !     SYNAME  CHAR*20    num_substances_total    INPUT   names of substances
      !     num_substances_transported   INTEGER       1     INPUT   Number of active substances
      !     ASMASS  REAL       num_substances_total,*  IN/OUT  Cummulative balance per dump area
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
      !     STOCHI  REAL   num_substances_total*NOFLUX INPUT   Proces stochiometry
      !     DMPQ    REAL  num_substances_transported*NDMPQ*? INPUT   mass balance dumped exchange
      !     num_boundary_conditions   INTEGER     1       INPUT   number of boundaries
      !     num_boundary_types  INTEGER     1       INPUT   number of boundaries types
      !     BNDTYP  CHAR*20  num_boundary_types     INPUT   boundary types names
      !     INBTYP  INTEGER  num_boundary_conditions      INPUT   boundary type number (index in BNDTYP)
      !     num_constants  INTEGER     1       INPUT   number of constants
      !     CONAME  CHAR*20  num_constants     INPUT   constants names
      !     CONS    REAL     num_constants     INPUT   constants array
      !     num_exchanges     INTEGER     1       INPUT   number of exchanges
      !     IPOINT  INTEGER   4,num_exchanges     INPUT   pointer array
      !     FLXNAM  INTEGER  NOFLUX     INPUT   flux names
      !     INTOPT  INTEGER     1       INPUT   Integration and balance suboptions
      !     VOLUME  REAL     num_cells      INPUT   Volume
      !     SURF    REAL     num_cells      INPUT   horizontal surface area
      !     DMPBAL  INTEGER  NDMPAR     INPUT   if dump area is included in balance
      !     num_waste_loads   INTEGER     1       INPUT   number of wasteloads
      !     num_waste_load_types  INTEGER     1       INPUT   number of wasteload types
      !     WSTTYP  CHAR*20  num_waste_load_types     INPUT   wasteload types names
      !     IWASTE  INTEGER  num_waste_loads      INPUT   segment number wasteloads
      !     INWTYP  INTEGER  num_waste_loads      INPUT   wasteload type number (index in WSTTYP)
      !     WSTDMP  REAL     num_substances_total,num_waste_loads,2  I   accumulated wasteloads 1/2 in and out
      !     ==================================================================
      use m_write_binary_output, only: write_binary_history_output
      use m_logger_helper, only: stop_with_error, get_log_unit_number
      use data_processing, only: extract_value_from_group
      use m_cli_utils, only: get_command_argument_by_name
      use m_open_waq_files
      use timers
      integer(kind=int_wp) :: num_substances_total, ITIME, num_substances_transported, num_cells, LUNOUT, &
                              NOFLUX, NDMPAR, NDMPQ, NTDMPQ, &
                              num_boundary_conditions, ITSTOP, IMSTOP, IMSTRT, IMSTEP, &
                              num_boundary_types, num_constants, num_exchanges, INIOUT, INTOPT
      integer(kind=int_wp) :: IQDMP(*), IPDMP(*), &
                              INBTYP(num_boundary_conditions), IPOINT(4, num_exchanges)
      real(kind=real_wp) :: DMPQ(num_substances_transported, NDMPQ, *), &
                            ASMASS(num_substances_total, NDMPAR, *), FLXINT(NOFLUX, *), &
                            STOCHI(num_substances_total, NOFLUX), CONS(num_constants), &
                            VOLUME(*), SURF(*)
      character(len=20) SYNAME(*), DANAM(*), &
         BNDTYP(num_boundary_types), CONAME(num_constants), &
         FLXNAM(NOFLUX)
      character(len=40) MONAME(4)
      character(len=255) LCHOUT
      integer(kind=int_wp) :: dmpbal(ndmpar) ! indicates if dump area is included in the balance
      integer(kind=int_wp) :: num_waste_loads ! number of wasteloads
      integer(kind=int_wp) :: num_waste_load_types ! number of wasteload types
      character(len=20) :: wsttyp(num_waste_load_types) ! wasteload types names
      integer(kind=int_wp) :: iwaste(num_waste_loads) ! segment numbers of the wasteloads
      integer(kind=int_wp) :: inwtyp(num_waste_loads) ! wasteload type number (index in wsttyp)
      real(kind=real_wp) :: wstdmp(num_substances_total, num_waste_loads, 2) ! accumulated wasteloads 1/2 in and out
      integer(kind=int_wp), intent(in) :: isegcol(*) ! pointer from segment to top of column

      !     Local declarations
      !
      !     NAME    KIND     LENGTH     DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     NOSUM   INTEGER     1       Nr of sum parameters
      !     SFACTO  REAL   NOSUM,num_substances_total  Factor for substance in sum parameters
      !     STOCHL  REAL   NOSUM,NOFLUX Local STOCHI for sum parameters
      !     NOOUT   INTEGER     1       Nr of balance terms per dump segment
      !     IMASSA  INTEGER num_substances_total+NOSUM Pointer to accumulation term in balance
      !     IEMISS  INTEGER num_substances_total+NOSUM Pointer to boundary terms in balance
      !     NEMISS  INTEGER     1       Nr of boundary terms in balance
      !     ITRANS  INTEGER num_substances_total+NOSUM Pointer to int. transport terms in balance
      !     IPROCS  INTEGER num_substances_total+NOSUM Pointer to processes term(s) in balance
      !     NPROCS  INTEGER num_substances_total+NOSUM Nr of processes terms in balance
      !     BALANS  REAL   NOOUT,*      Mass balances for current time
      !     BALTOT  REAL   NOOUT,*      Integrated mass balances
      !     OUNAME  C*20      NOOUT     Names of terms in balances
      !     FL2BAL  INT   num_substances_total+NOSUM,* Pointer to relevant fluxes per substance
      !     DANAMP  C*20     NDMPAR+1   Copy of DANAM including sum segment
      !     SYNAMP  C*20  num_substances_total+NOSUM   Copy of SYNAME including sum parameters
      !     IBSTRT  INTEGER     1       Proper start time of the balance period
      !     IBSTOP  INTEGER     1       Proper stop time of the balance period

      integer(kind=int_wp) :: IOSOBH, IOBALI, ISYS, IBOUN, NEMISS, IFRAC, &
                              IFLUX, IDUMP, IPQ, ISYS2, ITEL, IINIT, &
                              ITEL1, IP1, ITEL2, NQC, IQC, IQ, &
                              IPOIN, NOOUT, IERR, IOUT, ISUM, NOSUM, &
                              NSC, ISC, LUNREP, IBSTRT, IBSTOP
      parameter(NOSUM=2)
      real(kind=real_wp), allocatable :: SFACTO(:, :), &
                                         STOCHL(:, :), &
                                         FLTRAN(:, :), &
                                         BALANS(:, :), &
                                         BALTOT(:, :), &
                                         DMP_SURF(:), &
                                         DMP_VOLU(:)
      integer(kind=int_wp), allocatable :: JDUMP(:), &
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
      logical :: parsing_error
      logical LUMPEM, LUMPPR, IFIRST, SUPPFT, ONLYSM, &
         INCLUD, BOUNDA, LUMPTR, B_AREA, B_VOLU
      real(kind=real_wp) :: RDUM(1)
      real(kind=real_wp) :: ST, TFACTO(NOSUM)
      character(len=20) C20, SYNAMS(NOSUM)
      character(len=40) CDUM
      character(len=255) FILNAM
      character(len=2) c2
      character(:), allocatable :: inifil
      integer(kind=int_wp) :: lunini
      integer(kind=int_wp) :: iseg, iw, idum, ivan, ibal_off, idump_out
      integer(kind=int_wp) :: ndmpar_out, ntrans, indx
      integer(kind=int_wp) :: inaar, itstrt
      real(kind=real_wp) :: tot_surf, tot_volu
      data LUMPEM/.true./
      data LUMPPR/.true./
      data SUPPFT/.true./
      data ONLYSM/.true./
      data LUMPTR/.false./
      data B_AREA/.false./
      data B_VOLU/.false./
      data SYNAMS/'TotN', 'TotP'/

      save
      integer(kind=int_wp) :: ithandl = 0

      ! Skip this routine when there are no balance area's
      if (NDMPAR == 0) return

      if (timon) call timstrt("write_balance_text_output", ithandl)

      if (INIOUT == 1) then
         IFIRST = .true.
         call get_log_unit_number(LUNREP)

         lumppr = .not. btest(intopt, 8)
         lumpem = .not. btest(intopt, 9)
         lumptr = .not. btest(intopt, 10)
         b_area = btest(intopt, 11)
         b_volu = btest(intopt, 12)
         onlysm = .not. btest(intopt, 13)
         suppft = .not. btest(intopt, 14)

         ! from ini file
         if (get_command_argument_by_name('-i', inifil, parsing_error)) then
            if (parsing_error) then
               inifil = ' '
            end if
         else
            inifil = 'delwaq.ini'
         end if
         open (newunit=lunini, file=inifil, status='old', err=123)
         call extract_value_from_group(lunini, 'Balance Options', 'LumpProcessesContributions', c2)
         if (c2 == '-1') lumppr = .true.
         if (c2 == '0') lumppr = .false.
         call extract_value_from_group(lunini, 'Balance Options', 'LumpBoundaryContributions', c2)
         if (c2 == '-1') lumpem = .true.
         if (c2 == '0') lumpem = .false.
         call extract_value_from_group(lunini, 'Balance Options', 'SumOfMonitoringAreas', c2)
         if (c2 == '-1') onlysm = .true.
         if (c2 == '0') onlysm = .false.
         call extract_value_from_group(lunini, 'Balance Options', 'SuppressTimeDependentOutput', c2)
         if (c2 == '-1') suppft = .true.
         if (c2 == '0') suppft = .false.
         close (lunini)
123      continue

         ! count number of output dump areas
         ndmpar_out = 0
         do idump = 1, ndmpar
            if (dmpbal(idump) == 1) then
               ndmpar_out = ndmpar_out + 1
            end if
         end do

         ! Dimension arrays
         if (allocated(fltran)) then
            deallocate (FLTRAN, JDUMP, SFACTO, DANAMP, SYNAMP, IMASSA, IEMISS, ITRANS, IPROCS, NPROCS, &
                        STOCHL, FL2BAL)
         end if

         allocate (FLTRAN(2, num_substances_transported), &
                   JDUMP(NDMPAR_OUT + 1), &
                   SFACTO(NOSUM, num_substances_total), &
                   DANAMP(NDMPAR_OUT + 1), &
                   SYNAMP(num_substances_total + NOSUM), &
                   IMASSA(num_substances_total + NOSUM), &
                   IEMISS(num_substances_total + NOSUM), &
                   ITRANS(num_substances_total + NOSUM), &
                   IPROCS(num_substances_total + NOSUM), &
                   NPROCS(num_substances_total + NOSUM), &
                   STOCHL(NOSUM, NOFLUX), &
                   FL2BAL(num_substances_total + NOSUM, NOFLUX), &
                   STAT=IERR)
         if (IERR > 0) goto 9000
         if (.not. LUMPTR) then

            ! allocate and set SEGDMP, first dump number for each segment (if any)
            if (allocated(segdmp)) then
               deallocate (segdmp)
            end if
            allocate (SEGDMP(num_cells), &
                      STAT=IERR)
            if (IERR > 0) goto 9000
            SEGDMP = 0
            ITEL = 0
            IDUMP_OUT = 0
            do IDUMP = 1, NDMPAR
               NSC = IPDMP(NDMPAR + NTDMPQ + IDUMP)
               if (DMPBAL(IDUMP) == 1) then
                  IDUMP_OUT = IDUMP_OUT + 1
                  do ISC = 1, NSC
                     ITEL = ITEL + 1
                     ISEG = IPDMP(NDMPAR + NTDMPQ + NDMPAR + ITEL)
                     if (ISEG > 0) then
                        if (SEGDMP(ISEG) == 0) then
                           SEGDMP(ISEG) = IDUMP_OUT
                        end if
                     end if
                  end do
               else
                  ITEL = ITEL + NSC
               end if
            end do

         end if
         if (.not. lumpem) then
            ! allocate and set IWDMP, set to true is wasteload is in dump area
            if (allocated(iwdmp)) then
               deallocate (iwdmp)
            end if
            allocate (iwdmp(num_waste_loads, ndmpar), stat=ierr)
            if (ierr > 0) goto 9000
            iwdmp = .false.
            itel = 0
            idump_out = 0
            do idump = 1, ndmpar
               nsc = ipdmp(ndmpar + ntdmpq + idump)
               if (dmpbal(idump) == 1) then
                  idump_out = idump_out + 1
                  do isc = 1, nsc
                     itel = itel + 1
                     iseg = ipdmp(ndmpar + ntdmpq + ndmpar + itel)
                     if (iseg > 0) then
                        do iw = 1, num_waste_loads
                           if (iwaste(iw) == iseg) then
                              iwdmp(iw, idump_out) = .true.
                           end if
                        end do
                     end if
                  end do
               else
                  itel = itel + nsc
               end if
            end do

         end if

         !         Balances are constructed for all system variables
         !         + total N + total P (IF RELEVANT!)
         !         Find which state variables contribute to what extent

         call comsum(nosum, tfacto, num_substances_total, syname, sfacto, &
                     num_constants, coname, cons)
         do isys = 1, num_substances_total
            synamp(isys) = syname(isys)
         end do
         do isum = 1, nosum
            synamp(num_substances_total + isum) = synams(isum)
         end do

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
         if (lumpem) then
            nemiss = 2
         else

            !             boundary types and loads as seperate term (all in and out)
            nemiss = 2 * num_boundary_types + 2 * num_waste_load_types
         end if

         if (lumptr) then
            ntrans = 2
         else

            !             internal transport from every dump area possible plus the other term
            ntrans = 2 * ndmpar_out + 2
         end if

         noout = 0
         do isys = 1, num_substances_total + nosum
            if (isys > num_substances_total) then
               if (tfacto(isys - num_substances_total) > 0.0001) then
                  includ = .true.
               else
                  includ = .false.
                  imassa(isys) = -1
               end if
            else
               includ = .true.
            end if
            if (includ) then
               imassa(isys) = noout + 1
               noout = noout + 1
               iemiss(isys) = noout + 1
               noout = noout + nemiss
               itrans(isys) = noout + 1
               noout = noout + ntrans
               iprocs(isys) = noout + 1
               if (lumppr) then
                  nprocs(isys) = 1
               else
                  ! find sum stochi coefficients for sum parameters
                  if (isys > num_substances_total) then
                     isum = isys - num_substances_total
                     do iflux = 1, noflux
                        stochl(isum, iflux) = 0.0
                        do isys2 = 1, num_substances_total
                           stochl(isum, iflux) = &
                              stochl(isum, iflux) &
                              + stochi(isys2, iflux) &
                              * sfacto(isum, isys2)
                        end do
                     end do
                  end if

                  ! Make sure that irrelevant fluxes are not included
                  nprocs(isys) = 0
                  do iflux = 1, noflux
                     if (isys <= num_substances_total) then
                        st = stochi(isys, iflux)
                     else
                        st = stochl(isys - num_substances_total, iflux)
                     end if
                     if (abs(st) > 1.e-20) then
                        nprocs(isys) = nprocs(isys) + 1
                        fl2bal(isys, nprocs(isys)) = iflux
                     end if
                  end do
               end if
               noout = noout + nprocs(isys)
            end if
         end do

         ! Dimension additional arrays
         if (allocated(balans)) then
            deallocate (balans, baltot, ouname)
         end if

         allocate (balans(noout, ndmpar_out + 1), &
                   baltot(noout, ndmpar_out + 1), &
                   ouname(noout), &
                   stat=ierr)
         if (ierr > 0) goto 9000

         ! Set balance term names
         do isys = 1, num_substances_total + nosum
            if (imassa(isys) > 0) then
               c20 = synamp(isys)
               ouname(imassa(isys)) = c20(1:6)//'_Storage'
               if (lumpem) then
                  ouname(iemiss(isys)) = c20(1:6)//'_All Bo+Lo_In'
                  ouname(iemiss(isys) + 1) = c20(1:6)//'_All Bo+Lo_Out'
               else
                  itel2 = iemiss(isys) - 1
                  do ifrac = 1, num_boundary_types
                     itel2 = itel2 + 1
                     ouname(itel2) = &
                        c20(1:6)//'_'//bndtyp(ifrac) (1:9)//'_In'
                     itel2 = itel2 + 1
                     ouname(itel2) = &
                        c20(1:6)//'_'//bndtyp(ifrac) (1:9)//'_Out'
                  end do
                  do ifrac = 1, num_waste_load_types
                     itel2 = itel2 + 1
                     ouname(itel2) = &
                        c20(1:6)//'_'//wsttyp(ifrac) (1:9)//'_In'
                     itel2 = itel2 + 1
                     ouname(itel2) = &
                        c20(1:6)//'_'//wsttyp(ifrac) (1:9)//'_Out'
                  end do
               end if
               if (lumptr) then
                  ouname(itrans(isys)) = c20(1:6)//'_Transport In'
                  ouname(itrans(isys) + 1) = c20(1:6)//'_Transport Out'
               else
                  itel2 = itrans(isys) - 1
                  itel2 = itel2 + 1
                  ouname(itel2) = c20(1:6)//'_'//'Other    '//'_In'
                  itel2 = itel2 + 1
                  ouname(itel2) = c20(1:6)//'_'//'Other    '//'_Out'
                  do idump = 1, ndmpar
                     if (dmpbal(idump) == 1) then
                        itel2 = itel2 + 1
                        ouname(itel2) = c20(1:6)//'_'//danam(idump) (1:9)//'_In'
                        ITEL2 = ITEL2 + 1
                        OUNAME(ITEL2) = C20(1:6)//'_'//DANAM(IDUMP) (1:9)//'_Out'
                     end if
                  end do
               end if
               if (LUMPPR) then
                  OUNAME(IPROCS(ISYS)) = C20(1:6)//'_Processes'
               else
                  do ITEL = 1, NPROCS(ISYS)
                     IFLUX = FL2BAL(ISYS, ITEL)
                     OUNAME(IPROCS(ISYS) + ITEL - 1) = &
                        C20(1:6)//'_'//FLXNAM(IFLUX) (1:13)
                  end do
               end if
            end if
         end do

         idump_out = 0
         do IDUMP = 1, NDMPAR
            if (dmpbal(idump) == 1) then
               idump_out = idump_out + 1
               DANAMP(IDUMP_out) = DANAM(IDUMP)
               JDUMP(IDUMP_out) = IDUMP_OUT
            end if
         end do
         DANAMP(NDMPAR_OUT + 1) = 'Sum_of_balance_areas'
         JDUMP(NDMPAR_OUT + 1) = NDMPAR_OUT + 1

         if (.not. SUPPFT) &
            call open_waq_files(LUNOUT, LCHOUT, 21, 1, IDUM)

         !         Zero output matrices
         do IOUT = 1, NOOUT
            do IDUMP = 1, NDMPAR_OUT + 1
               BALTOT(IOUT, IDUMP) = 0.0
               BALANS(IOUT, IDUMP) = 0.0
            end do
         end do
      else
         IFIRST = .false.
      end if

      !**** END OF **** INITIALIZATION *************************************

      !     Loop over dump areas

      ITEL1 = NDMPAR
      IP1 = NDMPAR + NTDMPQ
      ITEL2 = NDMPAR + NTDMPQ + NDMPAR
      IDUMP_OUT = 0
      do IDUMP = 1, NDMPAR

         !         ONLY second and following calls !!!!!!
         if (.not. IFIRST) then

            if (DMPBAL(IDUMP) == 1) then
               IDUMP_OUT = IDUMP_OUT + 1

               !            Mass / accumulation term, previous mass already here
               !            Subtract current mass
               call UPDBAL(IDUMP_OUT, num_substances_total, IMASSA, IMASSA, 0, &
                           BALANS, NOSUM, ASMASS(1, IDUMP, 1), &
                           -1.0, 1, SFACTO, NOOUT, num_substances_total)

               !            Process INFLOW/OUTFLOW terms, both boundaries and internal
               !            (the IPOINT array is used to distinguish the two)

               !            Loop over exchanges relevant for current MON AREA
               NQC = IPDMP(IDUMP)
               do IQC = 1, NQC
                  ITEL1 = ITEL1 + 1
                  IQ = IPDMP(ITEL1)

                  !                Is it a boundary? Which type/fraction?
                  IPOIN = abs(IQ)
                  IVAN = IPOINT(1, IPOIN)
                  INAAR = IPOINT(2, IPOIN)
                  if (IVAN < 0 .or. &
                      INAAR < 0) then
                     !                    BOUNDARY!!!
                     BOUNDA = .true.
                     if (IVAN < 0) then
                        !                        -I TO +J BOUNDARY!!!
                        IBOUN = -IVAN
                     else
                        !                        +I TO -J BOUNDARY!!!
                        IBOUN = -INAAR
                     end if
                     IFRAC = INBTYP(IBOUN)
                  else
                     !                    INTERNAL
                     BOUNDA = .false.
                     IFRAC = 1
                     if (.not. LUMPTR) then
                        if (IQ < 0) then
                           if (IVAN > 0) IFRAC = SEGDMP(IVAN) + 1
                        else
                           if (INAAR > 0) IFRAC = SEGDMP(INAAR) + 1
                        end if
                     end if
                  end if

                  !                Find fluxes
                  if (IQ > 0) then
                     IPQ = IQDMP(IQ)
                     do ISYS = 1, num_substances_transported
                        FLTRAN(1, ISYS) = DMPQ(ISYS, IPQ, 2)
                        FLTRAN(2, ISYS) = -DMPQ(ISYS, IPQ, 1)
                     end do
                  else
                     IPQ = IQDMP(-IQ)
                     do ISYS = 1, num_substances_transported
                        FLTRAN(1, ISYS) = DMPQ(ISYS, IPQ, 1)
                        FLTRAN(2, ISYS) = -DMPQ(ISYS, IPQ, 2)
                     end do
                  end if

                  !                Update balances
                  if (BOUNDA) then
                     if (LUMPEM) then
                        call UPDBAL(IDUMP_OUT, num_substances_transported, IMASSA, IEMISS, &
                                    0, BALANS, NOSUM, FLTRAN, &
                                    1.0, 2, SFACTO, NOOUT, &
                                    num_substances_total)
                     else
                        call UPDBAL(IDUMP_OUT, num_substances_transported, IMASSA, IEMISS, &
                                    (IFRAC - 1) * 2, BALANS, NOSUM, FLTRAN, &
                                    1.0, 2, SFACTO, NOOUT, &
                                    num_substances_total)
                     end if
                  else
                     call UPDBAL(IDUMP_OUT, num_substances_transported, IMASSA, ITRANS, &
                                 (IFRAC - 1) * 2, BALANS, NOSUM, FLTRAN, &
                                 1.0, 2, SFACTO, NOOUT, &
                                 num_substances_total)
                  end if
               end do

               !            Loads

               if (LUMPEM) then
                  call UPDBAL(IDUMP_OUT, num_substances_total, IMASSA, IEMISS, 0, &
                              BALANS, NOSUM, ASMASS(1, IDUMP, 3), 1.0, 1, &
                              SFACTO, NOOUT, num_substances_total)
                  call UPDBAL(IDUMP_OUT, num_substances_total, IMASSA, IEMISS, 1, &
                              BALANS, NOSUM, ASMASS(1, IDUMP, 4), -1.0, 1, &
                              SFACTO, NOOUT, num_substances_total)
               else
                  do IW = 1, num_waste_loads
                     if (IWDMP(IW, IDUMP_OUT)) then
                        IFRAC = INWTYP(IW)
                        IBAL_OFF = (num_boundary_types + IFRAC - 1) * 2
                        call UPDBAL(IDUMP_OUT, num_substances_total, IMASSA, IEMISS, IBAL_OFF, &
                                    BALANS, NOSUM, WSTDMP(1, IW, 1), 1.0, 1, &
                                    SFACTO, NOOUT, num_substances_total)
                        IBAL_OFF = (num_boundary_types + IFRAC - 1) * 2 + 1
                        call UPDBAL(IDUMP_OUT, num_substances_total, IMASSA, IEMISS, IBAL_OFF, &
                                    BALANS, NOSUM, WSTDMP(1, IW, 2), -1.0, 1, &
                                    SFACTO, NOOUT, num_substances_total)
                     end if
                  end do
               end if

               !            Process sources and sinks

               if (LUMPPR) then
                  !                Copy term from ASMASS array
                  call UPDBAL(IDUMP_OUT, num_substances_total, IMASSA, IPROCS, 0, &
                              BALANS, NOSUM, ASMASS(1, IDUMP, 2), &
                              1.0, 1, SFACTO, NOOUT, num_substances_total)
               else

                  !                Loop over substances, including sum parameters
                  do ISYS = 1, num_substances_total + NOSUM
                     if (IMASSA(ISYS) > 0) then
                        !                        Substance (sum parameter) is active
                        !                        Loop over relevant processes
                        do ITEL = 1, NPROCS(ISYS)
                           !                            Pointers to balance and fluxes array
                           IOUT = IPROCS(ISYS) + ITEL - 1
                           IFLUX = FL2BAL(ISYS, ITEL)
                           !                            Find stoichiometry constant
                           if (ISYS <= num_substances_total) then
                              ST = STOCHI(ISYS, IFLUX)
                           else
                              ST = STOCHL(ISYS - num_substances_total, IFLUX)
                           end if
                           !                            Update balance
                           BALANS(IOUT, IDUMP_OUT) = FLXINT(IFLUX, IDUMP) * ST
                        end do
                     end if
                  end do
               end if

            else

               ! dump area excluded from mass balance

               NQC = IPDMP(IDUMP)
               ITEL1 = ITEL1 + NQC

            end if

            !         End of actions only if we are not at the first level!
         end if
      end do

      !     Fill balance matrix for sum of areas
      !     zero accumulation term of sum segment first
      do ISYS = 1, num_substances_total + NOSUM
         IOUT = IMASSA(ISYS)
         if (IOUT > 0) &
            BALANS(IOUT, NDMPAR_OUT + 1) = 0.0
      end do
      do IDUMP_OUT = 1, NDMPAR_OUT
         do IOUT = 1, NOOUT
            BALANS(IOUT, NDMPAR_OUT + 1) = BALANS(IOUT, NDMPAR_OUT + 1) &
                                           + BALANS(IOUT, IDUMP_OUT)
         end do
      end do

      !     Update integrated balance matrix FOR ALL TERMS EXCEPT ACCUMULATION
      do IDUMP_OUT = 1, NDMPAR_OUT + 1
         do ISYS = 1, num_substances_total + NOSUM
            if (IMASSA(ISYS) > 0) then
               ITEL1 = IMASSA(ISYS) + 1
               ITEL2 = IPROCS(ISYS) + NPROCS(ISYS) - 1
               do IOUT = ITEL1, ITEL2
                  BALTOT(IOUT, IDUMP_OUT) = BALTOT(IOUT, IDUMP_OUT) + &
                                            BALANS(IOUT, IDUMP_OUT)
               end do
            end if
         end do
      end do

      !     Optionally scale the balance per volume or area

      if (B_AREA) then

         allocate (DMP_SURF(NDMPAR), STAT=IERR)
         if (IERR > 0) goto 9000
         call sum_sub_areas_surfaces(num_cells, NDMPAR, IPDMP(NDMPAR + NTDMPQ + 1), ISEGCOL, SURF, DMP_SURF)
         IDUMP_OUT = 0
         do IDUMP = 1, NDMPAR
            if (DMPBAL(IDUMP) == 1) then
               IDUMP_OUT = IDUMP_OUT + 1
               do IOUT = 1, NOOUT
                  if (DMP_SURF(IDUMP) > 1.0e-20) then
                     BALANS(IOUT, IDUMP_OUT) = BALANS(IOUT, IDUMP_OUT) / DMP_SURF(IDUMP)
                  else
                     BALANS(IOUT, IDUMP_OUT) = -999.0
                  end if
               end do
               TOT_SURF = TOT_SURF + DMP_SURF(IDUMP)
            end if
         end do
         do IOUT = 1, NOOUT
            if (TOT_SURF > 1.0e-20) then
               BALANS(IOUT, NDMPAR_OUT + 1) = BALANS(IOUT, NDMPAR_OUT + 1) / TOT_SURF
            else
               BALANS(IOUT, NDMPAR_OUT + 1) = -999.0
            end if
         end do
         deallocate (DMP_SURF)

      elseif (B_VOLU) then

         allocate (DMP_VOLU(NDMPAR), STAT=IERR)
         if (IERR > 0) goto 9000
         call sum_sub_areas_values(NDMPAR, IPDMP(NDMPAR + NTDMPQ + 1), VOLUME, DMP_VOLU)
         IDUMP_OUT = 0
         do IDUMP = 1, NDMPAR
            if (DMPBAL(IDUMP) == 1) then
               IDUMP_OUT = IDUMP_OUT + 1
               do IOUT = 1, NOOUT
                  BALANS(IOUT, IDUMP_OUT) = BALANS(IOUT, IDUMP_OUT) / DMP_VOLU(IDUMP)
               end do
               TOT_VOLU = TOT_VOLU + DMP_VOLU(IDUMP)
            end if
         end do
         do IOUT = 1, NOOUT
            BALANS(IOUT, NDMPAR_OUT + 1) = BALANS(IOUT, NDMPAR_OUT + 1) / TOT_VOLU
         end do
         deallocate (DMP_VOLU)

      end if

      !     Write time dependent output
      if (IFIRST) then
         IINIT = 1
      else
         IINIT = 0
      end if
      if (.not. SUPPFT) then
         if (ONLYSM) then
            call write_binary_history_output(LUNOUT, CDUM, ITIME, MONAME, 1, &
                                             JDUMP(NDMPAR_OUT + 1), DANAMP(NDMPAR_OUT + 1), &
                                             NOOUT, OUNAME, BALANS, &
                                             0, CDUM, RDUM, IINIT)
         else
            call write_binary_history_output(LUNOUT, CDUM, ITIME, MONAME, NDMPAR_OUT + 1, &
                                             JDUMP, DANAMP, NOOUT, OUNAME, BALANS, &
                                             0, CDUM, RDUM, IINIT)
         end if
      end if

      !     Zero output matrix
      do IOUT = 1, NOOUT
         do IDUMP = 1, NDMPAR_OUT + 1
            BALANS(IOUT, IDUMP) = 0.0
         end do
      end do

      !     Store current mass in Mass term as a starting point for next step
      IDUMP_OUT = 0
      do IDUMP = 1, NDMPAR
         if (DMPBAL(IDUMP) == 1) then
            IDUMP_OUT = IDUMP_OUT + 1
            call UPDBAL(IDUMP_OUT, num_substances_total, IMASSA, IMASSA, 0, &
                        BALANS, NOSUM, ASMASS(1, IDUMP, 1), &
                        1.0, 1, SFACTO, NOOUT, num_substances_total)
            !           Sum segment (OBSOLETE??)
            do ISYS = 1, num_substances_total + NOSUM
               IOUT = IMASSA(ISYS)
               if (IOUT > 0) then
                  BALANS(IOUT, NDMPAR_OUT + 1) = BALANS(IOUT, NDMPAR_OUT + 1) &
                                                 + BALANS(IOUT, IDUMP_OUT)
               end if
            end do
         end if
      end do

      !     Update integrated balance matrix
      !     Mass/accumulation term only FIRST time
      if (IFIRST) then
         do ISYS = 1, num_substances_total + NOSUM
            IOUT = IMASSA(ISYS)
            if (IOUT > 0) then
               do IDUMP = 1, NDMPAR_OUT + 1
                  BALTOT(IOUT, IDUMP) = BALANS(IOUT, IDUMP)
               end do
            end if
         end do
      end if

      !     This is an incorrect statement in case ITIME never reaches
      !     one of the two time levels
      if (ITIME >= ITSTOP - IMSTEP + 1 .or. ITIME >= IMSTOP) then

         IBSTRT = max(ITSTRT, IMSTRT)
         IBSTOP = min(ITIME, IMSTOP)

         if (.not. SUPPFT) close (LUNOUT)
         do ISYS = 1, num_substances_total + NOSUM
            IOUT = IMASSA(ISYS)
            if (IOUT > 0) then
               do IDUMP = 1, NDMPAR_OUT + 1
                  BALTOT(IOUT, IDUMP) = BALTOT(IOUT, IDUMP) &
                                        - BALANS(IOUT, IDUMP)
               end do
            end if
         end do

         FILNAM = LCHOUT
         INDX = index(FILNAM, '-bal.his')
         if (INDX > 0) then
            FILNAM(INDX:) = '-bal.prn'
         else
            FILNAM = 'sobwqbal.prn'
         end if
         open (NEWUNIT=IOBALI, FILE=FILNAM)

         !         In mass
         call OUTBAI(IOBALI, MONAME, IBSTRT, IBSTOP, NOOUT, &
                     num_substances_total, NDMPAR_OUT + 1, DANAMP, OUNAME, SYNAMP, &
                     IMASSA, IEMISS, NEMISS, ITRANS, NTRANS, &
                     IPROCS, NPROCS, BALTOT, ONLYSM, NOSUM, &
                     SFACTO, 0, 1)

         !         In mass/m2
         allocate (DMP_SURF(NDMPAR), STAT=IERR)
         if (IERR > 0) goto 9000
         call sum_sub_areas_surfaces(num_cells, NDMPAR, IPDMP(NDMPAR + NTDMPQ + 1), ISEGCOL, SURF, DMP_SURF)
         IDUMP_OUT = 0
         do IDUMP = 1, NDMPAR
            if (DMPBAL(IDUMP) == 1) then
               IDUMP_OUT = IDUMP_OUT + 1
               do IOUT = 1, NOOUT
                  if (DMP_SURF(IDUMP) > 1.0e-20) then
                     BALTOT(IOUT, IDUMP_OUT) = BALTOT(IOUT, IDUMP_OUT) / DMP_SURF(IDUMP)
                  else
                     BALTOT(IOUT, IDUMP_OUT) = -999.0
                  end if
               end do
               TOT_SURF = TOT_SURF + DMP_SURF(IDUMP)
            end if
         end do
         do IOUT = 1, NOOUT
            if (TOT_SURF > 1.0e-20) then
               BALTOT(IOUT, NDMPAR_OUT + 1) = BALTOT(IOUT, NDMPAR_OUT + 1) / TOT_SURF
            else
               BALTOT(IOUT, NDMPAR_OUT + 1) = -999.0
            end if
         end do
         call OUTBAI(IOBALI, MONAME, IBSTRT, IBSTOP, NOOUT, &
                     num_substances_total, NDMPAR_OUT + 1, DANAMP, OUNAME, SYNAMP, &
                     IMASSA, IEMISS, NEMISS, ITRANS, NTRANS, &
                     IPROCS, NPROCS, BALTOT, ONLYSM, NOSUM, &
                     SFACTO, 1, 0)

         !         In mass/m3
         allocate (DMP_VOLU(NDMPAR), STAT=IERR)
         if (IERR > 0) goto 9000
         call sum_sub_areas_values(NDMPAR, IPDMP(NDMPAR + NTDMPQ + 1), VOLUME, DMP_VOLU)
         IDUMP_OUT = 0
         do IDUMP = 1, NDMPAR
            if (DMPBAL(IDUMP) == 1) then
               IDUMP_OUT = IDUMP_OUT + 1
               do IOUT = 1, NOOUT
                  BALTOT(IOUT, IDUMP_OUT) = BALTOT(IOUT, IDUMP_OUT) * DMP_SURF(IDUMP) / DMP_VOLU(IDUMP)
               end do
               TOT_VOLU = TOT_VOLU + DMP_VOLU(IDUMP)
            end if
         end do
         do IOUT = 1, NOOUT
            BALTOT(IOUT, NDMPAR_OUT + 1) = BALTOT(IOUT, NDMPAR_OUT + 1) * TOT_SURF / TOT_VOLU
         end do
         call OUTBAI(IOBALI, MONAME, IBSTRT, IBSTOP, NOOUT, &
                     num_substances_total, NDMPAR_OUT + 1, DANAMP, OUNAME, SYNAMP, &
                     IMASSA, IEMISS, NEMISS, ITRANS, NTRANS, &
                     IPROCS, NPROCS, BALTOT, ONLYSM, NOSUM, &
                     SFACTO, 2, 0)
         deallocate (DMP_VOLU)
         deallocate (DMP_SURF)

         close (IOBALI)
      end if

      INIOUT = 0

      if (timon) call timstop(ithandl)
      return
9000  write (lunrep, *) 'Error allocating memory'
      write (*, *) 'Error allocating memory'
      call stop_with_error()
   end subroutine write_balance_text_output

   subroutine outbai(iobali, moname, ibstrt, ibstop, noout, &
                     num_substances_total, ndmpar, danamp, ouname, syname, &
                     imassa, iemiss, nemiss, itrans, ntrans, &
                     iprocs, nprocs, baltot, onlysm, nosum, &
                     sfacto, iunit, init)
      use timers

      integer(kind=int_wp) :: IOBALI, IBSTRT, IBSTOP, NOOUT, num_substances_total, NDMPAR, &
                              IMASSA(*), IEMISS(*), NEMISS, ITRANS(*), NTRANS, &
                              IPROCS(*), NPROCS(*), NOSUM, IUNIT, INIT
      character(len=40) MONAME(4)
      character(len=20) DANAMP(NDMPAR), OUNAME(*), SYNAME(*)
      real(kind=real_wp) :: BALTOT(NOOUT, NDMPAR), SFACTO(NOSUM, *)
      logical ONLYSM

      real(kind=real_wp) :: value, VALUE1, VALUE2, SUMPOS, SUMNEG
      integer(kind=int_wp) :: IDUMP, ISYS, ITEL, I, ITEL2, ISUM
      integer(kind=int_wp) :: ithandl = 0
      if (timon) call timstrt("outbai", ithandl)

      if (INIT == 1) then
         !         Write header
         write (IOBALI, 1000) MONAME(1), MONAME(2), MONAME(3), MONAME(4)

         !         Write timers
         write (IOBALI, 1010) real(IBSTRT) / 86400., real(IBSTOP) / 86400.

         !         Write sum parameters
         do ISUM = 1, NOSUM
            if (IMASSA(num_substances_total + ISUM) > 0) then
               write (IOBALI, 1020) SYNAME(num_substances_total + ISUM)
               do ISYS = 1, num_substances_total
                  if (SFACTO(ISUM, ISYS) >= 0.0001) then
                     write (IOBALI, 1030) SYNAME(ISYS), SFACTO(ISUM, ISYS)
                  end if
               end do
            end if
         end do
      end if

      !     Write the unit of the balance
      if (IUNIT == 0) then
         write (IOBALI, 1040)
      elseif (IUNIT == 1) then
         write (IOBALI, 1050)
      elseif (IUNIT == 2) then
         write (IOBALI, 1060)
      end if

      !     The balance per area
      do IDUMP = 1, NDMPAR
         if (.not. ONLYSM .or. IDUMP == NDMPAR) then
            write (IOBALI, 1100) DANAMP(IDUMP)
            do ISYS = 1, num_substances_total + NOSUM
               ITEL = IMASSA(ISYS)
               if (ITEL > 0) then
                  write (IOBALI, 1110) SYNAME(ISYS)
                  SUMPOS = 0.0
                  SUMNEG = 0.0

                  !                     Mass term
                  value = BALTOT(ITEL, IDUMP)
                  if (value > 0.0) then
                     VALUE1 = value
                     VALUE2 = 0.0
                  else
                     VALUE1 = 0.0
                     VALUE2 = value
                  end if
                  write (IOBALI, 1120) SYNAME(ISYS) (1:6), &
                     VALUE1, VALUE2
                  SUMPOS = SUMPOS + VALUE1
                  SUMNEG = SUMNEG + VALUE2

                  !                     Inputs from boundaries
                  do I = 1, NEMISS / 2
                     ITEL2 = (I - 1) * 2 + IEMISS(ISYS)
                     VALUE1 = BALTOT(ITEL2, IDUMP)
                     VALUE2 = BALTOT(ITEL2 + 1, IDUMP)
                     write (IOBALI, 1130) OUNAME(ITEL2) (1:16), &
                        VALUE1, VALUE2
                     SUMPOS = SUMPOS + VALUE1
                     SUMNEG = SUMNEG + VALUE2
                  end do

                  !                     Internal transport
                  if (NTRANS == 2) then
                     VALUE1 = BALTOT(ITRANS(ISYS), IDUMP)
                     VALUE2 = BALTOT(ITRANS(ISYS) + 1, IDUMP)
                     write (IOBALI, 1140) SYNAME(ISYS) (1:6), &
                        VALUE1, VALUE2
                     SUMPOS = SUMPOS + VALUE1
                     SUMNEG = SUMNEG + VALUE2
                  else
                     do I = 1, NTRANS / 2
                        ITEL2 = (I - 1) * 2 + ITRANS(ISYS)
                        VALUE1 = BALTOT(ITEL2, IDUMP)
                        VALUE2 = BALTOT(ITEL2 + 1, IDUMP)
                        write (IOBALI, 1130) OUNAME(ITEL2) (1:16), &
                           VALUE1, VALUE2
                        SUMPOS = SUMPOS + VALUE1
                        SUMNEG = SUMNEG + VALUE2
                     end do
                  end if

                  !                     Processes
                  do I = 1, NPROCS(ISYS)
                     ITEL2 = IPROCS(ISYS) - 1 + I
                     value = BALTOT(ITEL2, IDUMP)
                     if (value >= 0.0) then
                        VALUE1 = value
                        VALUE2 = 0.0
                     else
                        VALUE1 = 0.0
                        VALUE2 = value
                     end if
                     write (IOBALI, 1150) OUNAME(ITEL2), &
                        VALUE1, VALUE2
                     SUMPOS = SUMPOS + VALUE1
                     SUMNEG = SUMNEG + VALUE2
                  end do
                  write (IOBALI, 1160) SUMPOS, SUMNEG
               end if
            end do
         end if
      end do

1000  format('Mass balances output file'// &
             a40 / a40 / a40// &
             'All terms in basic mass units,' &
             'for Processes Library always (g)'// &
             'Simulation starts: ', a40)
1010  format('Mass balances output period:'/ &
             'start: ', f9.3, ' days'/ &
             'stop : ', f9.3, ' days')
1020  format(/'Balance for sum parameter ', a, ' consists of:'/ &
              'substance           scale factor')
1030  format(A20, F10.4)
1040  format(// &
              'MASS BALANCE PER DUMPAREA'/ &
              'All terms in basic mass units,' &
              'for Processes Library always (g)')
1050  format(// &
              'MASS BALANCE PER SURFACE'/ &
              'All terms in basic mass units/m2,' &
              'for Processes Library always (g)')
1060  format(// &
              'MASS BALANCE PER VOLUME'/ &
              'All terms in basic mass units/m3,' &
              'for Processes Library always (g)')
1100  format(// &
              '============================================================'/ &
              'Mass balances for ', a/ &
              '============================================================')
1110  format(/'Substance ', a20, ' Sources/Inflows Sinks/Outflows'/ &
              '------------------------------------------------------------')
1120  format(a6, '_Storage ', 15x, 2e15.5)
1130  format(a16, 14x, 2e15.5)
1140  format(a6, '_Internal transport', 5x, 2e15.5)
1150  format(a20, 10x, 2e15.5)
1160  format('SUM OF ALL TERMS              ', 2e15.5)

      if (timon) call timstop(ithandl)
      return
   end

   subroutine comsum(nosum, tfacto, num_substances_total, syname, sfacto, num_constants, coname, cons)

      use m_logger_helper, only: stop_with_error, get_log_unit_number
      use timers
      use bloom_data_mass_balance

      implicit none

      integer(kind=int_wp) :: nosum, num_substances_total, num_constants
      character(len=20) syname(num_substances_total), coname(num_constants)
      real(kind=real_wp) :: tfacto(nosum), sfacto(nosum, num_substances_total), cons(num_constants)

      !      INCLUDE 'cblbal.inc'

      integer(kind=int_wp) :: isum, isys, icons, ires, nres1, nres2, ityp, lunrep
      real(kind=real_wp) :: factor
      parameter(nres1=23, nres2=2)
      character(len=20) resna1(nres1), resna2(nres2)
      character(len=10) ratna2(2, nres2)
      real(kind=real_wp) :: facres(2, nres1), ratdef(2, nres2)

      data resna1/'DetP                ', &
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
      data facres/0.0, 1.0, &
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
      data resna2/'Diat                ', &
         'Green               '/
      data ratna2/'NCRatDiat ', 'PCRatDiat ', &
         'NCRatGreen', 'PCRatGreen'/
      data ratdef/0.16, 0.02, &
         0.16, 0.02/
      integer(kind=int_wp) :: ithandl = 0
      if (timon) call timstrt("consum", ithandl)

      !     Compose sum parameters
      !     local functionality nosum = 2, check!!!!!!!!!!!!!

      if (nosum /= 2) then
         call get_log_unit_number(lunrep)
         write (lunrep, *) 'BUG IN COMSUM!'
         write (*, *) 'BUG IN COMSUM!'
         call stop_with_error()
      end if

      !     Initialise substance shares in sum parameters as well as totals
      !     (totals are used to find out if sum parameter is active)

      do isum = 1, nosum
         tfacto(isum) = 0.0
         do isys = 1, num_substances_total
            sfacto(isum, isys) = 0.0
         end do
      end do

      do isys = 1, num_substances_total

         !         Reserved substance names, FIXED scale factor
         ires = index_in_array(syname(isys), resna1)
         if (ires > 0) then
            do isum = 1, nosum
               tfacto(isum) = tfacto(isum) + facres(isum, ires)
               sfacto(isum, isys) = facres(isum, ires)
            end do
         end if

         !         Reserved substance names, scale factors from CONS with default
         ires = index_in_array(syname(isys), resna2)
         if (ires > 0) then
            do isum = 1, nosum
               icons = index_in_array(ratna2(isum, ires), coname)
               if (icons > 0) then
                  factor = cons(icons)
               else
                  factor = ratdef(isum, ires)
               end if
               tfacto(isum) = tfacto(isum) + factor
               sfacto(isum, isys) = factor
            end do
         end if
      end do

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
         end do
      end if

      if (timon) call timstop(ithandl)
      return
   end

   subroutine updbal(IDUMP, num_substances_total, IMASSA, ITERMS, IOFFSE, &
                     BALANS, NOSUM, DMASSA, FACTOR, NTEL, &
                     SFACTO, NOOUT, NOLAST)

      !     IDUMP               index of current monitoring area
      !     num_substances_total               nr of substances to be processed
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
      integer(kind=int_wp) :: IDUMP, num_substances_total, NOSUM, NOOUT, IOFFSE, NTEL, &
                              NOLAST
      integer(kind=int_wp) :: IMASSA(*), ITERMS(*)
      real(kind=real_wp) :: BALANS(NOOUT, *), DMASSA(NTEL, *), &
                            FACTOR, SFACTO(NOSUM, *)

      integer(kind=int_wp) :: ISYS, IOUT, ISYSS, IOUT2, ISUM, &
                              ITEST, ITEL, ITEL2
      integer(kind=int_wp) :: ithandl = 0
      if (timon) call timstrt("updbal", ithandl)

      do ISYS = 1, num_substances_total
         IOUT = ITERMS(ISYS) + IOFFSE
         do ITEL = 1, NTEL
            ITEL2 = IOUT + (ITEL - 1)
            BALANS(ITEL2, IDUMP) = &
               BALANS(ITEL2, IDUMP) + DMASSA(ITEL, ISYS) * FACTOR
         end do
         do ISUM = 1, NOSUM
            !              Bug fix, 5-1-2002
            !              ISYSS = num_substances_total+ISUM
            ISYSS = NOLAST + ISUM
            ITEST = IMASSA(ISYSS)
            if (ITEST > 0) then
               !                 Sum parameter is active
               if (SFACTO(ISUM, ISYS) >= 0.0001) then
                  !                     Current substance contributes
                  IOUT2 = ITERMS(ISYSS) + IOFFSE
                  do ITEL = 1, NTEL
                     ITEL2 = IOUT2 + (ITEL - 1)
                     BALANS(ITEL2, IDUMP) = BALANS(ITEL2, IDUMP) &
                                            + DMASSA(ITEL, ISYS) * SFACTO(ISUM, ISYS) &
                                            * FACTOR
                  end do
               end if
            end if
         end do
      end do

      if (timon) call timstop(ithandl)

   end subroutine UPDBAL

   subroutine sum_sub_areas_surfaces(nosss, ndmpar, ipdmp, isegcol, surf, dmp_surf)

        !! sums surf for sub-area's no double counting over the layers

      use timers

      integer(kind=int_wp), intent(in) :: nosss ! total number of segments
      integer(kind=int_wp), intent(in) :: ndmpar ! Number of dump areas
      integer(kind=int_wp), intent(in) :: ipdmp(*) ! pointer structure dump area's
      integer(kind=int_wp), intent(in) :: isegcol(*) ! pointer from segment to top of column
      real(kind=real_wp), intent(in) :: surf(*) ! horizontal surface per segment
      real(kind=real_wp), intent(out) :: dmp_surf(*) ! horizontal surface per dump area

      ! local declarations

      integer(kind=int_wp) :: itel ! index counter
      integer(kind=int_wp) :: idump ! dump area number
      integer(kind=int_wp) :: nsc ! number of segment contributions
      integer(kind=int_wp) :: isc ! index of segment contributions
      integer(kind=int_wp) :: iseg ! segment number
      integer(kind=int_wp) :: icol ! segment number top of column
      integer(kind=int_wp), allocatable :: i_surf(:) ! indication if column is already in area
      integer(kind=int_wp) :: ithandl = 0
      if (timon) call timstrt("sum_sub_areas_surfaces", ithandl)

      ! loop over the dump area's, sum value

      allocate (i_surf(nosss))
      dmp_surf(1:ndmpar) = 0.0
      itel = 0
      do idump = 1, ndmpar
         i_surf = 0
         nsc = ipdmp(idump)
         do isc = 1, nsc
            itel = itel + 1
            iseg = ipdmp(ndmpar + itel)
            if (iseg > 0) then
               icol = isegcol(iseg)
               if (i_surf(icol) == 0) then
                  dmp_surf(idump) = dmp_surf(idump) + surf(iseg)
                  i_surf(icol) = 1
               end if
            end if
         end do
      end do
      deallocate (i_surf)

      if (timon) call timstop(ithandl)
   end subroutine sum_sub_areas_surfaces

   subroutine sum_sub_areas_values(NDMPAR, IPDMP, VALSEG, VALDMP)
      !  sums values for sub-area's

      !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     NDMPAR  INTEGER       1     INPUT   Number of dump areas
      !     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
      !     VALSEG  REAL          *     INPUT   values on segment grid
      !     VALDMP  REAL          *     INPUT   values on dump grid

      use timers

      integer(kind=int_wp) :: NDMPAR
      integer(kind=int_wp) :: IPDMP(*)
      real(kind=real_wp) :: VALSEG(*)
      real(kind=real_wp) :: VALDMP(*)

      integer(kind=int_wp) :: ITEL, IDUMP, NSC, ISC, ISEG
      integer(kind=int_wp) :: ithandl = 0
      if (timon) call timstrt("sum_sub_areas_values", ithandl)

      ! Loop over the dump area's, sum value

      VALDMP(1:NDMPAR) = 0.0
      ITEL = 0
      do IDUMP = 1, NDMPAR
         NSC = IPDMP(IDUMP)
         do ISC = 1, NSC
            ITEL = ITEL + 1
            ISEG = IPDMP(NDMPAR + ITEL)
            if (ISEG > 0) then
               VALDMP(IDUMP) = VALDMP(IDUMP) + VALSEG(ISEG)
            end if
         end do
      end do

      if (timon) call timstop(ithandl)

   end subroutine sum_sub_areas_values

end module m_write_balance_output
