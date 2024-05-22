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
module m_write_output
    use m_waq_precision
    use m_values
    use m_sobbal
    use m_raatra
    use m_write_monitoring_output
    use m_write_nefis_output
    use m_write_map_output
    use m_write_nefis_history_ouput
    use m_write_netcdf_output
    use m_write_history_output
    use m_outdmp
    use m_fioutv
    use m_fiosub


    implicit none

    private
    public :: write_output

contains


    subroutine write_output(notot, noseg, nopa, nosfun, itime, &
            moname, syname, duname, idump, nodump, &
            conc, cons, param, func, segfun, &
            volume, nocons, nofun, idt, noutp, &
            file_name_list, file_unit_list, ioutps, iopoin, riobuf, &
            ousnm, ouuni, oudsc, sysnm, syuni, sydsc, &
            ounam, nx, ny, lgrid, cgrid, &
            nosys, bound, ip, amass, amass2, &
            asmass, noflux, flxint, isflag, iaflag, &
            ibflag, imstrt, imstop, imstep, idstrt, &
            idstop, idstep, ihstrt, ihstop, ihstep, &
            imflag, idflag, ihflag, noloc, proloc, &
            nodef, defaul, itstrt, itstop, ndmpar, &
            danam, ndmpq, ndmps, iqdmp, isdmp, &
            ipdmp, dmpq, dmps, flxdmp, ntdmpq, &
            nambuf, noraai, ntraaq, ioraai, nqraai, &
            iqraai, trraai, ranam, stochi, nogrid, &
            novar, vararr, varidx, vartda, vardag, &
            arrknd, arrpoi, arrdm1, arrdm2, vgrset, &
            grdnos, grdseg, a, nobnd, nobtyp, &
            bndtyp, inwtyp, coname, noq, ipoint, &
            intopt, paname, funame, sfname, dmpbal, &
            nowst, nowtyp, wsttyp, iwaste, inxtyp, &
            wstdmp, iknmrk, isegcol)
        ! Driver output system
        !
        !     Parameters
        !
        !     Name    Kind     Length     Funct.  Description
        !     ----    -----    ------     ------- -----------
        !     notot   integer       1     input   Total number of substances
        !     noseg   integer       1     input   Nr. of computational elements
        !     nopa    integer       1     input   Number of parameters
        !     nosfun  integer       1     input   Number of segment functions
        !     itime   integer       1     input   Time in system clock units
        !     moname  char*40       4     input   Model and run names
        !     syname  char*20    notot    input   names of substances
        !     duname  char*20    nodump   input   names of dump locations
        !     idump   integer    nodump   input   dump segment numbers
        !     nodump  integer       1     input   number of dump locations
        !     conc    real   notot,noseg  input   Model concentrations
        !     cons    real          *     in/out  Model constants
        !     param   real    nopa,noseg  in/out  Model parameters
        !     func    real          *     in/out  Model functions at ITIME
        !     segfun  real   noseg,nosfun in/out  Segment functions at ITIME
        !     volume  real      noseg     input   Segment volumes
        !     nocons  integer       1     input   Number of constants used
        !     nofun   integer       1     input   Number of functions ( user )
        !     idt     integer       1     input   Simulation timestep
        !     noutp   integer       1     input   Number of output files
        !     file_name_list   char*(*)      *     input   File names
        !     file_unit_list     integer       *     input   Uint numbers
        !     ioutps  integer 7*noutp    in/out   Output structure
        !                                            index 1 = start time
        !                                            index 2 = stop time
        !                                            index 3 = time step
        !                                            index 4 = number of vars
        !                                            index 5 = kind of output
        !                                            index 6 = grid of output
        !                                            index 7 = initialize flag
        !     iopoin  integer       *     input   Pointer to DELWAQ array's
        !     riobuf  real          *     local   Output buffer
        !     ounam   char*20       *     input   name of output variable
        !     nx      integer       1     input   Width of output grid
        !     ny      integer       1     input   Depth of output grid
        !     lgrid   integer     nx*ny   input   grid-layout
        !     cgrid   char*20       *     local   Char buffer for dmp output
        !     nosys   integer       1     input   Number of active substances
        !     bound   real          *     input   Bounary conditions
        !     ip      integer       *     in/out  Paging structure
        !     amass   real       notot,*  input   Mass array
        !     amass2  real       notot,*  in/out  Cummulative balance on whole
        !     asmass  real       notot,*  in/out  Cummulative balance per segment
        !     noflux  integer       1     input   Number of fluxes
        !     flxint  real  noflux*ndmpar in/out  Integrated fluxes at dump segments
        !     isflag  integer       1     input   if 1 then dd-hh:mm'ss"
        !     iaflag  integer       1     output  if 1 then accumulate mass bal
        !     ibflag  integer       1     input   Flag = 1 then balances
        !     imstrt  integer       1     input   Monitoring start time ( scu )
        !     imstop  integer       1     input   Monitoring stop time ( scu )
        !     imstep  integer       1     input   Monitoring time step ( scu )
        !     idstrt  integer       1     input   Dump start time ( scu )
        !     idstop  integer       1     input   Dump stop time ( scu )
        !     idstep  integer       1     input   Dump time step ( scu )
        !     ihstrt  integer       1     input   History start time ( scu )
        !     ihstop  integer       1     input   History stop time ( scu )
        !     ihstep  integer       1     input   History time step ( scu )
        !     imflag  logical       1     output  If .T. then monitor step
        !     idflag  logical       1     output  If .T. then dump step
        !     ihflag  logical       1     output  If .T. then history step
        !     noloc   integer       1     input   Number of variables in PROLOC
        !     param   real   noloc,noseg  input   Parameters local in PROCES system
        !     nodef   integer       1     input   Number of used defaults
        !     defaul  real          *     input   Default proces parameters
        !     itstrt  integer     1       input   start time
        !     itstop  integer     1       input   stop time
        !     ndmpar  integer     1       input   Number of dump areas
        !     danam   char*20  ndmpar     input   Dump area names
        !     ndmpq   integer     1       input   Number of dumped exchanges
        !     ndmps   integer     1       input   Number of dumped segments
        !     iqdmp   integer       *     input   Exchange to dumped exchange pointer
        !     isdmp   integer       *     input   Segment to dumped segment pointer
        !     ipdmp   integer       *     input   pointer structure dump area's
        !     dmpq    real  notot*ndmps*? input   mass balance dumped segments
        !     dmps    real  nosys*ndmpq*? input   mass balance dumped exchange
        !     flxdmp  real  noflux*ndmps  input   Integrated fluxes
        !     nambuf  char*20       *     input   Buffer for names
        !     noraai  integer       1     input   Number of raaien
        !     ntraaq  integer       1     input   Total number of exch. in raaien
        !     ioraai  integer       *     input   Output option for raai
        !     nqraai  integer       *     input   Number of exchanges in raai
        !     iqraai  integer       *     input   Exchanges in raai
        !     trraai  real notot*ndmpar*6 in/out  Cummulative transport over raai
        !     ranam   char*20       *     input   Raaien names
        !     stochi  real   notot*noflux input   Proces stochiometry
        !     intopt  integer     1       input   Integration and balance suboptions
        !     ==================================================================
        !
        use m_write_restart_map_file
        use m_array_manipulation, only : initialize_real_array
        use m_logger_helper, only : stop_with_error
        use m_cli_utils, only : is_command_arg_specified
        use timers
        use results
        use nan_check_module

        integer(kind = int_wp) :: notot, noseg, nopa, nosfun, itime, &
                nodump, nocons, nofun, idt, noutp, &
                nx, ny, nosys, noflux, isflag, &
                iaflag, ibflag, imstrt, imstop, imstep, &
                idstrt, idstop, idstep, ihstrt, ihstop, &
                ihstep, noloc, nodef, itstrt, itstop, &
                ndmpar, ndmpq, ndmps, ntdmpq, noraai, &
                ntraaq, nogrid, novar, nobnd, nobtyp, &
                noq
        integer(kind = int_wp) :: idump(*), file_unit_list(*), &
                ioutps(7, *), iopoin(*), &
                lgrid(*), ip(*), &
                iqdmp(*), isdmp(*), &
                ipdmp(*), ioraai(*), &
                nqraai(*), iqraai(*), &
                vararr(novar), varidx(novar), &
                vartda(novar), vardag(novar), &
                arrknd(*), arrpoi(*), &
                arrdm1(*), arrdm2(*), &
                vgrset(novar, *), grdnos(nogrid), &
                grdseg(noseg, nogrid), &
                inwtyp(*), ipoint(4, noq)
        integer(kind = int_wp), intent(in) :: iknmrk(noseg)      ! Feature array. Bit zero set means active.
        real(kind = real_wp) :: conc (notot, noseg), &
                cons(*), &
                param(nopa, noseg), &
                func(*), &
                segfun(noseg, nosfun), &
                volume(*), &
                riobuf(*), bound(*), &
                amass(notot, noseg), &
                amass2(notot, 5), &
                asmass(*), flxint(*), &
                proloc(*), defaul(*), &
                dmpq(*), dmps(*), &
                flxdmp(*), trraai(nosys, *), &
                stochi(notot, noflux), a(*)
        character(len = 20)  syname(*), duname(*), &
                ounam(*), cgrid(*), &
                danam(*), nambuf(*), &
                ranam(*), bndtyp(*), &
                coname(*), paname(*), &
                funame(*), sfname(*)
        character(len = 100) ousnm(*), sysnm(*)
        character(len = 40)  ouuni(*), syuni(*)
        character(len = 60)  oudsc(*), sydsc(*)

        character(len = 100), allocatable :: hnc_standard(:)
        character(len = 40), allocatable :: hnc_unit(:)
        character(len = 60), allocatable :: hnc_description(:)

        character(len = 40) :: moname(4)
        character(len = *) :: file_name_list(*)
        logical       imflag, idflag, ihflag
        integer(kind = int_wp) :: dmpbal(ndmpar)        ! indicates if dump area is included in the balance
        integer(kind = int_wp) :: nowst                 ! number of wasteloads
        integer(kind = int_wp) :: nowtyp                ! number of wasteload types
        character(len = 20) :: wsttyp(nowtyp)        ! wasteload types names
        integer(kind = int_wp) :: iwaste(nowst)         ! segment numbers of the wasteloads
        integer(kind = int_wp) :: inxtyp(nowst)         ! wasteload type number (index in wsttyp)
        real(kind = real_wp) :: wstdmp(notot, nowst, 2) ! accumulated wasteloads 1/2 in and out
        integer(kind = int_wp), intent(in) :: isegcol(*)            ! pointer from segment to top of column
        !
        !     Local declarations
        !
        integer(kind = int_wp), parameter :: igseg = 1
        integer(kind = int_wp), parameter :: igmon = 2
        integer(kind = int_wp), parameter :: iggrd = 3
        integer(kind = int_wp), parameter :: igsub = 4
        integer(kind = int_wp), parameter :: luoff = 18
        integer(kind = int_wp), parameter :: luoff2 = 36
        integer(kind = int_wp) :: k1, iostrt, iostop, iostep, nrvar, &
                isrtou, igrdou, iniout, lunout, iout, &
                ierr, ierr2, i, i1, i2, &
                ifi, ncout, nrvar2, nrvar3, ip1, &
                iof, nsegou, intopt

        character(len = 255) lchout
        character(len = 20)  name
        logical       loflag, lmfirs, ldfirs, lhfirs, ldummy, lnonans
        logical       lget, lread
        real(kind = real_wp), allocatable :: surf(:)
        integer(kind = int_wp) :: idummy       ! dummy not used
        real(kind = real_wp) :: rdummy       ! dummy not used
        character(len = 256) :: adummy       ! dummy not used
        logical :: lfound       ! Keyword found (or not)
        logical, save :: lnancheck    ! Do check on NAN in conc array

        integer(kind = int_wp), save :: mncrec = 0                            ! netCDF map
        integer(kind = int_wp), save :: hncrec = 0                            ! netCDF history
        integer(kind = int_wp), save :: timeid, bndtimeid                     ! netCDF map
        integer(kind = int_wp), save :: timeidh, bndtimeidh                   ! netCDF history
        integer(kind = int_wp), allocatable, save :: mncwqid1(:, :), mncwqid2(:, :)  ! netCDF map
        integer(kind = int_wp), allocatable, save :: hncwqid1(:, :), hncwqid2(:, :)  ! netCDF history

        logical, save :: first = .true.

        real(kind = dp) :: damass2(notot, 5)

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("write_output", ithandl)

        if (first) then
            allocate(mncwqid1(notot, 3), mncwqid2(novar, 3))
            allocate(hncwqid1(notot, 2), hncwqid2(novar, 2))
            ! allow switching of NAN concentrations check
            lnancheck = .not. is_command_arg_specified('-nonancheck')
            first = .false.
        endif

        if (lnancheck) then
            ! Check for NANs in the concentration array
            lunout = file_unit_list(19)
            lnonans = nan_check(conc, 'conc(notot, noseg)', lunout, 1, 1)
            if (.not. lnonans) then
                write(lunout, '(/A)')  '  ERROR : NAN found the concentration array, ending calculation.'
                write(*, '(/A)')  '  ERROR : NAN found the concentration array, ending calculation. See location in mon-file.'
                write(lunout, '(A)')   '          Current concentration fields written to _res.map.'
                write(*, '(A)')   '          Current concentration fields written to _res.map.'
                write(lunout, '(/A/)') '  INFO  : If you don''t want NAN checks, use -nonancheck at command line.'
                write(*, '(/A/)') '  INFO  : If you don''t want NAN checks, use -nonancheck at command line.'
                call write_restart_map_file(file_unit_list, file_name_list, conc, itime, moname, syname, notot, noseg)
                call stop_with_error()
            endif
        endif
        !
        !     Evaluate standard DELWAQ output timers
        !
        call evaluate_timers (itime, idt, imstrt, imstop, imstep, imflag, lmfirs)
        call evaluate_timers (itime, idt, idstrt, idstop, idstep, idflag, ldfirs)
        call evaluate_timers (itime, idt, ihstrt, ihstop, ihstep, ihflag, lhfirs)
        !
        !     Fill mass in AMASS2 array by summing AMASS over all segments
        !
        if (imflag) then
            damass2 = amass2
            iaflag = 1
            do i2 = 1, notot
                amass2(i2, 1) = 0.0
                do i1 = 1, noseg
                    damass2(i2, 1) = damass2(i2, 1) + amass(i2, i1)
                enddo
            enddo
            amass2 = damass2
        endif
        !
        !     Fill mass in ASMASS array using DMPQ and DMPS
        !
        if (imflag .or. (ihflag .and. noraai > 0)) then
            if (ibflag == 1) then
                call fill_dump_areas_balances(notot, nosys, noflux, ndmpar, ndmpq, &
                        ndmps, ntdmpq, iqdmp, isdmp, ipdmp, &
                        dmpq, amass, dmps, flxdmp, asmass, &
                        flxint)
            endif

            if (noraai > 0) then
                if (lhfirs) then
                    call initialize_real_array   (trraai, noraai * nosys)
                else
                    call raatra (nosys, ndmpq, noraai, ntraaq, ioraai, &
                            nqraai, iqraai, iqdmp, dmpq, trraai)
                endif
            endif
            !
        endif
        !
        !     Initialize K1, pointer in IOPOIN and OUNAM
        !
        lread = .true.
        k1 = 1
        !
        !     Loop over the output files
        !
        do iout = 1, noutp
            !
            !        Map output structure to single variables part 1
            !
            iostrt = ioutps(1, iout)
            iostop = ioutps(2, iout)
            iostep = ioutps(3, iout)
            nrvar = ioutps(4, iout)
            !
            !        Output required ?
            !
            call evaluate_timers (itime, idt, iostrt, iostop, iostep, &
                    loflag, ldummy)
            !
            if (.not. loflag) goto 100
            !
            !        Map output structure to single variables part 2
            !
            isrtou = ioutps(5, iout)
            igrdou = ioutps(6, iout)
            iniout = ioutps(7, iout)
            if (iout <= 4) then
                ifi = iout + luoff
            elseif (iout <= 7) then
                ifi = iout + luoff2 - 4
            else
                ifi = iout + luoff2 - 2
            endif
            lunout = file_unit_list(ifi)
            lchout = file_name_list(ifi)

            ! No balance output if they are not active
            if ((isrtou == ibal .or. isrtou == iba2 .or. &
                    isrtou == iba2) .and. ibflag /= 1) goto 100

            ! Set all local variables used active on base grid
            call actloc (iopoin, nrvar, nocons, nopa, nofun, &
                    nosfun, notot, noseg, noloc, nogrid, &
                    novar, vararr, varidx, vartda, vardag, &
                    arrknd, arrpoi, arrdm1, arrdm2, vgrset, &
                    grdnos, grdseg, a)

            ! Fill output buffer
            if (isrtou == iba2) then

                call flxbal (notot, noflux, ndmpar, nrvar, stochi, flxint, asmass, riobuf)

            elseif (isrtou == iba3) then
                !     jos doet het zelf
            elseif (igrdou == igsub) then
                if (isrtou == imo3 .or. &
                        isrtou == ihi3 .or. &
                        isrtou == ihnc3 .or. &
                        isrtou == ihn3) then
                    ncout = notot
                else
                    ncout = 0
                endif
                nrvar2 = nrvar / 2
                ! For the dump area's
                call fiosub (riobuf, iopoin(k1), nrvar2, nocons, nopa, &
                        nofun, nosfun, notot, conc, segfun, &
                        func, param, cons, idt, itime, &
                        volume, noseg, nosys, ndmpar, ipdmp, &
                        bound, noloc, proloc, nodef, defaul, &
                        ncout, ntdmpq, paname, sfname, funame, &
                        danam)

                ! For the raaien
                if ((isrtou == ihi3 .or. &
                        isrtou == ihnc3 .or. &
                        isrtou == ihn3) .and. &
                        noraai > 0) then
                    nrvar3 = notot + nrvar2
                    ip1 = (ncout + nrvar2) * ndmpar + 1
                    call fioraa (riobuf(ip1), nrvar3, trraai, noraai, nosys)
                endif
            else
                nrvar2 = nrvar
                call fioutv (riobuf, iopoin(k1), nrvar, nocons, nopa, &
                        nofun, nosfun, notot, conc, segfun, &
                        func, param, cons, idt, itime, &
                        volume, noseg, nosys, nodump, idump, &
                        nx, ny, lgrid, igrdou, bound, &
                        noloc, proloc, nodef, defaul)
            endif

            ! Fill character buffer with substance names and output names
            if (isrtou == imnf .or. &
                    isrtou == ihnf .or. &
                    isrtou == ihnf .or. &
                    isrtou == ihnc3 .or. &
                    isrtou == imo3 .or. &
                    isrtou == ihi3 .or. &
                    isrtou == ihn3) then

                if (allocated(hnc_standard)) then
                    deallocate(hnc_standard)
                    deallocate(hnc_unit)
                    deallocate(hnc_description)
                endif

                allocate(hnc_standard(notot + nrvar2))
                allocate(hnc_unit(notot + nrvar2))
                allocate(hnc_description(notot + nrvar2))

                do i = 1, notot
                    nambuf(i) = syname(i)
                    hnc_standard(i) = sysnm(i)
                    hnc_unit(i) = syuni(i)
                    hnc_description(i) = sydsc(i)
                end do
                do i = 1, nrvar2
                    nambuf(notot + i) = ounam(k1 + i - 1)
                    hnc_standard(notot + i) = ousnm(k1 + i - 1)
                    hnc_unit(notot + i) = ouuni(k1 + i - 1)
                    hnc_description(notot + i) = oudsc(k1 + i - 1)
                end do
            endif

            ! Perform output
            if (isrtou == imon) then

                call write_monitoring_output (lunout, idump, conc, amass2, itime, &
                        duname, syname, moname, nodump, notot, &
                        ip, isflag, asmass, ibflag, nrvar, &
                        ounam(k1), riobuf, itstrt, itstop, ndmpar, &
                        danam)

            elseif (isrtou == imo2) then

                call write_monitoring_output (lunout, idump, conc, amass2, itime, &
                        duname, syname, moname, nodump, 0, &
                        ip, isflag, asmass, ibflag, nrvar, &
                        ounam(k1), riobuf, itstrt, itstop, ndmpar, &
                        danam)
                !
            elseif (isrtou == imo3) then
                !
                call outmo3 (lunout, amass2, itime, syname, moname, &
                        notot, ip, isflag, asmass, ibflag, &
                        nrvar2, ounam(k1), riobuf, itstrt, itstop, &
                        ndmpar, danam)
                !
            elseif (isrtou == imo4) then
                !
                call outmo3 (lunout, amass2, itime, syname, moname, &
                        0, ip, isflag, asmass, ibflag, &
                        nrvar2, ounam(k1), riobuf, itstrt, itstop, &
                        ndmpar, danam)
                !
            elseif (isrtou == idmp) then
                !
                call outdmp (lunout, lchout, itime, moname, nx, &
                        ny, lgrid, cgrid, notot, nosys, &
                        syname, conc, bound, nrvar, ounam(k1), &
                        riobuf, ip(5), isflag, iniout)
                !
            elseif (isrtou == idm2) then
                !
                call outdmp (lunout, lchout, itime, moname, nx, &
                        ny, lgrid, cgrid, 0, 0, &
                        syname, conc, bound, nrvar, ounam(k1), &
                        riobuf, ip(5), isflag, iniout)
                !
            elseif (isrtou == ihis) then
                !
                call write_binary_history_output(lunout, lchout, itime, moname, nodump, &
                        idump, duname, notot, syname, conc, &
                        nrvar, ounam(k1), riobuf, iniout)

            elseif (isrtou == ihnf) then

                iof = nrvar * nodump + 1
                call write_nefis_history_output(lunout, lchout, itime, moname, noseg, &
                        notot, conc, nambuf, nrvar, riobuf, &
                        iostrt, iostop, iostep, nodump, idump, &
                        duname, riobuf(iof), iniout)

            elseif (isrtou == ihnc) then
                hncrec = hncrec + 1
                iof = nrvar * nodump + 1
                call write_netcdf_history_output(file_unit_list(47), file_name_list(47), file_name_list(46), timeidh, &
                        bndtimeidh, hncrec, itime, moname, &
                        idump, duname, nodump, notot, &
                        conc, syname, sysnm, syuni, &
                        sydsc, hncwqid1, nrvar, riobuf, &
                        ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), &
                        hncwqid2, file_unit_list(19))

            elseif (isrtou == ihi2) then

                call write_binary_history_output(lunout, lchout, itime, moname, nodump, &
                        idump, duname, 0, syname, conc, &
                        nrvar, ounam(k1), riobuf, iniout)

            elseif (isrtou == ihn2) then

                iof = nrvar * nodump + 1
                call write_nefis_history_output(lunout, lchout, itime, moname, noseg, &
                        0, conc, ounam(k1), nrvar, riobuf, &
                        iostrt, iostop, iostep, nodump, idump, &
                        duname, riobuf(iof), iniout)

            elseif (isrtou == ihnc2) then

                hncrec = hncrec + 1
                iof = nrvar * nodump + 1
                call write_netcdf_history_output(file_unit_list(47), file_name_list(47), file_name_list(46), timeidh, &
                        bndtimeidh, hncrec, itime, moname, &
                        idump, duname, nodump, 0, &
                        conc, syname, sysnm, syuni, &
                        sydsc, hncwqid1, nrvar, riobuf, &
                        ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), &
                        hncwqid2, file_unit_list(19))
                !
            elseif (isrtou == ihi3) then
                !
                !           Let op RANAM achter DANAM
                !
                nrvar3 = notot + nrvar2
                nsegou = ndmpar + noraai
                call write_binary_history_output(lunout, lchout, itime, moname, nsegou, &
                        idump, danam, 0, syname, conc, &
                        nrvar3, nambuf, riobuf, iniout)

            elseif (isrtou == ihn3) then

                ! Let op RANAM achter DANAM
                nrvar3 = notot + nrvar2
                nsegou = ndmpar + noraai
                iof = nrvar3 * nsegou + 1
                call write_nefis_history_output(lunout, lchout, itime, moname, noseg, &
                        0, conc, nambuf, nrvar3, riobuf, &
                        iostrt, iostop, iostep, nsegou, idump, &
                        danam, riobuf(iof), iniout)

            elseif (isrtou == ihnc3) then

                ! Let op RANAM achter DANAM
                hncrec = hncrec + 1
                nrvar3 = notot + nrvar2
                nsegou = ndmpar + noraai
                iof = nrvar3 * nsegou + 1
                call write_netcdf_history_output(file_unit_list(47), file_name_list(47), file_name_list(46), timeidh, &
                        bndtimeidh, hncrec, itime, moname, &
                        idump, danam, nsegou, 0, &
                        conc, nambuf, sysnm, syuni, &
                        sydsc, hncwqid1, nrvar3, riobuf, &
                        nambuf, hnc_standard, hnc_unit, &
                        hnc_description, hncwqid2, file_unit_list(19))

            elseif (isrtou == ihi4) then

                call write_binary_history_output(lunout, lchout, itime, moname, ndmpar, &
                        idump, danam, 0, syname, conc, &
                        nrvar2, ounam(k1), riobuf, iniout)

            elseif (isrtou == ihn4) then
                iof = nrvar2 * ndmpar + 1
                call write_nefis_history_output(lunout, lchout, itime, moname, noseg, &
                        0, conc, ounam(k1), nrvar2, riobuf, &
                        iostrt, iostop, iostep, ndmpar, idump, &
                        danam, riobuf(iof), iniout)

            elseif (isrtou == ihnc4) then

                hncrec = hncrec + 1
                iof = nrvar2 * ndmpar + 1
                call write_netcdf_history_output(file_unit_list(47), file_name_list(47), file_name_list(46), timeidh, &
                        bndtimeidh, hncrec, itime, moname, &
                        idump, danam, nsegou, 0, &
                        conc, syname, sysnm, syuni, &
                        sydsc, hncwqid1, nrvar2, riobuf, &
                        ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), &
                        hncwqid2, file_unit_list(19))

            elseif (isrtou == imap) then

                call write_map_output (lunout, lchout, itime, moname, noseg, &
                        notot, conc, syname, nrvar, riobuf, &
                        ounam(k1), iknmrk, iniout)

            elseif (isrtou == imnf) then

                iof = nrvar * noseg + 1
                call write_nefis_map_output(lunout, lchout, itime, moname, noseg, notot, conc, syname, nrvar, riobuf, &
                        ounam(k1), iostrt, iostop, iostep, riobuf(iof), iniout)

            elseif (isrtou == imnc) then

                mncrec = mncrec + 1
                call write_map_output_to_netcdf(file_unit_list(49), file_name_list(49), file_name_list(46), timeid, bndtimeid, mncrec, &
                        itime, moname, noseg, notot, conc, syname, sysnm, syuni, sydsc, mncwqid1, nrvar, &
                        riobuf, ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), mncwqid2, volume, iknmrk, file_unit_list(19))

            elseif (isrtou == ima2) then

                call write_map_output (lunout, lchout, itime, moname, noseg, 0, conc, syname, nrvar, riobuf, &
                        ounam(k1), iknmrk, iniout)

            elseif (isrtou == imn2) then

                iof = nrvar * noseg + 1
                call write_nefis_map_output(lunout, lchout, itime, moname, noseg, 0, conc, syname, nrvar, riobuf, &
                        ounam(k1), iostrt, iostop, iostep, riobuf(iof), iniout)

            elseif (isrtou == imnc2) then

                mncrec = mncrec + 1
                call write_map_output_to_netcdf (file_unit_list(49), file_name_list(49), file_name_list(46), timeid, bndtimeid, mncrec, &
                        itime, moname, noseg, 0, &
                        conc, syname, sysnm, syuni, sydsc, mncwqid1, nrvar, &
                        riobuf, ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), mncwqid2, &
                        volume, iknmrk, file_unit_list(19))

            elseif (isrtou == ibal) then

                call write_balance_output(lunout, itime, moname, notot, noflux, syname, ndmpar, danam, asmass, &
                        flxint, nrvar2, riobuf, iniout)

            elseif (isrtou == iba2) then

                call write_binary_history_output(lunout, lchout, itime, moname, ndmpar, idump, danam, 0, syname, conc, &
                        nrvar, ounam(k1), riobuf, iniout)
            elseif (isrtou == iba3) then
                allocate(surf(noseg))
                name = 'SURF'
                lget = .true.
                call values (name, noseg, surf, nocons, nopa, &
                        nofun, nosfun, cons, coname, param, &
                        paname, func, funame, segfun, sfname, &
                        lget, ierr)

                call sobbal (notot, itime, nosys, noflux, ndmpar, &
                        ndmpq, ntdmpq, itstop, imstrt, imstop, &
                        iqdmp, ipdmp, asmass, flxint, stochi, &
                        syname, danam, moname, dmpq, nobnd, &
                        nobtyp, bndtyp, inwtyp, nocons, coname, &
                        cons, noq, ipoint, ounam(k1), intopt, &
                        volume, surf, noseg, lunout, lchout, &
                        iniout, dmpbal, nowst, nowtyp, wsttyp, &
                        iwaste, inxtyp, wstdmp, isegcol, imstep)

                file_unit_list(ifi) = lunout ! Ad hoc: routine open_waq_files sets the LU-number via newunit
                deallocate (surf)

            endif

            ioutps(7, iout) = iniout

            100    continue

            ! Update K1, pointer in IOPOIN and OUNAM
            k1 = k1 + nrvar

        end do

        if (timon) call timstop (ithandl)
    end subroutine write_output

    subroutine fill_dump_areas_balances(NOTOT, NOSYS, NOFLUX, NDMPAR, NDMPQ, &
            NDMPS, NTDMPQ, IQDMP, ISDMP, IPDMP, &
            DMPQ, MASS, DMPS, FLXDMP, ASMASS, &
            FLXINT)

        ! Fills balances for sub-area's


        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     NOSYS   INTEGER       1     INPUT   Total number of active substances
        !     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
        !     NDMPAR  INTEGER       1     INPUT   Number of dump areas
        !     NDMPQ   INTEGER       1     INPUT   Number of dump exchanges
        !     NDMPS   INTEGER       1     INPUT   Number of dump segments
        !     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
        !     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
        !     ISDMP   INTEGER       *     INPUT   Segment to dumped segment pointer
        !     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
        !     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
        !     DMPS    REAL  NOTOT*NDMPS*? INPUT   mass balance dumped segments
        !     FLXDMP  REAL  NOFLUX*NDMPS  INPUT   Integrated fluxes
        !     ASMASS  REAL NOTOT*NDMPAR*6 OUTPUT  Mass balance terms
        !     FLXINT  REAL  NOFLUX*NDMPAR OUTPUT  Integrated fluxes

        use timers

        INTEGER(kind = int_wp) :: NOTOT, NOSYS, NOFLUX, NDMPAR, NDMPQ, &
                NDMPS, NTDMPQ
        INTEGER(kind = int_wp) :: IQDMP(*), ISDMP(*), &
                IPDMP(*)
        REAL(kind = real_wp) :: DMPQ(NOSYS, NDMPQ, *), MASS(NOTOT, *), &
                DMPS(NOTOT, NDMPS, *), FLXDMP(NOFLUX, *), &
                ASMASS(NOTOT, NDMPAR, *), FLXINT(NOFLUX, *)

        INTEGER(kind = int_wp) :: ITEL1, ITEL2, IP1, IDUMP, NQC, &
                IQC, IQ, IPQ, ISYS, NSC, &
                ISC, ISEG, IPS
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("fill_dump_areas_balances", ithandl)

        !
        !     Loop over the dump area's
        !
        ITEL1 = NDMPAR
        IP1 = NDMPAR + NTDMPQ
        ITEL2 = NDMPAR + NTDMPQ + NDMPAR
        DO IDUMP = 1, NDMPAR
            !
            !        the exchange contributes
            !
            NQC = IPDMP(IDUMP)
            DO IQC = 1, NQC
                ITEL1 = ITEL1 + 1
                IQ = IPDMP(ITEL1)
                IF (IQ > 0) THEN
                    IPQ = IQDMP(IQ)
                    DO ISYS = 1, NOSYS
                        ASMASS(ISYS, IDUMP, 5) = ASMASS(ISYS, IDUMP, 5) + &
                                DMPQ(ISYS, IPQ, 2)
                        ASMASS(ISYS, IDUMP, 6) = ASMASS(ISYS, IDUMP, 6) + &
                                DMPQ(ISYS, IPQ, 1)
                    end do
                ELSE
                    IPQ = IQDMP(-IQ)
                    DO ISYS = 1, NOSYS
                        ASMASS(ISYS, IDUMP, 5) = ASMASS(ISYS, IDUMP, 5) + &
                                DMPQ(ISYS, IPQ, 1)
                        ASMASS(ISYS, IDUMP, 6) = ASMASS(ISYS, IDUMP, 6) + &
                                DMPQ(ISYS, IPQ, 2)
                    end do
                ENDIF
            end do
            !
            !        the segment contributes
            !
            DO ISYS = 1, NOTOT
                ASMASS(ISYS, IDUMP, 1) = 0.0
            ENDDO
            NSC = IPDMP(IP1 + IDUMP)
            DO ISC = 1, NSC
                ITEL2 = ITEL2 + 1
                ISEG = IPDMP(ITEL2)
                IF (ISEG > 0) THEN
                    IPS = ISDMP(ISEG)
                    DO ISYS = 1, NOTOT
                        ASMASS(ISYS, IDUMP, 1) = ASMASS(ISYS, IDUMP, 1) + &
                                MASS(ISYS, ISEG)
                        ASMASS(ISYS, IDUMP, 2) = ASMASS(ISYS, IDUMP, 2) + &
                                DMPS(ISYS, IPS, 1)
                        ASMASS(ISYS, IDUMP, 3) = ASMASS(ISYS, IDUMP, 3) + &
                                DMPS(ISYS, IPS, 2)
                        ASMASS(ISYS, IDUMP, 4) = ASMASS(ISYS, IDUMP, 4) + &
                                DMPS(ISYS, IPS, 3)
                    end do
                ENDIF

            end do

        end do

        if (timon) call timstop (ithandl)

    end subroutine fill_dump_areas_balances

    SUBROUTINE FIORAA (OUTVAL, NRVAR, TRRAAI, NORAAI, NOSYS)

        !  Fills output buffer OUTVAL for raaien

        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     OUTVAL  REAL    NRVAR,*     OUTPUT  Values for vars on output grid
        !     NRVAR   INTEGER       1     INPUT   Number of output vars
        !     TRRAAI  REAL    NOSYS,*     INPUT   Tranport over raai for active substanc
        !     NORAAI  INTEGER       1     INPUT   Number of raaien
        !     NOSYS   INTEGER       1     INPUT   Number of parameters in TRRAAI

        use timers

        INTEGER(kind = int_wp) :: NRVAR, NORAAI, NOSYS
        REAL(kind = real_wp) :: OUTVAL(NRVAR, *), TRRAAI(NOSYS, *)

        integer(kind = int_wp) :: iraai, isys
        real(kind = real_wp), PARAMETER :: RMISS = -999.
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("fioraa", ithandl)

        ! Copy values into output buffer
        DO IRAAI = 1, NORAAI
            DO ISYS = 1, NOSYS
                OUTVAL(ISYS, IRAAI) = TRRAAI(ISYS, IRAAI)
            end do
            DO ISYS = NOSYS + 1, NRVAR
                OUTVAL(ISYS, IRAAI) = RMISS
            end do
        end do

        if (timon) call timstop (ithandl)

    END SUBROUTINE FIORAA

    SUBROUTINE ACTLOC (IOPOIN, NRVAR, NOCONS, NOPA, NOFUN, &
            NOSFUN, NOTOT, NOSEG, NOLOC, NOGRID, &
            NOVAR, VARARR, VARIDX, VARTDA, VARDAG, &
            ARRKND, ARRPOI, ARRDM1, ARRDM2, VGRSET, &
            GRDNOS, GRDSEG, A)
        ! Sets all variable from the LOCAL array used for output actual for the base grid.
        ! (ouput always uses the value from base grid)


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

        use m_dhgvar
        use m_array_manipulation, only: set_array_parameters
        use timers
        use aggregation, only: resample_v2

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

                ENDIF

            ENDIF

        ENDDO

        if (timon) call timstop (ithandl)

    END SUBROUTINE ACTLOC

    SUBROUTINE evaluate_timers (ITIME, IDT, ISTRT, ISTOP, ISTEP, LFLAG, LFIRST)
        !  Evaluates if action is necessary according to timers

        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     ITIME   INTEGER       1     INPUT   Time in system clock units
        !     IDT     INTEGER       1     INPUT   Simulation timestep
        !     IMSTRT  INTEGER       1     INPUT   start time of timer
        !     IMSTOP  INTEGER       1     INPUT   stop time of timer
        !     IMSTEP  INTEGER       1     INPUT   time step of timer
        !     LFLAG   LOGICAL       1     OUTPUT  If .T. then action else not
        !     LFIRST  LOGICAL       1     OUTPUT  If .T. then first step

        use timers

        INTEGER(kind = int_wp) :: ITIME, IDT, ISTRT, ISTOP, ISTEP
        LOGICAL :: LFLAG, LFIRST
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("evaluate_timers", ithandl)

        ! Evaluate timer
        LFLAG = .TRUE.
        LFIRST = .FALSE.
        IF (ISTEP <= 0 .AND. ISTRT /= ISTOP) THEN
            LFLAG = .FALSE.
            GOTO 100
        ENDIF
        IF (ISTRT > ITIME) THEN
            LFLAG = .FALSE.
            GOTO 100
        ENDIF
        IF (ISTOP <= ITIME - IDT) THEN
            LFLAG = .FALSE.
            GOTO 100
        ENDIF
        IF (MOD(ITIME - ISTRT, ISTEP) >= IDT) LFLAG = .FALSE.
        IF (LFLAG) THEN
            IF (ITIME - ISTRT < ISTEP) LFIRST = .TRUE.
        ENDIF

        100 CONTINUE

        if (timon) call timstop (ithandl)

    END SUBROUTINE evaluate_timers

    SUBROUTINE FLXBAL (NOTOT, NOFLUX, NDMPAR, NOBALT, STOCHI, &
            FLXINT, ASMASS, BALINT)

        ! Makes BALINT from FLXINT and STOCHI

        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
        !     NDMPAR  INTEGER       1     INPUT   Nr. of dump areas
        !     NOBALT  INTEGER       1     INPUT   Nr. of balance terms total
        !     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
        !     FLXINT  REAL  NOFLUX*NDMPAR INPUT   Accumulated fluxes
        !     ASMASS  REAL NOTOT*NDMPAR*6 INPUT   Mass balance terms
        !     BALINT  REAL  NOBALT*NDMPAR OUTPUT  Balance terms

        use m_logger_helper, only : stop_with_error, get_log_unit_number
        use timers

        INTEGER(kind = int_wp) :: NOTOT, NOFLUX, NDMPAR, NOBALT
        REAL(kind = real_wp) :: STOCHI(NOTOT, NOFLUX), FLXINT(NOFLUX, NDMPAR), &
                ASMASS(NOTOT, NDMPAR, 6), BALINT(NOBALT, NDMPAR)

        ! local
        integer(kind = int_wp) :: ibalt, isys, i, idmp, iflx, lurep
        real(kind = real_wp) :: st
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("flxbal", ithandl)
        !
        !     We construeren nu de BALINT's
        !
        IBALT = 0
        DO ISYS = 1, NOTOT
            DO I = 1, 4
                IBALT = IBALT + 1
                IF (I == 1 .OR. I == 3) THEN
                    DO IDMP = 1, NDMPAR
                        BALINT(IBALT, IDMP) = ASMASS(ISYS, IDMP, I + 2)
                    ENDDO
                ELSE
                    DO IDMP = 1, NDMPAR
                        BALINT(IBALT, IDMP) = -ASMASS(ISYS, IDMP, I + 2)
                    ENDDO
                ENDIF
            ENDDO
            DO IFLX = 1, NOFLUX
                ST = STOCHI(ISYS, IFLX)
                IF (ABS(ST) > 1.E-20) THEN
                    IBALT = IBALT + 1
                    IF (IBALT > NOBALT) THEN
                        CALL get_log_unit_number(LUREP)
                        WRITE(LUREP, *) 'ERROR, INTERNAL FLXBAL'
                        WRITE(*, *)     'ERROR, INTERNAL FLXBAL'
                        CALL stop_with_error()
                    ENDIF
                    DO IDMP = 1, NDMPAR
                        BALINT(IBALT, IDMP) = FLXINT(IFLX, IDMP) * ST
                    ENDDO
                ENDIF
            ENDDO
        ENDDO

        if (timon) call timstop (ithandl)

    END SUBROUTINE FLXBAL

    subroutine write_balance_output(balance_file_unit, simulation_time, model_name, num_substances, &
            num_fluxes, substances_names, num_dump_segments, monitoring_station_names, mass_balance_terms, &
            integrated_fluxes, num_extra_variables, extra_variables, initialize_file)
        ! Writes balance output

        use timers

        integer(kind = int_wp), intent(in) :: balance_file_unit     !! balance_file_unit
        integer(kind = int_wp), intent(in) :: simulation_time
        integer(kind = int_wp), intent(in) :: num_fluxes
        integer(kind = int_wp) :: initialize_file
        integer(kind = int_wp), intent(in) :: num_substances        !! Total number of substances
        integer(kind = int_wp), intent(in) :: num_dump_segments
        integer(kind = int_wp), intent(in) :: num_extra_variables
        character(len = 40), intent(in) :: model_name(4)

        real(kind = real_wp), intent(in) :: mass_balance_terms(num_substances, num_dump_segments, 6)
        real(kind = real_wp), intent(in) :: integrated_fluxes(num_fluxes, num_dump_segments)
        real(kind = real_wp), intent(in) :: extra_variables(num_extra_variables, num_dump_segments)
        character(len = 20), intent(in) :: substances_names(*), monitoring_station_names(*)

        integer(kind = int_wp) :: j, i, k, isys, iflx, ihlp
        integer(kind = int_wp) :: ithandl = 0
        integer(kind = int_wp) :: nopout
        if (timon) call timstrt ("write_balance_output", ithandl)

        ! Initialize file
        if (initialize_file == 1) then
            initialize_file = 0

            ! write header
            write (balance_file_unit) (model_name(i), i = 1, 4)
            nopout = 6 * num_substances + num_fluxes + 2
            write (balance_file_unit) nopout, num_dump_segments, num_substances
            write (balance_file_unit) (substances_names(i), i = 1, num_substances)
            write (balance_file_unit) (monitoring_station_names(i), i = 1, num_dump_segments)
        endif

        ! Perform output
        write (balance_file_unit) simulation_time, (&
                ((mass_balance_terms(isys, j, k), k = 1, 6), isys = 1, num_substances), &
                (integrated_fluxes(iflx, j), iflx = 1, num_fluxes), &
                (extra_variables(ihlp, j), ihlp = 1, 2), &
                j = 1, num_dump_segments)

        if (timon) call timstop (ithandl)

    end subroutine write_balance_output

end module m_write_output
