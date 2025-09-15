!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!

!     Program to decompose a PROCES.ASC file into tables
program waqpb_import
   use m_validate_input, only: validate_names
   use m_waqpb_import_utils, only: check_read_error, get_input_type, parse_item_line
   use m_string_utils
   use m_waqpb_import_settings
   use m_cli_utils
   use m_waqpb_data

   implicit none

   character(len=1) :: c1
   character(len=10) :: c10, c10b, c10a
   character(len=20) :: c20
   character(len=30) :: grp
   character(len=50) :: c50
   character(len=10) :: initialConfgId
   character(len=50) :: initialConfgName
   character(len=255) :: line_buffer

   real :: value
   integer :: jndex, num_items, idx_item, iproc, i, ihulp, &
              noffse, ihulp2, ihulp3, ihulp4, num_proc_exp, &
              num_proc_read, noffsf, iitem, iconf
   integer :: delete, replac, insert, abort, none
   parameter(delete=1, replac=2, insert=3, abort=0, none=4)
   logical :: status
   integer :: ierr, linecount
   integer :: io_mes, io_asc, io_inp, lunfil
   character(:), dimension(:), allocatable :: substrings

   type(waqpb_import_settings) :: settings ! Settings for the import tool

   data grp/'DummyGroup                          '/
   data initialConfgId/'DummyConfg'/

   ! Format specifiers
   character(len=*), parameter :: FMT31 = "(a10,f18.0,a1,1x,a50,5x,a20)"
   character(len=*), parameter :: FMT32 = "(a10,18x,  a1,1x,a50,5x,a20)"

   call settings%init()

   inquire (file=settings%processes_overview_file_path, exist=status)
   if (.not. status) then
      write (*, '(A,A,A)') 'Error: the processes overview file "', trim(settings%processes_overview_file_path), '" cannot be found.'
      stop 1
   end if

   open (newunit=io_mes, file='waqpb_import.log')
   if (settings%create_new_tables) then
      write (io_mes, '(''Creating new tables'')')
   else
      write (io_mes, '(''Updating existing tables'')')
   end if
   initialConfgName = initialConfgId

   nitem = 0
   nfort = 0
   nproc = 0
   ninpu = 0
   noutp = 0
   noutf = 0
   nstoc = 0
   ndisp = 0
   nvelo = 0

   if (.not. settings%create_new_tables) then
      write (*, '('' Loading database......'')')
      write (*, *)
      write (io_mes, '(''Loading database......'')')
      ! Read the existing tables
      call readdb(io_inp, io_mes, settings%csv_folder_path)
      ! Store R1 in relational way
      ncnpr = 0
      do iproc = 1, nproc
         do iconf = 1, nconf
            if (conpro(iconf, iproc)) then
               ncnpr = ncnpr + 1
               r1_pid(ncnpr) = procid(iproc)
               r1_cid(ncnpr) = confid(iconf)
            end if
         end do
      end do
      ! Remove primary   table  P4
      ! Remove secondary tables R4 till R8
      nproc = 0
      ninpu = 0
      noutp = 0
      noutf = 0
      nstoc = 0
      ndisp = 0
      nvelo = 0
   end if

   write (*, '('' Decomposing '',a,''......'')') settings%processes_overview_file_path
   write (*, *)
   write (io_mes, '('' Decomposing '',a,''......'')') settings%processes_overview_file_path
   open (newunit=io_asc, file=settings%processes_overview_file_path, status='old', err=999)

   !----------------------------------------------------------------------c
   !     Parsing the processes overview file
   !----------------------------------------------------------------------c
   linecount = 1
   read (io_asc, *, iostat=ierr) num_proc_exp
   call check_read_error(io_mes, ierr, linecount, 'number of processes')
   if (num_proc_exp <= 0) stop 'No processes found in the overview file'

   call iniind()

   ! Loop over processes
   iproc = 1
   do
      ! process name and description
      linecount = linecount + 1
      read(io_asc, '(A)', iostat=ierr) line_buffer
      if (ierr <0) then
         write (*, '(A, A)') 'Succesfully finished reading processes overview file ', settings%processes_overview_file_path
         exit
      else if (ierr > 0) then
         write (*, '(A,I5)') 'Finished normally reading processes overview file at line ', linecount
         exit
      end if

      line_buffer = adjustl(line_buffer)
      substrings = split_string_non_empty(line_buffer, ' ') !< Split the line into substrings based on spaces
      if (len_trim(substrings(1)) > 10) then
            write (*, '(A, A, A, I0)') 'Error: process name "', trim(substrings(1)), '" exceeds 10 characters at line ', linecount
            error stop
      else
         c10 = substrings(1)
         line_buffer = adjustl(line_buffer(len_trim(substrings(1))+1:len(line_buffer)))
         if (len_trim(line_buffer) > 50) then
            write (*, '(A, A, A, I0)') 'Error: process description "', trim(line_buffer), '" exceeds 50 characters at line ', linecount
            error stop
         end if
         c50 = trim(line_buffer)
      end if

      write (io_mes, '(A, I3, A, A)' ), 'Process ', iproc,' : ', c10
      write (*, '(A, I3, A, A)' ), 'Process ', iproc,' : ', c10

      ! name of Fortran subroutine
      linecount = linecount + 1
      read(io_asc, '(A)', iostat=ierr) line_buffer
      substrings = split_string_non_empty(line_buffer, ' ') !< Split the line into substrings based on spaces
      if (len_trim(substrings(1)) > 10) then
            write (*, '(A, A, A, I0)') 'Error: name of Fortran subroutine "', trim(substrings(1)), '" exceeds 10 characters at line ', linecount
            error stop
      else
         c10a = substrings(1)
      end if

      ! transport switch
      linecount = linecount + 1
      read (io_asc, *, iostat=ierr) jndex
      call check_read_error(io_mes, ierr, linecount, 'transport switch')

      if (nproc + 1 > nprocm) then
         write (*, '(A, I5, A, I5)') 'Error: already read ', nproc + 1, ' processes, exceeding maximum number of processes = ', nprocm
         error stop
      end if
      call validate_names([c10a], io_mes) ! process Fortran name
      nproc = nproc + 1
      procid(nproc) = c10
      procnm(nproc) = c50
      procfo(nproc) = c10a
      procco(nproc) = jndex

      call upd_p3(c10a, settings%create_new_tables, io_mes)

      ! input items on cells
      linecount = linecount + 1
      read (io_asc, '(i10)', iostat=ierr) num_items
      call check_read_error(io_mes, ierr, linecount, 'number of input items on cells (segments)')

      ihulp = num_items
      do idx_item = 1, num_items
         linecount = linecount + 1
         call parse_item_line(io_asc, io_mes, linecount, c10, value, c1, c50, c20, is_input = .true.)
         call upd_p2(c10, c50, value, 1, settings%create_new_tables, grp, io_mes, iitem, c20, .false.)

         ninpu = ninpu + 1
         if (ninpu > ninpum) then
            write (*, *) ninpu
            stop 'DIMENSION NINPUM'
         end if

         inpupr(ninpu) = procid(nproc)
         inpuit(ninpu) = itemid(iitem)
         inpunm(ninpu) = idx_item
         inpudo(ninpu) = c1

         inpude(ninpu) = get_input_type(value)
         ! Switch to decide segment/exchange!
         inpusx(ninpu) = 1

      end do

      ! input items on exchanges
      linecount = linecount + 1
      read (io_asc, '(i10)', iostat=ierr) num_items
      call check_read_error(io_mes, ierr, linecount, 'number of input items on exchanges')

      do idx_item = 1, num_items
         linecount = linecount + 1
         call parse_item_line(io_asc, io_mes, linecount, c10, value, c1, c50, c20, is_input = .true.)
         call upd_p2(c10, c50, value, 2, settings%create_new_tables, grp, io_mes, iitem, c20, .false.)
         ninpu = ninpu + 1
         if (ninpu > ninpum) then
            write (*, *) ninpu
            stop 'DIMENSION NINPUM'
         end if

         inpupr(ninpu) = procid(nproc)
         inpuit(ninpu) = itemid(iitem)
         inpunm(ninpu) = idx_item + ihulp
         inpudo(ninpu) = c1
         inpude(ninpu) = get_input_type(value)
         ! Switch to decide segment/exchange!
         inpusx(ninpu) = 0
      end do

      ! output items on cells
      linecount = linecount + 1
      read (io_asc, '(i10)', iostat=ierr) num_items
      call check_read_error(io_mes, ierr, linecount, 'number of output items on cells (segments)')

      ihulp = num_items
      do idx_item = 1, num_items
         linecount = linecount + 1
         call parse_item_line(io_asc, io_mes, linecount, c10, value, c1, c50, c20, is_input = .false.)

         value = -999.
         call upd_p2(c10, c50, value, 1, settings%create_new_tables, grp, io_mes, iitem, c20, .false.)
         noutp = noutp + 1
         if (noutp > noutpm) stop 'DIMENSION NOUTPM'
         outppr(noutp) = procid(nproc)
         outpit(noutp) = itemid(iitem)
         outpnm(noutp) = idx_item
         outpdo(noutp) = c1
         ! Switch to decide segment/exchange!
         outpsx(noutp) = 1
      end do

      ! output items on exchanges
      linecount = linecount + 1
      read (io_asc, '(i10)', iostat=ierr) num_items
      call check_read_error(io_mes, ierr, linecount, 'number of output items on exchanges')

      do idx_item = 1, num_items
         linecount = linecount + 1
         call parse_item_line(io_asc, io_mes, linecount, c10, value, c1, c50, c20, is_input = .false.)

         value = -999.
         call upd_p2(c10, c50, value, 2, settings%create_new_tables, grp, io_mes, iitem, c20, .false.)
         noutp = noutp + 1
         if (noutp > noutpm) stop 'DIMENSION NOUTPM'
         outppr(noutp) = procid(nproc)
         outpit(noutp) = itemid(iitem)
         outpnm(noutp) = idx_item + ihulp
         outpdo(noutp) = c1
         ! Switch to decide segment/exchange!
         outpsx(noutp) = 0
      end do

      ! fluxes
      noffsf = noutf
      linecount = linecount + 1
      read (io_asc, '(i10)', iostat=ierr) num_items
      call check_read_error(io_mes, ierr, linecount, 'number of fluxes')

      do idx_item = 1, num_items
         linecount = linecount + 1
         call parse_item_line(io_asc, io_mes, linecount, c10, value, c1, c50, c20, is_input = .false.)

         value = -999.
         call upd_p2(c10, c50, value, 1, settings%create_new_tables, grp, io_mes, iitem, c20, .false.)
         noutf = noutf + 1
         if (noutf > noutfm) stop 'DIMENSION NOUTFM'
         outfpr(noutf) = procid(nproc)
         outffl(noutf) = c10
         outfnm(noutf) = idx_item
         outfdo(noutf) = c1
      end do

      ! stoichiometry lines
      noffse = nstoc
      linecount = linecount + 1
      read (io_asc, '(i10)', iostat=ierr) num_items
      call check_read_error(io_mes, ierr, linecount, 'number of stoichiometry lines')

      do idx_item = 1, num_items
         linecount = linecount + 1
         read (io_asc, '(a10,2x,a10,2x,f10.0)', iostat=ierr) c10, c10b, value
         call check_read_error(io_mes, ierr, linecount, 'stoichiometry line')

         ! check presence of current flux in fluxes under current process
         ihulp = index_in_array(c10b(:10), outffl(noffsf + 1:noutf))
         if (ihulp <= 0) then
            write (*, *) ' Illegal flux in stoichiometry line!!'
            write (*, *) c10b
            stop ' Fatal error'
         end if
         nstoc = nstoc + 1
         if (nstoc > nstocm) stop 'DIMENSION NSTOCM'
         stocfl(nstoc) = c10b
         stocsu(nstoc) = c10
         stocsc(nstoc) = value

         value = -999.
         c50 = ' '
         c20 = ' '
         call upd_p2(c10, c50, value, 0, settings%create_new_tables, grp, io_mes, iitem, c20, .false.)
      end do

      ! stoichiometry lines dispersion
      noffse = ndisp
      linecount = linecount + 1
      read (io_asc, '(i10)', iostat=ierr) num_items
      call check_read_error(io_mes, ierr, linecount, 'number of stoichiometry lines (dispersion)')

      do idx_item = 1, num_items
         linecount = linecount + 1
         read (io_asc, '(a10,2x,a10,2x,f10.0)', iostat=ierr) c10, c10b, value
         call check_read_error(io_mes, ierr, linecount, 'stoichiometry line (dispersion)')

         ndisp = ndisp + 1
         if (ndisp > ndispm) stop 'DIMENSION NDISPM'
         dispit(ndisp) = c10b
         dispsu(ndisp) = c10
         dispsc(ndisp) = value

         value = -999.
         c50 = ' '
         c20 = ' '
         call upd_p2(c10, c50, value, 0, settings%create_new_tables, grp, io_mes, iitem, c20, .false.)
      end do

      ! stoichiometry lines velocity
      noffse = nvelo
      linecount = linecount + 1
      read (io_asc, '(i10)', iostat=ierr) num_items
      call check_read_error(io_mes, ierr, linecount, 'number of stoichiometry lines (velocity)')

      do idx_item = 1, num_items
         linecount = linecount + 1
         read (io_asc, '(a10,2x,a10,2x,f10.0)', iostat=ierr) c10, c10b, value
         call check_read_error(io_mes, ierr, linecount, 'stoichiometry line (velocity)')

         nvelo = nvelo + 1
         if (nvelo > nvelom) stop 'DIMENSION NVELOM'
         veloit(nvelo) = c10b
         velosu(nvelo) = c10
         velosc(nvelo) = value

         value = -999.
         c50 = ' '
         c20 = ' '
         call upd_p2(c10, c50, value, 0, settings%create_new_tables, grp, io_mes, iitem, c20, .false.)
      end do
      linecount = linecount + 1
      read (io_asc, '(a10)', iostat=ierr) c10
      if (c10(1:3) /= 'END') stop 'error'
      iproc = iproc + 1
   end do
   iproc = iproc - 1
   ! End of reading processes overview file
   if (iproc /= num_proc_exp) then
      write (*, '(A,I0,A, I0, A)') 'Warning: the process overview file indicated ', num_proc_exp, ' processes, however, ', iproc, ' have been found and read.'
      write (io_mes, '(A,I0,A, I0, A)') 'Warning: the process overview file indicated ', num_proc_exp, ' processes, however, ', iproc, ' have been found and read.'
   end if

   close (io_asc)

   ! Sort and check R6-R7-R8
   call sortst(stocfl, stocsu, stocsc, nstoc)
   call chksto(stocfl, stocsu, stocsc, nstoc, itemid, nitem, io_mes)
   call sortst(dispit, dispsu, dispsc, ndisp)
   call chksto(dispit, dispsu, dispsc, ndisp, itemid, nitem, io_mes)
   call sortst(veloit, velosu, velosc, nvelo)
   call chksto(veloit, velosu, velosc, nvelo, itemid, nitem, io_mes)

   ! Create/update tables R1, R2
   call cratab(grp, settings%create_new_tables, initialConfgId, initialConfgName)

   ! Clear tables
   call cldept

   !----------------------------------------------------------------------c
   !     Adhoc correction on default values
   !     BLOOMAlgii must be -101
   !     Only -dis and -par quantities may have default value -11
   !----------------------------------------------------------------------c

   if (settings%create_new_tables) then

      do i = 1, nitem
         if (string_equals(itemid(i) (1:8), 'bloomalg')) then
            if (itemid(i) (9:10) == '01') then
               itemde(i) = -999.
            else
               itemde(i) = -101.
            end if
         end if
         if (abs(itemde(i) + 11.0) < 1e-10) then
            ihulp = index(itemid(i), '-dis')
            ihulp2 = index(itemid(i), '-par')
            ihulp3 = index(itemid(i), '-Dis')
            ihulp4 = index(itemid(i), '-Par')
            if (ihulp <= 0 .and. ihulp2 <= 0 .and. ihulp3 <= 0 .and. ihulp4 <= 0) then
               itemde(i) = -999.
            end if
         end if
         if (string_equals(itemid(i) (1:5), 'depth') &
             .or. string_equals(itemid(i) (1:4), 'delt') &
             .or. string_equals(itemid(i) (1:10), 'totaldepth')) then

            itemde(i) = -999.
         end if
      end do

   end if

   !----------------------------------------------------------------------c
   !     Dump tables
   !----------------------------------------------------------------------c

   call writdb(io_inp, settings%csv_folder_path)

   close (io_mes)

   stop 'Normal end'
999 stop settings%processes_overview_file_path  // ' does not exist!'

end program waqpb_import

subroutine iniind()
   use m_waqpb_data, only: &
      nitemm, nfortm, nprocm, ninpum, noutpm, noutfm, nstocm, nvelom, ndispm, &
      item_i, fort_i, proc_i, inpu_i, outp_i, outf_i, stoc_i, velo_i, disp_i

   integer :: iitem, ifort, iproc, iinpu, ioutp, ioutf, istoc, ivelo, idisp

   !     Initialise indexes arrays

   do iitem = 1, nitemm
      item_i(iitem) = iitem
   end do

   do ifort = 1, nfortm
      fort_i(ifort) = ifort
   end do

   do iproc = 1, nprocm
      proc_i(iproc) = iproc
   end do

   do iinpu = 1, ninpum
      inpu_i(iinpu) = iinpu
   end do

   do ioutp = 1, noutpm
      outp_i(ioutp) = ioutp
   end do

   do ioutf = 1, noutfm
      outf_i(ioutf) = ioutf
   end do

   do istoc = 1, nstocm
      stoc_i(istoc) = istoc
   end do

   do ivelo = 1, nvelom
      velo_i(ivelo) = ivelo
   end do

   do idisp = 1, ndispm
      disp_i(idisp) = idisp
   end do

   return
end subroutine iniind

subroutine cratab(grp, newtab, initialConfgId, initialConfgName)

   use m_string_utils
   use m_waqpb_data, only: &
      nproc, nconf, ncnpr, nitem, ncnsb, ncnsbm, &
      procid, confid, conpro, itemid, itemgr, r1_pid, r1_cid, &
      r2_cid, r2_sid, sgrpid, grp, sgrpnm, confnm

   character(len=30) :: grp
   character(len=10) :: initialConfgId
   character(len=50) :: initialConfgName

   integer :: iitem, iproc, icnpr, iconf
   logical :: newtab

   if (newtab) then
      ! NEW TABLES
      ! Dummy versions of tables P1 and P5
      nsgrp = 1
      sgrpid(1) = grp
      sgrpnm(1) = grp
      nconf = 1
      confid(1) = initialConfgId
      confnm(1) = initialConfgName

      ! Table R1
      ! include all processes in Dummy configuration

      do iproc = 1, nproc
         conpro(1, iproc) = .true.
      end do

      ! Table R2
      ! add all substances to Dummy configuration

      ncnsb = 0
      do iitem = 1, nitem
         if (itemgr(iitem) == grp) then
            ! This must be a substance
            ncnsb = ncnsb + 1
            r2_cid(ncnsb) = confid(1)
            r2_sid(ncnsb) = itemid(iitem)
         end if
      end do

   else

      ! UPDATE TABLES
      ! Recreate Table R1

      do iproc = 1, nproc
         do iconf = 1, nconf
            conpro(iconf, iproc) = .false.
         end do
      end do

      do icnpr = 1, ncnpr
         iproc = index_in_array(r1_pid(icnpr) (:10), procid(:nproc))
         iconf = index_in_array(r1_cid(icnpr) (:10), confid(:nconf))
         if (iconf <= 0) stop 'BUG CRATAB'
         if (iproc > 0) conpro(iconf, iproc) = .true.
      end do

      ! Table R2
      ! add all new substances to Dummy configuration
      ! NO EFFORT DONE TO CLEAR OLD ENTRIES

      do iitem = 1, nitem
         if (itemgr(iitem) == grp) then
            ! This must be a NEW substance
            if (ncnsb + 1 > ncnsbm) stop 'DIMENSION NCNSBM'
            ncnsb = ncnsb + 1
            r2_cid(ncnsb) = 'DummyConfg'
            r2_sid(ncnsb) = itemid(iitem)
         end if
      end do

   end if
   return
end subroutine cratab

