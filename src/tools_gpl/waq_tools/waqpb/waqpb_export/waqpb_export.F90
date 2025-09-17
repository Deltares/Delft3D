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
!     Program to compose a PROCES.ASC file from tables
!
!     This program consists of the following parts:
!     - Reading of tables holding PROCLIB data structure
!     - Loop over the processes:
!       - Empty PDF structure
!       - Construct and write PDF structure
!       - Dump structure to PROCES.ASC

!     Include data structures for tables and PDF-file
program waqpb_export
   use m_obtain_number_decimals
   use m_string_utils
   use m_waqpb_export_helper
   use m_waqpb_export_settings
   use m_cli_utils
   use m_waqpb_data

   implicit none

   include 'pdf_ff.inc'
   integer jndex, iproc, iinpu, iitem, ioutp, idisp, &
      ioutf, isubs, naanta, ioffse, ioffs2, ivelo, &
      istoc, iconf, naant2, &
      ierror, icnsb, imodv, i
   logical :: itmswi(nitemm)
   logical :: status
   character(len=10) c10, num_decimals_version_char
   character(len=20) c20
   character(len=50) adduni

   real actdef
   integer lu_inp, lu_mes, lunfil, num_decimals_version
   type(waqpb_export_settings) :: settings ! Settings for the export tool

   call settings%init()

   inquire (directory=settings%process_definition_folder_path, exist=status)
   if (.not. status) then
      write (*, '(A,A,A)') 'Error: "', trim(settings%process_definition_folder_path), '" is not a valid path for the process definition folder.'
      stop 1
   end if

   write (*, *) "Running with version ", settings%version, " and serial ", settings%serial

   itmswi = .false.
   open (newunit=lu_mes, file=(settings%process_definition_folder_path//'waqpb_export.log'))

   write (*, '('' Reading data...'')')

!----------------------------------------------------------------------c
!     READ DATABASE
!----------------------------------------------------------------------c
   call readdb(lu_inp, lu_mes, settings%csv_folder_path)

!     Check validity of table R9

   do imodv = 1, nmodv
      iconf = index_in_array(modvci(imodv), confid(:nconf))
      if (iconf <= 0) then
         write (lu_mes, '(''Unknown config in TABLE5: '',a10,1x, a10)') modvci(imodv), modvit(imodv)
      end if
      iitem = index_in_array(modvit(imodv), itemid(:nitem))
      if (iitem <= 0) then
         write (lu_mes, '(''Unknown item in TABLE5: '',a10,1x, a10)') modvci(imodv), modvit(imodv)
      end if
   end do

!     Create auxiliary table of substances

   nsubs = 0
   do icnsb = 1, ncnsb
      c10 = r2_sid(icnsb)

!         Lookup substance in item array
      iitem = index_in_array(c10, itemid(:nitem))
      if (iitem <= 0) then
         write (*, *) ' ITEM: ', c10
         stop 'Unknown substance in R2 table'
      end if

!         Add to substances array
      isubs = index_in_array(c10(:10), subsid(:nsubs))
      if (isubs <= 0) then
         if (nsubs + 1 > nsubsm) stop 'Dimension NSUBSM'
         nsubs = nsubs + 1
         subsid(nsubs) = c10
      end if
   end do

!     Dump TRM tables

!      write (*,'('' Writing TRM tables......'')')
!      call writrm
   if (settings%generate_latex_tables) then
      write (*, '('' Writing TRM tables for LaTeX...'')')
      call writex
   end if

!----------------------------------------------------------------------c
!     SET VERSION, SERIAL AND WRITE NEFIS FILE
!----------------------------------------------------------------------c

   write (lu_mes, '(''Writing NEFIS process definition file'')')
   call makind()
   call pdfnef(lu_mes, settings%serial, settings%version, settings%process_definition_folder_path, ierror, settings%generate_latex_tables)
   if (ierror /= 0) then
      write (lu_mes, '(''ERROR writing NEFIS file'')')
      write (*, '(''ERROR writing NEFIS file, see report file'')')
   end if

!----------------------------------------------------------------------c
!     LOOP OVER PROCESSES
!----------------------------------------------------------------------c

   write (*, '(A)') 'Creating processes overview file: ' // settings%processes_overview_file_path
   write (*, *)
   open (newunit=lunfil, file=settings%processes_overview_file_path)

   ! obtain number of decimals of version number
   num_decimals_version = obtain_num_decimals_version(settings%version)
   write (num_decimals_version_char, '(I10)') num_decimals_version

   write (lunfil, '(i10,50x,f8.'//num_decimals_version_char//',2x, I10)') nproc, settings%version, settings%serial

   do iproc = 1, nproc

      write (*, '(''+Process: '',a10)') procid(iproc)

!----------------------------------------------------------------------c
!         CONSTRUCT PROCESS
!----------------------------------------------------------------------c

!         Clear PDF structure

      ins = 0
      ine = 0
      ous = 0
      oue = 0
      flu = 0
      sto = 0
      dis = 0
      vel = 0

!         Fill PDF structure

!         INPUT ITEMS ON SEGMENT LEVEL/EXCHANGE LEVEL

!         scan input items table for FIRST occurence of proces
      ioffse = index_in_array(procid(iproc), inpupr(:ninpu))
      naanta = 0
      if (ioffse > 0) then

!             loop over all INPU rows related to this process

410      continue
         naanta = naanta + 1

!             Process current row

!             Lookup item in items table
         iinpu = ioffse + naanta - 1
         iitem = index_in_array(inpuit(iinpu), itemid(:nitem))
         if (iitem <= 0) stop 'unknown ITEM'

         !             Documented items are marked for COEFEDIT.DAT
         if (inpudo(iinpu) == 'x') itmswi(iitem) = .true.

         !             Find item properties and store in PDF structure
         if (inpude(iinpu) == 'Y') then
            actdef = itemde(iitem)
         elseif (inpude(iinpu) == 'G') then
            actdef = -888.
         elseif (inpude(iinpu) == 'B') then
            actdef = -101.
         elseif (inpude(iinpu) == 'M') then
            actdef = -11.
         elseif (inpude(iinpu) == 'O') then
            actdef = -1.
         else
            actdef = -999.
         end if
         if (inpusx(iinpu) == 1) then
            ins = ins + 1
            if (ins > insmax) stop 'DIMENSION insmax'
            ins_id(ins) = itemid(iitem)
            ins_nm(ins) = itemnm(iitem)
            ins_un(ins) = itemun(iitem)
            ins_va(ins) = actdef
            ins_do(ins) = inpudo(iinpu)
         else
            ine = ine + 1
            if (ine > inemax) stop 'DIMENSION inemax'
            ine_id(ine) = itemid(iitem)
            ine_nm(ine) = itemnm(iitem)
            ine_un(ine) = itemun(iitem)
            ine_va(ine) = actdef
            ine_do(ine) = inpudo(iinpu)
         end if

         !             Back for next row in table INPU,
         !             if it still matches current proces

         if ((iinpu + 1) <= ninpu) then
            if (string_equals(procid(iproc), inpupr(iinpu + 1))) goto 410
         end if
      end if

!         OUTPUT ITEMS ON SEGMENT LEVEL/EXCHANGE LEVEL

      !         scan output items table for FIRST occurence of proces
      ioffse = index_in_array(procid(iproc), outppr(:noutp))
      naanta = 0
      if (ioffse > 0) then

!             loop over all OUTP rows related to this process

440      continue
         naanta = naanta + 1

         !             Process current row

         !             Lookup item in items table
         ioutp = ioffse + naanta - 1
         iitem = index_in_array(outpit(ioutp), itemid(:nitem))
         if (iitem <= 0) stop 'unknown ITEM'

         !             Find item properties and store in PDF structure
         if (outpsx(ioutp) == 1) then
            ous = ous + 1
            if (ous > ousmax) stop 'DIMENSION ousmax'
            ous_id(ous) = itemid(iitem)
            ous_nm(ous) = itemnm(iitem)
            ous_un(ous) = itemun(iitem)
            ous_do(ous) = outpdo(ioutp)
         else
            oue = oue + 1
            if (oue > ouemax) stop 'DIMENSION ouemax'
            oue_id(oue) = itemid(iitem)
            oue_nm(oue) = itemnm(iitem)
            oue_un(oue) = itemun(iitem)
            oue_do(oue) = outpdo(ioutp)

!                 SCAN VELO and DISP TABLES FOR LINES ASSOCIATED WITH
!                 CURRENT OUTPUT ITEM ON EXCHANGE LEVEL

!                 scan dispersion lines table for FIRST occurence of item
            ioffs2 = index_in_array(itemid(iitem), dispit(:ndisp))
            naant2 = 0
            if (ioffs2 > 0) then

!                     loop over all DISP rows related to this item

450            continue
               naant2 = naant2 + 1
               dis = dis + 1
               if (dis > dismax) stop 'dimension DISMAX'

               !                     Process current row

               idisp = ioffs2 + naant2 - 1
               dis_su(dis) = dispsu(idisp)
               dis_it(dis) = dispit(idisp)
               dis_sc(dis) = dispsc(idisp)

               !                     Back for next row in table DISP,
               !                     if it still matches current item

               if ((idisp + 1) <= ndisp) then
                  if (string_equals(itemid(iitem), dispit(idisp + 1))) goto 450
               end if
            end if

            !                 scan velocity lines table for FIRST occurence of item
            ioffs2 = index_in_array(itemid(iitem), veloit(:nvelo))
            naant2 = 0
            if (ioffs2 > 0) then

!                     loop over all VELO rows related to this item

460            continue
               naant2 = naant2 + 1
               vel = vel + 1
               if (vel > velmax) stop 'dimension VELMAX'

               !                     Process current row

               ivelo = ioffs2 + naant2 - 1
               vel_su(vel) = velosu(ivelo)
               vel_it(vel) = veloit(ivelo)
               vel_sc(vel) = velosc(ivelo)

               !                     Back for next row in table VELO,
               !                     if it still matches current item

               if ((ivelo + 1) <= nvelo) then
                  if (string_equals(itemid(iitem), veloit(ivelo + 1))) goto 460
               end if
            end if

            !                 END of processing output item on exchange level!

         end if

         !             Back for next row in table OUTP,
         !             if it still matches current proces

         if ((ioutp + 1) <= noutp) then
            if (string_equals(procid(iproc), outppr(ioutp + 1))) goto 440
         end if
      end if

      !         FLUXES

      !         scan output fluxes table for FIRST occurence of proces
      ioffse = index_in_array(procid(iproc), outfpr(:noutf))
      if (ioffse > 0) then

!             loop over all FLUX rows related to this process

470      continue
         flu = flu + 1
         if (flu > flumax) stop 'dimension FLUMAX'

         !             Process current row

         !             Lookup flux in items table
         ioutf = ioffse + flu - 1
         !             write (lu_mes,*) ' flu ',flu,' ioutf ', ioutf
         iitem = index_in_array(outffl(ioutf), itemid(:nitem))
         if (iitem <= 0) stop 'unknown FLUX'

         !             Find and store flux properties
         flu_id(flu) = itemid(iitem)
         flu_nm(flu) = itemnm(iitem)
         flu_un(flu) = itemun(iitem)
         flu_do(flu) = outfdo(ioutf)

         !             SCAN STOCHI TABLE FOR LINES ASSOCIATED WITH PRESENT FLUX

         !             scan stochi lines table for FIRST occurence of flux
         ioffs2 = index_in_array(itemid(iitem), stocfl(:nstoc))
         naant2 = 0
         if (ioffs2 > 0) then

            !                 loop over all STOC rows related to this flux

480         continue
            naant2 = naant2 + 1
            sto = sto + 1
            if (sto > stomax) stop 'dimension STOMAX'

            !                 Process current row

            istoc = ioffs2 + naant2 - 1
            sto_su(sto) = stocsu(istoc)
            sto_fl(sto) = stocfl(istoc)
            sto_sc(sto) = stocsc(istoc)

            !                 Back for next row in table STOC,
            !                 if it still matches current flux

            if ((istoc + 1) <= nstoc) then
               if (string_equals(itemid(iitem), stocfl(istoc + 1))) goto 480
            end if
         end if

         !             Back for next row in table OUTF,
         !             if it still matches current proces

         if ((ioutf + 1) <= noutf) then
            if (string_equals(procid(iproc), outfpr(ioutf + 1))) goto 470
         end if
      end if

!----------------------------------------------------------------------c
!         WRITE PROCESS
!----------------------------------------------------------------------c

!         Write PDF file (formats as in HARMONIZE to allow comparison)
      call wripdn(procid(iproc), procnm(iproc), procco(iproc), procfo(iproc), lunfil)
   end do
   close (lunfil)

   !     Write all active coefficients to COEFEDIT.DAT in the Sobek-format
   call coefed(settings%serial, itmswi)

900 continue
   close (lu_mes)

   write (*,*)
   if (settings%wrong_version) then
      write (*, '(A, f0.1)') 'WARNING: no or not a valid version was specified (see above). Used generated default version instead: ', settings%version
   end if

   if (settings%wrong_serial) then
      write (*, '(A, I0)') 'WARNING: no or not a valid serial number was specified (see above). Used generated default serial instead: ', settings%serial
   end if

   write (*, *)
   write (*, '(A)') 'Export completed successfully.'
   stop 'Normal end.'
end program waqpb_export

function adduni(name, unit)
   character(len=50) adduni, name
   character(len=20) unit

   integer lennam, lenuni, i

!     find length of name and unit

   lennam = -1
   do 10 i = 50, 1, -1
      if (name(i:i) /= ' ') then
         lennam = i
         goto 11
      end if
10    continue
11    continue
      if (lennam <= 1) then
         write (*, *) ' ', name
         stop 'ADDUNI Fatal Error 1'
      end if

      lenuni = 0
      if (unit(2:3) == 'no' .and. unit(5:8) == 'unit') then
         lenuni = 0
      else
         do 20 i = 20, 1, -1
            if (unit(i:i) /= ' ') then
               lenuni = i
               goto 21
            end if
20          continue
21          continue
            end if
            if (lenuni < 0) then
               write (*, *) ' ', unit
               stop 'ADDUNI Fatal Error 1'
            end if

            if (lennam + lenuni > 50) then
               lennam = 50 - lenuni
            end if

            write (adduni(1:lennam), '(a)') name(1:lennam)
            do 30 i = 1, 50 - lennam - lenuni
30             adduni(lennam + i:lennam + i) = ' '
               if (lenuni > 0) then
                  write (adduni(50 - lenuni + 1:50), '(a)') unit(1:lenuni)
               end if

               return
               end function adduni

