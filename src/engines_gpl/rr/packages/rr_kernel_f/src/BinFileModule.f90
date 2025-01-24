!----- AGPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
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

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 15-08-97 3:30p   $
!
! current revision: $Revision:: 8               $




module BinFile

  ! uses
  use Boundary
  use CONF_FIL
  use CONF_ARR
  use Greenhouse
  use Link
  use RR_Meteo
  use Network
  use NWRW
  use Sacramento
  use RRRunoff
  use Openwater
  use Paved
  use Salts
  use Structures
  use NewTables
  use Unpaved
  use Hash
  use Rwzi
  use Industry
  use RRConnectionBifurcationNodes
  use ReadLib
  use globals

  !###########################
    !use TREE_DATA_TYPES
    !use TREE_STRUCTURES
    !use properties
    !use m_hash_search
    !use m_node
    !use m_branch
    !use m_network
    use m_1d_networkreader_rr
  !###########################

  implicit none

  ! variables
  real  SaltConcentrationDWF

  ! none

contains

  SUBROUTINE RDBINF (SkipBinFile, ParseTokenMethod, MeteoNetcdfInput)
    ! *********************************************************************
    ! *** Last update: 3 March 1997       By : Peter Schrier
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***   Read all node-data of timestep X from binary file
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  binOutputfileHandle = file unit
    ! ***  IDEBUG = debug file unit
    ! *********************************************************************


    ! variables
    Logical     SkipBinfile, ParseTokenMethod, MeteoNetcdfInput
    Logical     success
    Integer     teller, iOW, iCall, in
    Integer     iNode, iKind, iNr
    Character(CharIdLength) idNode, FileName
    Integer iDebug, iOut1, infile1, infile2, infile3, infile4, infile5, infile6
    Integer SetMaxTabNr, SetMaxDataNr
    Character(Len=FilCharIdLength)            :: boundCondFile

    Character(1000) :: inifile_01, inifile_02, tmpfile
!    Integer i, hashcode

! huidige datum/tijd
!   CHARACTER*10 CDATE, CTIME, CZONE
!   INTEGER      VALUES(8)

    ! body
    iDebug = ConfFil_get_iDebug()
    iOut1 = ConfFil_get_iOut1()

    IF (IDEBUG /= 0) WRITE (IDEBUG,1)
  1 FORMAT (' RDBINF')

    iOW = 0

! Lees invoer files
! Vroeger Twee opties: Uit Bin file
!                      Of uit ASCII files
! Nu wordt alleen de ASCII file optie ondersteund


! zet vaste dimensies reeds eerder gedaan in Delft_3B.f90
!     call confar0
!     call confar0

     if (.not. SkipBinfile) then
        call SetMessage(LEVEL_FATAL, 'Internal program error SkipBinFile')
     Else
!       Alles uit ASCII files lezen
!       ARS 16442: control data elders eerder ingelezen!
!       Record 1: Control data
!        Call Openfl (infile1, ConfFil_get_namFil(1),1,1)   !Delft_3B.Ini file
!        Call Network_ReadAscii (infile1)
!        close (infile1)
        ! scan de topografie files om het aantal knopen en takken te bepalen




        !inifile_01 = "D:\PROJECTS\Rekenhart_hydrologie\Compare_iniswitch\Test_001_INI\001_010.LIT\1\3B_topology.ini"
         inifile_01 = ConfFil_get_namFil(2)
         inifile_02 = ConfFil_get_namFil(4)





        if (ini_switch) then

            if ((trim(inifile_01) .ne. 'rr_topology.ini') .or. (trim(inifile_02) .ne. '3brunoff.ini')  ) then
                error stop '** ERROR: ini_switch set to true but input files not found or not in correct format. Is rr_topology.ini and 3brunoff.ini called in sobek_3b.fnm?'
            endif

            call ReadNrNodes_ini(inifile_01,inifile_02,ncnode)


        else
            Call Openfl (infile1, ConfFil_get_namFil(2),1,1) !file met polderknopen in schematisatie
            Call Openfl (infile2, ConfFil_get_namFil(4),1,1) !file met NWRW-knopen in schematisatie
            Call Openfl (infile3, ConfFil_get_namFil(3),1,1) !file met links in schematisatie

            Call ReadNrNodes (infile1, infile2, infile3)

            Call CloseGP (infile1)
            Call CloseGP (infile2)
            Call CloseGP (infile3)
        endif
        !error will occur at runtime if both are called



     Endif


     if (iStuurM /= 0) then
        inSbk = 1
     else
        inSbk = 0
     endif

     ! record 2: dimensions

!     check how many NWRW DWA records and tables possible
      Call ScanFile (infile1, Conffil_Get_Namfil(9), 1, 1, 'DWA ', NcPluvDwa, .true. )
!     Set maximum table dimensions; include NcNode assuming 1 evap table for each node
      SetMaxTabNr  = NcVhg + 4 * Ncovhg + 3* Ncow + 2 * Ncstru + Ncboun + 2 * NcIndus + NcPluvDwa + NcNode
      SetMaxDataNr = SetMaxTabNr * 1461 ! was *100; SoftSup151
      if (NcNode .ne. NcPluv .and. NcNode .ne. NcSacr) then
         SetMaxTabNr  = SetMaxTabNr + 4
         SetMaxDataNr = SetMaxDataNr  + 187900  ! 163820  Taiwan Oct 2007 nav Jira issue NewFormatKasData
      endif

      if (idebug .ne. 0) write(IdebugLunRR,*) ' Set Table dimensions', SetMaxTabNr, SetMaxDataNr
      Call NewTablesCreate
      TableHandle = NewTablesModelFindOrCreate(0)
      success = NewTableConfar (TableHandle, SetMaxTabNr, SetMaxDataNr)
      if (.not. success) then
!         call ErrMsgStandard (....)
          call SetMessage(LEVEL_FATAL, 'Error allocating table arrays')
      endif
!
      UseNewTables = SkipBinFile     ! Als SkipBinfile=true, dan uit ASCII files lezen en nieuwe tabellenmodule gebruiken

      ! conversie externe nummers naar interne nummers
      NNOD = MAX (1, NCNODE)
      IF (NNOD .gt. 0 .and. iOut1 .ne. 0)  WRITE(IOUT1,*) ' Number of nodes       =',NNOD

      call CONFAR1               ! alloceren van globale arrays

      call Hashinit



      if (ini_switch) then

            call ReadNetwork_ini(inifile_01, inifile_02)

      else
            ! record 3: node schematisation data
            ! Fill Node Hashing arrays, optional first Read node schematisation data from Bin file
            ! Read Node schematisation data from 2 ASCII files
            Call Openfl (infile1, ConfFil_get_namFil(2),1,1) !file met polderknopen in schematisatie
            Call Openfl (infile2, ConfFil_get_namFil(4),1,1) !file met NWRW-knopen in schematisatie

            call ReadNetwork (infile1, infile2)

            Call CloseGP (infile1)
            Call CloseGP (infile2)
        endif



      ! Fill Node Hashing arrays, optional first Read node schematisation data from Bin file


      do teller = 1, NCNODE
        idnode = Id_Nod(teller)
        call Hashfill (idNode, teller)
      enddo

      call convertExtern2Intern  ! aanpassen nummering knopen

      iCall = 1
      call Salt_initSl(iCall)     ! set NSalt
      call Salt_confar2

!     write(*,*) ' Binfile NewFormatKasData=',NewFormatKasData
      If (NewFormatKasData) then
         CALL OPENFL(IN, ConfFil_get_NAMFIL(108),1,1)
      Else
         CALL OPENFL(IN, ConfFil_get_NAMFIL(12),1,1)
      Endif

      iCall = 1
!     write(*,*) ' Binfile call RdKkls NewFormatKasData=',NewFormatKasData
      call RdKkls(in, iCall)  ! set NCKKL
      Call CloseGP (in)

      call Boundary_confar4  ! set NCSOBK
      call Boundary_confAr1  ! moved from block 10-boundaries

    ! record 4: link schematisation data
      call Link_Confar1

!     Link data

      if (ini_switch) then
          Call Openfl (infile4, ConfFil_get_namFil(117),1,1) !file met routing links in schematisatie
          tmpfile = "tmp_ini.ini"
          call Link_ReadAscii_ini(tmpfile,infile4)
      else
        Call Openfl (infile3, ConfFil_get_namFil(3),1,1)   !file met links in schematisatie
        Call Openfl (infile4, ConfFil_get_namFil(117),1,1) !file met routing links in schematisatie
        Call Link_ReadAscii (infile3, infile4)
        Call CloseGP (infile3)
        Call CloseGP (infile4)
      endif


    ! Alloceren Meteo arrays
      call Meteo_confAr1

    ! record 5: paved area data
      call Paved_confAr1(iOut1)
      call Openwater_confAr1(iOut1)  ! moved from block 8

      call Link_determineLinkData

      If (ncvhg .gt. 0) then
         Call Openfl (infile1, ConfFil_get_namFil(5),1,1)  !paved.3b
         Call Openfl (infile2, ConfFil_get_namFil(6),1,1)  !paved.sto
         Call Openfl (infile3, ConfFil_get_namFil(7),1,1)  !paved.dwa
         Call Openfl (infile4, ConfFil_get_namFil(8),1,1)  !paved.tbl
         call Paved_readAscii (infile1, infile2, infile3, infile4, SaltConcentrationDWF)
         Call CloseGP(infile1)
         Call CloseGP(infile2)
         Call CloseGP(infile3)
         Call CloseGP(infile4)
      elseif (CleanRRFiles) then      ! also clean when no paved nodes are used
         FileName = ConfFil_get_namFil(5)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,2)  !paved.3b_cleaned
         Call CloseGP(infile1)
         FileName = ConfFil_get_namFil(6)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile2, FileName, 1,2)  !paved.sto_cleaned
         Call CloseGP(infile2)
         FileName = ConfFil_get_namFil(7)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile3, FileName, 1,2)  !paved.dwa_cleaned
         Call CloseGP(infile3)
         FileName = ConfFil_get_namFil(8)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile4, FileName, 1,2)  !paved.tbl_cleaned
         Call CloseGP(infile4)
      Endif

    ! record 6: unpaved area data
      call Crop_confAr1_part1
      call Unpaved_confar1
      call Crop_confAr1
      If (ncovhg .gt. 0 .or. NcCell .gt. 0) then
!Unpaved area rechtstreeks uit ASCII; ook aanroepen als geen unpaved maar wel cell, ivm fixed crop, bergcoef data etc.
         Call Openfl (infile1, ConfFil_get_namFil(15),1,1)  !unpaved.3b
         Call Openfl (infile2, ConfFil_get_namFil(16),1,1)  !unpaved.sto
         Call Openfl (infile3, ConfFil_get_namFil(21),1,1)  !unpaved.alf
         Call Openfl (infile4, ConfFil_get_namFil(31),1,1)  !unpaved.inf
         Call Openfl (infile5, ConfFil_get_namFil(33),1,1)  !unpaved.sep
         Call Openfl (infile6, ConfFil_get_namFil(34),1,1)  !unpaved.tbl
         call Unpaved_readAsciiInput (infile1, infile2, infile3, infile4, infile5, infile6)
         Call CloseGP(infile1)
         Call CloseGP(infile2)
         Call CloseGP(infile3)
         Call CloseGP(infile4)
         Call CloseGP(infile5)
         Call CloseGP(infile6)
      elseif (CleanRRFiles) then      ! also clean when no unpaved nodes are used
         FileName = ConfFil_get_namFil(15)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,2)  !unpaved.3b_cleaned
         Call CloseGP(infile1)
         FileName = ConfFil_get_namFil(16)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile2, FileName, 1,2)  !unpaved.sto_cleaned
         Call CloseGP(infile2)
         FileName = ConfFil_get_namFil(21)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile3, FileName, 1,2)  !unpaved.alf_cleaned
         Call CloseGP(infile3)
         FileName = ConfFil_get_namFil(31)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile4, FileName, 1,2)  !unpaved.inf_cleaned
         Call CloseGP(infile4)
         FileName = ConfFil_get_namFil(33)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile4, FileName, 1,2)  !unpaved.sep_cleaned
         Call CloseGP(infile4)
         FileName = ConfFil_get_namFil(34)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile4, FileName, 1,2)  !unpaved.tbl_cleaned
         Call CloseGP(infile4)
      Endif

    ! record 7: greenhouse area data
      call Greenhouse_confAr1
      If (NcKas .gt. 0) then
!record 7 rechtstreeks uit ASCII
         Call Openfl (infile1, ConfFil_get_namFil(35),1,1) !greenhse.3b
         Call Openfl (infile2, ConfFil_get_namFil(36),1,1) !greenhse.rf
         Call Openfl (infile3, ConfFil_get_namFil(48),1,1) !greenhse.sil
         call Greenhouse_readAsciiInput (infile1, infile2, infile3)
         Call CloseGP(infile1)
         Call CloseGP(infile2)
         Call CloseGP(infile3)
      elseif (CleanRRFiles) then      ! also clean when no greenhouse nodes are used
         FileName = ConfFil_get_namFil(35)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,2)  !greenhse.3b_cleaned
         Call CloseGP(infile1)
         FileName = ConfFil_get_namFil(36)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile2, FileName, 1,2)  !greenhse.rf_cleaned
         Call CloseGP(infile2)
         FileName = ConfFil_get_namFil(48)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile3, FileName, 1,2)  !greenhse.sil_cleaned
         Call CloseGP(infile3)
      Endif

    ! record 8: open water data
      If (ncow .gt. 0) then
!record 8 open water rechtstreeks uit ASCII
         Call Openfl (infile1, ConfFil_get_namFil(49),1,1) !openwate.3b
         Call Openfl (infile2, ConfFil_get_namFil(50),1,1) !openwate.sep
         Call Openfl (infile3, ConfFil_get_namFil(51),1,1) !openwate.tbl
         call Openwater_readAsciiInput (infile1, infile2, infile3)
         Call CloseGP(infile1)
         Call CloseGP(infile2)
         Call CloseGP(infile3)
      elseif (CleanRRFiles) then      ! also clean when no RR open water nodes are used
         FileName = ConfFil_get_namFil(49)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,2)  !openwate.3b_cleaned
         Call CloseGP(infile1)
         FileName = ConfFil_get_namFil(50)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile2, FileName, 1,2)  !openwate.sep_cleaned
         Call CloseGP(infile2)
         FileName = ConfFil_get_namFil(51)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile3, FileName, 1,2)  !openwate.tbl_cleaned
         Call CloseGP(infile3)
      Endif
    ! open water precipitation only
      If (ncowrain .gt. 0) then
         Call Openfl (infile1, ConfFil_get_namFil(49),1,1) !openwate.3b
         call OpenwaterPrecip_readAsciiInput (infile1)
         Call CloseGP(infile1)
      elseif (CleanRRFiles) then      ! also clean when no RR open precipitation is used, cleaned file may already exist (append!)
         FileName = ConfFil_get_namFil(49)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,3)  !openwate.3b_cleaned
         Call CloseGP(infile1)
      Endif

    ! record 9: structure data
      call Structures_confAr1
      If (ncstru .gt. 0) then
! record 9 (structures) rechtstreeks uit ASCII
         Call Openfl (infile1, ConfFil_get_namFil(52),1,1) !struct3b.dat
         Call Openfl (infile2, ConfFil_get_namFil(53),1,1) !struct3b.def
         Call Openfl (infile3, ConfFil_get_namFil(54),1,1) !contr3b.def
         Call Openfl (infile4, ConfFil_get_namFil(55),1,1) !struct3b.tbl
         call Structures_readAsciiInput (infile1, infile2, infile3, infile4)
         Call CloseGP(infile1)
         Call CloseGP(infile2)
         Call CloseGP(infile3)
         Call CloseGP(infile4)
      elseif (CleanRRFiles) then      ! also clean when no RR structure nodes are used
         FileName = ConfFil_get_namFil(52)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,2)  !struct3b.dat_cleaned
         Call CloseGP(infile1)
         FileName = ConfFil_get_namFil(53)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile2, FileName, 1,2)  !struct3b.def_cleaned
         Call CloseGP(infile2)
         FileName = ConfFil_get_namFil(54)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile3, FileName, 1,2)  !contr3b.def_cleaned
         Call CloseGP(infile3)
         FileName = ConfFil_get_namFil(55)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile4, FileName, 1,2)  !struct3b.tbl_cleaned
         Call CloseGP(infile4)
      Endif

    ! record 10: boundary data
!     Hier lezen van de ASCII file 3B_Nod.TP om de RR-CF connections te vinden en te onderscheiden van de gewone kale RR-boundaries

    if (ini_switch) then
!       tmpfile = "tmp_ini.ini"
        call RRConnections_ini
    else
      Call Openfl (infile1, ConfFil_get_namFil(2),1,1)  !file met knopen in schematisatie
      Call RRConnections (infile1)
      Call CloseGP (infile1)
    endif


      If (ncboun .gt. 0) then
          if (idebug .ne. 0) write(IdebugLunRR,*) ' RDBINF: dll_mode', dll_mode

          if (dll_mode) then
             ! Read Boundaries for ec_module
             boundCondFile = ConfFil_get_namFil(122)
             if (boundCondFile == ' ') then
                boundCondFile = 'BoundaryConditions.bc'
             endif
             If (CleanRRFiles) then
                Call Openfl (infile1, boundCondFile,1,1) !boundaryconditions.bc file
                call CleanBoundaryConditionsbcFile (infile1)
                Call CloseGP(infile1)
             Endif

!GP          ! removed; if dll_mode, boundaryconditions.bc file should be present and read, otherwise error
!                old code did not switch back to old bound3b.3b file, but did not read anything and used 0 values as boundary)
!GP          ! TEMPORARY: Switch back to old Boundary if BC-File not available
!GP          inquire(file=boundCondFile, exist=bnd_Bc_File_present)
!GP          if (bnd_Bc_File_present) then
                call readBoundaryConditionsInto_ec(boundCondFile)
!GP          else
                ! This lets the old boundary being used.
!GP             dll_mode = .false.
!GP          endif
         else
             !record 10 (boundaries) rechtstreeks uit ASCII
             Call Openfl (infile1, ConfFil_get_namFil(56),1,1) !bound3b.dat
             Call Openfl (infile2, ConfFil_get_namFil(57),1,1) !bound3b.tbl
             call Boundary_readAsciiInput (infile1, infile2)
             Call CloseGP(infile1)
             Call CloseGP(infile2)
         endif
      elseif (CleanRRFiles) then      ! also clean when no RR boundary nodes are used
         FileName = ConfFil_get_namFil(56)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,2)  !bound3b.dat_cleaned
         Call CloseGP(infile1)
         FileName = ConfFil_get_namFil(57)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile2, FileName, 1,2)  !bound3b.tbl_cleaned
         Call CloseGP(infile2)
         ! pm boundaryconditions file
         !FileName = ConfFil_get_namFil(122)
         !FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         !Call Openfl (infile1, FileName, 1,2)  !boundarycondition.bc_cleaned
         !Call CloseGP(infile1)
      Endif
!
      if (idebug .ne. 0) then
        Write(Idebug,*) 'RRCFConnections'
        Do teller=1,ncboun
           Write(idebug,*) teller, bndnam(teller), RRCFConnect(bndnam(teller))
        Enddo
      Endif

!     idebug = 0
!

    ! record 11: NWRW or Pluvius inloop data
    ! record 12: general NWRW or Pluvius inloop data
    ! internal aggregation data
      call NWRW_confAr1
      If (ncPluv .gt. 0) then
          Call Openfl (infile1, ConfFil_get_namFil(10),1,1)  !Pluvius.3b
          Call Openfl (infile2, ConfFil_get_namFil(11),1,1)  !Pluvius.alg
          Call Openfl (infile3, ConfFil_get_namFil(9),1,1)   !Pluvius.dwa
          Call Openfl (infile4, ConfFil_get_namFil(106),1,1) !Pluvius.tbl
          if (ParseTokenMethod) then
            call NWRW2_readAscii (infile1, infile2, infile3, infile4)
          else
            call NWRW_readAscii (infile1, infile2, infile3, infile4)
          endif
          Call CloseGP(infile1)
          Call CloseGP(infile2)
          Call CloseGP(infile3)
          Call CloseGP(infile4)
      elseif (CleanRRFiles) then      ! also clean when no NWRW nodes are used
         FileName = ConfFil_get_namFil(10)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,2)  !pluvius.3b_cleaned
         Call CloseGP(infile1)
         FileName = ConfFil_get_namFil(11)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile2, FileName, 1,2)  !pluvius.alg_cleaned
         Call CloseGP(infile2)
         FileName = ConfFil_get_namFil(9)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile3, FileName, 1,2)  !pluvius.dwa_cleaned
         Call CloseGP(infile3)
         FileName = ConfFil_get_namFil(106)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile4, FileName, 1,2)  !pluvius.tbl_cleaned
         Call CloseGP(infile4)
      endif

!August 1998: Add records RWZI and Industry
    ! record 13: WWTP/RWZI data
    ! no data needed
      call Rwzi_confAr1
      If (ncrwzi .gt. 0) then
         Call Openfl (infile1, ConfFil_get_namFil(59),1,1)   !rwzi.dat
         Call Openfl (infile2, ConfFil_get_namFil(60),1,1)   !rwzi.tbl
         call RWZI_readAscii (infile1, infile2)
         Call CloseGP(infile1)
         Call CloseGP(infile2)
      elseif (CleanRRFiles) then      ! also clean when no RWZI/WWTP nodes are used
         FileName = ConfFil_get_namFil(59)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,2)  !rwzi.dat_cleaned
         Call CloseGP(infile1)
         FileName = ConfFil_get_namFil(60)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile2, FileName, 1,2)  !rwzi.tbl_cleaned
         Call CloseGP(infile2)
      Endif

    ! record 14: Industry
      call Industry_confAr1
      If (ncindus .gt. 0) then
         Call Openfl (infile1, ConfFil_get_namFil(61),1,1)  !industry.3b
         Call Openfl (infile2, ConfFil_get_namFil(77),1,1)  !industry.tbl
         call Industry_readAsciiInput (infile1, infile2)
         Call CloseGP(infile1)
         Call CloseGP(infile2)
      elseif (CleanRRFiles) then      ! also clean when no Industry nodes are used
         FileName = ConfFil_get_namFil(61)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,2)  !industry.3b_cleaned
         Call CloseGP(infile1)
         FileName = ConfFil_get_namFil(77)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile2, FileName, 1,2)  !industry.tbl_cleaned
         Call CloseGP(infile2)
      Endif

    ! nodetype 16: Sacramento
      call Sacramento_confAr1
      if (ncSacr .gt. 0) then
         Call Openfl (infile1, ConfFil_get_namFil(44),1,1) !Sacramento.3b
!        Call Openfl (infile2, ConfFil_get_namFil(79),1,1) !Sacramento.cap
!        Call Openfl (infile3, ConfFil_get_namFil(80),1,1) !Sacramento.uh
!        Call Openfl (infile4, ConfFil_get_namFil(88),1,1) !Sacramento.oth
         call Sacramento_readAscii (infile1)
         Call CloseGP(infile1)
!        close(infile2)
!        close(infile3)
!        close(infile4)
      elseif (CleanRRFiles) then      ! also clean when no Sacramento nodes are used
         FileName = ConfFil_get_namFil(44)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,2)  !sacrmnto.3b_cleaned
         Call CloseGP(infile1)
      Endif
      call Sacramento_confAr3

    ! nodetype 17: Cell
!       call Cel_confAr1
!       if (ncCell .gt. 0) then
!          Call Openfl (infile1, ConfFil_get_namFil(118),1,1) !3b_cel.3b
!          call Cel_readAscii (infile1)
!          close(infile1)
!       Endif
!       call Cel_confAr3

    ! nodetype 18: External Runoff
    ! nodetype 19: HBV
    ! nodetype 20: SCS
    ! nodetype 22: LGSI
    ! nodetype 23: Wagmod (Wageningen model) or Walrus (successor WagMod), depending on switch in INI file
    ! nodetype 31: NAM
    ! note: NcRRRunoff = NcRRRunoffExternal + NCRRRunoffHBV + NcRRRunoffSCS + NcRRRunoffNAM + NcRRRunoffLGSI + NcRRRunoffWagmod + NcRRRunoffWalrus
      call RRRunoffNode_confAr1
      if (ncRRRunoff .gt. 0) then
         Call Openfl (infile1, ConfFil_get_namFil(44),1,1) ! all data is added to the file Sacramento.3b
         call RRRunoffNode_readAscii (infile1)
         Call CloseGP(infile1)
      elseif (CleanRRFiles) then      ! also clean when no RR_Runoff nodes are used (note: file also used for Sacramento; open with append option)
         FileName = ConfFil_get_namFil(44)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,3)  !sacrmnto.3b_cleaned
         Call CloseGP(infile1)
      Endif
      call RRRunoffNode_confAr3


    ! nodetype 30: RRConnection
      call RRConnection_ConfAr1

    ! nodetype 32: RRBifurcation
      call RRBifurcation_ConfAr1
      If (ncBifur .gt. 0) then
         Call Openfl (infile1, ConfFil_get_namFil(44),1,1)  !link file, met optioneel bifurcation information
         call RRBifurcation_ReadAsciiInput (infile1)
         Call CloseGP(infile1)
      elseif (CleanRRFiles) then      ! also clean when no Bifurcations nodes are used (note: file also used for Sacrmamento nodes, open in append mode
         FileName = ConfFil_get_namFil(44)
         FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
         Call Openfl (infile1, FileName, 1,3)  !sacrmnto.3b_cleaned
         Call CloseGP(infile1)
      Endif

    ! record 15: meteostation data
!     write(*,*) ' Read Meteo data '
      call Meteo_readBinInput (RRRunoff_CompOption, NRRRunoff, MeteoNetcdfInput)

    ! eerst moeten Meteodata aanwezig zijn voordat aggplv aangeroepen kan worden
      iCall = 1
      call aggPlv(iCall) ! set NCPLV2
      call NWRW_confAr3
      iCall = 2
      call aggPlv(iCall) ! set INDIKP

    ! kan pas na record 16 !!!!!
      iCall = 2

!     eerst netwerk consistentie checken,
!     anders zou InitSl eruit kunnen klappen bij foute schematisaties
!    (bv WWTP zonder benedenstroomse tak)
      Call CheckNetwork
      call Salt_initSl(iCall)     ! set INDSLT


!     write(*,*) ' Initializing '

! Zet voor open waters, het nivo van het laagste aangelegen maaiveld.
! In geval van Scurve onverhard gebied, per onverhard gebied kijken naar het hoogste maaiveld en daarvan dus het minimum nemen
! Betekent dat al eerder delen van gebieden onder kunnen lopen, maar dat wordt in CmpOvh afgehandeld.
! Het maximale open water peil dat getoond kan worden is gedefinieerd als
!  OWMNMA = het minimum van (de bij het open water aangelegen maaivelden) + 0.5 meter
   DO INODE=1,NCNODE
       IKIND = EiNode(INODE,3)
       INR   = EiNode(INODE,2)
       IOW   = EIOW  (INODE)
       if (IOW .gt. 0) then
          IF (IKIND .EQ. 1) THEN
             OWMNMA(IOW) = MIN (OWMNMA(IOW), LVLVH(INR))
          ELSEIF (IKIND .EQ. 2) THEN
             OWMNMA(IOW) = MIN (OWMNMA(IOW), real(LVLOHMx(INR)))
          ENDIF
       endif
    ENDDO

!  OWMNMA = het minimum van (de bij het open water aangelegen maaivelden) + 0.5 meter
!  op verzoek ARS 8584 / 8603 de beperking 0.5 verwijderd (vervangen door 999.5);
    DO IOW  =1,NCOW
       OWMNMA(IOW) = OWMNMA(IOW) + 999.50  ! was + 0.5
       IF (IDEBUG /= 0) WRITE(IDEBUG,*) ' IOW OWMNMA',IOW,OWMNMA(IOW)
    ENDDO

! dimensioneer RSLMAP

    call confar2

    iDebug = IdebugLunRR
    if (idebug .ne. 0) then
      Do teller=1,ncnode
         if (RRCFConnect(teller)) then
           Write(idebug,*) ' online connected', teller, ' id ', Id_Nod(teller)
         endif
      Enddo
    endif

  RETURN
  END SUBROUTINE RDBINF



  subroutine convertExtern2Intern

    ! variables
    Integer i, iNr, iKind, teller, iExt, Iout1

    ! body

    Iout1 = ConfFil_get_iOut1()
    iKind = 0
    iNr = 0

    do i = 1, NNOD
      ! einode(i, 1) en einode(i, 3) zijn al gezet
      call ConfArr_set_EINODE(i,2, 0)
    end do

    do i = 1, NNOD
       call ConfArr_set_EIOW(I, 0)
       call ConfArr_set_EIBND(I, 0)
       call ConfArr_set_EIRWZI(I, 0)
       call ConfArr_set_UPNODE(I, 0)
       call ConfArr_set_DONODE(I, 0)
    end do

 ! Conversie extern naar intern nr. in array IINODE
    DO I=1,NCNODE
       IEXT = EiNode(I,1)
       IF (IEXT .GT. 2*NNOD) THEN
         call SetMessage(LEVEL_FATAL, 'Dimension array IINODE too small. Use smaller nodenumbers or ask for an new executable')
       ENDIF
       IINODE (IEXT) = I
    ENDDO

    NCOW   = 0
    NCOWRain = 0
    NCVHG  = 0
    NCOVHG = 0
    NCKAS  = 0
    NCSTRU = 0
    NCBOUN = 0
    NCPLUV = 0
 !   NCMET = 0
    NcRWZI  = 0
    NcIndus = 0
    NcSacr  = 0
    NcCell  = 0
    NcRRRunoff = 0
    NcRRRunoffExternal = 0
    NcRRRunoffHBV      = 0
    NcRRRunoffSCS      = 0
    NcRRRunoffNAM      = 0
    NcRRRunoffLGSI     = 0
    NcRRRunoffWagMod   = 0
    NcRRRunoffWalrus   = 0
    NcConn  = 0
    NcBifur = 0

    do teller = 1, NCNODE
    ! Aanpassing structures: nu nog allemaal type 5
      IF (EiNode(teller, 3) .ge. 8 .and. EiNode(teller, 3) .le. 13) then
        call ConfArr_set_EINODE (teller, 3, 5)
      endif
    ! IKDNTR netter knooptype; wordt hier nog niet gebruikt

      select case (EiNode(teller, 3))
        case (1)
          NCVHG  = NCVHG + 1
          call ConfArr_set_EINODE(teller, 2, NCVHG)
        case (2)
          NcOvhg = ncOvhg + 1
          call ConfArr_set_EINODE(teller,2, NCOVHG)
        case (3)
          ncKas = ncKas + 1
          call ConfArr_set_EINODE(teller,2, NCKAS)
        case (4)
          ncOW = ncOW + 1
          call ConfArr_set_EINODE(teller,2, NCOW)
        case (5)
          ncStru = ncStru + 1
          call ConfArr_set_EINODE(teller,2, NCSTRU)
        case (6)
          ncBoun = ncBoun + 1
          call ConfArr_set_EINODE(teller,2, NCBOUN)
        case (7)
          NCPLUV = NCPLUV + 1
          call ConfArr_set_EINODE(teller,2, NCPLUV)
        case (8:12)
          ncStru = ncStru + 1
          call ConfArr_set_EINODE(teller,2, NCSTRU)
!      toe te voegen case 13 (Culvert) is ook een structure
        case (14)
          ncRwzi = ncRwzi + 1
          call ConfArr_set_EINODE(teller,2, NCRWZI)
        case (15)
          ncIndus = ncIndus + 1
          call ConfArr_set_EINODE(teller,2, NCIndus)
!      Sacramento
        case (16)
          ncSacr = ncSacr + 1
          call ConfArr_set_EINODE(teller,2, NCSacr)
!      Cel
        case (17)
!         ncCell = ncCell + 1
!         call ConfArr_set_EINODE(teller,2, NcCell)
!      RRRunoff
        case (18)
          ncRRRunoff = ncRRRunoff + 1
          ncRRRunoffExternal = ncRRRunoffExternal + 1
          call ConfArr_set_EINODE(teller,2, NCRRRunoff)
        case (19)
          ncRRRunoff = ncRRRunoff + 1
          ncRRRunoffHBV = ncRRRunoffHBV + 1
          call ConfArr_set_EINODE(teller,2, NCRRRunoff)
        case (20)
          ncRRRunoff = ncRRRunoff + 1
          ncRRRunoffSCS = ncRRRunoffSCS + 1
          call ConfArr_set_EINODE(teller,2, NCRRRunoff)
        case (21)
          ncOWRain = ncOWRain + 1
          call ConfArr_set_EINODE(teller,2, NCOWRain)
        case (22)
          ncRRRunoff = ncRRRunoff + 1
          ncRRRunoffLGSI = ncRRRunoffLGSI + 1
          call ConfArr_set_EINODE(teller,2, NCRRRunoff)
        case (23)
          if (useWalrus) then
            ncRRRunoff       = ncRRRunoff + 1
            ncRRRunoffWalrus = ncRRRunoffWalrus + 1
          else
            ncRRRunoff = ncRRRunoff + 1
            ncRRRunoffWagmod = ncRRRunoffWagmod + 1
          endif
          call ConfArr_set_EINODE(teller,2, NCRRRunoff)
!      Connection node
        case (30)
          ncConn = ncConn + 1
          call ConfArr_set_EINODE(teller,2, NCConn)
        case (31)
          ncRRRunoff = ncRRRunoff + 1
          ncRRRunoffNAM = ncRRRunoffNAM + 1
          call ConfArr_set_EINODE(teller,2, NCRRRunoff)
        case (32)
          ncBifur = ncBifur + 1
          call ConfArr_set_EINODE(teller,2, NCBifur)
      end select

    end do

!  vector initialisation
   OWDOKW    = .FALSE.
   SIMSEQ    = 0

!  make NcConn and NcBifur equal to sum of both
   NcConn  = NcConn + NcBifur
   NcBifur = NcConn
!

  end subroutine convertExtern2Intern
  !############################################################################

  subroutine RRConnections_ini
    type(t_network), target :: network2
    integer :: i, ndscount, tp
    Character(CharIdLength) ::  id

    ndscount = network2%nds%count

    if (ndscount > 0) then
        do i = 1, ndscount
            id = network2%nds%node(i)%id
            tp = network2%nds%node(i)%nodetype
            if (tp .eq. 6) then
                RRCFConnect(i)= .false.
            else
                RRCFConnect(i) = .true.
            endif
        end do

    endif

    return
  end subroutine RRConnections_ini


  subroutine RRConnections (infile1)


! Reads Network from ASCII files: RR-CF connections
! infile1 = 3B_Nod.Tp file (knopen)

    Integer :: RetVal

    Integer(4)     infile1
    Integer        teller, nhlp, iecode, iout1
    Character(CharIdLength)  id
    Character(1000) string
    Logical         allow, found, endfil

    Parameter     (NHLP=5)
    Integer       IDUM(NHLP)
    Real          RDUM(NHLP)

    allow = .false.
    found = .false.
    iOut1 = ConfFil_get_iOut1()
! Read 3B_Nod.Tp file
    endfil= .false.
    RetVal = 0
    do while (.not. endfil)
         CALL SKPCOM (INfile1, ENDFIL,'ODS')
         IF (ENDFIL) GOTO 21
         READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword (NODE)
         If (STRING(1:4) .eq. 'NODE')  then

! Node id/name
           RetVal = RetVal + GetVAR2 (STRING,' id ',1,' RRConnections',' 3B_Nod.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           Call FndNod(teller,id)

! Node type and object type: om onderscheid te kunnen maken tussen RR-CF connections en kale RR boundaries
           RetVal = RetVal + GetVAR2 (STRING,' mt 1 ',1,' RRConnections',' 3B_Nod.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           Read(id,*) idum(1)

           if (idum(1) .eq. 6) then
             RetVal = RetVal + GetVAR2 (STRING,' ObID ',1,' RRConnections',' 3B_Nod.TP file',IOUT1, &
                           ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             if (id(1:11) .eq. '3B_BOUNDARY' .or. id(1:11) .eq. '3b_boundary') then
                 RRCFConnect(teller) = .false.
             else
                 RRCFConnect(teller) = .true.
             endif
           endif
         endif
    enddo


 150 CONTINUE
     call SetMessage(LEVEL_FATAL, 'Read error in RRConnections')
  21 Continue

     If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading RRConnections', ' Error getting NODE records')

  return

  end subroutine RRConnections



  subroutine combine_files(infile_1, infile_2)
    character(1000), intent(in)  :: infile_1, infile_2
    type(t_network), target :: network2, network3
    integer :: i, ndscount, ndscount_nwrw, brscount
    logical :: check


    check = .false.
    call NetworkReader(network2, trim(infile_1), check)
    call NetworkReader(network3, trim(infile_2), check)

    ndscount = network2%nds%count
    ndscount_nwrw = network3%nds%count

! GP TO CHECK (change FWilms) fixed unit 61 could be dangerous, maybe linked to other file already?
    OPEN(61,file="tmp_nodes.ini",action='write',status='replace')
    do i = 1, ndscount
        write(61,*) "[NODE]"
        write(61,*) "id = ", network2%nds%node(i)%id
        write(61,*) "name = ", network2%nds%node(i)%name
        write(61,*) "type = ", network2%nds%node(i)%nodetype
        write(61,*) "x = ", network2%nds%node(i)%x
        write(61,*) "y = ", network2%nds%node(i)%y
    end do

     do i = 1, ndscount_nwrw
        write(61,*) "[NODE]"
        write(61,*) "id = ", network3%nds%node(i)%id
        write(61,*) "name = ", network3%nds%node(i)%name
        write(61,*) "type = ", network3%nds%node(i)%nodetype
        write(61,*) "x = ", network3%nds%node(i)%x
        write(61,*) "y = ", network3%nds%node(i)%y
     end do

     brscount = network2%brs%count

     do i = 1, brscount
         write(61,*) "[BRANCH]"
         write(61,*) "id = ", network2%brs%branch(i)%id
         write(61,*) "name = ", network2%brs%branch(i)%name
         write(61,*) "brtype = ", network2%brs%branch(i)%brtype
         write(61,*) "fromNode = ", network2%brs%branch(i)%fromnode2
         write(61,*) "toNode = ",  network2%brs%branch(i)%tonode2
     end do

     Call CloseGP(61)

  end subroutine combine_files


  subroutine readnetwork_ini(infile_1, infile_2)
    Integer :: RetVal
    character(1000), intent(in)  :: infile_1, infile_2         ! input
    type(t_network), target :: network2
    integer :: i, ndscount, tp
    Character(1000) :: tmpfile
    Character(CharIdLength) ::  id,nm
    real :: x,y
    logical :: check

    check = .true.


    tmpfile = 'tmp_nodes.ini'

    call NetworkReader(network2, tmpfile, check)


    retval = 0
    ndscount = network2%nds%count

    if (ndscount > 0) then
        do i = 1, ndscount

            id = network2%nds%node(i)%id
            nm = network2%nds%node(i)%name
            tp = network2%nds%node(i)%nodetype
            x = network2%nds%node(i)%x
            y = network2%nds%node(i)%y

            call ConfArr_set_idNode(i,id)
            call ConfArr_set_nameNode(i,nm)
            call ConfArr_set_EINODE(i, 1, i)
            call ConfArr_set_EINODE(i, 3, tp)
            XCoor(i) = x
            Ycoor(i) = y
        end do
    endif
 end subroutine readnetwork_ini



  subroutine ReadNetwork(infile1, infile2)

! Reads Network from ASCII files
! infile1 = 3B_Nod.Tp file (knopen)
! infile2 = 3BRunoff.Tp file (NWRW inloopknopen)
! infile3 = 3B_Link.Tp file (takken)  in aparte routine, nadat hashing arrays van nodes zijn gevuld

    Integer :: RetVal

    Integer(4)     infile1, infile2
    Integer        teller, nhlp, iecode, iout1
    Character(CharIdLength)  id
    Character(1000) string
    Logical         allow, found, endfil

    Parameter     (NHLP=5)
    Integer       IDUM(NHLP)
    Real          RDUM(NHLP)
!    Character(32) CDUM(NHLP)

    allow = .false.
    found = .false.
    iOut1 = ConfFil_get_iOut1()

! Read 3B_Nod.Tp file
    endfil= .false.
    teller = 0
    RetVal = 0
    do while (.not. endfil)
         CALL SKPCOM (INfile1, ENDFIL,'ODS')
         IF (ENDFIL) GOTO 21
         READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword (NODE)
         If (STRING(1:4) .eq. 'NODE')  then
           teller = teller + 1
! Node id/name
           RetVal = RetVal + GetVAR2 (STRING,' id ',1,' ReadNetwork',' 3B_Nod.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IFlRtn)

           call ConfArr_set_idNode(teller, id)
!          call ConfArr_set_nameNode(teller, id)
! ARS xxxx 13June 2002: get name if it is there, otherwise use id (as set before)
           allow = .true.
           RetVal = RetVal + GetVAR2 (STRING,' nm ',1,' ReadNetwork',' 3B_Nod.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IFlRtn)
           if (found) call ConfArr_set_nameNode(teller, id)
! Node type
           allow = .false.
           RetVal = RetVal + GetVAR2 (STRING,' mt 1 ',1,' ReadNetwork',' 3B_Nod.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           Read(id,*) idum(1)
           call ConfArr_set_EINODE(teller, 1, teller)
           call ConfArr_set_EINODE(teller, 3, idum(1))
! Object type alleen voor boundaries, om onderscheid te kunnen maken tussen RR-CF connections en kale RR boundaries
! Altijd via routine RRConnections, later; want RRCFConnect is nog niet gealloceerd
!           if (idum(1) .eq. 6) then
!             RetVal = RetVal + GetVAR (STRING,' ObID ',1,' ReadNetwork',' 3B_Nod.TP file',IOUT1, ID, RDUM(1), IDUM(1), ALLOW, FOUND)
!             if (id(1:11) .eq. '3B_BOUNDARY' .or. id(1:11) .eq. '3b_boundary') then
!                 RRCFConnect(teller) = .false.
!             else
!                 RRCFConnect(teller) = .true.
!             endif
!           endif
! X coordinate, Y coordinate
           allow = .false.
           RetVal = RetVal + GetVAR2 (STRING,' px ',2,' ReadNetwork',' 3B_Nod.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           XCoor(teller) = rdum(1)
           allow = .false.
           RetVal = RetVal + GetVAR2 (STRING,' py ',2,' ReadNetwork',' 3B_Nod.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           YCoor(teller) = rdum(1)

!          write(*,*) ' 3B_Nod.tp file', teller, idum(1)
         endif
    end do
21  continue
    If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading 3B_Nod.tp file ', ' Error getting NODE records')

!3BRunoff.tp file
    endfil = .false.
    RetVal = 0
! teller moet doortellen voor gecombineerde polder/NWRW schematisaties!!
!!  teller = 0
    do while (.not. endfil)
         CALL SKPCOM (INfile2, ENDFIL,'ODS')
         IF (ENDFIL) GOTO 211
         READ(Infile2,'(A1000)',END=211,ERR=150,IOSTAT=IECODE)  STRING
         if (STRING(1:4) .eq. 'NODE')  then
! Node id/name; omdat alleen NWRW nodes kun je gewoon bij teller beginnen te vullen
           teller = teller + 1
           RetVal = RetVal + GetVAR2 (STRING,' id ',1,' ReadNetwork',' 3B_Runoff.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           call ConfArr_set_idNode(teller, id)
!          call ConfArr_set_nameNode(teller, id)
           allow = .true.
           RetVal = RetVal + GetVAR2 (STRING,' nm ',1,' ReadNetwork',' 3B_Runoff.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           if (found) call ConfArr_set_nameNode(teller, id)
           allow = .false.
! Node type
           RetVal = RetVal + GetVAR2 (STRING,' mt 1 ',1,' ReadNetwork',' 3B_Runoff.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           Read(id,*) idum(1)
           call ConfArr_set_EINODE(teller, 1, teller)
           call ConfArr_set_EINODE(teller, 3, idum(1))
! X coordinate, Y coordinate
           allow = .false.
           RetVal = RetVal + GetVAR2 (STRING,' px ',2,' ReadNetwork',' 3B_Nod.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           XCoor(teller) = rdum(1)
           allow = .false.
           RetVal = RetVal + GetVAR2 (STRING,' py ',2,' ReadNetwork',' 3B_Nod.TP file',IOUT1, &
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           YCoor(teller) = rdum(1)

!          write(*,*) ' 3B_Runoff.tp file', teller, idum(1)
         endif
    end do
211 CONTINUE


     GOTO 999

 150 CONTINUE
     call SetMessage(LEVEL_FATAL, 'Read error in ReadNetwork')
 999 CONTINUE

    If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading 3BRunoff.tp file ', ' Error getting NODE records')

  end subroutine ReadNetwork







  subroutine ReadNrNodes_ini(infile_1,infile_2,ndscount)
    character(1000), intent(in)  :: infile_1, infile_2            ! input
    integer, intent(out) :: ndscount      ! output
    type(t_network), target :: network2
    integer :: i, tp
    character(1000) :: tmpfile
    logical :: check

    check = .true.
    !infile = "D:\PROJECTS\Rekenhart_hydrologie\e112_dflowfm-drr\f01_1d_rr\c04_multiple_catchments\rr\3B_topology.ini"
    call combine_files(infile_1, infile_2)
    tmpfile = 'tmp_nodes.ini'
    call NetworkReader(network2, tmpfile, check)


    ndscount = network2%nds%count


    if (ndscount > 0) then
        do i = 1, ndscount
            tp = network2%nds%node(i)%nodetype
            select case (tp)
                case (1)
                    NCVHG  = NCVHG + 1
                case (2)
                    NcOvhg = ncOvhg + 1
                case (3)
                    ncKas = ncKas + 1
                case (4)
                    ncOW = ncOW + 1
                case (5)
                    ncStru = ncStru + 1
                case (6)
                    ncBoun = ncBoun + 1
                case (7)
                    NCPLUV = NCPLUV + 1
                case (8:12)
                    ncStru = ncStru + 1
                case (14)
                    ncRwzi = ncRwzi + 1
                case (15)
                    ncIndus = ncIndus + 1
                case (16)
                    ncSacr  = ncSacr  + 1
                case (17)
                !            ncCell  = ncCell  + 1
                case (18)
                    ncRRRunoff    = ncRRRunoff + 1
                    ncRRRunoffExternal = ncRRRunoffExternal + 1
                case (19)
                    ncRRRunoff    = ncRRRunoff + 1
                    ncRRRunoffHBV = ncRRRunoffHBV+ 1
                case (20)
                    ncRRRunoff    = ncRRRunoff + 1
                    ncRRRunoffScs = ncRRRunoffSCS+ 1
                case (21)
                    ncOwRain      = ncOwRain + 1
                case (22)
                    ncRRRunoff     = ncRRRunoff + 1
                    ncRRRunoffLGSI = ncRRRunoffLGSI+ 1
                case (23)
                    if (useWalrus) then
                        ncRRRunoff       = ncRRRunoff + 1
                        ncRRRunoffWalrus = ncRRRunoffWalrus + 1
                    else
                        ncRRRunoff       = ncRRRunoff + 1
                        ncRRRunoffWagmod = ncRRRunoffWagmod + 1
                    endif
                case (30)
                    ncConn  = ncConn  + 1
                case (31)
                    ncRRRunoff    = ncRRRunoff + 1
                    ncRRRunoffNAM = ncRRRunoffNAM+ 1
                case (32)
                    ncBifur  = ncBifur  + 1
            end select
        enddo
    endif

    nclink = network2%brs%count

    ncNode = max (ncNode, ncVhg + ncOvhg + ncKas + ncOw + ncOwRain + ncStru + ncBoun + &
                    ncPluv + NcRwzi + NcIndus + NcSacr + NcCell + NcRRRunoff + NcConn + NcBifur)

    !  make NcConn and NcBifur equal to sum of both
    NcConn  = NcConn + NcBifur
    NcBifur = NcConn





  end subroutine ReadNrNodes_ini

  subroutine ReadNrnodes(infile1, infile2, infile3)

! Determines Number of nodes and branches from ASCII files
! infile1 = 3B_Nod.Tp file (knopen)
! infile2 = 3BRunoff.Tp file (NWRW inloopknopen)
! infile3 = 3B_Link.Tp file (takken)

    Integer :: RetVal

    Integer(4)     infile1, infile2, infile3
    integer        nhlp, iecode, iout1, idebug
    Character(CharIdLength)  id
    Character(1000) string
    Logical         allow, found, endfil

    Parameter     (NHLP=5)
    Integer       IDUM(NHLP)
    Real          RDUM(NHLP)
!    Character(32) CDUM(NHLP)

    allow = .false.
    found = .false.
    iOut1 = ConfFil_get_iOut1()
    iDebug = ConfFil_get_iDebug()

! Read 3B_Nod.Tp file
    endfil = .false.
    RetVal = 0
    if (idebug .ne. 0) write(idebug,*) ' typeread   ncvhg  ncovhg  nckas  ncow  ncboun  ncpluv  ',&
                                                    ' ncstru  ncrwzi  ncindus  ncsacr NcRRRunoff ncOwRain NcConn'
    do while (.not. endfil)
       CALL SKPCOM (INfile1, ENDFIL,'ODS')
       IF (ENDFIL) GOTO 21
       READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword (NODE)
       IF (STRING(1:4) .EQ. 'NODE')  THEN
! Node type
         RetVal = RetVal + GetVAR2 (STRING,' mt 1 ',1,' ReadNrNodes',' 3B_Nod.TP file',IOUT1, &
                       ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
         Read(id,*) idum(1)
         select case (IDUM(1))
           case (1)
             NCVHG  = NCVHG + 1
           case (2)
             NcOvhg = ncOvhg + 1
           case (3)
             ncKas = ncKas + 1
           case (4)
             ncOW = ncOW + 1
           case (5)
             ncStru = ncStru + 1
           case (6)
             ncBoun = ncBoun + 1
           case (7)
             NCPLUV = NCPLUV + 1
           case (8:12)
             ncStru = ncStru + 1
!          toe te voegen case 13 (Culvert) is ook een structure
           case (14)
             ncRwzi = ncRwzi + 1
           case (15)
             ncIndus = ncIndus + 1
           case (16)
             ncSacr  = ncSacr  + 1
           case (17)
!            ncCell  = ncCell  + 1
           case (18)
             ncRRRunoff    = ncRRRunoff + 1
             ncRRRunoffExternal = ncRRRunoffExternal + 1
           case (19)
             ncRRRunoff    = ncRRRunoff + 1
             ncRRRunoffHBV = ncRRRunoffHBV+ 1
           case (20)
             ncRRRunoff    = ncRRRunoff + 1
             ncRRRunoffScs = ncRRRunoffSCS+ 1
           case (21)
             ncOwRain      = ncOwRain + 1
           case (22)
             ncRRRunoff     = ncRRRunoff + 1
             ncRRRunoffLGSI = ncRRRunoffLGSI+ 1
           case (23)
             if (useWalrus) then
               ncRRRunoff       = ncRRRunoff + 1
               ncRRRunoffWalrus = ncRRRunoffWalrus + 1
             else
               ncRRRunoff       = ncRRRunoff + 1
               ncRRRunoffWagmod = ncRRRunoffWagmod + 1
             endif
           case (30)
             ncConn  = ncConn  + 1
           case (31)
             ncRRRunoff    = ncRRRunoff + 1
             ncRRRunoffNAM = ncRRRunoffNAM+ 1
           case (32)
             ncBifur  = ncBifur  + 1
         end select
       endif
!
       if (idebug .ne. 0) write(idebug,'(13I7)') idum(1), ncvhg, ncovhg, nckas, ncow, ncboun, ncpluv, &
                                                 ncstru, ncrwzi, ncindus, ncsacr, NCRRRunoff, NcOwRain, NcConn
    end do
21  CONTINUE

    If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading 3B_NOD.tp file ', ' Error getting NODE records')

!3BRunoff.tp file
    endfil = .false.
    RetVal = 0
    do while (.not. endfil)
         CALL SKPCOM (INfile2, ENDFIL,'ODS')
         IF (ENDFIL) GOTO 211
         READ(Infile2,'(A1000)',END=211,ERR=150,IOSTAT=IECODE)  STRING
         if (STRING(1:4) .eq. 'NODE') then
 ! Node type
           RetVal = RetVal + GetVAR2 (STRING,' mt 1 ',1,' ReadNrNodes',' 3B_Runoff.TP file',IOUT1,&
                         ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           Read(id,*) idum(1)
           If (idum(1) .eq. 7) ncpluv = ncpluv + 1
         endif
    end do
211 CONTINUE

    If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading 3BRunoff.tp file ', ' Error getting NODE records')

!3B_link.tp file
    endfil = .false.
    RetVal = 0
    do while (.not. endfil)
       CALL SKPCOM (INfile3, ENDFIL,'ODS')
       IF (ENDFIL) GOTO 2111
       READ(Infile3,'(A1000)',END=2111,ERR=150,IOSTAT=IECODE)  STRING
       IF (STRING(1:4) .eq. 'BRCH') nclink = nclink + 1
    end do

2111 CONTINUE

     ncNode = max (ncNode, ncVhg + ncOvhg + ncKas + ncOw + ncOwRain + ncStru + ncBoun + &
                           ncPluv + NcRwzi + NcIndus + NcSacr + NcCell + NcRRRunoff + NcConn + NcBifur)

!  make NcConn and NcBifur equal to sum of both
       NcConn  = NcConn + NcBifur
       NcBifur = NcConn
!
     GOTO 999

 150 CONTINUE
     call SetMessage(LEVEL_FATAL, 'Read error in ReadNrNodes')
 999 CONTINUE

    If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading 3BLink.tp file ', ' Error getting BRCH records')

  end subroutine ReadNrNodes









end module BinFile
