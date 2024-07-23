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

     module PMInterface

     use ReadLib


    ! variables voor de interface met de Process Manager
      implicit none
    
      Integer       PmcNMsg, RtnCode
      Parameter    (PmcNMsg = 3)
      Logical       UsePm
      Character*20  PmStringHeads, PmStringTimestep, PmStringFluxes
      Character*256 PmRRInport, PmRROutPort
      Character*256 PmcArgs(4)
      Character*256 PmcMsgTable (PmcNMsg)
      Character*256 PmcMsgIden  (PmcNMsg)
      Character*256 PmcRetIden
      Logical       PmSuccess

      Character(20), allocatable :: Pmc_Ids(:)
      REAL, ALLOCATABLE, SAVE    :: Pmc_RealArray(:)
      Integer, ALLOCATABLE, SAVE :: Pmc_IntegerArray(:)



    contains

!   subroutines

      Subroutine PMInterface_Init1
!     Default Initialisations for Process Manager use
      implicit none

      UsePm = .false.
      PmStringHeads    = 'SOBEK-RR.GWLTAB'
      PmStringTimestep = 'GWM.TSL'
      PmStringFluxes   = 'SOBEK-RR.BOTFLUX'
      PmRRInPort       = 'RRPM.IN'
      PmRROutPort      = 'RRPM.OUT'

      return
      end subroutine PMInterface_Init1


      Subroutine PMInterface_Init2
!     Other Initialisations for Process Manager use

      implicit none

      PmcMsgTable (1) = 'PUT * ' // PmStringTimestep(1:Len_trim(PmStringTimestep)) // ' * * * '
      PmcMsgIden  (1) = PmStringTimestep
      PmcMsgTable (2) = 'PUT * ' // PmStringHeads(1:Len_trim(PmStringHeads)) // ' * * * '
      PmcMsgIden  (2) = PmStringHeads
! outgoing message not needed in PmcMsgTable
      PmcMsgTable (3) = 'PUT SOBEK-RR ' // PmStringFluxes(1:Len_trim(PmStringFluxes)) // ' * -|d|mm/d * '
      PmcMsgIden  (3) = PmStringFluxes

      PmcArgs(1) = PmRRInPort
      PmcArgs(2) = PmRROutPort
      PmcArgs(3) = 'ascii'
      PmcArgs(4) = 'Sobek-RR'

      return
      end subroutine PMInterface_Init2


      Subroutine PMInterface_Alloc (Malloc)
!     Allocate PM exchange arrays of dimension MAlloc, and >=1.

      implicit none
      Integer NAlloc, Malloc, Allocation_Error

      NAlloc = max (1, MAlloc)

      Allocate (Pmc_Ids(NAlloc),Stat=Allocation_Error )
      If (Allocation_Error .ne. 0) Then
        call SetMessage(LEVEL_FATAL, 'Error allocating arrays in PMInterface')
      Endif

      Allocate (Pmc_RealArray(NAlloc),Stat=Allocation_Error )
      If (Allocation_Error .ne. 0) Then
        call SetMessage(LEVEL_FATAL, 'Error allocating arrays in PMInterface')
      Endif

      Allocate (Pmc_IntegerArray(NAlloc),Stat=Allocation_Error )
      If (Allocation_Error .ne. 0) Then
        call SetMessage(LEVEL_FATAL, 'Error allocating arrays in PMInterface')
      Endif

      return
      end subroutine PMInterface_Alloc


      Subroutine GetModflowTimestep (ModflowTimestep)

      !Get Modflow Timestep in days
      implicit none
      Real    ModflowTimestep
      Character*1024 Header
      Character*1    Quote
      Quote = ''''

!     Header voor message is opgebouwd uit MSG, Subid, message string
!                                      of  TAB, Subid, data type, afmeting, message string
      Header = 'TAB Sub R 1 ' // quote // PmcMsgTable(1)(1:Len_trim(PmcMsgTable(1))) // quote

      PmSuccess = .false.
      write(*,*) ' Msg1 ', PmcMsgTable(1)(1:100)
      write(*,*) ' Msg2 ', PmcMsgTable(2)(1:100)

!     Do While (.not. PmSuccess)
!       Call Pmc_Msg_Rd (PmcMsgTable, PmcMsgIden, PmcNmsg, PmcRetIden, RtnCode)
        Write(*,*) ' Pmc_Msg_Rd RtnCode', Rtncode
!       If (PmcRetIden(1:Len_trim(PmcRetIden)) .eq. PmStringTimestep(1:Len_trim(PmStringTimestep))) then
!         Write(*,*) ' Found PmcRetIden', PmcRetIden(1:20)
!         Write(*,*) ' Read ModflowTimestep'
!         Call Pmc_Rd_R (Pmc_RealArray,1)
!         Call Pmc_Rls_in
          PmSuccess = .true.
!       else
!         Write(*,*) ' Unknown PmcRetIden', PmcRetIden(1:20)
!         Call Pmc_Rls_In
!         Call GpSleep(10)
!       endif
!     Enddo

!     ModflowTimestep = Pmc_RealArray(1)
      Write(*,*) ' ModflowTimestep = ', ModflowTimestep

      Return
      End Subroutine GetModflowTimestep


      Subroutine GetModflowHeads (ModflowIds, ModflowHeads, NrLocations, Time)

      !Get Modflow Heads
      implicit none
      Integer      NrLocations
      Character*20 ModflowIds(NrLocations)
      Real         ModflowHeads(NrLocations), Time
      Integer      i
      Character*1024 Header
      Character*1    Quote

      Quote = ''''

!     Header voor message is opgebouwd uit MSG, Subid, message string
!                                      of  TAB, Subid, data type, afmeting, message string
      Header = 'TAB Sub CRR 1 '
      Write(Header,'(12X,I6)') NrLocations
      Header(19:19) = quote
      Header(20:) = PmcMsgTable(2)(1:Len_trim(PmcMsgTable(2)))
      i = Len_trim(Header)
      Header(i:i) = quote

      PmSuccess = .false.
      Write(*,*) ' Pmc_Msg_Rd RtnCode', Rtncode
      PmSuccess = .true.

      End Subroutine GetModflowHeads


      Subroutine PutSobekModflowFluxes (ModflowIds, ModflowFluxes, NrLocations, Time)

      !Put Sobek-Modflow Fluxes
      implicit none

      Integer      NrLocations
      Character*20 ModflowIds(NrLocations)
      Real         ModflowFluxes(NrLocations), Time
      Integer      i
      Character*1024 Header
      Character*1    Quote

      Quote = ''''

!     Header voor message is opgebouwd uit MSG, Subid, message string
!                                      of  TAB, Subid, data type, afmeting, message string
      Header = 'TAB Sub CRR 1 '
      Write(Header,'(12X,I6)') NrLocations
      Header(19:19) = quote
      Header(20:) = PmcMsgTable(3)(1:Len_trim(PmcMsgTable(3)))
      i = Len_trim(Header)
      Header(i:i) = quote


          Write(*,*) ' Write ModflowFluxes'

!         Call Pmc_Wr_Hdr(Header)
          Write(*,*) ' Header=',Header

          Write(*,*) ' Tijd ', Time
!         Call Pmc_Wr_C (ModflowIds,NrLocations)
!         Do i=1, NrLocations
!            Pmc_RealArray(i) = Time
!            Write(*,*) ' Location i, id, ModflowHeads = ',i, ModflowIds(i), ModflowFluxes(i)
!         Enddo
!         Call Pmc_Wr_R (Pmc_RealArray,NrLocations)
!         Call Pmc_Wr_R (ModflowFluxes,NrLocations)
!         Call Pmc_Cls_out

      Return
      End Subroutine PutSobekModflowFluxes


      Subroutine PmInterface_DeAllocateArrays

        implicit none

        if (Allocated(Pmc_Ids)) DeAllocate(Pmc_Ids)
        if (Allocated(Pmc_RealArray)) DeAllocate(Pmc_RealArray)
        if (Allocated(Pmc_IntegerArray)) DeAllocate(Pmc_IntegerArray)

      Return
      End subroutine PmInterface_DeallocateArrays


   end Module PMInterface
