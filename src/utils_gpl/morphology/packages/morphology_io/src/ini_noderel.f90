module m_ini_noderel
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!!--description-----------------------------------------------------------------
private
public ini_noderel
public get_noderel_idx
public clr_noderel

contains

subroutine ini_noderel(nrd, sedpar, lsedtot)

!
!    Function: - Read and Initialize NodeRelation Data
!

   use precision
   use morphology_data_module, only : CHARLEN, t_nodereldata, t_nodefraction, t_noderelation, sedpar_type
   use tree_data_types
   use properties
   use string_module, only:str_lower
   use messageHandling
    
   implicit none
   
   ! Global variables
   
   type(t_nodereldata)                    :: nrd
   type(sedpar_type)                      :: sedpar
   integer                                :: lsedtot

   ! Local variables

   type(t_nodefraction), pointer          :: pFrac
   type(t_noderelation),pointer           :: pNodRel
   type(tree_data), pointer               :: nrd_ptr => null()
   type(tree_data), pointer               :: block_ptr
   character(len=CHARLEN)                 :: block_name
   
   integer                                :: iFrac
   integer                                :: inod
   Integer                                :: icount
   integer                                :: istat
   character(256)                         :: fileName
   logical                                :: errFound

   ! executable statements -------------------------------------------------------

   call GetAndCheckFileNames(nrd, sedpar, lsedtot)
   
   allocate(nrd%nodefractions(nrd%nFractions), stat=istat)
   if (istat /= 0) then 
      call SetMessage(LEVEL_FATAL, 'Memory allocation error 1 in INI_NODEREL')
   endif
   
   do iFrac = 1, nrd%nFractions
   
      fileName = nrd%flnrd(iFrac)
      
      if (nrd%NRD_Overall) then
         nrd%nodefractions(1)%Name = 'Overall'
      else
         nrd%nodefractions(iFrac)%Name = sedpar%namsed(iFrac)
      endif

      pFrac => nrd%nodefractions(iFrac)
      
      if (.not. nrd%NRD_Default) then   
      
         ! Read File
         call tree_create("NodeRelation", nrd_ptr)
         call prop_file('ini', trim(fileName), nrd_ptr, istat)
         if (istat /= 0) then
            call SetMessage(LEVEL_FATAL, 'Error Opening/Reading File: '//trim(fileName))
         endif
      
         if (.not. associated(nrd_ptr%child_nodes) ) then
            call SetMessage(LEVEL_FATAL, 'No Proper Data Found in File: '//trim(fileName))
         endif

         ! Count and Allocate Nodal Point Relations
         do inod= 1, size(nrd_ptr%child_nodes)
            block_ptr => nrd_ptr%child_nodes(inod)%node_ptr
            block_name = tree_get_name(block_ptr)
            call str_lower(block_name)
            if (block_name == 'nodalpointrelation') then
               pFrac%nNodeRelations = pFrac%nNodeRelations + 1
            endif
         enddo
      
         if (pFrac%nNodeRelations == 0) then
            call SetMessage(LEVEL_FATAL, 'No Nodal Point Relations Found in File: '//trim(fileName))
         endif
      
         allocate(pFrac%noderelations(0:pFrac%nNodeRelations), stat=istat)
         if (istat /= 0) then 
            call SetMessage(LEVEL_FATAL, 'Memory allocation error 2 in INI_NODEREL')
         endif
         
         ! Set Defaults at Location 0
         pNodRel => pFrac%noderelations(0)
         pNodRel%Node = 'Default'
         pNodRel%Method = 'function'
         pNodRel%expQ = 1.0_fp
         pNodRel%expw = 0.0_fp
         
         ! Get Data from File
         
         ! First Get Name of Table File, so it is available when needed
         do inod = 1, size(nrd_ptr%child_nodes)
            block_ptr => nrd_ptr%child_nodes(inod)%node_ptr
            block_name = tree_get_name(block_ptr )
            call str_lower(block_name)
            pFrac%tableFile = ' '
            if (trim(block_name) == 'general') then
               call prop_get(block_ptr, '*', 'TableFile', pFrac%tableFile)
               if (pFrac%tableFile .ne. ' ') then
                  call combinepaths(fileName, pFrac%tableFile)
               endif
               exit
            endif
         enddo            
                    
         icount = 0
         do inod = 1, size(nrd_ptr%child_nodes)

            block_ptr => nrd_ptr%child_nodes(inod)%node_ptr
            block_name = tree_get_name(block_ptr )
            call str_lower(block_name)

            if (trim(block_name) == 'nodalpointrelation') then
         
               icount = icount + 1
               pNodRel => pFrac%noderelations(icount)
            
               call prop_get(block_ptr, '*', 'Node', pNodRel%Node)
               if (pNodRel%Node .eq. ' ') then
                  call SetMessage(LEVEL_FATAL, 'No Node Specified for Nodal Point Relation in File: '//trim(fileName))
               endif
               
               call prop_get(block_ptr, '*', 'Method', pNodRel%Method)
               if (pNodRel%Method .eq. ' ') then
                  call SetMessage(LEVEL_FATAL, 'No Method Specified for Nodal Point Relation in File: '//trim(fileName))
               endif
               call str_lower(pNodRel%Method)
               
               if (pNodRel%Method == 'table') then

                  call prop_get(block_ptr, '*', 'BranchIn', pNodRel%BranchIn)
                  if (pNodRel%BranchIn .eq. ' ') then
                     call SetMessage(LEVEL_FATAL, 'No Incoming Branch Specified for Nodal Point Relation in File: '//trim(fileName))
                  endif

                  call prop_get(block_ptr, '*', 'BranchOut1', pNodRel%Branchout1)
                  if (pNodRel%BranchOut1 .eq. ' ') then
                     call SetMessage(LEVEL_FATAL, 'No First Outgoing Branch Specified for Nodal Point Relation in File: '//trim(fileName))
                  endif

                  call prop_get(block_ptr, '*', 'BranchOut2', pNodRel%Branchout2)
                  if (pNodRel%BranchOut2 .eq. ' ') then
                     call SetMessage(LEVEL_FATAL, 'No Second Outgoing Branch Specified for Nodal Point Relation in File: '//trim(fileName))
                  endif

                  call prop_get(block_ptr, '*', 'Table', pNodRel%tableName)
                  if (pNodRel%tableName == ' ') then
                     call SetMessage(LEVEL_FATAL, 'No Table Specified for Table Method in File: '//trim(fileName))
                  endif
                  call str_lower(pNodRel%tableName)
                  call GetTableData(pNodRel, pFrac)      ! Read the Table
                  
               elseif (pNodRel%Method == 'function') then
                  call prop_get(block_ptr, '*', 'k', pNodRel%expQ)
                  call prop_get(block_ptr, '*', 'm', pNodRel%expW)
               else
                 call SetMessage(LEVEL_FATAL, 'Unknown Method Specified in File: '//trim(fileName))
               endif
               
            elseif (trim(block_name) == 'general') then
               continue     ! Already Done Above
            else
               call SetMessage(LEVEL_WARN, 'Unknown Data Block in File: '//trim(fileName))
            endif
                     
         enddo

         block_ptr => null()
         call tree_destroy(nrd_ptr)

      else
   
         ! Set Values for Default
         allocate(pFrac%noderelations(0:1), stat=istat)
         if (istat /= 0) then 
            call SetMessage(LEVEL_FATAL, 'Memory allocation error 3 in INI_NODEREL')
         endif
      
         do icount = 0, 1
            pNodRel => pFrac%noderelations(icount)
            pNodRel%Method = 'function'
      
            pNodRel%expQ = 1.0_fp
            pNodRel%expw = 0.0_fp
         enddo
         
         call SetMessage(LEVEL_WARN, 'Exponent of Discharge Ratio Set to Default: k = 1')
         call SetMessage(LEVEL_WARN, 'Exponent of Width Ratio Set to Default    : m = 0')
   
      endif
   
   enddo

   errFound = .false.
   do iFrac = 1, nrd%nFractions
   
      if (CheckNodeRelations(nrd%nodefractions(iFrac))) then
         errFound = .true.
      endif
      
   enddo

   if (errFound) then
      call SetMessage(LEVEL_FATAL, 'Double/Redundant Data Found')
   endif
   
end subroutine ini_noderel

integer function get_noderel_idx(iNod, pFrac, nodeIDIdx, branInIDLn, nodbrt)
!
!    Function: - Get the Nodal Point Relation for the Current Node/Branch
!                If nothing Found return 0 (zero which means default)

   use morphology_data_module, only : CHARLEN, t_nodefraction, t_noderelation
   use messageHandling
   
   ! Global variables
   integer                                :: iNod     !< Index of Actual Node
   type(t_nodefraction)                   :: pFrac
   integer                                :: nodeIDIdx
   integer                                :: branInIDLn
   integer                                :: nodbrt

   ! Local variables
   integer                                :: iNodeRel
   logical                                :: getFunctionRelation
   type(t_noderelation),pointer           :: pNodRel

   ! executable statements -------------------------------------------------------

   get_noderel_idx = 0
   getFunctionRelation = .true.
   
   
   if (branInIDLn .ne. 0 .and. branInIDLn .ne. -444 .and. nodbrt == 3) then
      
      ! Only One Incoming Branch at a Real Bifurcation
      do iNodeRel = 1, pFrac%nNodeRelations
      
         pNodRel => pFrac%noderelations(iNodeRel)
         
         if (pNodRel%NodeIdx == nodeIDIdx .and. pNodRel%BranchInLn == branInIDLn) then
         
            ! Found/Bingo
            get_noderel_idx = iNodeRel
            getFunctionRelation = .false.
            exit
         endif
         
      enddo
      
   endif
   
   if (getFunctionRelation) then
   
      do iNodeRel = 1, pFrac%nNodeRelations
      
         pNodRel => pFrac%noderelations(iNodeRel)
         
         if (pNodRel%NodeIdx == nodeIDIdx .and. pNodRel%BranchInLn == 0) then
         
            ! Found/Bingo
            get_noderel_idx = iNodeRel

         endif
         
      enddo

   endif
   
end function get_noderel_idx

subroutine clr_noderel(istat, nrd)

!
!    Function - Clear NodeRelation Data 
!

   use morphology_data_module, only : t_nodereldata, t_nodefraction, t_noderelation ! CHARLEN, , sedpar_type
   use properties
    
   implicit none

   ! Global variables
   
   type(t_nodereldata)                    , intent(inout)   :: nrd
   integer                                , intent(out)   :: istat
   
   ! Local variables

   
!
!! executable statements -------------------------------------------------------
!
   
   if (associated(nrd%nodefractions))            deallocate(nrd%nodefractions   , STAT = istat)    ! do we need to traverse further down the tree?
   if (istat==0 .and. associated(nrd%flnrd))     deallocate(nrd%flnrd           , STAT = istat)   

end subroutine clr_noderel   

subroutine GetAndCheckFileNames(nrd, sedpar, lsedtot)

   ! Function: - Get And Check File Names

   use morphology_data_module, only: t_nodereldata, sedpar_type
   use messageHandling
    
   implicit none
   
   type(sedpar_type)                      :: sedpar
   type(t_nodereldata)                    :: nrd
   
   integer                                :: lsedtot
   integer                                :: ised
   integer                                :: ifrac
   integer                                :: istat
   logical                                :: fileExists
   logical                                :: errFound
   integer                                :: iFile
   logical                                :: noFiles
   

   ! executable statements -------------------------------------------------------
   
   ! Check if not any File has been specified, in that case default Proportional
   noFiles = .true.
   do iFile = 0, lsedtot
      if (sedpar%flnrd(iFile) .ne. ' ') then
         noFiles = .false.
         exit
      endif
   enddo
   
   if (noFiles) then
      call SetMessage(LEVEL_WARN, 'No Nodal Point Relations Specified, Set to (Default) Proportional.')
      nrd%NRD_Overall = .true.
      nrd%NRD_Default = .true.
   else
      nrd%NRD_Default = .false.
   endif
    
   ! Check Consistency of Specified Files
   if (.not. noFiles) then
      if (sedpar%flnrd(0) .ne. ' ') then
         ! Overall Specified
         do ised = 1, lsedtot
           if (sedpar%flnrd(ised) .ne. ' ') then
              call SetMessage(LEVEL_FATAL, 'When Overall Nodal Point Relations specified it is not allowed to specify Node Relations for Fractions')
           endif
         enddo
         nrd%NRD_Overall = .true.
      else
         ! Overall NOT Specified
         do ised = 1, lsedtot
           if (sedpar%flnrd(ised) .eq. ' ') then
              call SetMessage(LEVEL_FATAL, 'Nodal Point Relations not Specified for each Fraction')
           endif
         enddo
         nrd%NRD_Overall = .false.
      endif
   endif
  
   if (nrd%NRD_Overall) then
     nrd%nFractions = 1
   else
     nrd%nFractions = lsedtot
   endif
 
   ! Allocate and Copy (Already Checked) File Names
   allocate(nrd%flnrd(nrd%nFractions), stat=istat)
   if (istat /= 0) then 
      call SetMessage(LEVEL_FATAL, 'Memory allocation error in INI_NODEREL-GetAndCheckFileNames')
   endif

   if (nrd%NRD_Overall) then
      nrd%flnrd(1) = sedpar%flnrd(0)
   else
      do ifrac = 1, nrd%nFractions
         nrd%flnrd(ifrac) = sedpar%flnrd(ifrac)
      enddo
   endif
   
   ! Check If File(s) Exist When Specified
   if (.not. noFiles) then
      errFound = .false.
      do ifrac = 1, nrd%nFractions
         inquire (file = nrd%flnrd(ifrac), exist = fileExists)
         if (.not. fileExists) then
            call SetMessage(LEVEL_ERROR, 'Nodal Point Relation Data File not Found: '//trim(nrd%flnrd(ifrac)))
            errFound = .true.
         endif
      enddo

      if (errFound) then
         call SetMessage(LEVEL_FATAL, 'Nodal Point Relation Data File(s) not Found')
      endif
   endif

end subroutine GetAndCheckFileNames


subroutine GetTableData(nodRel, nodFrac)

   ! Function: - Get the Table Data from the Specified Table File

   use precision
   use morphology_data_module, only : CHARLEN, t_nodefraction, t_noderelation
   use string_module, only:str_lower
   use m_tables
   use messageHandling
   
   ! Global variables

   type(t_noderelation)                   :: nodRel
   type(t_nodefraction)                   :: nodFrac

   ! Local variables
   character(256)                         :: tableFileName
   character(len=CHARLEN)                 :: tableName
   logical                                :: fileExists
   logical                                :: tableFound = .false.
   integer                                :: istat
   integer                                :: fileUnit
   integer                                :: nRows
   integer                                :: nCols
   integer                                :: iRow
   real(hp), allocatable, dimension(:)    :: x
   real(hp), allocatable, dimension(:)    :: y
   
   tableFileName = nodFrac%tableFile

   if (tableFileName == ' ') then
      call SetMessage(LEVEL_FATAL, 'No Table File Specified for Fraction: '//trim(nodFrac%Name))
   endif
   
   ! Check if File Exists
   inquire (file = tableFileName , exist = fileExists)
   if (.not. fileExists) then
      call SetMessage(LEVEL_FATAL, 'Table File not Found: '//trim(tableFileName))
   endif
   
   ! Read File
   open(newunit=fileUnit, file=tableFileName, status='old', action='READ', iostat=istat)
   if (istat /= 0) then
      call SetMessage(LEVEL_FATAL, 'Error Openening Table File: '//trim(tableFileName))
   endif
   
   rewind(fileUnit)

   read(fileUnit, '(a)', iostat=istat) tableName
   if (istat /= 0) then
      call SetMessage(LEVEL_FATAL, 'Error Reading Table File: '//trim(tableFileName))
   endif
   
   do while (istat == 0)
      
      call str_lower(tableName)

      if (tableName == nodRel%tableName) then
         
         tableFound = .true.
         
         ! Get Table Size
         read(fileUnit, *, iostat=istat) nRows, nCols
         if (istat > 0) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Table Size from Table '''//trim(nodRel%tableName)//'''')
         endif
         
         if (nRows < 2) then
            call SetMessage(LEVEL_FATAL, 'Less Than 2 Rows Specified in Table '''//trim(nodRel%tableName)//'''')
         endif
         
         if (nCols > 2) then
            call SetMessage(LEVEL_WARN, 'More Than 2 Columns Specified in Table '''//trim(nodRel%tableName)//''' Will Be Ignored')
            nCols = 2
         endif

         allocate(x(nRows), stat=istat)
         if (istat == 0) allocate(y(nRows), stat=istat)
         if (istat /= 0) then 
            call SetMessage(LEVEL_FATAL, 'Memory allocation error in INI_NODEREL-GetTableData')
         endif
         
         ! Get the Table Data
         do iRow = 1, nRows
            read(fileUnit, *, iostat=istat) x(iRow), y(iRow)
            if (istat > 0) then
               call SetMessage(LEVEL_FATAL, 'Error Reading Table Data from Table '''//trim(nodRel%tableName)//'''')
            endif
         enddo
         
         ! Store Table
         call setTable(nodRel%Table, 0, x, y, nRows)
         
         deallocate(x, stat=istat)
         if (istat == 0) deallocate(y, stat=istat)
         if (istat /= 0) then 
            call SetMessage(LEVEL_FATAL, 'Memory deallocation error in INI_NODEREL-GetTableData')
         endif
         
         ! Job Done
         exit
         
      endif
      
      read(fileUnit, '(a)', iostat=istat) tableName
      if (istat > 0) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Table File: '//trim(tableFileName))
      endif
      
   enddo
   
   close(fileUnit,iostat=istat)
   if (istat /= 0) then
      call SetMessage(LEVEL_FATAL, 'Error Closing Table File: '//trim(tableFileName))
   endif
   
   if (.not. tableFound) then
      call SetMessage(LEVEL_FATAL, 'Table '''//trim(nodRel%tableName)//''' Not Found in Table File: '//trim(tableFileName))
   endif
   
end subroutine GetTableData

logical function CheckNodeRelations(pFrac)

   ! Function: - Check data for Double/Redundant Entries

   use morphology_data_module, only : t_nodefraction
   use messageHandling
   
   ! Global variables
   type(t_nodefraction)                   :: pFrac

   ! Local variables
   integer                                :: iNodRel1
   integer                                :: iNodRel2
   
   CheckNodeRelations = .false.
   
   do iNodRel1 = 1, pFrac%nNodeRelations - 1
   
      do iNodRel2 = iNodRel1 + 1, pFrac%nNodeRelations
      
         if (pFrac%noderelations(iNodRel1)%Node == pFrac%noderelations(iNodRel2)%Node .and. &
            pFrac%noderelations(iNodRel1)%BranchIn == pFrac%noderelations(iNodRel2)%BranchIn) then
            
            call SetMessage(LEVEL_WARN, 'Double/Redundant Data for Node: '''//trim(pFrac%noderelations(iNodRel1)%Node)//''' in Fraction: '''//trim(pFrac%name)//'''')
            
            CheckNodeRelations = .true.
    
         endif
         
      enddo
      
   enddo

end function CheckNodeRelations   

end module m_ini_noderel