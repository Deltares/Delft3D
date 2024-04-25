module m_compound
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
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
!  
!  
!-------------------------------------------------------------------------------

   use MessageHandling
   use m_GlobalParameters
   use m_alloc
   use m_branch
   use m_tables
   use m_CrossSections
   use m_Culvert
   use m_pump
   use m_General_Structure
   use m_Universal_Weir
   use m_Bridge
   use m_hash_search
   use m_Dambreak
   use iso_c_utils

   implicit none

   private

   public realloc
   public dealloc

   public initialize_compounds
   public computeCompound

   interface realloc
      module procedure reallocCompound
   end interface

   interface dealloc
      module procedure deallocCompound
   end interface dealloc

   type, public :: t_compound
      character(IdLen)                   :: id                    !< Id of the compound structure.
      character(IdLen)                   :: name                  !< Name of the compound structure.
      integer                            :: numstructs            !< Number of the structure elements in the compound.
      integer, dimension(:), pointer     :: structure_indices => null() !< Indices of the structure elements.
      integer                            :: numlinks              !< Number of links .
      integer, dimension(:), pointer     :: linknumbers => null() !< Link numbers.
      character(len=IdLen), dimension(:), pointer :: structureIds => null() !< ids of the structure elements
   end type t_compound

   type, public :: t_compoundSet
      integer                                               :: Size     = 0
      integer                                               :: growsBy = 2000
      integer                                               :: Count    = 0
     type(t_compound), pointer, dimension(:)                :: compound => null()
   end type t_compoundSet

   contains

   subroutine deallocCompound(cmps)
   ! Modules

   implicit none

   ! Input/output parameters
   type(t_compoundSet), intent(inout)          :: cmps

   ! Local variables
   integer  :: length, i

   ! Program code
   if (associated(cmps%compound)) then
      length = cmps%size
      do i = 1, length
         if (associated(cmps%compound(i)%structure_indices)) deallocate(cmps%compound(i)%structure_indices)
         if (associated(cmps%compound(i)%linknumbers))       deallocate(cmps%compound(i)%linknumbers)
         cmps%compound(i)%structure_indices => null()
         cmps%compound(i)%linknumbers       => null()
      enddo
      deallocate(cmps%compound)
   endif
   
   cmps%compound => null()
   cmps%size  = 0
   cmps%count = 0

end subroutine

   subroutine reallocCompound(cmps)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_compoundSet), intent(inout)          :: cmps

      ! Local variables
      type(t_compound), pointer, dimension(:)      :: oldcmps

      ! Program code

      if (cmps%Size > 0) then
         oldcmps=>cmps%compound
      endif

      if (cmps%growsBy <=0) then
         cmps%growsBy = 200
      endif
      allocate(cmps%compound(cmps%Size+cmps%growsBy))

      if (cmps%Size > 0) then
         cmps%compound(1:cmps%Size) = oldcmps(1:cmps%Size)
         deallocate(oldcmps)
      endif
      cmps%Size = cmps%Size+cmps%growsBy
   end subroutine


   !> Initialize compound structures. Set the link numbers and check if all structures links
   !! are consistent.
   function initialize_compounds(cmps, sts) result(istat)
      use m_1d_structures
      
      type(t_structureSet), intent(in   )   :: sts    !< Structure set
      type (t_compoundSet), intent(inout)   :: cmps   !< Compounds set
      integer                               :: istat  !< error status
      
      integer :: i, j
      integer :: L0
      integer :: istru
      integer :: numlinks
      
      istat = 0
      
      do i = 1, cmps%count
         istru = cmps%compound(i)%structure_indices(1)
         numlinks = sts%struct(istru)%numlinks
         if (numlinks > 0) then
            allocate(cmps%compound(i)%linknumbers(numlinks))
            cmps%compound(i)%linknumbers = sts%struct(istru)%linknumbers
         end if
         cmps%compound(i)%numlinks = numlinks
         ! now check if other members contain the same links
         do j = 2, cmps%compound(i)%numstructs
            istru = cmps%compound(i)%structure_indices(j)
            if (cmps%compound(i)%numlinks /= sts%struct(istru)%numlinks) then
               msgbuf = 'Error in compound ''' // trim(cmps%compound(i)%id) //''' the number of links in structure element ''' // &
                  trim(sts%struct(istru)%id) //''' is different from the first structure element.'
               call err_flush()
               istat = 1
               cycle
            endif
            do L0 = 1, numlinks
               if (cmps%compound(i)%linknumbers(L0) /= sts%struct(istru)%linknumbers(L0)) then
                  msgbuf = 'Error in compound ''' // trim(cmps%compound(i)%id) //''' the link numbers in structure element ''' // &
                     trim(sts%struct(istru)%id) //''' is inconsistent with the first structure element.'
                  call err_flush()
                  istat = 1
                  cycle
               endif
            enddo
         enddo
      enddo
      
   end function initialize_compounds
  
   !> Compute FU, RU and AU for compound at internal linknumber L0
   subroutine computeCompound(compound, struct, L0, u0, teta, fu, ru, au)
      use m_1d_structures
      
      type(t_compound),                intent(in   )  :: compound  !< Compound structure object.
      type(t_structure), dimension(:), intent(in   )  :: struct    !< Array containing structures.
      integer,                         intent(in   )  :: L0        !< Internal link number.
      double precision,                intent(in   )  :: u0        !< Flow velocity at previous time step.
      double precision,                intent(in   )  :: teta      !< Teta at flow link.
      double precision,                intent(  out)  :: fu        !< FU coefficient.
      double precision,                intent(  out)  :: ru        !< RU coefficient.
      double precision,                intent(  out)  :: au        !< Flow area.
      
      integer :: istru, i
      
      fu = 0d0
      ru = 0d0 
      au = 0d0
      
      ! The system of equations must be identical when the compound structure is taken as one structure or when the individual structures
      ! are computed
      ! In S1INI the matrix elements are set for the momentum equation.
      !
      ! * AU = sum_for_all_structures_in_compound(au_stru)
      !
      ! * FU: for the compound structure bb = bb + teta * Au * fu
      !       for individual structures  bb = bb + teta * sum_for_all_structures_in_compound(au_stru*fu_stru)   
      !   FU =  sum_for_all_structures_in_compound(au_stru*fu_stru)/AU
      !
      ! * RU: for the compound structure dd = dd + teta * Au * ru + (1-teta)*au*u0
      !       for individual structures  dd = dd + sum_for_all_structures_in_compound(teta * Au_stru * ru_stru + (1-teta)*au_stru*u0_stru)
      !   RU = sum_for_all_structures_in_compound(teta * Au_stru * ru_stru + (1-teta)*au_stru*u0_stru)/AU - (1-teta)*u0 /
      do i = 1, compound%numstructs
         istru = compound%structure_indices(i)
         fu = fu + struct(istru)%fu(L0)*struct(istru)%au(L0) 
         ru = ru + struct(istru)%au(L0) * (teta*struct(istru)%ru(L0)+ (1d0-teta)*struct(istru)%u0(L0))
         au = au + struct(istru)%au(L0) 
      enddo
      if (au > 0d0) then
         fu = fu/au
         ru = ru/(au*teta) - (1d0-teta)*u0/teta
      endif
      
   end subroutine computeCompound
end module m_compound
