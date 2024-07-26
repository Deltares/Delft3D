module m_tables
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!
!    Function: - Various table manipulation routines.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

   use MessageHandling
   use m_alloc

   implicit none

   private

   !
   ! functions and subroutines
   !
   public AddTable
   public setTable
   public deallocTable
   public CombineTables
   public shiftValues
   public interpolate
   public integrate
   public dealloc
   public realloc
   public hasTableData
   public printData

   interface interpolate
      module procedure interpolateTable
      module procedure interpolateArrs
   end interface

   interface ShiftValues
      module procedure ShiftValuesTbl
   end interface

   interface printData
      module procedure printTable
   end interface

   interface realloc
      module procedure reallocTable
      module procedure reallocTableSet
   end interface

   interface dealloc
      module procedure deallocTable
      module procedure deallocTableSet
   end interface dealloc

   type, public :: t_table                                                          !< table definition
      integer                                      :: length   = 0
      double precision, dimension(:), pointer  :: x => null()                       !< x-values of table
      double precision, dimension(:), pointer  :: y => null()                       !< y-values of table
      !> function type for interpolation\n
      !! - div(interpoltype, 10) == 1: periodical function
      !! - mod(interpoltype, 10) == 0: linear function
      !! - mod(interpoltype, 10) == 1: block function
      !! - mod(interpoltype, 10) == 2: linear function in degrees
      integer                                      :: interpoltype = 0
      integer                                      :: stCount = 0                   !< counter for inttab used for speeding up interpolation
   end type t_table

   type, public ::t_tableSet
      integer                                               :: Size = 0
      integer                                               :: growsBy = 2000
      integer                                               :: Count= 0
      type(t_table_pointer), pointer, dimension(:)          :: tb
   end type t_tableSet

   type, public :: t_table_pointer
      type(t_table), pointer :: table
   end type t_table_pointer

contains

   !> creates a table
   integer function AddTable( tbs, interpoltype, x, y, length)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_tableSet)                 :: tbs
      integer                          :: interpoltype
      integer                          :: length
      double precision, dimension(:)   :: x
      double precision, dimension(:)   :: y

      ! local parameters
      type(t_table), pointer           :: table

      ! Program code

      tbs%count = tbs%count+1
      if (tbs%count > tbs%size) then
         call realloc(tbs)
      endif
      allocate(tbs%tb(tbs%count)%table)
      table => tbs%tb(tbs%count)%table
      
      call setTable(table, interpoltype, x, y, length)
      Addtable = tbs%count
   end function AddTable

   subroutine setTable(table, interpoltype, x, y, length)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_table), pointer           :: table
      integer                          :: interpoltype
      integer                          :: length
      double precision, dimension(:)   :: x
      double precision, dimension(:)   :: y

      ! local parameters

      ! Program code
      if (.not. associated(table)) then
         allocate(table)
      else
         ! Table will be replaced
         if (associated(table%x)) deallocate(table%x)
         if (associated(table%y)) deallocate(table%y)
      endif

      table%length      = length
      if (length > 0) then
         table%interpoltype = interpoltype
         allocate(table%x(length))
         allocate(table%y(length))
         table%x     = x(1:length)
         table%y     = y(1:length)
      endif
      
   end subroutine setTable

   !> Shift table value(2) to value(1) and set (x, y) into value(2)
   subroutine ShiftValuesTbl(table, x, y)
      type(t_table)                    :: table
      double precision                 :: x
      double precision                 :: y

      if (table%length /=2 ) then
         ! for connection with openMI and Deltashell this subroutine expects a table of length 2
         deallocate(table%x, table%y)
         allocate(table%x(2), table%y(2))
         table%x(2) = x
         table%y(2) = y
      endif         
      table%x(1) = table%x(2)
      table%y(1) = table%y(2)
      table%x(2) = x
      table%y(2) = y

   end subroutine ShiftValuesTbl

   !> Reallocates a table's internal memory such that it can store a particular table length.
   !!
   !! @See setTable for setting actual new values into this table.
   subroutine reallocTable(table, new_length)

      ! Input/output parameters
      type(t_table), pointer       :: table      !< Table to reallocate; any existing data will be preserved.
      integer,       intent(in   ) :: new_length !< Desired new length of the table arrays. Note that the length attribute will not be changed, as no new values will added yet.

      if (.not. associated(table)) then
         allocate(table)
      end if
      
      call reallocP(table%x, new_length, keepExisting = .true.)
      call reallocP(table%y, new_length, keepExisting = .true.)
      
   end subroutine reallocTable


   subroutine deallocTable(table)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_table), intent(inout),pointer          :: table

      ! Local variables

      ! Program code
      if (associated(table%x)) deallocate(table%x)
      if (associated(table%y)) deallocate(table%y)
      table%length = 0
      deallocate(table)
      
      table => null()

   end subroutine deallocTable

   subroutine reallocTableSet(tbs)
      ! Modules
   
      implicit none
   
      ! Input/output parameters
      type(t_tableSet)           :: tbs
   
      ! Local variables
   
      ! Program code
   
      ! Local variables
      type(t_table_pointer), pointer, dimension(:)         :: oldTable
   
      ! Program code
   
      if (tbs%Size > 0) then
         oldtable=>tbs%tb
      endif
   
      if (tbs%growsBy <=0) then
         tbs%growsBy = 200
      endif
      allocate(tbs%tb(tbs%Size+tbs%growsBy))
   
      if (tbs%size > 0) then
         tbs%tb(1:tbs%size) = oldtable(1:tbs%size)
         deallocate(oldtable)
      endif
      tbs%Size = tbs%Size+tbs%growsBy
   
   end subroutine realloctableSet
   
   subroutine dealloctableSet(tbs)
      ! Modules
   
      implicit none
   
      ! Input/output parameters
      type(t_tableSet)       :: tbs
      ! Local variables
      integer i
   
      ! Program code
      if (tbs%size>0) then
         do i = 1, tbs%count
            call dealloc(tbs%tb(i)%table)
         enddo
   
         deallocate(tbs%tb)
         
         tbs%tb   => null() 
         tbs%size  = 0
         tbs%count = 0
      endif
      
   end subroutine deallocTableSet

   !> Checks whether a table contains any data.
   !! Combines the pointer and length > 0 check for convenience at call site.
   logical function hasTableData(table)
      implicit none

      type(t_table), pointer, intent(in) :: table !< Table to test, may be unassociated.

      if (associated(table)) then
         hasTableData = table%length > 0
      else
         hasTableData = .false.
      end if
   end function hasTableData


   !> Interpolate/look up a value in a table.
   !! Result value is determined by the input value and the table's interpolation type.

   double precision function InterpolateTable(table, xs)
       implicit none
   !
   ! Global variables
   !
       type(t_table),    intent(inout) :: table !< Table containing two-column data. When empty, the result value is undefined.
       double precision, intent(in   ) :: xs    !< Value to look up
   !
   !
   ! Local variables
   !
       integer                        :: i
       integer                        :: len
       integer                        :: ind
       integer                        :: perp1
       integer                        :: perp2
       integer                        :: interpoltype
       integer                        :: sa, si
       logical                        :: period
       double precision               :: bgn
       double precision               :: dmod
       double precision               :: fween
       double precision               :: lt
       double precision               :: rint
       double precision               :: y0
       double precision               :: y1
       double precision               :: ys, yso
   !
   !
   !! executable statements -------------------------------------------------------
   !
       !     Period defined ?
       len = table%length
       if (len <= 0) then
          ! No table contents, return with undefined result value.
          return
       end if
       
       period = table%interpoltype/10==1
       interpoltype = mod(table%interpoltype, 10)
       si = table%stCount
       !     in  lowland and urban periodical functions are not used and therefore
       !     period is made as false
       if (period) then
          if (table%x(2)<table%x(len)) then
             !
             !           Increasing arguments
             perp1 = 2
             perp2 = len
          else
             !
             !           Decreasing arguments
             perp1 = len
             perp2 = 2
          endif
          fween = table%x(perp1)
          bgn = table%x(perp2) - table%x(1)
          if (bgn<fween) then
             bgn = fween
          else
             !           interpolation itself is not periodical
             period = .false.
          endif
          if (xs>table%x(perp2)) then
             !           periodical; x > 0
             lt = dmod(xs - bgn, table%x(1)) + bgn
          elseif ((xs<table%x(perp1)) .and. (bgn<fween + 1.0D-2)) then
             !           periodical; x < 0
             lt = dmod(xs - bgn, table%x(1)) + bgn + table%x(1)
          else
             !           run-in part (also for x < 0)
             lt = xs
          endif
          sa = 2
       else
          lt = xs
          sa = 1
       endif
       !
       !     to be on safe side
       if (si==0) si = sa
       !
       !
       !     For discrete interpolation add small number to find proper
       !     table point if xs=table%x(i)
       !
       if (interpoltype==1) then
          lt = lt*1.0000005d0
       endif
       !
       !     Index table and find X value
       !
       ind = 0
       if (table%x(sa)<table%x(len)) then
          if (lt<table%x(si)) si = sa
          !
          do i = si, len
             if (lt>=table%x(i)) then
                ind = i
             else
                exit
             endif
          enddo
       else
          if (lt<table%x(si)) si = len
          !
          do i = si, sa, -1
             if (lt>=table%x(i)) then
                ind = i
             else
                exit
             endif
          enddo
       endif
       !
       !     Check for continuous or discrete interpolation
       !
       if (interpoltype/=1) then
          !
          !        Interpolate in X using ind and find Y value
          !
          if (table%x(sa)<table%x(len)) then
             if (ind==len) then
                if (period) then
                   rint = (lt - table%x(len))/(table%x(sa) + table%x(1) - table%x(len))
                   yso = table%y(len) + rint*(table%y(sa) - table%y(len))
                   y0 = table%y(len)
                   y1 = table%y(sa)
                   if (interpoltype==2) call regdir(y0, y1)
                   ys = y0 + rint*(y1 - y0)
                else
                   ys = table%y(len)
                endif
             elseif (ind==0) then
                if (period) then
                   rint = (lt - table%x(sa))/(table%x(len) + table%x(1) - table%x(sa))
                   yso = table%y(sa) + rint*(table%y(len) - table%y(sa))
                   y0 = table%y(sa)
                   y1 = table%y(len)
                   if (interpoltype==2) call regdir(y0, y1)
                   ys = y0 + rint*(y1 - y0)
                else
                   ys = table%y(sa)
                endif
             else
                rint = (lt - table%x(ind))/(table%x(ind + 1) - table%x(ind))
                yso = table%y(ind) + rint*(table%y(ind + 1) - table%y(ind))
                   y0 = table%y(ind)
                   y1 = table%y(ind+1)
                   if (interpoltype==2) call regdir(y0, y1)
                   ys = y0 + rint*(y1 - y0)
             endif
          elseif (ind==sa) then
             ys = table%y(sa)
          elseif (ind==0) then
             ys = table%y(len)
          else
             rint = (lt - table%x(ind))/(table%x(ind - 1) - table%x(ind))
             yso = table%y(ind) + rint*(table%y(ind - 1) - table%y(ind))
             y0 = table%y(ind)
             y1 = table%y(ind-1)
             if (interpoltype==2) call regdir(y0, y1)
             ys = y0 + rint*(y1 - y0)
          endif
       !
       !        Find Y value using ind
       !
       elseif (table%x(sa)<table%x(len)) then
          if (ind==0) then
             ys = table%y(sa)
          else
             ys = table%y(ind)
          endif
       elseif (ind==0) then
          ys = table%y(len)
       else
          ys = table%y(ind)
       endif
       !
       !     store counter
       table%stCount = ind
       InterpolateTable = ys
   end function InterpolateTable

   double precision function Integrate(table, xs)
       implicit none
   !
   ! Global variables
   !
       type(t_table)                  :: table
       double precision, intent(in)   :: xs
   !
   !
   ! Local variables
   !
       integer                        :: itel
       integer                        :: interpoltype
       integer                        :: length
       double precision               :: ys, fak
   
       length = table%length
       interpoltype = mod(table%interpoltype, 10)
   
       !
       !
       !     For discrete interpolation add small number to find proper
       !     table point if xs=table%x(i)
       !
       Integrate = 0d0
       if ( xs <= table%x(1) ) then
          Integrate = 0d0
          return
       endif
       itel = 1
       if (mod(table%interpoltype,10)==0) then
          ! interpolate values
          do while (itel < length .and. xs > table%x(min(itel+1, length)) )
             Integrate = Integrate + 0.5d0*(table%y(itel)+table%y(itel+1)) * (table%x(itel+1)-table%x(itel))
             itel = itel+1
          enddo
          if (itel ==length) then
             Integrate = Integrate + table%y(itel) * (xs - table%x(itel))
          else
             fak = (xs - table%x(itel))/(table%x (itel+1)-table%x(itel))
             ys = table%y(itel) + fak*(table%y(itel+1)-table%y(itel))
             Integrate = Integrate + 0.5d0*(table%y(itel)+ys)*(xs - table%x(itel))
          endif
       else
          ! use block function
          itel = 1
          do while (itel < length .and. xs > table%x(min(itel+1, length)) )
             Integrate = Integrate + table%y(itel) * (table%x(itel+1)-table%x(itel))
             itel = itel+1
          enddo
          Integrate = Integrate + table%y(itel) * (xs - table%x(itel))
       endif
   
   end function Integrate

   double precision function InterpolateArrs(xarr, yarr, len, xs)
       implicit none
   !
   ! Global variables
   !
       integer, intent(in)                           :: len
       double precision, dimension(len), intent(in)  :: xarr
       double precision, dimension(len), intent(in)  :: yarr
       double precision, intent(in)                  :: xs
   !
   ! Local variables
   !
       integer                        :: i
       integer                        :: ind
       integer                        :: interpoltype
       integer                        :: sa, si
       logical                        :: period
       double precision               :: lt
       double precision               :: rint
       double precision               :: y0
       double precision               :: y1
       double precision               :: ys, yso
   !
   !! executable statements -------------------------------------------------------
   !
       !     Period defined ?
       period = .true.
       interpoltype = 0
       !     in  lowland and urban periodical functions are not used and therefore
       !     period is made as false
       lt = xs
       sa = 1
       !
       !     to be on safe side
       si = sa
       !
       !
       !     For discrete interpolation add small number to find proper
       !     table point if xs=table%x(i)
       !
       if (interpoltype==1) then
          lt = lt*1.0000005
       endif
       !
       !     Index table and find X value
       !
       ind = 0
       if (xarr(sa)<xarr(len)) then
          if (lt<xarr(si)) si = sa
          !
          do i = si, len
             if (lt>=xarr(i)) then
                ind = i
             else
                exit
             endif
          enddo
       else
          if (lt<xarr(si)) si = len
          !
          do i = si, sa, -1
             if (lt>=xarr(i)) then
                ind = i
             else
                exit
             endif
          enddo
       endif
       !
       !     Check for continuous or discrete interpolation
       !
       if (interpoltype/=1) then
          !
          !        Interpolate in X using ind and find Y value
          !
          if (xarr(sa)<xarr(len)) then
             if (ind==len) then
                if (period) then
                   rint = (lt - xarr(len))/(xarr(sa) + xarr(1) - xarr(len))
                   yso = yarr(len) + rint*(yarr(sa) - yarr(len))
                   y0 = yarr(len)
                   y1 = yarr(sa)
                   if (interpoltype==2) call regdir(y0, y1)
                   ys = y0 + rint*(y1 - y0)
                else
                   ys = yarr(len)
                endif
             elseif (ind==0) then
                if (period) then
                   rint = (lt - xarr(sa))/(xarr(len) + xarr(1) - xarr(sa))
                   yso = yarr(sa) + rint*(yarr(len) - yarr(sa))
                   y0 = yarr(sa)
                   y1 = yarr(len)
                   if (interpoltype==2) call regdir(y0, y1)
                   ys = y0 + rint*(y1 - y0)
                else
                   ys = yarr(sa)
                endif
             else
                rint = (lt - xarr(ind))/(xarr(ind + 1) - xarr(ind))
                yso = yarr(ind) + rint*(yarr(ind + 1) - yarr(ind))
                   y0 = yarr(ind)
                   y1 = yarr(ind+1)
                   if (interpoltype==2) call regdir(y0, y1)
                   ys = y0 + rint*(y1 - y0)
             endif
          elseif (ind==sa) then
             ys = yarr(sa)
          elseif (ind==0) then
             ys = yarr(len)
          else
             rint = (lt - xarr(ind))/(xarr(ind - 1) - xarr(ind))
             yso = yarr(ind) + rint*(yarr(ind - 1) - yarr(ind))
             y0 = yarr(ind)
             y1 = yarr(ind-1)
             if (interpoltype==2) call regdir(y0, y1)
             ys = y0 + rint*(y1 - y0)
          endif
       !
       !        Find Y value using ind
       !
       elseif (xarr(sa)<xarr(len)) then
          if (ind==0) then
             ys = yarr(sa)
          else
             ys = yarr(ind)
          endif
       elseif (ind==0) then
          ys = yarr(len)
       else
          ys = yarr(ind)
       endif
       !
       !     store counter
       InterpolateArrs = ys
   end function InterpolateArrs

   subroutine regdir(w0,w1)
      !
      ! angle regularisation
      !
      implicit none
      double precision :: w0
      double precision :: w1
      if      ( (w1 - w0) > 180.0d0) then
         w0 = w0 + 360.0d0
      else if ( (w0 - w1) > 180.0d0) then
         w1 = w1 + 360.0d0
      endif
   
   end subroutine regdir

   subroutine printTable(table, unit)
      type(t_table)                  :: table
      integer unit
   
      integer i
   
      write(unit, '(''   TABLE: length     = '', i5)') table%length
      write(unit, '(''   TABLE: intpolType = '', i5)') table%interpoltype
      write(unit, '(''   TABLE: stCount    = '', i5)') table%stCount
   
      do i = 1, table%length
         write(unit, '(''   TABLE: x('',i5,'')= '', g14.4, ''      y('',i5,'')= '', g14.4)') i, table%x(i), i, table%y(i)
      enddo
   end subroutine printTable

   !> Add table to tableSet TABLES by interpolating table1 and table2 \n
   !! Limitation: x-values of table1 and table2 must be identical \n
   !! Interpolation method: yi = deltaR*table1%yi + (1-deltaR)*table2%yi
   integer function CombineTables(tables, table1, table2, deltaR)
      ! Modules
      use MessageHandling
   
      implicit none
   
      ! Input/output parameters
      type(t_tableSet), intent(inout)           :: tables
      type(t_table), intent(in)                 :: table1
      type(t_table), intent(in)                 :: table2
      double precision                          :: deltaR
   
      ! local parameters
      double precision                          :: deltaL
      integer     :: index
      integer     :: i
   
      ! Program code
   
      ! check input on restraints
      if (size(table1%x) /=   size(table2%x) ) then
         call SetMessage(LEVEL_FATAL, 'INTERNAL ERROR: Two tables of different length must be &
               interpolated. This is not supported')
      else
         do i = 1, size(table1%x)
            if (abs(table1%x(i)-table2%x(i)) > 1e-3) then
               call SetMessage(LEVEL_FATAL, 'INTERNAL ERROR: The x-values of the tables that must &
               be interpolated are not identical')
            endif
         enddo
      endif
   
      index = addTable(tables, table1%interpolType, table1%x, table1%y, size(table1%x))
   
      deltaL = 1d0 - deltaR
      do i = 1, size(table1%x)
         tables%tb(index)%table%y(i) = deltaR*table1%y(i) + deltaL*table2%y(i)
      enddo
      CombineTables = index
   
   end function CombineTables

end module m_tables
