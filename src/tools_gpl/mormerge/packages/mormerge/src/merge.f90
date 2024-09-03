subroutine merge (inputfile, workdir, runid)
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
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use tree_structures
    use properties, only: prop_file ! The only is needed to avoid a clash with
                                    ! the local defined cident
    use precision
    use mormerge_version_module
    !
    implicit none
    !
    !include 'sysdef.def'

!
! Local parameters
!
    integer, parameter :: scandim = 1
    integer, parameter :: readinp = 2
!
! Global variables
!
    character(*), intent(in) :: inputfile
    character(*), intent(in) :: workdir
    character(*), intent(in) :: runid
!
! Local variables
!
    integer                                             :: i
    integer                                             :: icolon
    integer                                             :: icond           ! condition counter
    integer                                             :: istat
    integer                                             :: maxconlen
    integer                                             :: ncond           ! number of conditions
    integer                                             :: nmmax           ! number of grid points
    integer                                             :: lsed            ! number of sediment fractions
    integer                                             :: loopcount
    integer                                             :: lundia
    integer                                             :: lunfil
    integer                                             :: scanmode
    integer, external                                   :: createstream
    integer       , dimension(:)  , allocatable         :: handles         ! data stream handles
    real(hp)                                            :: dim_real
    real(hp)                                            :: totalweight
    real(hp)                                            :: timestep
    real(hp)                                            :: timestep_min
    real(hp)      , dimension(:)  , allocatable         :: weight          ! weight of condition
    real(hp)      , dimension(:,:), allocatable         :: dbdsed          ! sediment mass per fraction
    real(hp)      , dimension(:,:), allocatable         :: dbdsed_merged   ! merged sediment mass per fraction
    real(hp)      , dimension(2)                        :: rn
    logical                                             :: debug
    logical                                             :: finished=.false.! logical indicating run has finished
    logical                                             :: success
    character(1)                                        :: slash
    character(10)                                       :: arch
    character(80)                                       :: parname
    character(256), dimension(:)  , allocatable         :: cond            ! condition id (=directory)
    character(256)                                      :: filnam
    character(256)                                      :: value
    character(256)                                      :: mmsyncfilnam
    character(256)                                      :: full_version   ! by calling getfullversionstring_DELFTFLOW, the version number is visible with the what command
    type(tree_data)    , pointer                        :: infile_root
    type(tree_data)    , pointer                        :: infile_ptr
    type(tree_data)    , pointer                        :: node_ptr
!
!! executable statements -------------------------------------------------------
!
   call getfullversionstring_mormerge(full_version)
   !
   call util_getenv('ARCH',value)
   call small(value,1000)
   if (value == 'win32' .or. value == 'w32') then
      arch = 'win32'
      slash = '\'
   elseif (value == 'win64' .or. value == 'x64') then
      arch = 'win64'
      slash = '\'
   else
      arch = 'linux'
      slash = '/'
   endif
   !
   ! Output initialization
   !
   write(filnam,'(3a)') 'mormerge_', trim(runid), '.log'
   open(newunit = lundia, file=trim(filnam), status='replace', action='write')
   write(lundia,'( a)') trim(full_version(5:))
   write(lundia,'( a)') ' '
   write(lundia,'( a)') 'COMMAND LINE ARGUMENTS'
   write(lundia,'(2a)') '   inputfile : ',trim(inputfile)
   write(lundia,'(2a)') '   workdir   : ',trim(workdir)
   write(lundia,'(2a)') '   runid     : ',trim(runid)
   !
   ! Read conditions and weight factors and create streams
   !
   call tree_create('Mormerge', infile_root)
   call tree_create_node( infile_root, 'Mormerge input file', infile_ptr )
   call tree_put_data( infile_ptr, transfer(trim(inputfile),node_value), 'STRING' )
   call prop_file('ini',trim(inputfile),infile_ptr,istat)
   if (istat /= 0) then
      write(*,*)      'ERROR: unable to read file ',trim(inputfile)
      write(lundia,*) 'ERROR: unable to read file ',trim(inputfile)
      close (lundia)
      stop
   endif
   write(lundia,'( a)') 'INPUT ECHO'
   ncond       = 0
   maxconlen   = 0
   totalweight = 0.0_hp
   debug       = .false.
   do scanmode = scandim, readinp
      icond = 0
      do i = 1,size(infile_ptr%child_nodes)
          node_ptr => infile_ptr%child_nodes(i)%node_ptr
          parname = tree_get_name( node_ptr )
          !
          ! Distinguish the cases:
          ! - 'condition:weight'
          ! - 'debug'
          !
          select case ( parname )
          case ('condition:weight')
             icond = icond + 1
             select case (scanmode)
             case (scandim)
                ncond = ncond + 1
             case (readinp)
                call tree_get_data_string(node_ptr,value,success)
                icolon = index(value,':')
                cond(icond) = trim(value(:icolon-1))
                maxconlen = max(maxconlen , len_trim(cond(icond)))
                read(value(icolon+1:),*) weight(icond)
                totalweight = totalweight + weight(icond)
             endselect
          case ('debug')
                if (scanmode == scandim) then
                   call tree_get_data_string(node_ptr,value,success)
                   if ( index(value,'1') >= 1) then
                      debug = .true.
                      write(lundia,'(a)') '   Debug flag switched on'
                   endif
                endif
          case default
             !
             ! Skip all other lines
             !
          endselect
       enddo
       select case (scanmode)
       case (scandim)
          write(lundia,'(a,i4,a)') '   Number of conditions : ',ncond,' :'
          allocate(cond   (ncond))
          allocate(weight (ncond))
          allocate(handles(ncond))
       case (readinp)
          !
          ! ready
          !
       endselect
   enddo
   !
   do icond = 1 , ncond
      weight(icond) = weight(icond) / totalweight
      write(lundia,'(6x,a,6x,f10.5)') cond(icond)(1:maxconlen), weight(icond)
   enddo
   if (debug) write(lundia,'(a,f10.5)') '   Totalweight : ',totalweight
   !
   ! Synchronisation
   !
   write(mmsyncfilnam,'(6a)') trim(workdir), slash, 'sync', slash, &
                               & 'merge', trim(runid)
   !write(*,*)'mmsyncfilnam:',trim(mmsyncfilnam)
   open (newunit = lunfil, file=mmsyncfilnam, position='append', action='write', iostat=istat)
   if (istat /= 0) then
      write(*,*)' *** WARNING: unable to write in file ',trim(mmsyncfilnam)
   else
      write(lunfil,'(a)') 'Initialized'
      close(lunfil)
   endif
   !
   ! create stream for each condition
   ! stream handle is placed in filnam and is returned
   !
   do icond = 1 , ncond
      write(*,'(2a)') trim(cond(icond)), ' :'
      write(filnam,'(5a)') trim(workdir), slash, trim(cond(icond)), 'stream', trim(runid)
      if (debug) write(*,'(2a)') '   Creating stream, using file ', trim(filnam)
      write(*,'( a)') '   Waiting for connection with Delft3D-FLOW...'
      handles(icond) = createstream(filnam)
      if (debug) write(*,'(a,i0,3x,a)') '   Handle created : ', handles(icond), trim(filnam)
      write(*,'(a)') '   Connection established, continuing...'
   enddo
   !
   ! Get dimensions for the arrays
   !
   do icond=1,ncond
      call getarray(handles(icond),rn,2)
      if (icond == 1) then
         nmmax = nint(rn(1))
         lsed  = nint(rn(2))   
      else
         if (nint(rn(1)) /= nmmax) then
            write(*,*)          'ERROR: nmmax differs'
            write(lundia,'(a)') 'ERROR: nmmax differs:'
            write(lundia,'(7x,2a,i0)') trim(cond(1    )), ': nmmax = ', nmmax
            write(lundia,'(7x,2a,i0)') trim(cond(icond)), ': nmmax = ', nint(rn(1))
            close (lundia)
            stop
         endif
         if (nint(rn(2)) /= lsed) then
            write(*,*)          'ERROR: lsed differs'
            write(lundia,'(a)') 'ERROR: lsed differs:'
            write(lundia,'(7x,2a,i0)') trim(cond(1    )), ': lsed = ', lsed
            write(lundia,'(7x,2a,i0)') trim(cond(icond)), ': lsed = ', nint(rn(2))
            close (lundia)
            stop
         endif
      endif
   enddo
   if (debug) write(lundia,'(a,i0)') 'nmmax : ', nmmax
   if (debug) write(lundia,'(a,i0)') 'lsed  : ', lsed
   !
   ! Allocate arrays
   !
   allocate (dbdsed        (nmmax,lsed))
   allocate (dbdsed_merged (nmmax,lsed))
   !
   ! Loop
   !
   loopcount = 0
   write(*,'(a)') 'Infinite loop started ...'
   do while (.not. finished)
      !
      ! Get the dimension of the array to process, sent in double precision format
      !
      do icond = 1, ncond
         call getarray(handles(icond), dim_real, 1)
      enddo
      !
      !
      !
      if (dim_real < 1.5_hp) then
         !
         ! dim=1: Assuming that the size of the new time step is going to be sent
         ! Use min-operator
         ! Initialize timestep_min on a huge value
         !
         timestep_min = 1.0e10
         do icond = 1, ncond
            call getarray(handles(icond), timestep, 1)
            timestep_min = min(timestep_min, timestep)
         enddo
         do icond=1,ncond
            call putarray(handles(icond), timestep_min, 1)
         enddo
      else
         !
         ! dim > 1.5: Assuming that the dbdsed is going to be sent
         ! Use merge operator
         ! Initialize dps_merged
         !
         dbdsed_merged = 0.0_hp
         !
         ! Get updated bodsed for all conditions
         !
         do icond = 1, ncond
            call getarray(handles(icond), dbdsed, nmmax*lsed)
              ! write(lundia,'(a,i0)') 'Getting:',handles(icond)
              ! do i=1,nmmax
              !    write(lundia,*) i, dbdsed(i,1)
              ! enddo
            dbdsed_merged  = dbdsed_merged +  dbdsed  * weight(icond)
         enddo
         !
         ! Put bodsed to Delft3D-FLOW
         !
         !     write(lundia,'(a)') 'Putting:'
         !     do i=1,nmmax
         !        write(lundia,*) i, dbdsed_merged(i,1)
         !     enddo
         do icond=1,ncond
            call putarray(handles(icond), dbdsed_merged, nmmax*lsed)
         enddo
         loopcount = loopcount + 1
         !
         ! Synchronisation
         !
         if (mod(loopcount,10) == 0) then
            open (newunit = lunfil, file=mmsyncfilnam, position='append', action='write', iostat=istat)
            if (istat /= 0) then
               write(*,*)' *** WARNING: unable to write in file ',trim(mmsyncfilnam)
            else
               write(lunfil,'(i0)') loopcount
               close(lunfil)
            endif
         endif
      endif
   enddo
   !
   ! Currently, this finishing part is never reached
   !
   call tree_destroy(infile_root)
   deallocate(cond   )
   deallocate(weight )
   deallocate(handles)
   write (lundia,'(a)') 'Mormerge finished normally'
   close (lundia)
end subroutine merge
