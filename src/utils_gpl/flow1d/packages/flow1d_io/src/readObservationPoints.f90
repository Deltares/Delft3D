module m_readObservationPoints
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
   use m_network
   use m_ObservationPoints
   use m_GlobalParameters

   use properties
   use m_hash_search
   use string_module

   implicit none

   private

   public readObservationPoints

   !> The file version number of the observation points file format: d.dd, [config_major].[config_minor], e.g., 1.03
   !!
   !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
   !! Convention for format version changes:
   !! * if a new format is backwards compatible with old files, only
   !!   the minor version number is incremented.
   !! * if a new format is not backwards compatible (i.e., old files
   !!   need to be converted/updated by user), then the major version number
   !!   is incremented.
   
   ! Observation points file current version: 2.00
   integer, parameter :: ObsFileMajorVersion = 2
   integer, parameter :: ObsFileMinorVersion = 0
   
   ! History observation points file versions:

   ! 2.00 (2019-06-18): Change LocationType from integer to strings, and change ExtrapolationMethod to yes/no value.
   ! 1.01 (2019-03-12): First version of *.ini type observation point file.

   contains
   !> Reads observation points from a *.ini file
   subroutine readObservationPoints(network, observationPointsFile)
      use m_missing, only: dmiss
      use string_module, only: strcmpi
      implicit none
      
      type(t_network), intent(inout)        :: network
      character(len=*)  , intent(in)           :: observationPointsFile

      logical                               :: success
      type(tree_data), pointer              :: md_ptr 
      integer                               :: istat
      integer                               :: numstr
      integer                               :: i

      character(len=IdLen)                  :: obsPointID
      character(len=IdLen)                  :: obsPointName
      character(len=IdLen)                  :: branchID
      character(len=IdLen)                  :: locationType
      character(len=IdLen)                  :: location_file
      double precision                      :: Chainage
      double precision                      :: xx, yy
      integer                               :: loctype
      integer                               :: branchIdx
      type(t_ObservationPoint), pointer     :: pOPnt
     ! character(len=IdLen)                 :: binfile
      integer                               :: formatbr       ! =1: use branchid and chainage, =0: use xy coordinate and LocationType
      integer                               :: major, minor, ierr
      
      call SetMessage(LEVEL_INFO, 'Reading Observation Points from '''//trim(observationPointsFile)//'''...')

      xx       = dmiss
      yy       = dmiss
      Chainage = dmiss
      loctype  = INDTP_1D
      branchIdx= 0
      
      branchID     = ''
      obsPointID   = ''
      obsPointName = ''
      
      call tree_create(trim(observationPointsFile), md_ptr, maxlenpar)
      call prop_file('ini',trim(observationPointsFile),md_ptr, istat)
      
      ! check FileVersion
      ierr = 0
      major = 0
      minor = 0
      call get_version_number(md_ptr, major = major, minor = minor, success = success)
      if (.not. success .or. major < ObsFileMajorVersion) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of observation point file detected in '''//trim(observationPointsFile)//''': v', major, minor, '. Current format: v',ObsFileMajorVersion,ObsFileMinorVersion,'. Ignoring this file.'
         call warn_flush()
         ierr = 1
      end if
      
      if (ierr /= 0) then
         goto 999
      end if
      
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      do i = 1, numstr
         
         if (strcmpi(tree_get_name(md_ptr%child_nodes(i)%node_ptr), 'ObservationPoint')) then
            call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'locationFile', location_file, success)
            if (success) then
                ! currently, locationFile is used for moving observation points that are processed in other subroutine
                cycle
            end if
            ! Read Data
            call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'name', obsPointName, success)
            if (success) then
               call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'branchId', branchID, success)
               if (success) then ! the obs is defined by branchid and chainage
                  formatbr = 1
                  call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'chainage', Chainage, success)
                  loctype = INDTP_1D
               else ! the obs is defined by x, y coordinate and locationtype
                  formatbr = 0
                  locationType = '2d' ! Default when not user-defined.
                  call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'locationType', locationType, success)
                  call locationTypeStringToInteger(locationType, loctype)
                  if (loctype < 0) then
                     call SetMessage(LEVEL_ERROR, 'Error reading observation point '''//trim(obsPointName)//''' from file ''' // &
                                                   trim(observationPointsFile)//'''. Invalid locationType '''//trim(locationType)//''' given.')
                     cycle
                  end if

                  call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'x', xx, success)
                  if (success) then
                     call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'y', yy, success)
                  end if
               end if
               
               if (.not. success) then
                  call SetMessage(LEVEL_ERROR, 'Error Reading Observation Point '''//trim(obsPointName)//''', location input is invalid.')
                  cycle
               end if
            else
               write (msgbuf, '(a,i0,a)') 'Error Reading Observation Point #', (i-1), ', name is missing.'
               call err_flush()
               cycle
            end if
      
            network%obs%Count = network%obs%Count+1
            if (network%obs%Count > network%obs%size) then
               call realloc(network%obs)
            endif
            
            
            pOPnt => network%obs%OPnt(network%obs%Count)
            
            pOPnt%id        = obsPointID
            pOPnt%name      = obsPointName
            if (formatbr == 1) then
               branchIdx = hashsearch(network%brs%hashlist, branchID)
               if (branchIdx == -1) then
                  msgbuf = 'Error Reading Observation Point '''//trim(obsPointName)//''' from file ''' // &
                           trim(observationPointsFile)//''', the branchId '''//trim(branchId)//''' does not exist.'
                  call err_flush()
               else
                  pOPnt%branch    => network%brs%branch(branchIdx)
                  pOPnt%branchIdx = branchIdx
                  pOPnt%chainage  = Chainage                
                  pOPnt%locationtype = loctype ! ==INDTP_1D
               end if
            else
               pOPnt%x         = xx
               pOPnt%y         = yy
               pOPnt%locationtype = loctype
               pOPnt%branchIdx = 0
            end if
         endif
      end do
      
      write(msgbuf,'(i10,2a)') network%obs%Count , ' static observation points have been read from file ', trim(observationPointsFile)
      call msg_flush()
      
      call fill_hashtable(network%obs)
      
999   continue
      call tree_destroy(md_ptr)

   end subroutine readObservationPoints

   !> Converts a location type as text string into the integer parameter constant.
   !! E.g. INDTP_1D, etc. If input string is invalid, -1 is returned.
   subroutine locationTypeStringToInteger(slocType, ilocType)
      implicit none
      character(len=*), intent(in   ) :: slocType        !< Location type string.
      integer,          intent(  out) :: ilocType        !< Location type integer. When string is invalid, -1 is returned.
      
      select case (trim(str_tolower(slocType)))
      case ('1d')
         ilocType = INDTP_1D
      case ('2d')
         ilocType = INDTP_2D
      case ('all')
         ilocType = INDTP_ALL
      case default
         ilocType = -1
      end select
      return
   
   end subroutine locationTypeStringToInteger
end module m_readObservationPoints
