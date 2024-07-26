!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
module timespace_parameters
   use string_module, only: str_tolower

   implicit none

   integer, parameter :: NODE_ID = -1 ! for a reference to a node ID
   integer, parameter :: LINK_ID = -1 ! for a reference to a link ID
   ! enumeration for filetypes van de providers
   integer, parameter :: FILE_TYPE_UNKNOWN = -1
   integer, parameter :: UNIFORM = 1 ! kx values per tijdstap 1 dim arr       uni
   integer, parameter :: UNIMAGDIR = 2 ! kx values per tijdstap 1 dim arr, mag/dir transf op index 1,2 u,v
   integer, parameter :: SVWP = 3 ! 3 velden per tijdstap 3 dim array      noint
   integer, parameter :: ARCINFO = 4 ! 1 veld per tijdstap 2 dim array        bilin/direct
   integer, parameter :: SPIDERWEB = 5 ! 3 veld per tijdstap 3 dim array        bilin/spw
   integer, parameter :: CURVI = 6 ! 1 veld per tijdstap 2 dim array        bilin/findnm
   integer, parameter :: TRIANGULATION = 7 ! 1 veld per tijdstap                    triang
   integer, parameter :: TRIANGULATIONMAGDIR = 8 ! 2 velden u,v per tijdstap 3 dim array  triang, vectormax = 2
   ! op basis van windreeksen op stations mag/dir
   integer, parameter :: POLY_TIM = 9 ! for line oriented bnd conditions, refs to uniform, fourier or harmonic
   integer, parameter :: INSIDE_POLYGON = 10 ! Constant value inside polygon, used for initial/parameter fields.
   integer, parameter :: NCGRID = 11 ! NetCDF grid, rectangular type as arcinfo
   integer, parameter :: NCFLOW = 12 ! NetCDF flow, with arbitrary type of input
   integer, parameter :: NCWAVE = 14 ! NetCDF com file, with arbitrary type of input
   integer, parameter :: BCASCII = 17 ! .bc format as ASCII file
   integer, parameter :: FIELD1D = 18 ! Scalar quantity on a 1D network, used for initial/parameter fields.
   integer, parameter :: GEOTIFF = 19 ! GeoTIFF, used for initial/parameter fields.
   integer, parameter :: MAX_FILE_TYPES = 103 !  max nr of supported types for end user in ext file.
   ! Enumeration for file types of sub-providers (not directly in ext file)
   integer, parameter :: FOURIER = 101 ! period(hrs), ampl(m), phas(deg) NOTE: not directly used in ext file by users.
   integer, parameter :: MULTIPLE_UNI = 102 ! multiple time series, no spatial relation
   integer, parameter :: QHTABLE = 103 ! used to link to dataprovider file

   ! het filetype legt vast  :  a) format file
   !                            b) vectormax van grootheid / heden in file
   !                            c) elementset waarop grootheid is gedefinieerd
   !                            d) is daarmee bepalend voor de toepasbare interpolatiemethodes
   !

   ! Enumeration for location specification types (used in selectelset_internal_nodes).
   integer, parameter :: LOCTP_UNKNOWN = -1 !< Undefined location specification type.
   integer, parameter :: LOCTP_POLYGON_FILE = 10 !< A polygon input file used for inside-polygon check.
   integer, parameter :: LOCTP_POLYGON_XY = 11 !< x/y arrays containing a polygon used for inside-polygon check.
   integer, parameter :: LOCTP_POLYLINE_FILE = 12 !< A polyline input file used for link-crosses-polyline check.
   integer, parameter :: LOCTP_POLYLINE_XY = 13 !< x/y arrays containing a polyline used for link-crosses-polyline check.
   integer, parameter :: LOCTP_BRANCHID_CHAINAGE = 14 !< branchid+chainage combination to select the 1D grid point closest to that network branch location.
   integer, parameter :: LOCTP_NODEID = 15 !< nodeid to select the 1D grid point closest to the network point with that nodeId.
   integer, parameter :: LOCTP_CONTACTID = 16 !< contactid to select the 1D flow link corresponding with that contactId.

   integer :: mdia = 0 !  -1  ! -1 = write dia, 0 = do not write dia

   ! enumeration for interpolation methods of providers
   integer, parameter :: METHOD_UNKNOWN = -1
   integer, parameter :: JUSTUPDATE = 0 ! provider just updates, another provider that
   ! pointers to this one does the actual interpolation
   integer, parameter :: SPACEANDTIME = 1 ! intp space and time (getval)
   ! keep  2 meteofields in memory
   integer, parameter :: SPACEFIRST = 2 ! first intp space (update), next intp. time (getval)
   ! keep 2 flowfields in memory
   integer, parameter :: WEIGHTFACTORS = 3 ! save weightfactors, intp space and time (getval)
   ! keep 2 pointer- and weight sets in memory
   integer, parameter :: METHOD_CONSTANT = 4
   integer, parameter :: METHOD_TRIANGULATION = 5
   integer, parameter :: METHOD_AVERAGING = 6
   integer, parameter :: WEIGHTFACTORS_EXTRAPOLATION = 103

contains

!> Converts fileType string to an integer.
!! Returns -1 when an invalid type string is given.
   function convert_file_type_string_to_integer(string) result(file_type)
      implicit none
      character(len=*), intent(in) :: string !< file type string
      integer :: file_type !< file type integer

      select case (str_tolower(trim(string)))
      case ('1dfield')
         file_type = FIELD1D
      case ('aaigrid')
         file_type = ARCINFO
      case ('bcascii')
         file_type = BCASCII
      case ('curvigrid')
         file_type = CURVI
      case ('geotiff')
         file_type = GEOTIFF
      case ('netcdf')
         file_type = NCGRID
      case ('polygon')
         file_type = INSIDE_POLYGON
      case ('sample')
         file_type = TRIANGULATION
      case ('spiderweb')
         file_type = SPIDERWEB
      case ('uniform')
         file_type = UNIFORM
      case ('unimagdir')
         file_type = UNIMAGDIR
      case default
         file_type = FILE_TYPE_UNKNOWN
      end select

   end function convert_file_type_string_to_integer

!> Converts interpolationMethod string to an integer.
!! Returns -1 when an invalid type string is given.
   function convert_method_string_to_integer(string) result(method)
      implicit none
      character(len=*), intent(in) :: string !< method string
      integer :: method !< method integer

      select case (str_tolower(trim(string)))
      case ('averaging')
         method = METHOD_AVERAGING
      case ('constant')
         method = METHOD_CONSTANT
      case ('linearspacetime')
         method = WEIGHTFACTORS
      case ('triangulation')
         method = METHOD_TRIANGULATION
      case default
         method = METHOD_UNKNOWN
      end select

   end function convert_method_string_to_integer

!> Provides default method for specific file type
!! Returns -1 when an invalid type string is given.
   function get_default_method_for_file_type(string) result(method)
      implicit none
      character(len=*), intent(in) :: string !< file type string
      integer :: method !< method integer

      select case (str_tolower(trim(string)))
      case ('bcascii')
         method = SPACEANDTIME
      case ('netcdf')
         method = WEIGHTFACTORS
      case ('sample')
         method = METHOD_TRIANGULATION
      case ('uniform')
         method = SPACEANDTIME
      case default
         method = METHOD_UNKNOWN
      end select

   end function get_default_method_for_file_type

   subroutine update_method_in_case_extrapolation(method, is_extrapolation_allowed)
      implicit none
      integer, intent(inout) :: method !< method integer
      logical, intent(in) :: is_extrapolation_allowed !< is extrapolation allowed

      if (.not. is_extrapolation_allowed) return

      if (method == WEIGHTFACTORS) then
         method = WEIGHTFACTORS_EXTRAPOLATION
      end if

   end subroutine update_method_in_case_extrapolation

end module timespace_parameters
