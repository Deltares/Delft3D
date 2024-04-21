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
!

module m_fm_wq_processes
   use data_processing, only : read_substances
   use m_dhnoseg
   use precision, only: fp, prec, fhp, pntrsize, hp, long
   use m_waq_precision, only : sp
   use processes_input
   use processes_pointers
   use m_waq_data_structure
   use processet
   use results, only : OutputPointers

   character(20), allocatable                :: syunit_sub(:)               !< substance unit from substance file
   character(20), allocatable                :: coname_sub(:)               !< constant names from substance file
   real         , allocatable                :: covalue_sub(:)              !< values for contants from substance file
   character(20), allocatable                :: ouname_sub(:)               !< output names from substance file
   character(80), allocatable                :: oudesc_sub(:)               !< output decriptions from substance file
   integer( 4)                               :: noout_sub                   !< number of outputs requested in substance file

   character(20), allocatable                :: syname_eho(:)               !< substance names from extra history output file
   character(20), allocatable                :: syunit_eho(:)               !< substance names from extra history output file
   character(20), allocatable                :: coname_eho(:)               !< constant names from extra history output file
   real         , allocatable                :: covalue_eho(:)              !< values for contants from extra history output file
   character(20), allocatable                :: ouname_eho(:)               !< output names from extra history output file
   character(80), allocatable                :: oudesc_eho(:)               !< output decriptions from extra history output file
   integer( 4)                               :: noout_eho                   !< number of outputs requested in extra history output file

   character(len=256)                        :: substance_file              !< substance file
   character(len=256)                        :: his_output_file             !< extra history output file
   character(len=256)                        :: proc_log_file               !< processes log file
   character(len=256)                        :: proc_def_file               !< processes definition file
   character(len=256)                        :: proc_dllso_file             !< open processes dll/so file
   character(len=256)                        :: bloom_file                  !< BLOOM algae spiecies parameter file
   character(len=256)                        :: bloom_output_file           !< BLOOM output file base name
   character(len=256)                        :: statistics_file             !< file with configuration for statistics

   integer, parameter                        :: NAMWAQLEN = 128
   integer                                   :: jawaqproc = 0               !< switch for water quality processes (1 = substances initiated, 2 = processes activated too)
   real(hp)                                  :: waq_vol_dry_thr = 1.0d-3    !< minimum volume for processes to be active
   real(hp)                                  :: waq_dep_dry_thr = 1.0d-3    !< minimum depth for processes to be active
   integer                                   :: flux_int                    !< flux integration by WAQ (1) or by FM (2, not implemented)
   integer                                   :: wqbot3D_output = 0          !< write 3D wqbot output
   integer                                   :: kbx                         !< pointer of first segment to D-Flow FM 3D administration
   integer                                   :: ktx                         !< pointer of last  segment to D-Flow FM 3D administration

   integer                                   :: noseg                       !< Nr. of computational volumes
   integer                                   :: noq1                        !< Number of exchanges first direction
   integer                                   :: noq2                        !< Number of exchanges second direction
   integer                                   :: noq3                        !< Number of exchanges vertical
   integer                                   :: noq4                        !< Number of exchanges in the bed
   integer,  allocatable, dimension(:)       :: iexpnt                      !< Exchange pointer

   real(hp), allocatable, dimension(:,:)     :: amass                       !< mass array to be updated
   logical , allocatable, dimension(:)       :: wqactive                    !< indicates if processes are active based on volume ('VolumeDryThreshold') and depth ('DepthDryThreshold') criteria
   logical , allocatable, dimension(:)       :: wqmydomain                  !< indicates if a segment is part of the current domain (idomain==my_rank)
   logical , allocatable, dimension(:)       :: wqdoproc                    !< indicates if processes are active based on 'ProcessesInactive' parameter
   integer , allocatable, dimension(:)       :: iknmrk                      !< segment characteristics.
                                                                            !< 1st digit from the right indicates wet/dry (1/0),
                                                                            !< 2nd digit indicates top/middle/bottom/top&bottom in water column (1/2/3/0).
                                                                            !< possible iknmrk values for processes
   integer, parameter                        :: IKNMRK_INACTIVE      = 1100 !< processes are inactive
   integer, parameter                        :: IKNMRK_ACTIVE_TOPBOT = 1101 !< processes are active, single active layer, both top and bottom of water column
   integer, parameter                        :: IKNMRK_ACTIVE_TOP    = 1111 !< processes are active, top of water column
   integer, parameter                        :: IKNMRK_ACTIVE_MIDDLE = 1121 !< processes are active, middle of water column
   integer, parameter                        :: IKNMRK_ACTIVE_BOTTOM = 1131 !< processes are active, bottom of water column

   integer                                   :: sizepmsa                    !< size of (pms)a-array
   real(sp), allocatable, dimension(:)       :: pmsa                        !< the actual data array

   real(hp), allocatable, dimension(:,:)     :: deriv                       !< Model derivatives in mass/m3/s (= stochi(notot ,noflux) * flux(noflux, noseg))

   integer,  allocatable, dimension(:)       :: isys2const                  !< WAQ substance to D-Flow FM constituents
   integer,  allocatable, dimension(:)       :: iconst2sys                  !< D-Flow FM constituents to WAQ substance
   integer,  allocatable, dimension(:)       :: imbs2sys                    !< D-Flow FM mass balance number to WAQ substance (0=not a WAQ substance)
   integer,  allocatable, dimension(:)       :: isys2trac                   !< WAQ active system to D-FlowFM tracer
   integer,  allocatable, dimension(:)       :: isys2wqbot                  !< WAQ inactive system to D-FlowFM water quality bottom variable
   integer,  allocatable, dimension(:)       :: ifall2vpnw                  !< substance-with-fall-velocity to WAQ numbering in fall-velocity array

   integer                                   :: numwqbots = 0               !< number of water quality bottom variables
   character(len=NAMWAQLEN), dimension(:), allocatable :: wqbotnames        !< water quality bottom variable names
   character(len=NAMWAQLEN), dimension(:), allocatable :: wqbotunits        !< water quality bottom variable units
   integer,  allocatable, dimension(:,:)     :: id_wqb                      !< wqbot id's in map-file
   integer,  allocatable, dimension(:,:)     :: id_wqb3d                    !< 3d wqbot id's in map-file
   real(hp), allocatable, dimension(:,:)     :: wqbot                       !< water quality bottom variable values in double precision

   type(OutputPointers)                      :: outputs                     !< output structure
   integer,  allocatable, dimension(:,:)     :: id_waq                      !< waq output id's in map-file
   integer,  allocatable, dimension(:,:)     :: id_wqst                     !< waq stat time output id's in map-file
   integer,  allocatable, dimension(:,:)     :: id_wqse                     !< waq stat end output id's in map-file
   real(hp), allocatable, dimension(:,:)     :: waqoutputs                  !< waq outputs, dim(noout,Ndkx)

   integer                                   :: isfsurf                     !< pointer to surface         segment function
   integer                                   :: isftau                      !< pointer to tau             segment function
   integer                                   :: isfvel                      !< pointer to velocity        segment function
   integer                                   :: isfsal                      !< pointer to Salinity        segment function
   integer                                   :: isftem                      !< pointer to Temperature     segment function
   integer                                   :: isfvwind                    !< pointer to wind vel. magn. segment function
   integer                                   :: isfwinddir                  !< pointer to wind direction  segment function
   integer                                   :: isffetchl                   !< pointer to fetch length    segment function
   integer                                   :: isffetchd                   !< pointer to fetch depth     segment function
   integer                                   :: isfradsurf                  !< pointer to solar radiation segment function
   integer                                   :: isfrain                     !< pointer to rain            segment function
   integer                                   :: isfvertdisper               !< pointer to vertdisper      segment function
   integer                                   :: isffmlayer                  !< pointer to fmlayer         segment function
   integer                                   :: isffmktop                   !< pointer to fmktop          segment function
   integer                                   :: isffmkbot                   !< pointer to fmkbot          segment function
   integer                                   :: isfwaveheight               !< pointer to wave height     segment function
   integer                                   :: isfwavelength               !< pointer to wave length     segment function
   integer                                   :: isfwaveperiod               !< pointer to wave period     segment function
!
!     Balance output
!
   integer                                         :: ibflag                      !< if 1 then mass balance output
   real(hp), allocatable, dimension(:,:,:), target :: flxdmp                      !< Fluxes at dump segments
   real(hp), allocatable, dimension(:,:,:)         :: flxdmpreduce                !< Fluxes at dump segments
   real(hp), allocatable, dimension(:,:,:), target :: flxdmptot                   !< Total fluxes at dump segments

   integer                                         :: nomon                       !< number of mass balance areas
   character(len=NAMWAQLEN),allocatable            :: monname(:)                  !< parameter names
   integer, allocatable                            :: mondef(:,:)                 !< monitoring area definition
end module m_fm_wq_processes
