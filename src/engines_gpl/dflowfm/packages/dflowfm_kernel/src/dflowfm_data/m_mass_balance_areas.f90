!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2023.
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

module m_mass_balance_areas
   integer, parameter                        :: NAMMBALEN = 128             !< maximum length of mass balance area names
   integer                                   :: jamba = 0                   !< switch for mass balance areas being active
   integer                                   :: nombs = 0                   !< number of mass balances
   character(len=NAMMBALEN),allocatable      :: mbsname(:)                  !< mass balance names
   integer                                   :: nomba = 0                   !< number of mass balance areas
   integer                                   :: nombabnd                    !< number of mass balance areas and boundaries
   character(len=NAMMBALEN),allocatable      :: mbaname(:)                  !< mass balance area names
   character(len=NAMMBALEN),allocatable      :: mbabndname(:)               !< mass balance area horizontal transport names
   integer, allocatable                      :: mbadef(:)                   !< mass balance area (mba) definition
   integer, allocatable                      :: mbadefdomain(:)             !< mass balance area (mba) definition without ghost cells
   integer                                   :: id_mba(3)                   !< mbd id's in map-file
   integer, allocatable                      :: mbalnfromto(:,:)            !< from mba (1:lnxi) or bnd (lnxi+1:lnx) to mba for each link (2D)
   integer, allocatable                      :: mbalnused(:,:)              !< number of links between mda and mbabnd that are actually active
   integer, allocatable                      :: mbasorsin(:,:)              !< mba for each side of a source sink
   integer, allocatable                      :: mbasorsinout(:,:)           !< (reduced) mba for each side of a source sink for output
   integer                                   :: nombaln                     !< number of links needed for mass balance (2D)
   integer, allocatable                      :: mbalnlist(:)                !< list of links needed for the mass balance (2D)
   logical                                   :: mbaremaining                !< mass balance area for ramaining cells added?
   integer                                   :: lunmbahis                   !< logical unit of mba his-file
   integer                                   :: lunmbatothis                !< logical unit of mba total his-file
   integer                                   :: lunmbabal                   !< logical unit of mba bal-file
   integer                                   :: lunmbacsvm                  !< logical unit of mba mass csv-file
   integer                                   :: lunmbacsvmb                 !< logical unit of mba mass balance csv-file
   integer                                   :: lunmbatotbal                !< logical unit of mba total bal-file
   integer                                   :: itimembastart               !< start time of balance period
   integer                                   :: itimembastarttot            !< start time of balance period
   integer                                   :: itimembaend                 !< end time of balance period
   double precision                          :: timembastart                !< start time of balance period
   double precision                          :: timembastarttot             !< start time of balance period
   double precision                          :: timembaend                  !< end time of balance period

   double precision, allocatable             :: mbaarea(:)                  !< surface area of mass balance area

   double precision, allocatable, target     :: mbavolumebegin(:)           !< begin volume in mass balance area
   double precision, allocatable, target     :: mbavolumebegintot(:)        !< total begin volume in mass balance area
   double precision, allocatable             :: mbavolumeend(:)             !< end volume in mass balance area

   double precision, allocatable, target     :: mbaflowhor(:,:,:)           !< periodical flow between balance areas and between boundaries and balance areas
   double precision, allocatable, target     :: mbaflowhortot(:,:,:)        !< total flow between balance areas and between boundaries and balance areas
   double precision, allocatable, target     :: mbaflowsorsin(:,:)          !< periodical flow from source sinks
   double precision, allocatable, target     :: mbaflowsorsintot(:,:)       !< total flow from source sinks
   double precision, allocatable, target     :: mbaflowraineva(:,:)         !< periodical flow from rain and prescribed evaporation
   double precision, allocatable, target     :: mbaflowrainevatot(:,:)      !< total flow from rain and prescribed evaporation
   double precision, allocatable, target     :: mbafloweva(:)               !< periodical flow from calculated evaporation
   double precision, allocatable, target     :: mbaflowevatot(:)            !< total flow from calculated evaporation

   double precision, allocatable, target     :: mbamassbegin(:,:)           !< begin volume in mass balance area
   double precision, allocatable, target     :: mbamassbegintot(:,:)        !< total begin volume in mass balance area
   double precision, allocatable             :: mbamassend(:,:)             !< end volume in mass balance area

   double precision, allocatable, target     :: mbafluxhor(:,:,:,:)         !< periodical fluxes between balance areas and between boundaries and balance areas
   double precision, allocatable, target     :: mbafluxhortot(:,:,:,:)      !< total fluxes between balance areas and between boundaries and balance areas
   double precision, allocatable, target     :: mbafluxsorsin(:,:,:,:)      !< periodical fluxes from source sinks
   double precision, allocatable, target     :: mbafluxsorsintot(:,:,:,:)   !< total fluxes from source sinks
   double precision, allocatable, target     :: mbafluxheat(:,:)            !< temperature heat flux
   double precision, allocatable, target     :: mbafluxheattot(:,:)         !< total temperature heat flux

   double precision, allocatable             :: mbavolumereduce    (:)      !< begin volume in mass balance area
   double precision, allocatable             :: mbaflowhorreduce   (:,:,:)  !< periodical flow between balance areas and between boundaries and balance areas
   double precision, allocatable             :: mbaflowsorsinreduce(:,:)    !< periodical flow from sources sinks
   double precision, allocatable             :: mbaflowrainevareduce(:,:)   !< periodical flow from rainfal and prescribed evaporation
   double precision, allocatable             :: mbaflowevareduce(:)         !< periodical flow from calculated evaporation
   double precision, allocatable             :: mbamassreduce      (:,:)    !< begin volume in mass balance area
   double precision, allocatable             :: mbafluxhorreduce   (:,:,:,:)!< periodical fluxes between balance areas and between boundaries and balance areas
   double precision, allocatable             :: mbafluxsorsinreduce(:,:,:,:)!< periodical fluxes from source sinks
   double precision, allocatable             :: mbafluxheatreduce(:,:)      !< temperature heat flux
end module m_mass_balance_areas
