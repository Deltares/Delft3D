!!  Copyright (C)  Stichting Deltares, 2021-2025.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine write_overall_dmo(hyd, domain_hyd_coll, lunrep, success)

      ! function : create an overall DMO file

      ! global declarations

      use m_hydmod                   ! module contains everything for the hydrodynamic description
      use m_file_path_utils, only : extract_file_extension

      implicit none

      type(t_hydrodynamics)          :: hyd              ! description of the overall hydrodynamics
      type(t_hydrodynamics_collection)     :: domain_hyd_coll  ! description of the overall hydrodynamics
      integer              :: lunrep           ! LU-number report file
      logical              :: success

      integer              :: i
      integer              :: j
      integer              :: k
      integer              :: isegl
      integer              :: ilay
      logical              :: exists
      logical              :: opened
      character(len=200)   :: dmoname
      character(len=20)    :: filext
      integer              :: extpos
      integer              :: extlen
      integer              :: noareas
      integer              :: nosegments
      character(len=40)    :: areaname
      integer              :: nosegl
      integer              :: nosegl_new
      integer              :: nosegl_offset
      integer              :: all_areas
      integer              :: idxarea

      type t_monitoring_area
          character(len=40) :: name
          integer, dimension(:), pointer :: segment
      end type t_monitoring_area

      type(t_monitoring_area), pointer, dimension(:) :: areas
      type(t_monitoring_area), pointer, dimension(:) :: new_areas

      nosegl_new    = hyd%nosegl
      nosegl_offset = 0

      all_areas     = 0
      idxarea       = 0

      !
      ! Check for existing DMO files
      !
      success = .false.
      opened  = .false.

      do i = 1,hyd%domain_coll%current_size
          dmoname = trim(hyd%domain_coll%domain_pnts(i)%name) // ".dmo"

          if ( i > 1 ) then
              nosegl_offset = nosegl_offset + domain_hyd_coll%hyd_pnts(i-1)%nosegl
          endif

          inquire( file = dmoname, exist = exists )
          if ( .not. exists ) then
              write(lunrep,'(2a)') '   Skipping domain - DMO file not found: ', dmoname
              cycle
          endif
          write(lunrep,'(2a,i10)') '   Detail DMO file found: ', trim(dmoname)

          if ( .not. opened ) then
              call extract_file_extension( hyd%file_hyd%name, filext, extpos, extlen )

              open( 88, file = hyd%file_hyd%name(1:extpos-1) // ".dmo" )
              success = .true.
              opened  = .true.
          endif

          nosegl = domain_hyd_coll%hyd_pnts(i)%nosegl

          open( 89, file = dmoname )
          read( 89, * ) noareas

          !
          ! Increase the capacity of the areas array
          ! (Do not deallocate the arrays of segment numbers)
          !
          allocate( new_areas(1:all_areas+noareas) )
          if ( associated(areas) ) then
              new_areas(1:all_areas) = areas
              deallocate( areas )
          endif

          all_areas = size(new_areas)
          areas => new_areas

          do j = 1,noareas
              idxarea = idxarea + 1

              read( 89, * ) areas(idxarea)%name, nosegments
              allocate( areas(idxarea)%segment(1:nosegments) )
              read( 89, * ) areas(idxarea)%segment

              do k = 1,nosegments
                  isegl = mod( areas(idxarea)%segment(k)-1, nosegl )+1
                  ilay  = ( areas(idxarea)%segment(k) + nosegl - 1 )/ nosegl

                  areas(idxarea)%segment(k) = isegl + (ilay-1) * nosegl_new + nosegl_offset
              enddo
          enddo

          close( 89 )
      enddo

      if ( opened ) then
          write( 88, '(i10)'    ) all_areas
          do j = 1,all_areas
              write( 88, '(3a,i10)' ) '''', trim(areas(j)%name), '''', size(areas(j)%segment)
              write( 88, '(5i10)'   ) areas(j)%segment
          enddo
          close( 88 )
      end if

      end subroutine write_overall_dmo
