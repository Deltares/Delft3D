!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

      subroutine write_sing_dis(hyd, file_names)

      ! function : write discharge names singapore models

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use waq_file_utils_external, only: create_new_file_unit_number
      use m_hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)                            :: hyd                   ! description of the hydrodynamics
      character(len=*)                       :: file_names            ! file with new discharge names and type

      ! local declarations

      integer                                :: lun_names             ! unit number
      integer                                :: nowast                ! number of wasteloads
      integer                                :: nowast_tot            ! number of wasteloads total in waq
      integer                                :: num_layers                 ! number of layers
      integer                                :: nolay_waste           ! number of layers for specific load
      integer                                :: iwaste_lay            ! follow number load
      integer                                :: ilay                  ! waq layer index
      integer                                :: iwaste                ! wasteload index
      integer                                :: m                     ! m index
      integer                                :: n                     ! n index
      integer                                :: k                     ! k index
      integer                                :: iseg                  ! segment number wasteload
      character(len=3)                       :: c3                    ! prefix
      character(len=20)                      :: org_name              ! original discharge name which equals type
      character(len=20)                      :: c20_name              ! final name WAQ-GUI
      character(len=100)                     :: c100_name             ! temporary name
      character(len=1), parameter            :: quote = ''''
      character(len=1), parameter            :: space = ' '

      ! some init

      nowast = hyd%wasteload_coll%current_size
      if ( nowast .le. 0 ) return
      num_layers  = hyd%num_layers

      ! count total number of waq loads

      nowast_tot = 0
      do iwaste = 1 , nowast
         if ( hyd%wasteload_coll%wasteload_pnts(iwaste)%k .eq. 0 ) then
            nolay_waste = num_layers
         else
            nolay_waste = 1
         endif
         do ilay = 1 , nolay_waste
            nowast_tot = nowast_tot + 1
         enddo
      enddo

      ! open file with names and types

      call create_new_file_unit_number(761,lun_names)
      open(lun_names,file=file_names)

      write(lun_names,'(i12,a)') nowast_tot,' ; number of wastelaods'

      ! loop over the wasteloads

      iwaste_lay = 0
      do iwaste = 1 , nowast

         ! now write the file whith the final WAQ-GUI names adding the org_name as type

         m = hyd%wasteload_coll%wasteload_pnts(iwaste)%m
         n = hyd%wasteload_coll%wasteload_pnts(iwaste)%n
         k = hyd%wasteload_coll%wasteload_pnts(iwaste)%k

         if ( k .eq. 0 ) then
            nolay_waste = num_layers
            k = 1
         else
            nolay_waste = 1
         endif
         do ilay = 1 , nolay_waste
            iwaste_lay = iwaste_lay + 1

            ! this will be the name as created by WAQ-GUI

            if ( iwaste_lay .lt. 10 ) then
               write(c100_name,'(i1,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            elseif ( iwaste_lay .lt. 100 ) then
               write(c100_name,'(i2,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            elseif ( iwaste_lay .lt. 1000 ) then
               write(c100_name,'(i3,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            elseif ( iwaste_lay .lt. 10000 ) then
               write(c100_name,'(i4,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            else
               write(c100_name,'(i5,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            endif

            c20_name = c100_name

            iseg = hyd%lgrid(n,m) + (k-1)*hyd%nosegl

            write(lun_names,'(i8,1x,11a)') iseg, & 
                                    quote,trim(c20_name),quote,space, & 
                                    quote,trim(c20_name),quote,space, & 
                                    quote,trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%waqtype),quote

            k = k + 1

         enddo
      enddo

      close(lun_names)

      return
      end
