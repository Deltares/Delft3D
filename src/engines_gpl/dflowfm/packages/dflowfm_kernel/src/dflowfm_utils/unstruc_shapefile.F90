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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

!> Write shape files
module unstruc_shapefile

   use, intrinsic :: iso_c_binding
#ifdef HAVE_SHAPELIB
   use shapelib
   use unstruc_messages
   use unstruc_files, only: defaultFilename
   use m_partitioninfo, only: my_rank, jampi
   implicit none

contains

!> Write a shape file for cross sections
   subroutine unc_write_shp_crs()
      use m_monitoring_crosssections
      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, j, n, ii, ishape, nshp
      character(len=lencharattr) :: filename, objectid
      character(len=6) :: lenobj_loc
      integer :: id_objectid, id_flowlinknr
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for cross sections.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for cross sections for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpcrs')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 2 dbf fields: ObjectID, FLOWLINKNR
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_flowlinknr = dbfaddfield(shphandle, 'FLOWLINKNR', ftinteger, 10, 0)
      if (id_flowlinknr /= 1) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "FLOWLINKNR" to shape file '''//trim(filename)//'''.')
         return
      end if

      do n = 1, ncrs
         !call mess(LEVEL_INFO, 'SHAPEFILE: Adding shapes for cross section: '''//trim(crs(n)%NAME)//'''.')
         nshp = crs(n)%PATH%LNX ! Nr. links(shapes)
      !! add nshp shapes
         do i = 0, nshp - 1
            write (lenobj_loc, '(I6.6)') i
            objectid = trim(crs(n)%NAME)//'_'//lenobj_loc
            !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

            ii = i + 1
            ! create a shape object with the "simple" method, for each shape 2 components are added x, y
            shpobj = shpcreatesimpleobject(tshp, 2, crs(n)%PATH%XK(:, ii), crs(n)%PATH%YK(:, ii))

            ! write the shape object to the shapefile object as i-th element, -1 = append
            ishape = shpwriteobject(shphandle, -1, shpobj)
            if (ishape == -1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
               return
            end if

            ! destroy the shape object to avoid memory leaks
            call shpdestroyobject(shpobj)

            ! write the attributes of different types for the i-th shape object to the shapefile object
            ! write ObjectID
            j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write flowlink nr.
            if (allocated(crs)) then
               j = dbfwriteattribute(shphandle, ishape, id_flowlinknr, crs(n)%PATH%LN(ii))
            end if

            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "FLOWLINKNR" to shape'''//trim(objectid)//'''.')
               return
            end if

         end do
      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_crs

!> Write a shape file for observation points
   subroutine unc_write_shp_obs()
      use m_observations
      use m_flowgeom, only: xz, yz

      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_point ! points
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, j, ii, ishp, nshp, k
      character(len=lencharattr) :: filename, objectid
      integer :: id_objectid, id_origx, id_origy, id_flownodenr
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for observation points.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for observation points for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpobs')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 4 dbf fields: ObjectID, ORIG_X, ORIG_Y, FLOWNODENR
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_origx = dbfaddfield(shphandle, 'ORIG_X', ftdouble, 20, 8)
      if (id_origx /= 1) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ORIG_X" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_origy = dbfaddfield(shphandle, 'ORIG_Y', ftdouble, 20, 8)
      if (id_origy /= 2) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ORIG_Y" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_flownodenr = dbfaddfield(shphandle, 'FLOWNODENR', ftinteger, 10, 0)
      if (id_flownodenr /= 3) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "FLOWNODENR" to shape file '''//trim(filename)//'''.')
         return
      end if

      nshp = NUMOBS ! Nr. obs(shapes)
      ! add nshp shapes
      do i = 0, nshp - 1
         ii = i + 1
         k = kobs(ii) ! Flownode Nr.
         if (k <= 0) then
            cycle ! Outside of domain, or probably: in a different partition. Skip it here.
         end if

         objectid = trim(namobs(ii))
         !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape for observation point: '''//trim(objectid)//'''.')

         ! create a shape object with the "simple" method, for each shape 2 components are added x, y
         shpobj = shpcreatesimpleobject(tshp, 2, xz(k), yz(k))

         ! write the shape object to the shapefile object as i-th element, -1 = append
         ishp = shpwriteobject(shphandle, -1, shpobj)
         if (ishp == -1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(namobs(ii))//'''shape object to shapefile object.')
            return
         end if

         ! destroy the shape object to avoid memory leaks
         call shpdestroyobject(shpobj)

         ! write the attributes of different types for the i-th shape object to the shapefile object
         ! write ObjectID
         j = dbfwriteattribute(shphandle, ishp, id_objectid, trim(objectid))
         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(namobs(ii))//'''.')
            return
         end if

         ! write original x-coordinate
         if (allocated(xobs)) then
            j = dbfwriteattribute(shphandle, ishp, id_origx, xobs(ii))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ORIG_X" to shape'''//trim(namobs(ii))//'''.')
            return
         end if

         ! write original y-coordinate
         if (allocated(yobs)) then
            j = dbfwriteattribute(shphandle, ishp, id_origy, yobs(ii))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ORIG_Y" to shape'''//trim(namobs(ii))//'''.')
            return
         end if

         ! write flownode nr.
         j = dbfwriteattribute(shphandle, ishp, id_flownodenr, k)
         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "FLOWNODENR" to shape'''//trim(namobs(ii))//'''.')
            return
         end if

      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_obs

!> Write a shape file for weirs
   subroutine unc_write_shp_weir()
      use fm_external_forcings_data
      use network_data, only: kn, xk, yk
      use m_flowgeom, only: ln2lne
      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, j, n, ishape, igen, Lf, La, L, k, k1, k2
      character(len=lencharattr) :: filename, objectid
      character(len=4) :: lenobj_loc
      integer :: id_objectid, id_flowlinknr, id_weirgen_cresth
      double precision :: tmp_x(2), tmp_y(2)
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for weirs.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for weirs for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpweir')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 3 dbf fields: ObjectID, CRESTLEV, FLOWLINKNR
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_weirgen_cresth = dbfaddfield(shphandle, 'CRESTLEV', ftdouble, 20, 8)
      if (id_weirgen_cresth /= 1) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "CRESTLEV" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_flowlinknr = dbfaddfield(shphandle, 'FLOWLINKNR', ftinteger, 10, 0)
      if (id_flowlinknr /= 2) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "FLOWLINKNR" to shape file '''//trim(filename)//'''.')
         return
      end if

      do n = 1, nweirgen
         igen = weir2cgen(n)
         !call mess(LEVEL_INFO, 'SHAPEFILE: Adding shapes for weir: '''//trim(cgen_ids(igen))//'''.')

      !! add shapes
         i = 0
         do L = L1cgensg(igen), L2cgensg(igen)
            write (lenobj_loc, '(I4.4)') i
            objectid = trim(cgen_ids(igen))//'_'//lenobj_loc
            !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

            ! create a shape object with the "simple" method, for each shape 2 components are added x, y
            Lf = kcgen(3, L)
            La = abs(Lf)
            k = ln2lne(La) ! netnode
            k1 = kn(1, k)
            k2 = kn(2, k)
            tmp_x(1) = xk(k1); tmp_x(2) = xk(k2)
            tmp_y(1) = yk(k1); tmp_y(2) = yk(k2)
            shpobj = shpcreatesimpleobject(tshp, 2, tmp_x, tmp_y)

            ! write the shape object to the shapefile object as i-th element, -1 = append
            ishape = shpwriteobject(shphandle, -1, shpobj)
            if (ishape == -1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
               return
            end if

            ! destroy the shape object to avoid memory leaks
            call shpdestroyobject(shpobj)

            ! write the attributes of different types for the i-th shape object to the shapefile object
            ! write ObjectID
            j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write initial crest level
            if (allocated(zcgen)) then
               j = dbfwriteattribute(shphandle, ishape, id_weirgen_cresth, zcgen((igen - 1) * 3 + 1))
            end if

            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "FLOWLINKNR" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write flowlink nr.
            j = dbfwriteattribute(shphandle, ishape, id_flowlinknr, Lf)
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "FLOWLINKNR" to shape'''//trim(objectid)//'''.')
               return
            end if

            i = i + 1
         end do
      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_weir

!> Write a shape file for thin dams
   subroutine unc_write_shp_thd()
      use m_thindams

      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, j, n, ii, ishape, nshp
      character(len=lencharattr) :: filename, objectid, thdname
      character(len=4) :: lenobj_loc, thdname_loc
      integer :: id_objectid
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for thin dams.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for thin dams for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpthd')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 1 dbf fields: ObjectID
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      do n = 1, nthd
         write (thdname_loc, '(I4.4)') n - 1
         thdname = 'thindam_'//thdname_loc
         !call mess(LEVEL_INFO, 'SHAPEFILE: Adding shapes for thin dam: '''//trim(thdname)//'''.')

         nshp = thd(n)%LNX ! Nr. links(shapes)
      !! add nshp shapes
         do i = 0, nshp - 1
            write (lenobj_loc, '(I4.4)') i
            objectid = trim(thdname)//'_'//lenobj_loc
            !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

            ii = i + 1
            ! create a shape object with the "simple" method, for each shape 2 components are added x, y
            shpobj = shpcreatesimpleobject(tshp, 2, thd(n)%XK(:, ii), thd(n)%YK(:, ii))

            ! write the shape object to the shapefile object as i-th element, -1 = append
            ishape = shpwriteobject(shphandle, -1, shpobj)
            if (ishape == -1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
               return
            end if

            ! destroy the shape object to avoid memory leaks
            call shpdestroyobject(shpobj)

            ! write the attributes of different types for the i-th shape object to the shapefile object
            ! write ObjectID
            j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
               return
            end if
         end do
      end do

      ! close the shapefile object
      call shpclose(shphandle)
   end subroutine unc_write_shp_thd

!> Write a shape file for gates
   subroutine unc_write_shp_gate()
      use fm_external_forcings_data
      use network_data, only: kn, xk, yk
      use m_flowgeom, only: ln2lne
      use m_strucs, only: generalstruc
      use m_structures, only: gates
      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, j, n, ishape, igen, Lf, La, L, L0, k, k1, k2
      character(len=lencharattr) :: filename, objectid
      character(len=4) :: lenobj_loc
      integer :: id_objectid, id_silllev, id_sillwidth, id_openwidth, id_loweredgel, id_doorheight, &
                 id_effwu, id_efflowere, id_flowlinknr
      double precision :: tmp_x(2), tmp_y(2)
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for gates.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for gates for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpgate')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 9 dbf fields: ObjectID, SILLLEV, SILLWIDTH, OPENWIDTH, LOWEREDGEL, DOORHEIGHT, EFF_WU, EFF_LOWERE, FLOWLINKNR
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_silllev = dbfaddfield(shphandle, 'CRESTLEV', ftdouble, 20, 8)
      if (id_silllev /= 1) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "CRESTLEV" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_sillwidth = dbfaddfield(shphandle, 'CRESTWIDTH', ftdouble, 20, 8)
      if (id_sillwidth /= 2) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "CRESTWIDTH" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_openwidth = dbfaddfield(shphandle, 'OPENWIDTH', ftdouble, 20, 8)
      if (id_openwidth /= 3) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "OPENWIDTH" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_loweredgel = dbfaddfield(shphandle, 'LOWEREDGEL', ftdouble, 20, 8)
      if (id_loweredgel /= 4) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "LOWEREDGEL" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_doorheight = dbfaddfield(shphandle, 'GATEHEIGHT', ftdouble, 20, 8)
      if (id_doorheight /= 5) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "GATEHEIGHT" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_effwu = dbfaddfield(shphandle, 'EFF_WU', ftdouble, 20, 8)
      if (id_effwu /= 6) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "EFF_WU" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_efflowere = dbfaddfield(shphandle, 'EFF_LOWERE', ftdouble, 20, 8)
      if (id_efflowere /= 7) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "EFF_LOWERE" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_flowlinknr = dbfaddfield(shphandle, 'FLOWLINKNR', ftinteger, 10, 0)
      if (id_flowlinknr /= 8) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "FLOWLINKNR" to shape file '''//trim(filename)//'''.')
         return
      end if

      do n = 1, ngategen
         igen = gate2cgen(n)
         !call mess(LEVEL_INFO, 'SHAPEFILE: Adding shapes for gate: '''//trim(cgen_ids(igen))//'''.')

      !! add shapes
         i = 0
         do L = L1cgensg(igen), L2cgensg(igen)
            write (lenobj_loc, '(I4.4)') i
            objectid = trim(cgen_ids(igen))//'_'//lenobj_loc
            !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

            ! create a shape object with the "simple" method, for each shape 2 components are added x, y
            Lf = kcgen(3, L)
            La = abs(Lf)
            k = ln2lne(La) ! netnode
            k1 = kn(1, k)
            k2 = kn(2, k)
            tmp_x(1) = xk(k1); tmp_x(2) = xk(k2)
            tmp_y(1) = yk(k1); tmp_y(2) = yk(k2)
            shpobj = shpcreatesimpleobject(tshp, 2, tmp_x, tmp_y)

            ! write the shape object to the shapefile object as i-th element, -1 = append
            ishape = shpwriteobject(shphandle, -1, shpobj)
            if (ishape == -1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
               return
            end if

            ! destroy the shape object to avoid memory leaks
            call shpdestroyobject(shpobj)

            ! write the attributes of different types for the i-th shape object to the shapefile object
            ! write ObjectID
            j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write CRESTLEV
            if (allocated(zcgen)) then
               j = dbfwriteattribute(shphandle, ishape, id_silllev, zcgen((igen - 1) * 3 + 1))
            end if

            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "CRESTLEV" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write CRESTWIDTH
            if (allocated(gates)) then
               j = dbfwriteattribute(shphandle, ishape, id_sillwidth, min(1d10, gates(n)%sill_width))
            end if

            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "CRESTWIDTH" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write OPENWIDTH
            if (allocated(zcgen)) then
               j = dbfwriteattribute(shphandle, ishape, id_openwidth, zcgen((igen - 1) * 3 + 3))
            end if

            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "OPENWIDTH" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write LOWEREDGEL
            if (allocated(zcgen)) then
               j = dbfwriteattribute(shphandle, ishape, id_loweredgel, zcgen((igen - 1) * 3 + 2))
            end if

            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "LOWEREDGEL" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write GATEHEIGHT
            if (allocated(gates)) then
               j = dbfwriteattribute(shphandle, ishape, id_doorheight, gates(n)%door_height)
            end if

            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "GATEHEIGHT" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write EFF_WU
            L0 = L - L1cgensg(igen) + 1
            if (allocated(generalstruc)) then
               j = dbfwriteattribute(shphandle, ishape, id_effwu, generalstruc(igen)%widthcenteronlink(L0))
            end if

            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "EFF_WU" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write EFF_LOWERE
            if (allocated(generalstruc)) then
               j = dbfwriteattribute(shphandle, ishape, id_efflowere, generalstruc(igen)%gateheightonlink(L0))
            end if

            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "EFF_LOWERE" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write flowlink nr.
            j = dbfwriteattribute(shphandle, ishape, id_flowlinknr, Lf)
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "FLOWLINKNR" to shape'''//trim(objectid)//'''.')
               return
            end if

            i = i + 1
         end do
      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_gate

!> Write a shape file for embankments
   subroutine unc_write_shp_emb()
      use m_sobekdfm
      use fm_external_forcings_data
      use network_data, only: kn, xk, yk
      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, j, ii, k1, k2, L, ishape, Lb, istart
      character(len=lencharattr) :: filename, objectid, ebmname_loc
      integer :: id_objectid, id_crestlev
      double precision :: tmp_x(2), tmp_y(2)
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for embankments.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for embankments for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpemb')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 2 dbf fields: ObjectID, CRESTLEV
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_crestlev = dbfaddfield(shphandle, 'CRESTLEV', ftdouble, 20, 8)
      if (id_crestlev /= 1) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "CRESTLEV" to shape file '''//trim(filename)//'''.')
         return
      end if

      do i = 1, nopenbndsect
         if (openbndtype(i) == IBNDTP_1D2D) then
            !call mess(LEVEL_INFO, 'SHAPEFILE: Adding shapes for embankment: '''//trim(openbndname(i))//'''.')
            if (i == 1) then
               istart = 0
            else
               istart = nopenbndlin(i - 1)
            end if

            ! on each open boundary
            ii = 0
            do Lb = istart + 1, nopenbndlin(i) ! only the flow links for this particular bnd *segment*
               write (ebmname_loc, '(I4.4)') ii
               objectid = trim(openbndname(i))//'_'//ebmname_loc
               !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

               L = openbndlin(Lb) ! Net link
               k1 = kn(1, L)
               k2 = kn(2, L)
               tmp_x(1) = xk(k1); tmp_x(2) = xk(k2)
               tmp_y(1) = yk(k1); tmp_y(2) = yk(k2)
               shpobj = shpcreatesimpleobject(tshp, 2, tmp_x, tmp_y)

               ! write the shape object to the shapefile object as i-th element, -1 = append
               ishape = shpwriteobject(shphandle, -1, shpobj)
               if (ishape == -1) then
                  call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
                  return
               end if

               ! destroy the shape object to avoid memory leaks
               call shpdestroyobject(shpobj)

               ! write the attributes of different types for the i-th shape object to the shapefile object
               ! write ObjectID
               j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
               if (j /= 1) then
                  call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
                  return
               end if

               ! write CRESTLEV
               if (allocated(zcrest1d2d)) then
                  j = dbfwriteattribute(shphandle, ishape, id_crestlev, zcrest1d2d(Lb))
               end if

               if (j /= 1) then
                  call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "CRESTLEV" to shape'''//trim(objectid)//'''.')
                  return
               end if

               ii = ii + 1
            end do
         end if
      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_emb

!> Write a shape file for fixed weirs
   subroutine unc_write_shp_fxw()
      use fm_external_forcings_data
      use network_data, only: kn, xk, yk
      use m_flowgeom, only: ln2lne, iadv, bob, wu
      use m_fixedweirs
      use m_polygon, only: iweirt
      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, j, Lf, La, k, k1, k2, ii, nshp, ishape
      character(len=lencharattr) :: filename, objectid
      character(len=6) :: lenobj_loc
      integer :: id_objectid, id_crestlev, id_crestlen, id_sillhl, id_sillhr, id_taludslpl, id_taludslpr, &
                 id_vegcoef, id_weirtype, id_advtype, id_effwu, id_flowlinknr
      double precision :: tmp_x(2), tmp_y(2)
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for fixed weirs.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for fixed weirs for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpfxw')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 12 dbf fields: ObjectID, CRESTLEV, CRESTLEN, SILLH_L, SILLH_R, TALUDSLP_L, TALUDSLP_R, VEGCOEF, &
      ! WEIRTYPE, ADVTYPE, FLOWLINKNR
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_crestlev = dbfaddfield(shphandle, 'CRESTLEV', ftdouble, 20, 8)
      if (id_crestlev /= 1) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "CRESTLEV" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_crestlen = dbfaddfield(shphandle, 'CRESTLEN', ftdouble, 20, 8)
      if (id_crestlen /= 2) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "CRESTLEN" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_sillhl = dbfaddfield(shphandle, 'SILLH_L', ftdouble, 20, 8)
      if (id_sillhl /= 3) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "SILLH_L" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_sillhr = dbfaddfield(shphandle, 'SILLH_R', ftdouble, 20, 8)
      if (id_sillhr /= 4) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "SILLH_R" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_taludslpl = dbfaddfield(shphandle, 'TALUDSLP_L', ftdouble, 20, 8)
      if (id_taludslpl /= 5) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "TALUDSLP_L" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_taludslpr = dbfaddfield(shphandle, 'TALUDSLP_R', ftdouble, 20, 8)
      if (id_taludslpr /= 6) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "TALUDSLP_R" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_vegcoef = dbfaddfield(shphandle, 'VEGCOEF', ftdouble, 20, 8)
      if (id_vegcoef /= 7) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "VEGCOEF" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_weirtype = dbfaddfield(shphandle, 'WEIRTYPE', ftinteger, 4, 0)
      if (id_weirtype /= 8) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "WEIRTYPE" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_advtype = dbfaddfield(shphandle, 'ADVTYPE', ftinteger, 2, 0)
      if (id_advtype /= 9) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ADVTYPE" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_effwu = dbfaddfield(shphandle, 'EFF_WU', ftdouble, 20, 8)
      if (id_effwu /= 10) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "EFF_WU" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_flowlinknr = dbfaddfield(shphandle, 'FLOWLINKNR', ftinteger, 10, 0)
      if (id_flowlinknr /= 11) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "FLOWLINKNR" to shape file '''//trim(filename)//'''.')
         return
      end if

      nshp = nfxw ! Nr. shapes
      ! add nshp shapes
      do i = 0, nshp - 1
         ii = i + 1
         write (lenobj_loc, '(I6.6)') i
         objectid = 'Fixed_weir'//'_'//lenobj_loc
         !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

         ! create a shape object with the "simple" method, for each shape 2 components are added x, y
         Lf = lnfxw(ii)
         La = abs(Lf)
         k = ln2lne(La) ! netnode
         k1 = kn(1, k)
         k2 = kn(2, k)
         tmp_x(1) = xk(k1); tmp_x(2) = xk(k2)
         tmp_y(1) = yk(k1); tmp_y(2) = yk(k2)
         shpobj = shpcreatesimpleobject(tshp, 2, tmp_x, tmp_y)

         ! write the shape object to the shapefile object as i-th element, -1 = append
         ishape = shpwriteobject(shphandle, -1, shpobj)
         if (ishape == -1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
            return
         end if

         ! destroy the shape object to avoid memory leaks
         call shpdestroyobject(shpobj)

         ! write the attributes of different types for the i-th shape object to the shapefile object
         ! write ObjectID
         j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write CRESTLEV
         if (allocated(bob)) then
            j = dbfwriteattribute(shphandle, ishape, id_crestlev, bob(1, La))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "CRESTLEV" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write CRESTLEN
         if (allocated(crestlxw)) then
            j = dbfwriteattribute(shphandle, ishape, id_crestlen, crestlxw(ii))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "CRESTLEN" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write SILLH_L
         if (allocated(shlxw)) then
            j = dbfwriteattribute(shphandle, ishape, id_sillhl, shlxw(ii))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "SILLH_L" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write SILLH_R
         if (allocated(shrxw)) then
            j = dbfwriteattribute(shphandle, ishape, id_sillhr, shrxw(ii))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "SILLH_R" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write TALUDSLP_L
         if (allocated(taludlxw)) then
            j = dbfwriteattribute(shphandle, ishape, id_taludslpl, taludlxw(ii))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "TALUDSLP_L" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write TALUDSLP_R
         if (allocated(taludrxw)) then
            j = dbfwriteattribute(shphandle, ishape, id_taludslpr, taludrxw(ii))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "TALUDSLP_R" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write VEGCOEF
         if (allocated(vegxw)) then
            j = dbfwriteattribute(shphandle, ishape, id_vegcoef, vegxw(ii))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "VEGCOEF" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write WEIRTYPE
         if (allocated(iweirtxw)) then
            j = dbfwriteattribute(shphandle, ishape, id_weirtype, iweirtxw(ii))
         end if
         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "WEIRTYPE" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write ADVTYPE
         if (allocated(iadv)) then
            j = dbfwriteattribute(shphandle, ishape, id_advtype, iadv(La))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ADVTYPE" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write EFF_WU
         if (allocated(wu)) then
            j = dbfwriteattribute(shphandle, ishape, id_effwu, wu(La))
         end if

         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "EFF_WU" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write FLOWLINKNR
         j = dbfwriteattribute(shphandle, ishape, id_flowlinknr, Lf)
         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "FLOWLINKNR" to shape'''//trim(objectid)//'''.')
            return
         end if

      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_fxw

!> Write a shape file for source-sinks
   subroutine unc_write_shp_src()
      use fm_external_forcings_data, only: ksrc, numsrc, xsrc, ysrc, nxsrc, srcname, arsrc, qstss
      use m_flowgeom, only: xz, yz
      use m_transportdata, only: NUMCONST
      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, j, k1, k2, ishape, maxnr
      character(len=lencharattr) :: filename, objectid
      integer :: id_objectid, id_area, id_origxsnk, id_origysnk, id_origxsrc, id_origysrc
      double precision :: tmp_x(2), tmp_y(2), snkx, snky, srcx, srcy
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for source-sinks.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for source-sinks for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpsrc')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 6 dbf fields: ObjectID, AREA, ORIGXSNK, ORIGYSNK, ORIGXSRC, ORIGYSRC
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_area = dbfaddfield(shphandle, 'AREA', ftdouble, 20, 8)
      if (id_area /= 1) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "AREA" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_origxsnk = dbfaddfield(shphandle, 'ORIGXSNK', ftdouble, 20, 8)
      if (id_origxsnk /= 2) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ORIGXSNK" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_origysnk = dbfaddfield(shphandle, 'ORIGYSNK', ftdouble, 20, 8)
      if (id_origysnk /= 3) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ORIGYSNK" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_origxsrc = dbfaddfield(shphandle, 'ORIGXSRC', ftdouble, 20, 8)
      if (id_origxsrc /= 4) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ORIGXSRC" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_origysrc = dbfaddfield(shphandle, 'ORIGYSRC', ftdouble, 20, 8)
      if (id_origysrc /= 5) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ORIGYSRC" to shape file '''//trim(filename)//'''.')
         return
      end if

      do i = 1, numsrc
         objectid = srcname(i)
         !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

         ! create a shape object with the "simple" method, for each shape 2 components are added x, y
         k1 = ksrc(1, i) ! flownode
         k2 = ksrc(4, i)
         if (k1 <= 0 .and. k2 <= 0) then ! if both points are not in the domain
            cycle
         else
            maxnr = nxsrc(i)
            if (k1 > 0) then
               tmp_x(1) = xz(k1); tmp_y(1) = yz(k1)
            else ! if this node is not in the model domain, then use the original coordinate
               tmp_x(1) = xsrc(i, 1); tmp_y(1) = ysrc(i, 1)
            end if
            if (k2 > 0) then
               tmp_x(2) = xz(k2); tmp_y(2) = yz(k2)
            else ! if this node is not in the model domain, then use the original coordinate
               tmp_x(2) = xsrc(i, maxnr); tmp_y(2) = ysrc(i, maxnr)
            end if
            shpobj = shpcreatesimpleobject(tshp, 2, tmp_x, tmp_y)

            ! write the shape object to the shapefile object as i-th element, -1 = append
            ishape = shpwriteobject(shphandle, -1, shpobj)
            if (ishape == -1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
               return
            end if

            ! destroy the shape object to avoid memory leaks
            call shpdestroyobject(shpobj)

            ! write the attributes of different types for the i-th shape object to the shapefile object
            ! write ObjectID
            j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write area
            if (allocated(arsrc)) then
               j = dbfwriteattribute(shphandle, ishape, id_area, arsrc(i))
            end if

            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "AREA" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! determine source and sink points
            if (qstss((NUMCONST + 1) * (i - 1) + 1) > 0) then
               snkx = xsrc(i, 1)
               snky = ysrc(i, 1)
               srcx = xsrc(i, maxnr)
               srcy = ysrc(i, maxnr)
            else
               snkx = xsrc(i, maxnr)
               snky = ysrc(i, maxnr)
               srcx = xsrc(i, 1)
               srcy = ysrc(i, 1)
            end if
            ! write ORIGXSNK
            j = dbfwriteattribute(shphandle, ishape, id_origxsnk, snkx)
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ORIGXSNK" to shape'''//trim(objectid)//'''.')
               return
            end if
            ! write ORIGYSNK
            j = dbfwriteattribute(shphandle, ishape, id_origysnk, snky)
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ORIGYSNK" to shape'''//trim(objectid)//'''.')
               return
            end if
            ! write ORIGXSRC
            j = dbfwriteattribute(shphandle, ishape, id_origxsrc, srcx)
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ORIGXSRC" to shape'''//trim(objectid)//'''.')
               return
            end if
            ! write ORIGYSRC
            j = dbfwriteattribute(shphandle, ishape, id_origysrc, srcy)
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ORIGYSRC" to shape'''//trim(objectid)//'''.')
               return
            end if
         end if
      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_src

!> Write a shape file for pumps
! =================================================================================================
! =================================================================================================
   subroutine unc_write_shp_pump()
      use fm_external_forcings_data
      use network_data, only: kn, xk, yk
      use m_flowgeom, only: ln2lne
      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, j, n, ishape, Lf, La, L, k, k1, k2
      character(len=lencharattr) :: filename, objectid
      character(len=4) :: lenobj_loc
      integer :: id_objectid, id_flowlinknr
      double precision :: tmp_x(2), tmp_y(2)
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for pumps.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for pumps for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shppump')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 2 dbf fields: ObjectID, FLOWLINKNR
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_flowlinknr = dbfaddfield(shphandle, 'FLOWLINKNR', ftinteger, 10, 0)
      if (id_flowlinknr /= 1) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "FLOWLINKNR" to shape file '''//trim(filename)//'''.')
         return
      end if

      do n = 1, npumpsg
         i = 0
         do L = L1pumpsg(n), L2pumpsg(n)
            write (lenobj_loc, '(I4.4)') i
            objectid = 'pump_'//lenobj_loc
            !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

            ! create a shape object with the "simple" method, for each shape 2 components are added x, y
            Lf = kpump(3, L)
            La = abs(Lf)
            k = ln2lne(La) ! netnode
            k1 = kn(1, k)
            k2 = kn(2, k)
            tmp_x(1) = xk(k1); tmp_x(2) = xk(k2)
            tmp_y(1) = yk(k1); tmp_y(2) = yk(k2)
            shpobj = shpcreatesimpleobject(tshp, 2, tmp_x, tmp_y)

            ! write the shape object to the shapefile object as i-th element, -1 = append
            ishape = shpwriteobject(shphandle, -1, shpobj)
            if (ishape == -1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
               return
            end if

            ! destroy the shape object to avoid memory leaks
            call shpdestroyobject(shpobj)

            ! write the attributes of different types for the i-th shape object to the shapefile object
            ! write ObjectID
            j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write flowlink nr.
            j = dbfwriteattribute(shphandle, ishape, id_flowlinknr, Lf)
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "FLOWLINKNR" to shape'''//trim(objectid)//'''.')
               return
            end if

            i = i + 1
         end do
      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_pump

!> Write a shape file for dry areas
! =================================================================================================
! =================================================================================================
   subroutine unc_write_shp_dry()
      use fm_external_forcings_data, only: nDryLinks, kdryarea
      use network_data, only: kn, xk, yk
      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: j, ishape, L, k, k1, k2
      character(len=lencharattr) :: filename, objectid
      character(len=6) :: lenobj_loc
      integer :: id_objectid, id_flowlinknr, id_linktype
      double precision :: tmp_x(2), tmp_y(2)
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for dry areas.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for dry areas for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpdry')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 3 dbf fields: ObjectID, FLOWLINKNR, LINKTYPE
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_flowlinknr = dbfaddfield(shphandle, 'NETLINKNR', ftinteger, 10, 0)
      if (id_flowlinknr /= 1) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "NETLINKNR" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_linktype = dbfaddfield(shphandle, 'LINKTYPE', ftstring, 7, 0)
      if (id_linktype /= 2) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "LINKTYPE" to shape file '''//trim(filename)//'''.')
         return
      end if

      do L = 1, nDryLinks
         write (lenobj_loc, '(I6.6)') L
         objectid = 'dryarea_'//lenobj_loc
         !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

         ! create a shape object with the "simple" method, for each shape 2 components are added x, y
         k = kdryarea(L)
         k1 = kn(1, k)
         k2 = kn(2, k)
         tmp_x(1) = xk(k1); tmp_x(2) = xk(k2)
         tmp_y(1) = yk(k1); tmp_y(2) = yk(k2)
         shpobj = shpcreatesimpleobject(tshp, 2, tmp_x, tmp_y)

         ! write the shape object to the shapefile object as i-th element, -1 = append
         ishape = shpwriteobject(shphandle, -1, shpobj)
         if (ishape == -1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
            return
         end if

         ! destroy the shape object to avoid memory leaks
         call shpdestroyobject(shpobj)

         ! write the attributes of different types for the i-th shape object to the shapefile object
         ! write ObjectID
         j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write flowlink nr.
         j = dbfwriteattribute(shphandle, ishape, id_flowlinknr, k)
         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "NETLINKNR" to shape'''//trim(objectid)//'''.')
            return
         end if

         ! write LINKTYPE
         j = dbfwriteattribute(shphandle, ishape, id_linktype, 'netlink')
         if (j /= 1) then
            call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "LINKTYPE" to shape'''//trim(objectid)//'''.')
            return
         end if
      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_dry

!> Write a shape file for general structures
! =================================================================================================
! =================================================================================================
   subroutine unc_write_shp_genstruc()
      use fm_external_forcings_data
      use network_data, only: kn, xk, yk
      use m_flowgeom, only: ln2lne, wu
      use m_strucs, only: generalstruc
      use m_structures, only: valgenstru
      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, j, n, ishape, igen, Lf, La, L, k, k1, k2
      character(len=lencharattr) :: filename, objectid
      character(len=4) :: lenobj_loc
      integer :: id_objectid, id_crestlev, id_crestwid, id_gateheight, id_doorheight, &
                 id_openwidth, id_effwu, id_flowlinknr
      integer :: checkerror
      double precision :: tmp_x(2), tmp_y(2)

      checkerror = 0
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for general structures.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for general structures for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpgenstruc')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 8 dbf fields: ObjectID, CRESTLEV, CRESTWID, GATEHEIGHT(LOWEREDGEL), DOORHEIGHT, OPENWIDTH, EFF_WU, FLOWLINKNR
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= checkerror) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      checkerror = checkerror + 1
      id_crestlev = dbfaddfield(shphandle, 'CRESTLEV', ftdouble, 20, 8)
      if (id_crestlev /= checkerror) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "CRESTLEV" to shape file '''//trim(filename)//'''.')
         return
      end if

      checkerror = checkerror + 1
      id_crestwid = dbfaddfield(shphandle, 'CRESTWIDTH', ftdouble, 20, 8)
      if (id_crestwid /= checkerror) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "CRESTWIDTH" to shape file '''//trim(filename)//'''.')
         return
      end if

      checkerror = checkerror + 1
      id_gateheight = dbfaddfield(shphandle, 'LOWEREDGEL', ftdouble, 20, 8)
      if (id_gateheight /= checkerror) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "LOWEREDGEL" to shape file '''//trim(filename)//'''.')
         return
      end if

      checkerror = checkerror + 1
      id_doorheight = dbfaddfield(shphandle, 'GATEHEIGHT', ftdouble, 20, 8)
      if (id_doorheight /= checkerror) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "GATEHEIGHT" to shape file '''//trim(filename)//'''.')
         return
      end if

      checkerror = checkerror + 1
      id_openwidth = dbfaddfield(shphandle, 'OPENWIDTH', ftdouble, 20, 8)
      if (id_openwidth /= checkerror) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "OPENWIDTH" to shape file '''//trim(filename)//'''.')
         return
      end if

      checkerror = checkerror + 1
      id_effwu = dbfaddfield(shphandle, 'EFF_WU', ftdouble, 20, 8)
      if (id_effwu /= checkerror) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "EFF_WU" to shape file '''//trim(filename)//'''.')
         return
      end if

      checkerror = checkerror + 1
      id_flowlinknr = dbfaddfield(shphandle, 'FLOWLINKNR', ftinteger, 10, 0)
      if (id_flowlinknr /= checkerror) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "FLOWLINKNR" to shape file '''//trim(filename)//'''.')
         return
      end if

      do n = 1, ngenstru
         igen = genstru2cgen(n)
         call mess(LEVEL_INFO, 'SHAPEFILE: Adding shapes for general structure: '''//trim(cgen_ids(igen))//'''.')

         ! add shapes
         i = 0
         do L = L1cgensg(igen), L2cgensg(igen)
            write (lenobj_loc, '(I4.4)') i
            objectid = trim(cgen_ids(igen))//'_'//lenobj_loc
            call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

            ! create a shape object with the "simple" method, for each shape 2 components are added x, y
            Lf = kcgen(3, L)
            La = abs(Lf)
            k = ln2lne(La) ! netnode
            k1 = kn(1, k)
            k2 = kn(2, k)
            tmp_x(1) = xk(k1); tmp_x(2) = xk(k2)
            tmp_y(1) = yk(k1); tmp_y(2) = yk(k2)
            shpobj = shpcreatesimpleobject(tshp, 2, tmp_x, tmp_y)

            ! write the shape object to the shapefile object as i-th element, -1 = append
            ishape = shpwriteobject(shphandle, -1, shpobj)
            if (ishape == -1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
               return
            end if

            ! destroy the shape object to avoid memory leaks
            call shpdestroyobject(shpobj)

            ! write the attributes of different types for the i-th shape object to the shapefile object
            ! write ObjectID
            j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write CRESTLEV
            if (allocated(generalstruc)) then
               j = dbfwriteattribute(shphandle, ishape, id_crestlev, generalstruc(igen)%levelcenter)
            end if
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "CRESTLEV" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write CRESTWIDTH
            if (allocated(generalstruc)) then
               j = dbfwriteattribute(shphandle, ishape, id_crestwid, generalstruc(igen)%widthcenter)
            end if
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "CRESTWIDTH" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write LOWEREDGEL
            if (allocated(valgenstru)) then
               j = dbfwriteattribute(shphandle, ishape, id_gateheight, valgenstru(7, n))
            end if
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "LOWEREDGEL" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write GATEHEIGHT
            if (allocated(generalstruc)) then
               j = dbfwriteattribute(shphandle, ishape, id_doorheight, generalstruc(igen)%gatedoorheight)
            end if
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "GATEHEIGHT" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write OPENWIDTH
            if (allocated(valgenstru)) then
               j = dbfwriteattribute(shphandle, ishape, id_openwidth, valgenstru(6, n))
            end if
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "OPENWIDTH" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write EFF_WU
            if (allocated(wu)) then
               j = dbfwriteattribute(shphandle, ishape, id_effwu, wu(La))
            end if
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "EFF_WU" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write flowlink nr.
            j = dbfwriteattribute(shphandle, ishape, id_flowlinknr, Lf)
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "FLOWLINKNR" to shape'''//trim(objectid)//'''.')
               return
            end if

            i = i + 1
         end do
      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_genstruc

!> Write a shape file for dam breaks
   subroutine unc_write_shp_dambreak()
      use fm_external_forcings_data, only: ndambreaksignals, dambreak_ids, L1dambreaksg, L2dambreaksg, kdambreak
      use network_data, only: kn, xk, yk
      use m_flowgeom, only: ln2lne
      implicit none

      integer, parameter :: lencharattr = 256, tshp = shpt_arc ! arcs (Polylines, possible in parts)
      type(shpfileobject) :: shphandle
      type(shpobject) :: shpobj
      integer :: i, ishape, j, k, k1, k2, L, La, Lf, n
      character(len=lencharattr) :: filename, objectid
      character(len=6) :: lenobj_loc
      integer :: id_objectid, id_flowlinknr
      double precision :: tmp_x(2), tmp_y(2)

      if (jampi == 0) then
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for dam breaks.')
      else
         call mess(LEVEL_INFO, 'SHAPEFILE: Writing a shape file for dam breaks for subdomain:', my_rank)
      end if

      ! create a new shapefile object with data of type tshp and associate it to a file, filename does not include extension
      filename = defaultFilename('shpdambreak')
      shphandle = shpcreate(trim(filename), tshp)
      ! error check
      if (shpfileisnull(shphandle) .or. dbffileisnull(shphandle)) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not open shape file '''//trim(filename)//''' for writing.')
         return
      end if

      ! add 2 dbf fields: ObjectID, FLOWLINKNR
      id_objectid = dbfaddfield(shphandle, 'ObjectID', ftstring, lencharattr, 0)
      if (id_objectid /= 0) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "ObjectID" to shape file '''//trim(filename)//'''.')
         return
      end if

      id_flowlinknr = dbfaddfield(shphandle, 'FLOWLINKNR', ftinteger, 10, 0)
      if (id_flowlinknr /= 1) then
         call mess(LEVEL_ERROR, 'SHAPEFILE: Could not add field "FLOWLINKNR" to shape file '''//trim(filename)//'''.')
         return
      end if

      do n = 1, ndambreaksignals

      !! add shapes
         i = 0
         do L = L1dambreaksg(n), L2dambreaksg(n)
            ! create a shape object with the "simple" method, for each shape 2 components are added x, y
            Lf = kdambreak(3, L)
            if (Lf == 0) cycle

            write (lenobj_loc, '(I4.4)') i
            objectid = trim(dambreak_ids(n))//'_'//lenobj_loc
            !call mess(LEVEL_INFO, 'SHAPEFILE: Creating shape: '''//trim(objectid)//'''.')

            La = abs(Lf)
            k = ln2lne(La) ! netnode
            k1 = kn(1, k)
            k2 = kn(2, k)
            tmp_x(1) = xk(k1); tmp_x(2) = xk(k2)
            tmp_y(1) = yk(k1); tmp_y(2) = yk(k2)
            shpobj = shpcreatesimpleobject(tshp, 2, tmp_x, tmp_y)

            ! write the shape object to the shapefile object as i-th element, -1 = append
            ishape = shpwriteobject(shphandle, -1, shpobj)
            if (ishape == -1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write '''//trim(objectid)//'''shape object to shapefile object.')
               return
            end if

            ! destroy the shape object to avoid memory leaks
            call shpdestroyobject(shpobj)

            ! write the attributes of different types for the i-th shape object to the shapefile object
            ! write ObjectID
            j = dbfwriteattribute(shphandle, ishape, id_objectid, trim(objectid))
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "ObjectID" to shape'''//trim(objectid)//'''.')
               return
            end if

            ! write flowlink nr.
            j = dbfwriteattribute(shphandle, ishape, id_flowlinknr, Lf)
            if (j /= 1) then
               call mess(LEVEL_ERROR, 'SHAPEFILE: Could not write attribute "FLOWLINKNR" to shape'''//trim(objectid)//'''.')
               return
            end if

            i = i + 1
         end do
      end do

      ! close the shapefile object
      call shpclose(shphandle)

   end subroutine unc_write_shp_dambreak
#endif

end module unstruc_shapefile
