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

!> @file unstruc_opengis.f90
!! Output Unstruc data in KML format.

!> Two types of KML output:
!!    * Unstructured grid as line network.
!!    * Depth values of unstructured grid cells.
!!
!! Line network (fast):
!! An unstructured net is exported as a set of <LineString> objects.
!! For improved performance, we traverse paths of connected net links
!! (greedily). This reduces numl links to #LineStrings by 1 or 2 orders
!! of magnitude, giving much smoother behavior in Google Earth.
!!
!! Depth grid:
!! All grid cells are exported as <LinearRing> objects.
!! Not much performance tuning is possible, since every patch has its
!! own colour (i.e. style reference).
module unstruc_opengis
   use precision
   implicit none
   private

   public :: kml_write_net
contains

!> Writes a D-Flow FM net to a new kml-file.
!! Overwrites an existing file, if present.
!!
!! Export parameters are taken from the m_kml_parameters module.
   subroutine kml_write_net(filename)

      use network_data, only: numl, numl1d, numk, xk, yk, zk, kn, tnod, nod, nmk, nump, netcell
      use m_missing, only: dmiss
      use m_sferic
      use m_kml_parameters
      use gridoperations

      character(len=*), intent(in) :: filename

      real(kind=hp), allocatable :: xloc(:), yloc(:), zloc(:)
      integer, allocatable :: lc(:)

      real(kind=hp) :: zmin, zmax, zp, half
      integer :: i, n, numls, numlt, kcur, knext, lcur, L, iloc, kn3cur, LMOD
      integer :: kmlunit, ios

      open (newunit=kmlunit, file=trim(filename), status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         return
      end if

      if (numk <= 0 .or. numl <= 0) then
         return
      end if

      call READYY('SAVE KML', 0d0)

      allocate (xloc(numl + 1), yloc(numl + 1), zloc(numl + 1), lc(numl))
      lc = 0

      call kml_write_header(kmlunit)
      call kml_write_netstyles(kmlunit)

      if (kml_jadepth == 1) then
         half = .5d0
      else
         half = 1d0
      end if

      if (kml_janet == 1) then

         LMOD = max(1, NUML / 100)

         iloc = 0
         ! Trivial loop to separate 1D and 2D links, to give them a different style
         ! in the KML-file.
         do kn3cur = 1, 2
            if (kn3cur == 1) then
               numls = 1
               numlt = numl1d
            else
               numls = numl1d + 1
               numlt = numl
            end if

            if (numlt - numls < 0) then
               cycle
            else
               write (kmlunit, '(a)') '<Placemark>'
               write (kmlunit, '(a)') '<name>FM unstructured grid</name>'
               write (kmlunit, '(a,i1,a)') '<styleUrl>#link', kn3cur, 'D</styleUrl>'
               write (kmlunit, '(a)') '<MultiGeometry>'
            end if

            ! Make sure all links are written to output file (but trace most of
            ! them as connected path in inner do-loop.
            do i = numls, numlt
               ! Check whether link was already written to file.
               if (lc(i) == 1) then
                  cycle
               end if

               kcur = kn(1, i)
               xloc(1) = xk(kcur)
               yloc(1) = yk(kcur)
               !if (zk(kcur) /= dmiss) then
               !    zloc(1) = transform_altitude(zk(kcur))
               !else
               zloc(1) = 0d0
               !end if

               kcur = kn(2, i)
               xloc(2) = xk(kcur)
               yloc(2) = yk(kcur)
               !if (zk(kcur) /= dmiss) then
               !    zloc(2) = transform_altitude(zk(kcur))
               !else
               zloc(2) = 0d0
               !end if
               iloc = 2
               lc(i) = 1
               ! We started a new path, now trace connected links as long as possible.
               do
                  if (mod(i + iloc - 2, LMOD) == 1) call READYY('SAVE KML', half * dble(i + iloc - 2) / dble(NUML))
                  lcur = 0
                  ! Find an outgoing link of current net node that wasn't yet written and has correct link type.
                  do L = 1, nmk(kcur)
                     if (lc(nod(kcur)%lin(L)) == 0 .and. (kn(3, nod(kcur)%lin(L)) == kn3cur &
                                                          .or. (kn3cur == 1 .and. kn(3, nod(kcur)%lin(L)) /= 2))) then ! allow kn3==1/3/4 for all 1D writing
                        lcur = nod(kcur)%lin(L)
                        exit
                     end if
                  end do
                  if (lcur == 0) then ! no further links in string found, leave this loop and write it.
                     exit
                  end if

                  ! lcur is new link: add it to linestring
                  iloc = iloc + 1
                  call othernode(kcur, lcur, knext)
                  xloc(iloc) = xk(knext)
                  yloc(iloc) = yk(knext)
                  !if (zk(knext) /= dmiss) then
                  !    zloc(iloc) = transform_altitude(zk(knext))
                  !else
                  zloc(iloc) = 0d0
                  !end if
                  lc(lcur) = 1
                  kcur = knext
               end do
               ! Write the traced path of net links
               call kml_write_linestring(kmlunit, xloc(1:iloc), yloc(1:iloc), zloc(1:iloc))
            end do
            write (kmlunit, '(a)') '</MultiGeometry>'
            write (kmlunit, '(a)') '</Placemark>'
         end do
         call READYY('SAVE KML', half)
      else
         half = 0d0 ! Nothing done yet, start progress bar of next (depths) step at 0.
      end if ! kml_janet == 1

      if (kml_jadepth == 1) then

         zmin = +huge(1d0)
         zmax = -huge(1d0)
         do kcur = 1, numk
            if (zk(kcur) /= dmiss) then
               zmin = min(zmin, zk(kcur))
               zmax = max(zmax, zk(kcur))
            end if
         end do
         if (kml_zmin == 0d0 .and. kml_zmax == 0d0) then
            kml_zmin = zmin
            kml_zmax = zmax
         end if

         call findcells(0)

         write (kmlunit, '(a)') '<Folder id="FM_depth_grid">'
         write (kmlunit, '(a)') '    <name>FM depth grid</name>'
         LMOD = max(1, NUML / 100)
         do n = 1, nump
            if (mod(n, LMOD) == 1) call READYY('SAVE KML', half + (1d0 - half) * dble(n) / dble(nump))

            zp = 0d0
            do i = 1, netcell(n)%n
               kcur = netcell(n)%nod(i)
               if (zk(kcur) == dmiss) then
                  zp = dmiss
                  exit
               end if
               zp = zp + zk(kcur)
            end do
            if (zp /= dmiss) then
               zp = zp / netcell(n)%n
            end if

            write (kmlunit, '(a)') '<Placemark>'
            if (zp == dmiss) then
               write (kmlunit, '(a)') '<styleUrl>#markmiss</styleUrl>'
            else
               i = max(1, min(30, floor((zp - kml_zmin) / (kml_zmax - kml_zmin) * 30) + 1))
               write (kmlunit, '(a,i2.2,a)') '<styleUrl>#mark', i, '</styleUrl>'
            end if

            write (kmlunit, '(a)') '<Polygon>'
            if (kml_jadepth3d == 1) then
               write (kmlunit, '(a)') '        <altitudeMode>absolute</altitudeMode>'
            else
               write (kmlunit, '(a)') '        <altitudeMode>clampToGround</altitudeMode>'
            end if

            write (kmlunit, '(a)') '        <outerBoundaryIs>'
            write (kmlunit, '(a)') '                <LinearRing>'
            write (kmlunit, '(a)') '                        <coordinates>'
            do i = 0, netcell(n)%n
               if (i == 0) then
                  kcur = netcell(n)%nod(netcell(n)%n)
               else
                  kcur = netcell(n)%nod(i)
               end if

               call kml_write_point_coords(kmlunit, xk(kcur), yk(kcur), transform_altitude(zk(kcur), zmin))
            end do
            write (kmlunit, '(a)') '                        </coordinates>'
            write (kmlunit, '(a)') '                </LinearRing>'
            write (kmlunit, '(a)') '        </outerBoundaryIs>'
            write (kmlunit, '(a)') '</Polygon>'
            write (kmlunit, '(a)') '</Placemark>'
         end do
         write (kmlunit, '(a)') '</Folder>'
      end if ! kml_jadepth == 1

      call kml_write_footer(kmlunit)
      close (kmlunit)

      deallocate (xloc, yloc, zloc, lc)
      call READYY('SAVE KML', -1d0)

   end subroutine kml_write_net

!!! Private routines
!> Transforms an actual altitude into an altitude that displays nicely:
!! Optionally add offset to lift bathymetry surface above the sea surface.
!! Optionally scale altitude differences by a factor (>= 5 recommended).
!! @see module m_kml_parameters
   function transform_altitude(altin, altmin) result(altout)
      use m_kml_parameters
      use m_missing
      real(kind=hp), intent(in) :: altin !< Original altitude
      real(kind=hp), intent(in) :: altmin !< Global minimal altitude (should be pre-computed at call site)
      real(kind=hp) :: altout !< Transformed altitude

      if (altin == dmiss) then
         altout = kml_dmiss
         return
      end if

      altout = kml_altfact * altin + kml_useroffset
      if (kml_jaoffsetzk == 1) then
         altout = altout - kml_altfact * altmin ! Do NOT offset, so: add the altmin back again, which was substracted in previous multiplication step.
      end if
   end function transform_altitude

!> Write a single line with coordinates of a single point into a kml file.
   subroutine kml_write_point_coords(kmlunit, lat, lon, alt)
      integer, intent(in) :: kmlunit !< Unit number of already opened KML file.
      real(kind=hp), intent(in) :: lat !< Point's latitude coordinate (deg)
      real(kind=hp), intent(in) :: lon !< Point's longitude coordinate (deg)
      real(kind=hp), intent(in) :: alt !< Point's altitude (m)

      character(len=22) :: tmpstring

      tmpstring = ' '
      write (tmpstring, '(f22.6)') lat
      write (kmlunit, '(a,a,a)', advance='no') '                ', trim(adjustl(tmpstring)), ','
      tmpstring = ' '
      write (tmpstring, '(f22.6)') lon
      write (kmlunit, '(a,a)', advance='no') trim(adjustl(tmpstring)), ','
      tmpstring = ' '
      write (tmpstring, '(f22.6)') alt
      write (kmlunit, '(a)') trim(adjustl(tmpstring))
   end subroutine kml_write_point_coords

!> Write standard KML header to file (version+namespace+Document)
   subroutine kml_write_header(kmlunit)
      integer, intent(in) :: kmlunit !< File unit number connected to output file.

      write (kmlunit, '(a)') '<?xml version="1.0" encoding="UTF-8"?>'
      write (kmlunit, '(a)') '<kml xmlns="http://www.opengis.net/kml/2.2">'
      write (kmlunit, '(a)') '<Document>'
      !AvD: TODO: name
   end subroutine kml_write_header

!> Write standard KML footer to file (Document+xml).
   subroutine kml_write_footer(kmlunit)
      integer, intent(in) :: kmlunit !< File unit number connected to output file.

      write (kmlunit, '(a)') '</Document>'
      write (kmlunit, '(a)') '</kml>'
   end subroutine kml_write_footer

!> Write a linestyle for each netlink type to file.
   subroutine kml_write_netstyles(kmlunit)
      integer, intent(in) :: kmlunit !< File unit number connected to output file.
      integer :: i
      real(kind=sp) :: r, g, b

      ! Color: aabbggrr, aa=alpha (00 to ff); bb=blue (00 to ff); gg=green (00 to ff); rr=red (00 to ff).
      write (kmlunit, '(a)') '<Style id="link2D">'
      write (kmlunit, '(a)') '        <LineStyle>'
      write (kmlunit, '(a)') '                <color>FFFFAA00</color>'
      write (kmlunit, '(a)') '        </LineStyle>'
      write (kmlunit, '(a)') '        <PolyStyle>'
      write (kmlunit, '(a)') '                <fill>0</fill>'
      write (kmlunit, '(a)') '        </PolyStyle>'
      write (kmlunit, '(a)') '</Style>'

      write (kmlunit, '(a)') '<Style id="link1D">'
      write (kmlunit, '(a)') '        <LineStyle>'
      write (kmlunit, '(a)') '                <color>FF7FFFFF</color>'
      write (kmlunit, '(a)') '                <width>5</width>'
      write (kmlunit, '(a)') '        </LineStyle>'
      write (kmlunit, '(a)') '        <PolyStyle>'
      write (kmlunit, '(a)') '                <fill>0</fill>'
      write (kmlunit, '(a)') '        </PolyStyle>'
      write (kmlunit, '(a)') '</Style>'

      write (kmlunit, '(a,i2.2,a)') '<Style id="markmiss">'
      write (kmlunit, '(a)') '        <LineStyle>'
      write (kmlunit, '(a)') '                <color>FF000000</color>'
      write (kmlunit, '(a)') '                <width>1</width>'
      write (kmlunit, '(a)') '        </LineStyle>'
      write (kmlunit, '(a)') '        <PolyStyle>'
      write (kmlunit, '(a)') '                <color>80FF00FF</color>'
      write (kmlunit, '(a)') '                <fill>1</fill>'
      write (kmlunit, '(a)') '        </PolyStyle>'
      write (kmlunit, '(a)') '</Style>'

      do i = 1, 30
         write (kmlunit, '(a,i2.2,a)') '<Style id="mark', i, '">'
         write (kmlunit, '(a)') '        <LineStyle>'
         write (kmlunit, '(a)') '                <color>FF000000</color>'
         write (kmlunit, '(a)') '                <width>1</width>'
         write (kmlunit, '(a)') '        </LineStyle>'
         write (kmlunit, '(a)') '        <PolyStyle>'
         call hvsrgb(240 - 240 * (i - 1) / 30.0, 100.0, 100.0, r, g, b)
         r = floor(r * 2.55)
         g = floor(g * 2.55)
         b = floor(b * 2.55)
         write (kmlunit, '(a,z2.2,z2.2,z2.2,z2.2,a)') '                <color>', 128, int(b), int(g), int(r), '</color>'
         write (kmlunit, '(a)') '                <fill>1</fill>'
         write (kmlunit, '(a)') '        </PolyStyle>'
         write (kmlunit, '(a)') '</Style>'
      end do
   end subroutine kml_write_netstyles

!> Write a line string (list of connected points) to file.
   subroutine kml_write_linestring(kmlunit, lat, lon, height)
      use m_kml_parameters
      implicit none
      integer, intent(in) :: kmlunit !< File unit number connected to output file.
      real(kind=hp), intent(in) :: lat(:), lon(:), height(:) !< Horizontal and vertical coordinates of all points.

      character(len=22) :: tmpstring
      integer :: i, npts
      npts = size(lat)

      write (kmlunit, '(a)') '        <LineString>'
      write (kmlunit, '(a)') '        <extrude>0</extrude>'
      write (kmlunit, '(a)') '        <tessellate>1</tessellate>'
!    write(kmlunit, '(a)') '        <altitudeMode>absolute</altitudeMode>'

      if (kml_jadepth3d == 1) then
         write (kmlunit, '(a)') '        <altitudeMode>absolute</altitudeMode>'
      else
         write (kmlunit, '(a)') '        <altitudeMode>clampToGround</altitudeMode>'
      end if

      write (kmlunit, '(a)') '        <coordinates>'

      do i = 1, npts
!        write(kmlunit, '(a,3(f12.6,a2))') '                ', lat(i), ', ', lon(i), ', ', height(i)
         tmpstring = ' '
         write (tmpstring, '(f22.6)') lat(i)
         write (kmlunit, '(a,a,a)', advance='no') '                ', trim(adjustl(tmpstring)), ','
         tmpstring = ' '
         write (tmpstring, '(f22.6)') lon(i)
         write (kmlunit, '(a,a)', advance='no') trim(adjustl(tmpstring)), ','
         tmpstring = ' '
         write (tmpstring, '(f22.6)') height(i)
         write (kmlunit, '(a)') trim(adjustl(tmpstring))
      end do

      write (kmlunit, '(a)') '        </coordinates>'
      write (kmlunit, '(a)') '        </LineString>'
   end subroutine kml_write_linestring

!-------------
!subroutine hvsrgb(h,v,s,r,g,b)
!
!   SYNOPSIS
!          HVSRGB() calculates the red, green, & blue components for a
!          color given in hue, value, & saturation values.
!
!   DESCRIPTION
!
!        real, intent=(in) :: h
!                H is the hue value in the range of 0 to 360 degrees
!
!        real, intent=(in) :: v
!                V is the "value" as a percent value from 0 to 100.
!
!        real, intent=(in) :: s
!                S is the saturation as a percent from 0 to 100.
!
!        real, intent=(out) :: r
!                R is the red component as a value of 0 to 100.
!
!        real, intent=(out) :: g
!                G is the green component as a value of 0 to 100.
!
!        real, intent=(out) :: b
!                B is the blue component as a value of 0 to 100.
!
!   DEPENDENCIES
!
!          + NONE
!
!   SEE ALSO
!          see JUCOLOR().
!
!   REFERENCES
!
!          + This is heavily based on chapter 17 of "Fundamentals of
!            Interactive Computer Graphics"; J. D. Foley and A. Van Dam.
!
!   AUTHOR
!
!          + John S. Urban
!          + Kevin Kendall
!   PUBLIC DOMAIN, taken from: http://fortranwiki.org/fortran/show/jucolor
!     _________________________________________________________________
   subroutine hvsrgb(h0, v0, s0, r, g, b)
!@(#) given hue, saturation, value calculate red, green, & blue components
!
!     given  : h as value of 0 to 360 degrees.
!     .        s and v each as a value of 0 to 100.
!     desired: r, g, and b as a value of 0 to 100.
!     this particular algorithm was taken from foley and van dam.
!
      real(kind=sp), intent(in) :: h0
      real(kind=sp), intent(in) :: v0
      real(kind=sp), intent(in) :: s0
      real(kind=sp), intent(out) :: r
      real(kind=sp), intent(out) :: g
      real(kind=sp), intent(out) :: b

      real(kind=sp) :: h, v, s
      integer :: ifloor
      real(kind=sp) :: f, p, q, t

      h = h0
      v = v0
      s = s0
      v = v / 100.0
      s = s / 100.0
      if (s == 0.0) then
         r = v
         g = v
         b = v
      end if
      if (h == 360.0) then
         h = 0.0
      end if
      h = h / 60.0
      ifloor = int(h)
      f = h - ifloor
      p = v * (1.0 - s)
      q = v * (1.0 - (s * f))
      t = v * (1.0 - (s * (1 - f)))
      if (ifloor == 0) then
         r = v
         g = t
         b = p
      else if (ifloor == 1) then
         r = q
         g = v
         b = p
      else if (ifloor == 2) then
         r = p
         g = v
         b = t
      else if (ifloor == 3) then
         r = p
         g = q
         b = v
      else if (ifloor == 4) then
         r = t
         g = p
         b = v
      else if (ifloor == 5) then
         r = v
         g = p
         b = q
      end if
      r = r * 100.0
      g = g * 100.0
      b = b * 100.0
      return
   end subroutine hvsrgb
end module unstruc_opengis

module io_openfoam
   implicit none

contains

!> Write D-Flow FM info+version as an OpenFOAM header into an ASCII file.
   subroutine foam_write_dflowfminfo(mout)
      use dflowfm_version_module
      integer, intent(in) :: mout !< File unit nr for output.

      character(len=20) :: rundat

      call datum(rundat)

      write (mout, '(a)') '/*---------------------------------------------------------------------------*\ '
      write (mout, '(a,a,a,a)') '| Generated on ', trim(rundat), repeat(' ', 79 - 16 - len_trim(rundat)), '|'
      write (mout, '(a,a,a,a)') '| ', trim(version_full), repeat(' ', max(0, 79 - 3 - len_trim(version_full))), '|'
      write (mout, '(a)') '\*---------------------------------------------------------------------------*/ '
   end subroutine foam_write_dflowfminfo

!> Writes an OpenFOAM data file header.
!!
!! The type of data should be described by the headdict dictionary,
!! according to http://www.openfoam.org/docs/user/basic-file-format.php
   subroutine foam_write_datafile_header(mout, headdict)
      use properties

      integer, intent(in) :: mout !< File unit nr for output.
      type(TREE_DATA), pointer :: headdict !< Tree structure containing dictionary entries (should be a list/flat tree)

      call foam_write_dflowfminfo(mout)

      call foam_write_dictionary(mout, 'FoamFile', headdict)

      write (mout, '(a)') '// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //'
      write (mout, '(a)') ''
      write (mout, '(a)') ''

   end subroutine foam_write_datafile_header

!> Generic writer for OpenFOAM dictionary (list of key-value pairs)
!! Used for printing the 'FoamFile' file header dictionary.
   subroutine foam_write_dictionary(mout, name, dicttree)
      use properties
      integer, intent(in) :: mout !< File unit number for output.
      character(len=*), intent(in) :: name !< name of the dictionary
      type(TREE_DATA), pointer :: dicttree !< Tree structure containing dictionary entries (should be a list/flat tree)

      character(len=1), allocatable :: lenmaxdata(:)
      logical :: dummylog
      integer :: keymaxlen

      write (mout, '(a)') name
      write (mout, '(a)') '{'

      allocate (lenmaxdata(size(transfer(123, node_value)))) ! Fit a single integer into char array (generally 4 bytes)

      ! Determine maximum key stringlength (used for prettyprinting/alignment in print_foam_dict)
      call tree_fold(dicttree, max_keylength, leaf_keylength, lenmaxdata, dummylog)
      keymaxlen = transfer(lenmaxdata, 123)

      ! Print the tree by traversing it depth-first, pass mout and lenmax by transfer into data variable.
      call tree_traverse(dicttree, print_foam_dict, transfer((/mout, keymaxlen/), node_value), dummylog)

      write (mout, '(a)') '}'

   end subroutine foam_write_dictionary

!> Prints the root of a tree (as a whitespace-separated key-value pair)
!! to be used in call to tree_traverse
   subroutine print_foam_dict(tree, data, stop)
      use properties

      type(TREE_DATA), pointer :: tree !< Tree whose root should be printed.
      character(len=1), dimension(:), intent(in) :: data !< Help data (max key length, used for alignment, and output file unit nr).
      logical, intent(inout) :: stop !< Whether to continue or stop.

      integer, dimension(2) :: inputdata
      integer :: mout
      integer :: maxkeylength
      character(len=1), dimension(:), pointer :: data_ptr
      character(len=256) :: string
      character(len=40) :: type_string
      logical :: success
      integer :: level

      inputdata = transfer(data, inputdata)
      mout = inputdata(1) !< File pointer
      maxkeylength = inputdata(2)

      level = tree_traverse_level()
      if (level == 0) return

      call tree_get_data_ptr(tree, data_ptr, type_string)
      if (associated(data_ptr)) then
         string = tree_get_name(tree)
         write (mout, '(a,a,a)', advance='no') &
            '    ', trim(string), repeat(' ', max(0, maxkeylength - len_trim(string)) + 4)
      end if

      select case (type_string)
      case ('STRING')
         string = ''
         call tree_get_data_string(tree, string, success)
         write (mout, '(a,a)') trim(string), ';'
      case default
         string = '(unknown data type)'
         write (mout, '(a,a,a,a)') '# ', trim(string), ' -- ', trim(type_string)
      end select
   end subroutine print_foam_dict

!> Writes the current network to a set of OpenFOAM polyMesh files.
   subroutine foam_write_polymesh(filename)
      use m_flowgeom
      use unstruc_files
      use properties
      character(len=*), intent(in) :: filename !< TODO: Output file names

      integer :: mfil
      integer :: L
      type(tree_data), pointer :: headdict
      character(len=16) :: strtmp

      call tree_create('FoamFile', headdict)
      call prop_set(headdict, '', 'version', '2.0')
      call prop_set(headdict, '', 'format', 'ascii')

      L = len_trim(filename)

      ! -- Step 1. points file ----------
      call newfil(mfil, filename)

      call prop_set(headdict, '', 'location', '"bla/points"')
      call prop_set(headdict, '', 'class', 'vectorField')
      call prop_set(headdict, '', 'object', 'points')

      call foam_write_datafile_header(mfil, headdict)
      call write_points_(mfil)
      call doclose(mfil)

      ! -- Step 2. faces file ----------

   contains
      subroutine write_points_(mout)
         use network_data
         integer, intent(in) :: mout !< File unit nr for output.

         integer :: k
         write (strtmp, '(i10)') numk
         write (mout, '(a)') adjustl(strtmp)
         write (mout, '(a)') '('
         do k = 1, numk
            write (mout, '(a,3(f25.16),a)') '(', xk(k), yk(k), zk(k), ')'
         end do
         write (mout, '(a)') ')'

      end subroutine write_points_

      subroutine write_faces_(mout)
         use network_data
         integer, intent(in) :: mout !< File unit nr for output.

         integer :: k
         write (strtmp, '(i10)') numk
         write (mout, '(a)') adjustl(strtmp)
         write (mout, '(a)') '('
         do k = 1, numk
            write (mout, '(a,3(f25.16),a)') '(', xk(k), yk(k), zk(k), ')'
         end do
         write (mout, '(a)') ')'

      end subroutine write_faces_
   end subroutine foam_write_polymesh

end module io_openfoam
