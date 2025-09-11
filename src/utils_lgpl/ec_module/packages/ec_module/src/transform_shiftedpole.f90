!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
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
!  
!  


  module transform_poleshift
!
!     $Header$
!     Original URL: https://repos.deltares.nl/repos/simona/bo_omgeving/simona/src/waqua/waqwnd/modules/vari.f90
!     Original Revision: 5968 $, $Date: 2015-04-08 17:12:44 +0200 (Wed, 08 Apr 2015)
!
!     DESCRIPTION
!
!     subroutines provided by KNMI to calculate the rotation
!     of the wind-vector in the case of shifted pole coordinates.
!
      use precision
      implicit none
      private

!     switch for single/double precision:
      integer, parameter, public :: vp = hp

!     some constants
      real(kind=vp), private :: pi, zrad, zrho
      real(kind=vp), private, parameter :: zsame = 0.01_vp

      public :: gb2lla

      contains

!-----------------------------------------------------------------------
      subroutine gb2lla(pxold,pyold,pxnew,pynew,pdiri,k,plam1,pthe1,phi1,plam2,pthe2,phi2)
!
!     subroutine gb2lla: as gb2llc, but also calculate angle of rotation
!
!     input:
!     pxold,pyold:     arrays of longitudes,latitudes to be transformed
!     pdiri:           array of initial orientation angles
!     k:               number of gridpoints to be transformed
!     plam1,pthe1,phi1:parameters defining input grid
!     plam2,pthe2,phi2:parameters defining output grid
!                      plam: longitude of south pole
!                      pthe: latitude of south pole
!                      phi:  angle of rotation as defined by grib
!                      note: coordinates of south poles of in- and
!                            output grids must be given in the same
!                            coordinate system, e.g. geographical
!
!     output:
!     pxnew,pynew: arrays of transformed longitudes,latitudes
!     pdiri:           array of incremented orientation angles
!
!     all parameters to be in degrees,
!     output is between -180(excl) and 180. (incl)
!
!     in- and output arrays of coordinates may coincide
!
!     all comments concerning the parameter pdiri in subroutine
!     gb2sla are applicable here - in fact, gb2lla is simply a
!     copy of gb2llc with gb2slc replaced by gb2sla
!
!     it is anticipated that this routine will mainly be used
!     to transform from or to the geographical system, therefore
!     the cases with pthe1=-90 or pthe2=-90 are treated separately
!
!                                                 G.J.Cats  16 dec 88.
!
      real(kind=vp), intent(in)  :: pxold(k),pyold(k)
      real(kind=vp), intent(out) :: pxnew(k),pynew(k),pdiri(k)
      real(kind=vp), intent(in)  :: plam1,pthe1,phi1,plam2,pthe2,phi2
      integer      , intent(in)  :: k
      real(kind=vp) :: zlasp(1),zlosp(1)
      real(kind=vp) :: zlam1, zlam2, zrot
      integer       :: j

!     initialization
      pi   = 4.0_vp * atan(1.0_vp)
      zrad = 180.0_vp / pi ! 57.2957795
      zrho = pi / 180.0_vp ! 0.0174532925

!<a name="s1.">
!     1.  preliminaries
!
!     1.1 eliminate ambiguities near coordinate singularities
!
      if(pthe1.lt.-89.9_vp)then
         zlam1=0.0_vp
      else
         zlam1=plam1
      endif
      if(pthe2.lt.-89.9_vp)then
         zlam2=0.0_vp
      else
         zlam2=plam2
      endif
!<a name="s2.">
!     2.  special cases:
!
!     2.1 south poles coincide
!
      if(abs(pthe1-pthe2).lt.zsame.and. &
      (abs(zlam1-zlam2).lt.zsame .or. abs(zlam1-zlam2).gt.360.0_vp-zsame))then
         call gb2rlc(pxold,pxnew,k,phi2-phi1)
         do j=1,k
            pynew(j)=pyold(j)
         enddo
!<a name="s2.2">
!     2.2 input grid is geographical
!
      elseif(pthe1.lt.-89.9_vp)then
         call gb2rlc(pxold,pxnew,k,zlam2-phi1)
         call gb2sla(pxnew,pyold,pxnew,pynew,pdiri,k,pthe2)
         call gb2rlc(pxnew,pxnew,k,phi2)
!<a name="s2.3">
!     2.3 output grid is geographical
!
      elseif(pthe2.lt.-89.9_vp)then
         call gb2rlc(pxold,pxnew,k,180.0_vp-phi1)
         call gb2sla(pxnew,pyold,pxnew,pynew,pdiri,k,pthe1)
         zrot=phi2+180.0_vp-zlam1
         if(zrot.gt. 180.0_vp)zrot=zrot-360.0_vp
         call gb2rlc(pxnew,pxnew,k,zrot)
!<a name="s3.">
!     3.  general case
!
      else
!
!     3.1 coordinates of output grid south pole in input grid
!
         zlasp(1)=pthe2
         zlosp(1)=zlam2-zlam1
         call gb2slc(zlosp,zlasp,zlosp,zlasp,1,pthe1)
!<a name="s3.2">
!     3.2 coordinate transformation from one pole to the other
!
         call gb2rlc(pxold,pxnew,k,zlosp(1)-phi1)
         call gb2sla(pxnew,pyold,pxnew,pynew,pdiri,k,zlasp(1))
!
!     3.3 pole1 is on 180w longitude of pole2 grid; rotate back
!
         zlosp(1)=zlam1-zlam2
         zlasp(1)=pthe1
         call gb2slc(zlosp,zlasp,zlosp,zlasp,1,pthe2)
         zrot=phi2-zlosp(1)+180.0_vp
         if(zrot.gt. 180.0_vp)zrot=zrot-360.0_vp
         call gb2rlc(pxnew,pxnew,k,zrot)
!<a name="s8.">
!     8.  ready
!
      endif

      end subroutine

!-----------------------------------------------------------------------
      subroutine gb2sla(pxold,pyold,pxnew,pynew,pdiri,k,pthe)
!
!     subroutine gb2sla: as gb2slc, but also calculate angle of rotation
!
!     input:
!     pxold,pyold: arrays of longitudes,latitudes to be transformed
!     pdiri:       array of initial orientation angles
!     k:           number of gridpoints to be transformed
!     pthe:        latitude of new south pole, measured in old grid
!
!     output:
!     pxnew,pynew: arrays of transformed longitudes,latitudes
!     pdiri:       array of incremented orientation angles
!
!     all parameters to be in degrees,
!     output is between -180(excl) and 180. (incl)
!
!     in- and output arrays of coordinates may coincide
!
!     this subroutine does the same as gb2slc, but also
!     calculates changes to the angles pdiri due to the
!     angle between the coordinate axes.  use of gb2slc
!     if this angle is not required, is slightly more
!     efficient, but if the angle is required, use
!     gb2sla also for the coordinate transformation itself
!     as use of gb2sla is much more efficient than use
!     of gb2slc followed by use of gb2sla.
!
!     note that this routine calculates the changes to
!     the angles as follows:
!     pdiri on output=pdiri on input + angle between
!                     coordinate axes
!
!     if you are interested in the angle between the
!     coordinate axes only, ensure that pdiri is zero
!     on input.
!
!     the sign of the increment of pdiri is such that it
!     denotes the angle of rotation to be applied to
!     the old coordinate system to let its axes
!     coincide with the new one, where the angle is
!     measured in the old coordinate system, so pdiri is positive
!     for anti-clockwise rotation from old x-axis to new one. as a
!     consequence, if the initial value of pdiri is
!     a wind direction dd (measured conform meteorological use)
!     in the old coordinate system, the output value of
!     pdiri will be dd in the new coordinate system
!     (modulo 360). note that the meteorological use of dd
!     has effectively an opposite sign to the mathematically
!     usual way of defining angles.
!
!                                                 G.J.Cats  15 dec 88.
!
      real(kind=vp), intent(in)  :: pxold(k),pyold(k)
      real(kind=vp), intent(inout) :: pxnew(k),pynew(k),pdiri(k)
      real(kind=vp), intent(in)  :: pthe
      integer      , intent(in)  :: k
      real(kind=vp) :: zlat, zlon, zsinla,zcosla,zarg,zst,zct
      real(kind=vp) :: zcoslo,zalph, zsinlo,zdiv,zxn,zdx
      integer       :: j

      zst=-sin(pthe*zrho)
      zct=-cos(pthe*zrho)
      do j=1,k
         zlat=pyold(j)*zrho
         zlon=pxold(j)
         if(zlon.gt.180.0_vp)zlon=zlon-360.0_vp
         zsinla=sin(zlat)
         zcosla=cos(zlat)
         zcoslo=cos(zlon*zrho)
         zsinlo=sin(zlon*zrho)
         zarg=zst*zsinla+zct*zcosla*zcoslo
         zarg=max(min(zarg,1.0_vp),-1.0_vp)
         zalph=asin(zarg)
         zdiv=cos(zalph)
         if(zdiv.eq.0.0_vp)zdiv=1.0_vp
         zdiv=1.0_vp/zdiv
         pynew(j)=zalph*zrad
         zarg=(zst*zcosla*zcoslo-zct*zsinla)*zdiv
         zarg=max(min(zarg,1.0_vp),-1.0_vp)
         zxn=sign(acos(zarg),zlon*zcosla)
         pxnew(j)=zxn*zrad
         zarg=zst*zsinlo*sin(zxn)+zcoslo*zarg
         zarg=max(min(zarg,1.0_vp),-1.0_vp)
         zdx=sign(acos(zarg),-zlon*zct)
         pdiri(j)=pdiri(j)-zdx*zrad
!<a name="n10">
!     if(pdiri(j).gt. 180.)pdiri(j)=pdiri(j)-360.
!     if(pdiri(j).le.-180.)pdiri(j)=pdiri(j)+360.
      enddo

      end subroutine

!-----------------------------------------------------------------------
      subroutine gb2rlc(pxold,pxnew,k,plam)
!
!     subroutine gb2rlc: rotate regular lat/lon coordinates about axis
!
!     input:
!     pxold:       arrays of longitudes to be transformed
!     k:           number of gridpoints to be transformed
!     plam:        rotation angle
!
!     output:
!     pxnew:       arrays of transformed longitudes
!
!     all parameters to be in degrees,
!     output is between -180(excl) and 180. (incl) (provided that
!     pxold was between these limits and -360.le.plam.le.360.).
!     in and output arrays may be the same
!
!                                                 G.J.Cats  08 nov 88.
!
      real(kind=vp), intent(in)  :: pxold(k),plam
      real(kind=vp), intent(out) :: pxnew(k)
      integer      , intent(in)  :: k
      integer :: j

      if(plam.gt.0.0_vp)then
         do j=1,k
            pxnew(j)=pxold(j)-plam
            if(pxnew(j).le.-180.0_vp) pxnew(j)=pxnew(j)+360.0_vp
         enddo
      else
         do j=1,k
            pxnew(j)=pxold(j)-plam
            if(pxnew(j).gt. 180.0_vp) pxnew(j)=pxnew(j)-360.0_vp
         enddo
      endif

      end subroutine

!-----------------------------------------------------------------------
      subroutine gb2slc(pxold,pyold,pxnew,pynew,k,pthe)
!
!     subroutine gb2slc: shift pole of regular lat/lon coordinate system
!
!     input:
!     pxold,pyold: arrays of longitudes,latitudes to be transformed
!     k:           number of gridpoints to be transformed
!     pthe:        latitude of new south pole, measured in old grid
!
!     output:
!     pxnew,pynew: arrays of transformed longitudes,latitudes
!
!     all parameters to be in degrees,
!     output is between -180(excl) and 180. (incl)
!
!     in- and output arrays of coordinates may coincide
!
!                                                 G.J.Cats  07 nov 88.
!
      real(kind=vp), intent(in)  :: pxold(k),pyold(k),pthe
      real(kind=vp), intent(out) :: pxnew(k),pynew(k)
      integer      , intent(in)  :: k
      real(kind=vp) :: zst, zct,zlat,zlon,zsinla,zcosla,zcoslo,zarg
      real(kind=vp) :: zalph, zdiv
      integer       :: j

      zst=-sin(pthe*zrho)
      zct=-cos(pthe*zrho)
      do j=1,k
         zlat=pyold(j)*zrho
         zlon=pxold(j)
         if(zlon.gt.180.0_vp)zlon=zlon-360.0_vp
         zsinla=sin(zlat)
         zcosla=cos(zlat)
         zcoslo=cos(zlon*zrho)
         zarg=zst*zsinla+zct*zcosla*zcoslo
         zarg=max(min(zarg,1.0_vp),-1.0_vp)
         zalph=asin(zarg)
         zdiv=cos(zalph)
         if(zdiv.eq.0.0_vp)zdiv=1.0_vp
         zdiv=1.0_vp/zdiv
         pynew(j)=zalph*zrad
         zarg=(zst*zcosla*zcoslo-zct*zsinla)*zdiv
         zarg=max(min(zarg,1.0_vp),-1.0_vp)
         pxnew(j)=sign(acos(zarg)*zrad,zlon*zcosla)
      enddo

      end subroutine

end module transform_poleshift
