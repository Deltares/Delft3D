subroutine reargf (lun   , x     , y     , mdim  , ndim  ,&
&mc    , nc    , spher                 )
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
!
   real    xymiss
   real    x( mdim  , ndim  )   , y( mdim  , ndim  )
   logical kw_keyword, kw_found , spher

   character*132  rec

   xymiss = 0.0
   spher = .false.
!
10 kw_keyword = .false.
   read(lun,'(a)',end = 9999,err=988) rec
   if (rec(1:1) == '*') goto 10

   L = INDEX(REC,'Coordinate System')
   if (L /= 0) then
      kw_found = .true.
      if (rec (21:29) == "Spherical") spher = .true.
   ENDIF

   L = INDEX(REC,'Missing Value')
   IF (L .NE. 0) THEN
      kw_found = .true.
      i = index(rec, '=') + 1
      read(rec(i:), *, iostat=istat) xymiss
   ENDIF

   if (kw_found) then
      kw_found = .false.
      goto 10
   endif
!
   read (rec,*) mc, nc
   if( mc.gt. mdim .or. nc.gt.ndim )then
      goto 9999
   endif
!
   read (lun,'(a)',err=977,end=988) rec
!
!---- skip part definitions
   npart = 0
   do ip = 1, 2*npart
      read (lun   ,*,err=977,end=988)
   enddo
!
   call ecrrea (x     , mc    , nc    , lun   , mdim  , ndim  )
   call ecrrea (y     , mc    , nc    , lun   , mdim  , ndim  )
   do j=1,nc
      do i=1,mc
         if (x(i,j)==xymiss .and. y(i,j)==xymiss) then
            x(i,j)=0.0
            y(i,j)=0.0
         endif
      enddo
   enddo

!
   goto 9999
!
977 write (*,*) ' *** ERROR *** while reading record rgf'
   stop
!
988 write (*,*) ' *** ERROR *** premature EOF rgf encountered'
   stop
!
9999 continue
!
   return
end
!-----------------------------------------------------------------------
!
subroutine ecrrea (x     , mc    , nc    , mrgf  , mdim  , ndim  )
!
!     lees rgf
   dimension   x (mdim,ndim)
   character*132 rec
   character*5   dummy
!
   do j=1,nc
      read(mrgf,*,err=777,end=999) dummy,dummy, (x(i,j),i=1,mc)
   enddo
   return
!
777 backspace (mrgf)
   backspace (mrgf)
   do j=1,nc
      read(mrgf,'(10x,5f12.0)',err=888,end=999) (x(i,j),i=1,mc)
   enddo
   return
!
888 write (*,*) ' *** ERROR *** while reading record rgf'
   stop
!
999 write (*,*) ' *** ERROR *** premature EOF rgf encountered'
   stop
!
   return
end
