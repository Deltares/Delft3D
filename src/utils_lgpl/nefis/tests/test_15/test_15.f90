!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
program test15
   INTEGER ntimes, mmax, nmax, kmax, nsrc
   parameter (ntimes = 3,&
   &mmax = 9,&
   &nmax = 7,&
   &kmax = 5,&
   &nsrc = 6&
   &)

   INTEGER*4 fds
   INTEGER&
   &Clsnef,&
   &Credat,&
   &Defcel,&
   &Defelm,&
   &Defgrp,&
   &getnfv,&
   &Crenef,&
   &Putelt,&
   &Neferr
   INTEGER Getelt
   INTEGER error,&
   &idum,&
   &i, m, n, k, nt,&
   &UINDEX(3,1),&
   &usrord(5)
   real    rbuff3(mmax, nmax, kmax)
   real    rbuff2(mmax, nmax)
   real    rbuff1(mmax)
   integer ibuff2(7, nsrc)
   integer ibuff1(nsrc)

   character*8  elmtps(8)
   character*16 elmnms(8), elmunt(8), elmqty(8)
   integer      elmsiz(8), elmndm(8), elmdms(3,8)
   character*64 elmdes(8)
   integer      grpdms(5), grpord(5)

   CHARACTER*1024 errstr
   CHARACTER*255  version

   error = getnfv(version)
   write(*,*)
   write(*,*) trim(version(5:))
   write(*,*)
!
! Test to check cells with different element types
! (cell according dwqtim on comm. file)
!
!
!     Define element names
!
   elmnms(1) = 'TIMCUR'
   elmnms(2) = 'RSAL'
   elmnms(3) = 'RTEM'
   elmnms(4) = 'DICUV'
   elmnms(5) = 'DICWW'
   elmnms(6) = 'DISCUM'
   elmnms(7) = 'MNKSRC'
   elmnms(8) = 'TAUMAX'
!
!     Define element qty
!
   elmqty(1) = '1'
   elmqty(2) = '2'
   elmqty(3) = '3'
   elmqty(4) = '4'
   elmqty(5) = '5'
   elmqty(6) = '6'
   elmqty(7) = '7'
   elmqty(8) = '8'
!
!     Define element type
!
   elmtps(1) = 'INTEGER'
   elmtps(2) = 'REAL'
   elmtps(3) = 'REAL'
   elmtps(4) = 'REAL'
   elmtps(5) = 'REAL'
   elmtps(6) = 'REAL'
   elmtps(7) = 'INTEGER'
   elmtps(8) = 'REAL'
!
!     Define size of element one item
!
   elmsiz(1) = 4
   elmsiz(2) = 4
   elmsiz(3) = 4
   elmsiz(4) = 4
   elmsiz(5) = 4
   elmsiz(6) = 4
   elmsiz(7) = 4
   elmsiz(8) = 4
!
!     Define element description
!
   elmdes(1) = 'Time'
   elmdes(2) = 'Salinity'
   elmdes(3) = 'Temperature'
   elmdes(4) = 'Eddy viscosity, horizontal'
   elmdes(5) = 'Eddy diffusivity, horizontal'
   elmdes(6) = 'Cumm disachrge'
   elmdes(7) = 'Discharge location'
   elmdes(8) = 'Schuifspanning (maximale)'
!
!     Define element unity
!
   elmunt(1) = '[tscale]'
   elmunt(2) = '[ppt]'
   elmunt(3) = '[Degree]'
   elmunt(4) = '[m2/s]'
   elmunt(5) = '[m2/s]'
   elmunt(6) = '[m3]'
   elmunt(7) = '[-]'
   elmunt(8) = '[-]'
!
!     fill dimension of element
!
   elmndm(1)   = 1
   elmdms(1,1) = 1

   elmndm(2)   = 3
   elmdms(1,2) = mmax
   elmdms(2,2) = nmax
   elmdms(3,2) = kmax

   elmndm(3)   = 3
   elmdms(1,3) = mmax
   elmdms(2,3) = nmax
   elmdms(3,3) = kmax

   elmndm(4)   = 3
   elmdms(1,4) = mmax
   elmdms(2,4) = nmax
   elmdms(3,4) = kmax

   elmndm(5)   = 3
   elmdms(1,5) = mmax
   elmdms(2,5) = nmax
   elmdms(3,5) = kmax

   elmndm(6)   = 1
   elmdms(1,6) = nsrc

   elmndm(7)   = 2
   elmdms(1,7) = 7
   elmdms(2,7) = nsrc

   elmndm(8)= 2
   elmdms(1,8) = mmax
   elmdms(2,8) = nmax
!
!     group dimensions
!
   grpdms(1) = ntimes
   grpdms(1) = 0
   grpdms(2) = 1
   grpdms(3) = 1
   grpdms(4) = 1
   grpdms(5) = 1
!
!     group order
!
   grpord(1) = 1
   grpord(2) = 2
   grpord(3) = 3
   grpord(4) = 4
   grpord(5) = 5
!
!     user order
!
   usrord(1) = 1
   usrord(2) = 2
   usrord(3) = 3
   usrord(4) = 4
   usrord(5) = 5
!
!------------------------------------------------------------------
!     openen nefis files
!
   error = crenef( fds, 'data_c15.dat', 'data_c15.def', 'B', 'C' )
!
!     define elements on definition file
!
   do i=1,8
      error= Defelm( fds, elmnms(i), elmtps(i), elmsiz(i),&
      &elmqty(i), elmunt(i), elmdes(i),&
      &elmndm(i), elmdms(1,i))
      if (error .ne. 0) goto 9999
   enddo
!
   error= Defcel( fds, 'cel_1', 7, elmnms(1))
   if (error .ne. 0) goto 9999
!
   error= Defcel( fds, 'cel_2', 1, elmnms(8))
   if (error .ne. 0) goto 9999
!
   error= Defgrp( fds, 'grp_1', 'cel_1', 1, grpdms, grpord)
   if (error .ne. 0) goto 9999
!
   error= Defgrp( fds, 'grp_2', 'cel_2', 1, grpdms, grpord)
   if (error .ne. 0) goto 9999
!
   error= Credat( fds, 'dat_grp_1', 'grp_1')
   if (error .ne. 0) goto 9999
!
   error= Credat( fds, 'dat_grp_2', 'grp_2')
   if (error .ne. 0) goto 9999

   error = Clsnef( fds )
!------------------------------------------------------------------
!     write data
!
   UINDEX (3,1) = 1
   DO nt= 1, ntimes
      UINDEX (1,1) = nt
      UINDEX (2,1) = nt
!
      error = crenef( fds, 'data_c15.dat', 'data_c15.def', ' ', 'u')
      if (error .ne. 0) goto 9999

!------------------------------------------------------------------
      write(*,*) elmnms(1)
!      'TIMCUR'
      ibuff1(1) = 10*nt+1.
      error= Putelt( fds, 'dat_grp_1', elmnms(1),&
      &UINDEX, usrord, ibuff1)
      if (error .ne. 0) goto 9999

!------------------------------------------------------------------
      write(*,*) elmnms(1)
!      'TIMCUR'
      ibuff1(1) = 10*nt+2.
      error= Putelt( fds, 'dat_grp_1', elmnms(1),&
      &UINDEX, usrord, ibuff1)
      if (error .ne. 0) goto 9999

!------------------------------------------------------------------
      write(*,*) elmnms(2)
!     'RSAL'
      do m = 1, mmax
         do n = 1, nmax
            do k = 1, kmax
               rbuff3(m,n,k) =&
               &1000.*real(m)+ 100.*real(n)+10.*real(k)+real(nt)
            enddo
         enddo
      enddo
      error= Putelt( fds, 'dat_grp_1', elmnms(2),&
      &UINDEX, usrord, rbuff3)
      if (error .ne. 0) goto 9999

!------------------------------------------------------------------
      write(*,*) elmnms(3)
!     elmnms(3) =
      do m = 1, mmax
         do n = 1, nmax
            do k = 1, kmax
               rbuff3(m,n,k) =&
               &1000.*real(m)+ 100.*real(n)+10.*real(k)+real(nt)
            enddo
         enddo
      enddo
      error= Putelt( fds, 'dat_grp_1', elmnms(3),&
      &UINDEX, usrord, rbuff3)
      if (error .ne. 0) goto 9999

!------------------------------------------------------------------
      write(*,*) elmnms(4)
!     'DICUV'
      do m = 1, mmax
         do n = 1, nmax
            do k = 1, kmax
               rbuff3(m,n,k) =&
               &1000.*real(m)+ 100.*real(n)+10.*real(k)+real(nt)
            enddo
         enddo
      enddo
      error= Putelt( fds, 'dat_grp_1', elmnms(4),&
      &UINDEX, usrord, rbuff3)
      if (error .ne. 0) goto 9999

!------------------------------------------------------------------
      write(*,*) elmnms(5)
!     'DICWW'
      do m = 1, mmax
         do n = 1, nmax
            do k = 1, kmax
               rbuff3(m,n,k) =&
               &1000.*real(m)+ 100.*real(n)+10.*real(k)+real(nt)
            enddo
         enddo
      enddo
      error= Putelt( fds, 'dat_grp_1', elmnms(5),&
      &UINDEX, usrord, rbuff3)
      if (error .ne. 0) goto 9999

!------------------------------------------------------------------
      write(*,*) elmnms(6)
      do m = 1, nsrc
         rbuff1(m) = 10.*real(m)+real(nt)
      enddo
!     'DISCUM'
      error= Putelt( fds, 'dat_grp_1', elmnms(6),&
      &UINDEX, usrord, rbuff1)
      if (error .ne. 0) goto 9999

!------------------------------------------------------------------
      write(*,*) elmnms(7)
!     'MNKSRC'
      do m = 1, 7
         do n = 1, nsrc
            ibuff2(m,n) = 100*m+10*n+nt
         enddo
      enddo
      error= Putelt( fds, 'dat_grp_1', elmnms(7),&
      &UINDEX, usrord, ibuff2)
      if (error .ne. 0) goto 9999

!------------------------------------------------------------------
      write(*,*) elmnms(8)
!     'TAUMAX'
      do m = 1, mmax
         do n = 1, nmax
            rbuff2(m,n) = 0.0
            rbuff2(m,n) =&
            &1000.*real(m)+ 100.*real(n)+real(nt)
         enddo
      enddo
      error= Putelt( fds, 'dat_grp_2', elmnms(8),&
      &UINDEX, usrord, rbuff2)
      if (error .ne. 0) goto 9999
!------------------------------------------------------------------
      error = Clsnef( fds )
   enddo

!====================================================================
9999 continue
!
   error = Neferr( 0, errstr)
   write(*,'(a)') trim(errstr)

   error = Clsnef( fds )
!
END
!====================================================================
!     Convert clock time to seconds
!
subroutine clock( cpu )

   integer ihr, imin, isec, i100th
   real cpu

   ihr = 0
   imin = 0
   isec = 0
   i100th = 0
   cpu = 0.
!      CALL Gettim(ihr, imin, isec, i100th)
!      cpu = ihr*3600.0 + imin*60.0 + isec + i100th/100.0
!      call system_clock(ihr,imin)
!      cpu = ihr/real(imin)

end
