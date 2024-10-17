subroutine IVBOUT(kode  ,ngrid  ,x     ,h    ,hmax   ,&
&nbran ,branch )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Kuipers
!
! Module:             IVBOUT(IVBdos OUTput of maximum water levels)
!
! Module description: The maximum water level in every grid point will
!                     be calculated
!                     At the end of the computation these will be
!                     written to file IVBDOS.MAX
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 kode              I  1 = Calculate maximum on first step and
!                             initialize.
!                         2 = Calculate maximum on next steps
!                         3 = Calculate maximum on last step and
!                             write to file.
!  2 ngrid             I  Number of grid points in network.
!  3 x(ngrid)          I  Coordinates in every grid point.
!  4 gridnm(ngrid)     I  Name of every grid point.
!  5 h(ngrid)          I  Water level in every grid point at t=n+1
!                         (TIME).
!  6 hmax(ngrid)      IO  Maximum water level in every grid point.
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: ivbout.pf,v $
! Revision 1.1  1999/03/15  14:30:15  kuipe_j
! IVBDOS
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer          kode       ,ngrid     ,nbran
   real             x(ngrid)   ,hmax(ngrid)
   double precision h(ngrid)
   integer          branch(4,nbran)
!
!     Declaration of local variables:
!
   integer          junit ,igr,itak

   if (kode .eq. 1) then
      do igr=1,ngrid
         hmax(igr) = -1.0e10
      enddo
   endif

   do igr=1,ngrid
      hmax(igr) = max(hmax(igr),h(igr))
   enddo

   if (kode .eq. 3) then

!        JvG 7 juni 2001
!        Het algoritme  om het branch nummer weg te schrijven werkt niet meer
!        omdat de volgorde van de gridpunten niet meer oplopend is langs de
!        taknummers

      junit = 31
      open (junit,file='ivbdos.max')
!         itak = 1
!         do igr=1,ngrid
!            write (junit,100) igr,itak,x(igr),hmax(igr)
!            if (igr.eq.branch(4,itak)) then
!               itak = itak + 1
!            endif
!         enddo
      do itak = 1,nbran
         do igr=branch(3,itak),branch(4,itak)
            write (junit,100) igr,itak,x(igr),hmax(igr)
         enddo
      enddo
      close (junit)
   endif

100 format (i5,2x,i4,f8.0,f9.3)

end
