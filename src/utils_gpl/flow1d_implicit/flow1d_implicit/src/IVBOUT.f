      subroutine IVBOUT(kode  ,ngrid  ,x     ,h    ,hmax   ,
     +                  nbran ,branch )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Kuipers
c
c Module:             IVBOUT(IVBdos OUTput of maximum water levels)
c
c Module description: The maximum water level in every grid point will
c                     be calculated
c                     At the end of the computation these will be
c                     written to file IVBDOS.MAX
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 kode              I  1 = Calculate maximum on first step and
c                             initialize.
c                         2 = Calculate maximum on next steps
c                         3 = Calculate maximum on last step and
c                             write to file.
c  2 ngrid             I  Number of grid points in network.
c  3 x(ngrid)          I  Coordinates in every grid point.
c  4 gridnm(ngrid)     I  Name of every grid point.
c  5 h(ngrid)          I  Water level in every grid point at t=n+1
c                         (TIME).
c  6 hmax(ngrid)      IO  Maximum water level in every grid point.
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: ivbout.pf,v $
c Revision 1.1  1999/03/15  14:30:15  kuipe_j
c IVBDOS
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer          kode       ,ngrid     ,nbran
      real             x(ngrid)   ,hmax(ngrid)
	double precision h(ngrid)
      integer          branch(4,nbran)
c
c     Declaration of local variables:
c
      integer          junit ,igr,itak

      if (kode .eq. 1) then
         do igr=1,ngrid
            hmax(igr) = -1.0e10
         enddo
      endif

      do igr=1,ngrid
         hmax(igr) = max(hmax(igr),real(h(igr)))
      enddo

      if (kode .eq. 3) then

c        JvG 7 juni 2001
c        Het algoritme  om het branch nummer weg te schrijven werkt niet meer
c        omdat de volgorde van de gridpunten niet meer oplopend is langs de
c        taknummers

         junit = 31
         open (junit,file='ivbdos.max')
c         itak = 1
c         do igr=1,ngrid
c            write (junit,100) igr,itak,x(igr),hmax(igr)
c            if (igr.eq.branch(4,itak)) then
c               itak = itak + 1 
c            endif
c         enddo
         do itak = 1,nbran
             do igr=branch(3,itak),branch(4,itak)
                write (junit,100) igr,itak,x(igr),hmax(igr)
             enddo
         enddo
         close (junit)
      endif

 100  format (i5,2x,i4,f8.0,f9.3)

      end
