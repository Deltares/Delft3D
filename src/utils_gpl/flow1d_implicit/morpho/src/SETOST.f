      subroutine setost (nbran  ,ngrid  ,branch ,sedinf ,wfs   ,ws     ,
     &                   sedtr  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SETOST (SEdiment Total Sediment Transport)
c
c Module description: Calculate total sediment transport through grid-
c                     point.
c
c                     The total sediment transport is calculated using
c                     the transport width and the flowing widths of the
c                     sections involved.
c                     [ Doc. S-FO-002.2KV / Eq. 6.12 ]
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  4 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c  7 sedtr(ngrid,*)    IO Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c  5 wfs(ngrid,2)      I  Actual flow width per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c  6 ws(ngrid)         I  Sediment transporting width for each grid
c                         point.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: setost.pf,v $
c Revision 1.2  1995/05/30  07:07:39  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:35  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:09  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:24  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nbran ,ngrid
      integer    branch(4,nbran) ,sedinf(2,nbran)
      real       wfs   (ngrid,2) ,ws    (ngrid)   ,sedtr (ngrid,*)
c
c     Declaration of local parameters
c
      integer    ibr   ,isec   ,igr
c
      do 40 ibr = 1,nbran
c
         if (sedinf(1,ibr) .ne. 0) then
c
c           Sedredge branch (ws = width of channel)
c
            do 20 igr = branch(3,ibr),branch(4,ibr)
               do 10 isec = 1, 2
                  sedtr(igr,isec) = sedtr(igr,isec) * wfs(igr,isec)
   10          continue
   20       continue
         else
c
c           Normal branch
c
            do 30 igr = branch(3,ibr),branch(4,ibr)
c
c              if flow widht > sediment transporting width use ws
c
               if (wfs(igr,1) .gt. ws(igr)) then
                  sedtr(igr,1) = sedtr(igr,1) * ws(igr)
               else
                  sedtr(igr,1) = sedtr(igr,1) * wfs(igr,1)
               endif
   30       continue
         endif
c
   40 continue
c
      end
