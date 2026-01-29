subroutine setost (nbran  ,ngrid  ,branch ,sedinf ,wfs   ,ws     ,&
&sedtr  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SETOST (SEdiment Total Sediment Transport)
!
! Module description: Calculate total sediment transport through grid-
!                     point.
!
!                     The total sediment transport is calculated using
!                     the transport width and the flowing widths of the
!                     sections involved.
!                     [ Doc. S-FO-002.2KV / Eq. 6.12 ]
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  4 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
!  7 sedtr(ngrid,*)    IO Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
!  5 wfs(ngrid,2)      I  Actual flow width per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
!  6 ws(ngrid)         I  Sediment transporting width for each grid
!                         point.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: setost.pf,v $
! Revision 1.2  1995/05/30  07:07:39  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:35  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:09  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:24  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nbran ,ngrid
   integer    branch(4,nbran) ,sedinf(2,nbran)
   real       wfs   (ngrid,2) ,ws    (ngrid)   ,sedtr (ngrid,*)
!
!     Declaration of local parameters
!
   integer    ibr   ,isec   ,igr
!
   do 40 ibr = 1,nbran
!
      if (sedinf(1,ibr) .ne. 0) then
!
!           Sedredge branch (ws = width of channel)
!
         do 20 igr = branch(3,ibr),branch(4,ibr)
            do 10 isec = 1, 2
               sedtr(igr,isec) = sedtr(igr,isec) * wfs(igr,isec)
10          continue
20       continue
      else
!
!           Normal branch
!
         do 30 igr = branch(3,ibr),branch(4,ibr)
!
!              if flow widht > sediment transporting width use ws
!
            if (wfs(igr,1) .gt. ws(igr)) then
               sedtr(igr,1) = sedtr(igr,1) * ws(igr)
            else
               sedtr(igr,1) = sedtr(igr,1) * wfs(igr,1)
            endif
30       continue
      endif
!
40 continue
!
end
