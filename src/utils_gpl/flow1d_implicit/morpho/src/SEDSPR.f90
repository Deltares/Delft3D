subroutine sedspr (nrout ,nbran  ,ngrid ,maxtab ,ntabm ,qtot  ,&
!i2
&stotfr,bgout  ,sedinf,sdrdbf ,ntab  ,q2    ,&
&table ,dissed ,nfrac ,nfracb )
!u   &                   stot  ,bgout  ,sedinf,sdrdbf ,ntab  ,q2    ,
!u   &                   table ,dissed )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEDSPR (SEdiment Distribe Sediment PRoportional)
!
! Module description: Calculate sediment transport according to a pro-
!                     portional distribution function.
!
!                     The sediment transport will be divided over all
!                     outflowing branches (i.e. that are branches that
!                     recieve transport from the node) proportional to
!                     the discharge. After this routine SEDISE is called
!                     in case one or more branches are sedredge bran-
!                     ches.
!                     [ S-FO-002.2KV / Eq. 4.8 ]
!                     Routine SEDISE distributes the sediment transport
!                     over the left and right channel.
!
! Precondition:       The total incoming discharge and transport in the
!                     node are positive.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 bgout(3,nrout)    I  Contains info of every outflowing branch i
!                         from the current node:
!                         (1,i) = Branch number
!                         (2,i) = Grid point number
!                         (3,i) = Direction in branch:
!                                 +1 : First grid point is in node.
!                                 -1 : Last grid point is in node.
! 14 dissed(4,nbran)   O  Redistributed sediment transport at begin and
!                         end of branches. At the outflow side of the
!                         branch the calculated transports are stored.
!                         At the inflow side the redistributed trans-
!                         ports are stored.
!                         (1,i)   Transport at section 1 (main or left
!                                 channel)  at begin of branch.
!                         (2,i)   Transport at section 2 (right channel)
!                                 at begin of branch.
!                         (3,i)   Transport at section 1 (main or left
!                                 channel)  at end of branch.
!                         (4,i)   Transport at section 2 (right channel)
!                                 at end of branch.
!  4 maxtab            I  Maximum number of defined tables.
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  1 nrout             I  Number of outflowing branches from a node
! 11 ntab              P  -
!  5 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 12 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
!  6 qtot              I  Total discharge flowing towards node.
! 10 sdrdbf            P  -
!  9 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
!  7 stot              I  Total sediment transport going towards node.
! 13 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! sedise  SEdiment DIstribute SEdredge
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sedspr.pf,v $
! Revision 1.2  1995/05/30  07:07:22  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:23  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:50  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:21  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
!i1
   integer    nrout ,nbran    ,ngrid   ,maxtab ,ntabm ,nfrac, nfracb
!u    integer    nrout ,nbran    ,ngrid   ,maxtab ,ntabm
   integer    sedinf(2,nbran) ,sdrdbf(2,*),&
   &ntab  (4,maxtab),bgout (3,nrout)
!i3
   real       qtot, table (ntabm)  ,stotfr (nfrac) ,&
   &dissed(nfracb,2,nbran)
!u    real       qtot  ,stot
!u    real       q2    (ngrid)    ,table (ntabm)  ,
!u   &           dissed(4,nbran)
   double precision q2(ngrid)
!
!     Declaration of local parameters
!
!i1
   integer    ibr    ,dir     ,igr   ,sedbr   ,i     ,iside, ifrac
!u    integer    ibr    ,dir     ,igr   ,sedbr   ,i
   real       ratio  ,s
   real       dissdb (2)
!
   do 10 i = 1,nrout
!
      ibr = bgout(1,i)
      igr = bgout(2,i)
      dir = bgout(3,i)
!
      if (qtot .gt. 1.e-10) then
         ratio = abs(q2(igr)) / qtot
      else
         ratio = 1. / real(i)
      endif
!i5
      iside = (3-dir)/2
      do 5 ifrac=1,nfrac
         s                 = ratio * stotfr(ifrac) * real(dir)
         dissed(ifrac,iside,ibr) = s
5     continue
!u       s                 = ratio * stot * real(dir)
!u       dissed(2-dir,ibr) = s
!
!        Get sedredge branch number
!
      sedbr = sedinf(1,ibr)
      if (sedbr .ne. 0) then
!
!           Get total sediment and total discharge of the sedredge
!           branch.
!
         call sedise (sedbr  ,maxtab ,ntabm  ,s   ,q2(igr),&
         &sdrdbf ,ntab   ,table  ,dissdb )
!
!i2
         dissed(1,iside,ibr) = dissdb(1)
         dissed(2,iside,ibr) = dissdb(2)
!u          dissed(2-dir,ibr) = dissdb(1)
!u          dissed(3-dir,ibr) = dissdb(2)
!
      endif
10 continue
!
end
