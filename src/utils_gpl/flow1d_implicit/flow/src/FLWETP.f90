subroutine FLWETP(nbran  ,ngrid  ,branch ,typcr  ,h1     ,h      ,&
&maxlev ,nlev   ,hlev   ,wft    ,of     ,wf     ,&
&prslot ,juer   ,o      ,ker    ,theta2 ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLWETP (FLow Wetted Perimeter)
!
! Module description: In subroutine FLWETP the wetted perimeter will be
!                     computed.
!
!                     This subroutine calculates the wetted perimeter
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  5 h1(ngrid)         I  Water level in every grid point at time t(n).
!  6 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  9 hlev              P  -
! 14 juer              P  -
! 16 ker               P  -
!  7 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  8 nlev              P  -
! 15 o                 P  -
! 11 of                P  -
! 13 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 18 psltvr(7,ngrid)   P  -
! 17 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 12 wf                P  -
! 10 wft               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flcirc  FLow CIRCLe cross section
! flperi  FLow PERImeter
! flsedr  FLow SEDRedge branch
! indwgh  Compute INDex and WeiGHt factor
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flwetp.pf,v $
! Revision 1.6  1997/01/23  08:29:19  kuipe_j
! Make flow module robust
!
! Revision 1.5  1995/09/22  10:02:32  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:11:06  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.2  1993/11/26  15:31:48  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:56  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer  ngrid, maxlev, juer, ker,&
   &nbran, branch(4,nbran), typcr(nbran), nlev(ngrid)
   real     wft(ngrid,maxlev), of (ngrid,maxlev),&
   &wf(ngrid), prslot(3,nbran), o (ngrid), theta2,&
   &psltvr(7,ngrid)
   double precision hlev(ngrid,maxlev), h(ngrid), h1(ngrid)
!
!     Declaration of local variables:
!
   logical lslot
   integer ilev, ibr, i1, i2, i
   real    dummy1, dummy2
   double precision wght, hact
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Computation of
!          - actual perimeter   o (1:ngrid) at h=h(n+1/2)
!
   do 40 ibr = 1, nbran
!
!        i1 = global grid point number at node n1
!        i2 = global grid point number at node n2
!
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
      lslot = int(prslot(1,ibr)) .eq. cslena
!
      if      ( typcr(ibr) .eq. ccrtab ) then
!
!          Arbitrary tabulated cross section for this branch
!
         do 10 i = i1, i2
!             hact = (h1(i)+h(i))/2.0
            hact = theta2*h(i)+(1.-theta2)*h1(i)
!
!              Compute index (ilev) and weight factor (wght) w.r.t.
!              tables of widths.
!
            call INDWGH (ngrid  ,i      ,maxlev ,&
            &nlev   ,hlev   ,&
            &hact   ,ilev   ,wght   )
!
!              Compute of actual wetted perimeter
!
            call FLPERI (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &hact   ,ilev   ,&
            &wft    ,wf(i)  ,&
            &of     ,o(i)   ,psltvr )
10       continue

      else if ( typcr(ibr) .eq. ccrcir ) then
!
!          Circle used as cross section in this branch
!
         do 20 i = i1, i2
!
!              reflev : hlev(i,1)
!              radius : wft(i,1)
!
!              Compute of perimeter for circle cross section
!
!             call FLCIRC ((h1(i)+h(i))/2.0 ,
            call FLCIRC ((theta2*h(i)+(1.-theta2)*h1(i)) ,&
            &hlev(i,1) ,wft(i,1),juer   ,&
            &dummy1    ,dummy2  ,o(i)   ,ker    )
20       continue

      else if ( typcr(ibr) .eq. ccrsed ) then
!
!           Sedredge branch
!
         do 30 i = i1, i2
!             hact = (h1(i)+h(i))/2.0
            hact = theta2*h(i)+(1.-theta2)*h1(i)
            call FLSEDR (ibr      ,hact     ,hlev(i,1) ,hlev(i,2) ,&
            &wft(i,1) ,wft(i,2) ,juer      ,dummy1    ,&
            &dummy2   ,o(i)     ,ker       )
30       continue
      endif
40 continue
!
end
