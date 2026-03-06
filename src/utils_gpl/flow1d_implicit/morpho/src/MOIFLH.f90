subroutine MOIFLH ( ibr    ,igpbou ,igpcel ,isec   ,&
&ngrid  ,nbran  ,nboun  ,nnode  ,&
&maxlev ,branch ,node   ,ibrtyp ,&
&mbdpar ,x      ,hlev   ,grid   ,&
&maxtab ,ntabm  ,ntab   ,table  ,&
&time   ,dtm    ,alphac ,h      ,&
&wf     ,wfh0   ,ws     ,wft    ,&
&afs    ,celer  ,sedtr  ,dissed ,&
&intbou ,intcel ,flwdir&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOIFLH (MOrphology Integral on First or Last halve point)
!
! Module description: Calculate integral on point 3/2 or n-1/2
!
!                     The integral I3/2 or In-1/2 is determined diffe-
!                     rently depending on the inflow or outflow situati-
!                     on. The point will be calculated as an internal
!                     point in the following cases:
!
!                     o   | adapted courant number | < 1/2
!                     o   boundary on point 1 is defined as z = f(t).
!
!                     In the call parameter list the grid point of the
!                     boundary (gpb) will be passed as well as the grid
!                     point i+1 or n-1 (gpc). Also the calculated inte-
!                     gral value Ibou from the call to MOIBOU will be
!                     passed.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 29 afs(ngrid,2)      I  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 23 alphac            I  Stability factor for bottom scheme (>1)
! 10 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 30 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
! 32 dissed(4,nbran)   I  Redistributed sediment transport at begin and
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
! 22 dtm               I  Morphology time step.
! 35 flwdir(ngrid)     I  Indicator for flow direction at each grid
!                         point      1 = positive flow
!                                    0 = zero flow
!                                   -1 = negative flow
! 16 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
! 24 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
! 15 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  1 ibr               I  Branch number
! 12 ibrtyp            I  Type of branch
!                           ccrtab (1) : tabulated branch
!                           ccrcir (2) : circle branch
!                           ccrsed (3) : sedredge branch
!  2 igpbou            I  Calculated integral value on boundary
!  3 igpcel            I  Calculated integral for first or last cell in
!                         branch
! 33 intbou            I  Integral value for begin or end point of a
!                         branch
! 34 intcel            O  Calculated integral value for first or last
!                         cel of a branch
!  4 isec              I  Section number (1 or 2)
!  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 17 maxtab            I  Maximum number of defined tables.
! 13 mbdpar(5,nboun)   I  Morphodynamic boundary conditions:
!                         (1,i) = Type of boundary condition:
!                                 cmbsft (1) : Condition S=f(t).
!                                 cmbsfq (2) : Condition S=f(Q).
!                                 cmbzft (3) : Condition z=f(t).
!                         (2,i) = Location (node number).
!                         (3,i) = Branch number that is connected.
!                         (4,i) = Table pointer for boundary table. In
!                                 case of a connected sedredge branch
!                                 the pointer will be assigned to the
!                                 left channel.
!                         (5,i) = Table pointer for boundary table. In
!                                 case of a connected sedredge branch
!                                 the pointer will be assigned to the
!                                 right channel. In other cases undefi-
!                                 ned.
!  7 nboun             I  Number of boundary nodes.
!  6 nbran             I  Number of branches.
!  5 ngrid             I  Number of grid points in network.
!  8 nnode             I  Number of nodes.
! 11 node(4,nnode)     I  Definition of nodes:
!                         (1,i) = Type of node i:
!                                 cintnd (1) : Internal node
!                                 chbou  (2) : H-boundary
!                                 cqbou  (3) : Q-boundary
!                                 cqhbou (4) : QH-boundary
!                                 chqbou (5) : HQ-boundary
!                         (2,i) = Gridpoint in case of boundary, else
!                                 undefined.
!                         (3,i) = Station number for boundary, undefined
!                                 for internal nodes:
!                                 HQ, H-boundary: station nr H-station.
!                                 QH, Q-boundary: station nr Q-station.
!                         (4,i) = Boundary number in case of boundary.
! 19 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
! 18 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 31 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
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
! 20 table             P  -
! 21 time              I  Actual time level tn+1. in sec.
! 25 wf(ngrid)         I  Actual flow width at every grid point.
! 26 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
!                         grid point.
! 28 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
! 27 ws(ngrid)         I  Sediment transporting width for each grid
!                         point.
! 14 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moiflh.pf,v $
! Revision 1.7  1998/06/11  11:47:10  kuipe_j
! Estuary special integrated
!
! Revision 1.6  1996/03/08  09:39:06  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.5  1996/03/07  10:44:15  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.4  1995/10/18  08:59:59  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:55:51  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:47  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:15  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  13:05:32  kuipe_j
! Improvement in calculation of shock celerity (exceptions)
!
! Revision 1.3  1994/11/28  08:52:34  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:42  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer    ibr    ,igpbou  ,igpcel ,isec   ,ngrid  ,nbran  ,&
   &nboun  ,nnode   ,maxtab ,ntabm  ,ibrtyp ,maxlev

   integer    branch (4,nbran),&
   &grid   (ngrid)  ,&
   &flwdir (ngrid)  ,&
   &node   (4,nnode),&
   &mbdpar (5,nboun),&
   &ntab   (4,maxtab)

   real       alphac ,intbou  ,intcel

   real       x      (ngrid),&
   &table  (ntabm),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &wf     (ngrid),&
   &wfh0   (ngrid),&
   &ws     (ngrid),&
   &wft    (ngrid,maxlev),&
   &afs    (ngrid)

   double precision time ,dtm, hlev(ngrid,maxlev), h(ngrid)

!
!     Local variables
!
   integer    ixdis ,inode ,iboun ,itab  ,indir
   logical    intern
   real       bb    ,depth ,sinp  ,sin1  ,sin2  ,dx    ,cboun ,&
   &alpcel,predic,adcour,dta   ,sedtrw,sedcel,dtms  ,&
   &rbb   ,srat, scel, sbou, ccel, cbou
!
   logical    epsequ
   external   epsequ
!
!     Include sobek constants
!
   include '..\include\sobcon.i'

   dtms = sngl(dtm)
!
!     Check for internal or external point (i+1/2 or n-1/2)
!
   intern = .false.
!
!     Check if end point of branch = boundary
!
   if (branch(3,ibr) .eq. igpbou) then
!
!        Begin of branch: inflowing is positive
!
      ixdis = isec
      indir = 1
      inode = branch(1,ibr)
      iboun = node(4,inode)
!
   else if (branch(4,ibr) .eq. igpbou) then
!
!        End of branch  : inflowing is negative
!
      ixdis = isec+2
      indir = -1
      inode = branch(2,ibr)
      iboun = node(4,inode)
!
   else
!
!        Structure cell : determine direction using structure definition
!
      ixdis = 0
!
      if (grid(igpbou) .eq. cstrcl) then
         indir = -1
      else
         indir = 1
      endif
      inode = 0
      iboun = 0
!
   endif

!
!     Process boundary conditions
!
   if (iboun .gt. 0) then
      if     (mbdpar(1,iboun) .eq. cmbsft) then
!
!           S = f(t), fetch table number (index 4 or 5)
!
         itab = mbdpar(3+isec,iboun)
!
!           Interpolate on time level time
!
         call inttab ( ntab(1,itab),&
         &ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &time,&
         &sinp&
         &)
      elseif (mbdpar(1,iboun) .eq. cmbsfq) then
!
!           S = f(Q), Sinp equals calculated S by sediment module
!
         sinp = dissed(ixdis,ibr)

      elseif (mbdpar(1,iboun) .eq. cmbzft) then
!
!           z = f(t), internal point
!
         intern = .true.
      else
         intern = .true.
      endif
   else
!
!        Branch connected to an internal node or no boundary condition
!
      if ((flwdir(igpbou) * indir) .lt. 0) then
!
!           Outflowing, evaluate as an internal point
!
         intern = .true.

      else
!
!           Inflowing node
!
         sinp = intbou / dtms

      endif
   endif
   if (indir .eq. 1) then
      sinp = MAX(sinp,0.)
      sbou = MAX(sedtr(igpbou,isec),0.)
      scel = MAX(sedtr(igpcel,isec),0.)
      cbou = MAX(celer(igpbou,isec),0.)
      ccel = MAX(celer(igpcel,isec),0.)
   else
      sinp = MIN(sinp,0.)
      sbou = MIN(sedtr(igpbou,isec),0.)
      scel = MIN(sedtr(igpcel,isec),0.)
      cbou = MIN(celer(igpbou,isec),0.)
      ccel = MIN(celer(igpcel,isec),0.)
   endif

!
!     Check for s1 = 0. In this case the point should be processed
!     as an internal point
!
   if (.not. intern) then
      intern = epsequ ( sedtr(igpbou,isec), 0., 1e-20 )
   endif
!
!     Calculate delta X
!
   dx = abs (x(igpcel) - x(igpbou))

   if (.not. intern) then
!
!        Determine depth
!
      if (ibrtyp .eq. ccrtab) then
!
!           Tabulated cross section
!
         if (wf(igpbou) .ge. wfh0(igpbou)) then
            depth = afs(igpbou) / wfh0(igpbou)
         else
            depth = afs(igpbou) / wf(igpbou)
         endif
!
!           Determine sediment transport width
!
         if (wf(igpbou) .gt. ws(igpbou)) then
            sedtrw = ws(igpbou)
         else
            sedtrw = wf(igpbou)
         endif

      elseif (ibrtyp .eq. ccrsed) then
!
!           Sedredge cross section
!
         depth = h(igpbou) - hlev(igpbou,isec)
!
!           Determine sediment transport width
!
         sedtrw = wft(igpbou,isec)

      endif
!
!        Determine power bb
!
      if (abs(sedtr(igpbou,isec)) .gt. 1.e-10) then
         srat = sinp / sbou
         bb   = depth * cbou /&
         &(sbou / sedtrw)
         rbb  = 1./bb

!
!


!           Prevention for overflow of alpcel
         if (srat .gt.1e-5) then
!
            if (alog10(srat)*rbb .lt.10.) then
               alpcel = srat**rbb
            else
               alpcel = 1.e10
            endif
         else
!
! wijziging, zie routine MOITBP
!
            alpcel = 1.e-10
         endif
      else
!
!           Local transport at boundary point is zero
!
         alpcel = 1.e10
      endif
!
!        Determine chock celerity
!
      if (abs(alpcel-1.) .lt. 0.001) then
         cboun = cbou
      else
         cboun = (alpcel/(alpcel-1.))*&
         &(sinp/sedtrw - sbou/sedtrw) / depth
      endif
!
!        Determine adapted courant number
!
      adcour = alphac * cboun * dtms / dx
!
!        Internal if | c+ | < 1/2
!
      intern = abs(adcour) .lt. .5

   endif

!
!     Calculate sediment transport on i+1/2 or n-1/2
!
   sedcel = (sbou+scel) / 2.

   if (intern) then
!
!        Determine adapted courant number
!
      adcour = .5 * alphac * (cbou+ccel)&
      &* dtms / dx

!
!        Determine predictor
!
      if (indir .eq. 1) then
!
!           Begin of branch
!
         predic = (adcour+.5)*sbou +&
         &(.5-adcour)*scel
      else
!
!           End of branch
!
         predic = (adcour+.5)*scel +&
         &(.5-adcour)*sbou
      endif
!
!        Determine I on halve point
!
      intcel = .5 * (sedcel + predic) * dtms
   else
!
!        External
!
      dta = .5 * ( dtms / abs(adcour) )

      if (iboun .gt. 0) then
         if (mbdpar(1,iboun) .eq. cmbsft) then
!
!              S = f(t) boundary, fetch boundary table number
!
            itab = mbdpar(3+isec,iboun)
!
!              Interpolate on time level time
!
            call inttab ( ntab(1,itab),&
            &ntab(4,itab),&
            &table(ntab(2,itab)),&
            &table(ntab(3,itab)),&
            &time,&
            &sin1&
            &)
!
!              Interpolate on time level time+dtm-dta
!
            call inttab ( ntab(1,itab),&
            &ntab(4,itab),&
            &table(ntab(2,itab)),&
            &table(ntab(3,itab)),&
            &time+dtm-dble(dta),&
            &sin2&
            &)
         else
!
!              S = f(Q) boundary
!
            sin1 = sinp
            sin2 = sinp
         endif
      else
!
!           Branch connected to an internal node
!
         sin1 = sinp
         sin2 = sinp
      endif
      if (indir .eq. 1) then
         sin1 = MAX(sin1,0.)
         sin2 = MAX(sin2,0.)
      else
         sin1 = MIN(sin1,0.)
         sin2 = MIN(sin2,0.)
      endif
!
!        Determine I on halve point
!
      intcel = .5 * dta * ( sedcel + sbou) +&
      &.5 * ( dtms - dta ) * ( sin1 + sin2 )
   endif

   return
end
