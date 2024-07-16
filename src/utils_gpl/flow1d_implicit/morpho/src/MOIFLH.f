      subroutine MOIFLH ( ibr    ,igpbou ,igpcel ,isec   ,
     +                    ngrid  ,nbran  ,nboun  ,nnode  ,
     +                    maxlev ,branch ,node   ,ibrtyp ,
     +                    mbdpar ,x      ,hlev   ,grid   ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,
     +                    time   ,dtm    ,alphac ,h      ,
     +                    wf     ,wfh0   ,ws     ,wft    ,
     +                    afs    ,celer  ,sedtr  ,dissed ,
     +                    intbou ,intcel ,flwdir
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOIFLH (MOrphology Integral on First or Last halve point)
c
c Module description: Calculate integral on point 3/2 or n-1/2
c
c                     The integral I3/2 or In-1/2 is determined diffe-
c                     rently depending on the inflow or outflow situati-
c                     on. The point will be calculated as an internal
c                     point in the following cases:
c
c                     o   | adapted courant number | < 1/2
c                     o   boundary on point 1 is defined as z = f(t).
c
c                     In the call parameter list the grid point of the
c                     boundary (gpb) will be passed as well as the grid
c                     point i+1 or n-1 (gpc). Also the calculated inte-
c                     gral value Ibou from the call to MOIBOU will be
c                     passed.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 29 afs(ngrid,2)      I  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 23 alphac            I  Stability factor for bottom scheme (>1)
c 10 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 30 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c 32 dissed(4,nbran)   I  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c 22 dtm               I  Morphology time step.
c 35 flwdir(ngrid)     I  Indicator for flow direction at each grid
c                         point      1 = positive flow
c                                    0 = zero flow
c                                   -1 = negative flow
c 16 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c 24 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c 15 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  1 ibr               I  Branch number
c 12 ibrtyp            I  Type of branch
c                           ccrtab (1) : tabulated branch
c                           ccrcir (2) : circle branch
c                           ccrsed (3) : sedredge branch
c  2 igpbou            I  Calculated integral value on boundary
c  3 igpcel            I  Calculated integral for first or last cell in
c                         branch
c 33 intbou            I  Integral value for begin or end point of a
c                         branch
c 34 intcel            O  Calculated integral value for first or last
c                         cel of a branch
c  4 isec              I  Section number (1 or 2)
c  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 17 maxtab            I  Maximum number of defined tables.
c 13 mbdpar(5,nboun)   I  Morphodynamic boundary conditions:
c                         (1,i) = Type of boundary condition:
c                                 cmbsft (1) : Condition S=f(t).
c                                 cmbsfq (2) : Condition S=f(Q).
c                                 cmbzft (3) : Condition z=f(t).
c                         (2,i) = Location (node number).
c                         (3,i) = Branch number that is connected.
c                         (4,i) = Table pointer for boundary table. In
c                                 case of a connected sedredge branch
c                                 the pointer will be assigned to the
c                                 left channel.
c                         (5,i) = Table pointer for boundary table. In
c                                 case of a connected sedredge branch
c                                 the pointer will be assigned to the
c                                 right channel. In other cases undefi-
c                                 ned.
c  7 nboun             I  Number of boundary nodes.
c  6 nbran             I  Number of branches.
c  5 ngrid             I  Number of grid points in network.
c  8 nnode             I  Number of nodes.
c 11 node(4,nnode)     I  Definition of nodes:
c                         (1,i) = Type of node i:
c                                 cintnd (1) : Internal node
c                                 chbou  (2) : H-boundary
c                                 cqbou  (3) : Q-boundary
c                                 cqhbou (4) : QH-boundary
c                                 chqbou (5) : HQ-boundary
c                         (2,i) = Gridpoint in case of boundary, else
c                                 undefined.
c                         (3,i) = Station number for boundary, undefined
c                                 for internal nodes:
c                                 HQ, H-boundary: station nr H-station.
c                                 QH, Q-boundary: station nr Q-station.
c                         (4,i) = Boundary number in case of boundary.
c 19 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c 18 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 31 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
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
c 20 table             P  -
c 21 time              I  Actual time level tn+1. in sec.
c 25 wf(ngrid)         I  Actual flow width at every grid point.
c 26 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
c                         grid point.
c 28 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c 27 ws(ngrid)         I  Sediment transporting width for each grid
c                         point.
c 14 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moiflh.pf,v $
c Revision 1.7  1998/06/11  11:47:10  kuipe_j
c Estuary special integrated
c
c Revision 1.6  1996/03/08  09:39:06  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.5  1996/03/07  10:44:15  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.4  1995/10/18  08:59:59  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:55:51  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:47  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:15  hoeks_a
c Initial check-in
c
c Revision 1.4  1994/12/02  13:05:32  kuipe_j
c Improvement in calculation of shock celerity (exceptions)
c
c Revision 1.3  1994/11/28  08:52:34  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:42  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer    ibr    ,igpbou  ,igpcel ,isec   ,ngrid  ,nbran  ,
     +           nboun  ,nnode   ,maxtab ,ntabm  ,ibrtyp ,maxlev

      integer    branch (4,nbran),
     +           grid   (ngrid)  ,
     +           flwdir (ngrid)  ,
     +           node   (4,nnode),
     +           mbdpar (5,nboun),
     +           ntab   (4,maxtab)

      real       alphac ,intbou  ,intcel

      real       x      (ngrid),
     +           table  (ntabm),
     +           celer  (ngrid,*),
     +           sedtr  (ngrid,*),
     +           dissed (4,nbran),
     +           wf     (ngrid),
     +           wfh0   (ngrid),
     +           ws     (ngrid),
     +           wft    (ngrid,maxlev),
     +           afs    (ngrid)

      double precision time ,dtm, hlev(ngrid,maxlev), h(ngrid)

c
c     Local variables
c
      integer    ixdis ,inode ,iboun ,itab  ,indir
      logical    intern
      real       bb    ,depth ,sinp  ,sin1  ,sin2  ,dx    ,cboun ,
     +           alpcel,predic,adcour,dta   ,sedtrw,sedcel,dtms  ,
     +           rbb   ,srat, scel, sbou, ccel, cbou
c
      logical    epsequ
      external   epsequ
c
c     Include sobek constants
c
      include '..\include\sobcon.i'

      dtms = sngl(dtm)
c
c     Check for internal or external point (i+1/2 or n-1/2)
c
      intern = .false.
c
c     Check if end point of branch = boundary
c
      if (branch(3,ibr) .eq. igpbou) then
c
c        Begin of branch: inflowing is positive
c
        ixdis = isec
         indir = 1
        inode = branch(1,ibr)
         iboun = node(4,inode)
c
      else if (branch(4,ibr) .eq. igpbou) then
c
c        End of branch  : inflowing is negative
c
        ixdis = isec+2
         indir = -1
        inode = branch(2,ibr)
         iboun = node(4,inode)
c
      else
c
c        Structure cell : determine direction using structure definition
c
         ixdis = 0
c
         if (grid(igpbou) .eq. cstrcl) then
            indir = -1
         else
            indir = 1
         endif
         inode = 0
         iboun = 0
c
      endif

c
c     Process boundary conditions
c
      if (iboun .gt. 0) then
        if     (mbdpar(1,iboun) .eq. cmbsft) then
c
c           S = f(t), fetch table number (index 4 or 5)
c
           itab = mbdpar(3+isec,iboun)
c
c           Interpolate on time level time
c
           call inttab ( ntab(1,itab),
     +                    ntab(4,itab),
     +                    table(ntab(2,itab)),
     +                    table(ntab(3,itab)),
     +                    time,
     +                    sinp
     +                  )
        elseif (mbdpar(1,iboun) .eq. cmbsfq) then
c
c           S = f(Q), Sinp equals calculated S by sediment module
c
           sinp = dissed(ixdis,ibr)

        elseif (mbdpar(1,iboun) .eq. cmbzft) then
c
c           z = f(t), internal point
c
           intern = .true.
        else
           intern = .true.
        endif
      else
c
c        Branch connected to an internal node or no boundary condition
c
         if ((flwdir(igpbou) * indir) .lt. 0) then
c
c           Outflowing, evaluate as an internal point
c
            intern = .true.

         else
c
c           Inflowing node
c
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

c
c     Check for s1 = 0. In this case the point should be processed
c     as an internal point
c
      if (.not. intern) then
         intern = epsequ ( sedtr(igpbou,isec), 0., 1e-20 )
      endif
c
c     Calculate delta X
c
      dx = abs (x(igpcel) - x(igpbou))

      if (.not. intern) then
c
c        Determine depth
c
        if (ibrtyp .eq. ccrtab) then
c
c           Tabulated cross section
c
           if (wf(igpbou) .ge. wfh0(igpbou)) then
              depth = afs(igpbou) / wfh0(igpbou)
           else
              depth = afs(igpbou) / wf(igpbou)
           endif
c
c           Determine sediment transport width
c
            if (wf(igpbou) .gt. ws(igpbou)) then
               sedtrw = ws(igpbou)
            else
               sedtrw = wf(igpbou)
            endif

        elseif (ibrtyp .eq. ccrsed) then
c
c           Sedredge cross section
c
           depth = h(igpbou) - hlev(igpbou,isec)
c
c           Determine sediment transport width
c
            sedtrw = wft(igpbou,isec)

        endif
c
c        Determine power bb
c
        if (abs(sedtr(igpbou,isec)) .gt. 1.e-10) then
            srat = sinp / sbou
           bb   = depth * cbou /
     &            (sbou / sedtrw)
            rbb  = 1./bb

c
c


c           Prevention for overflow of alpcel
            if (srat .gt.1e-5) then
c
               if (alog10(srat)*rbb .lt.10.) then
                  alpcel = srat**rbb
               else
                  alpcel = 1.e10
               endif
            else
c
c wijziging, zie routine MOITBP
c
               alpcel = 1.e-10
            endif
        else
c
c           Local transport at boundary point is zero
c
            alpcel = 1.e10
         endif
c
c        Determine chock celerity
c
        if (abs(alpcel-1.) .lt. 0.001) then
           cboun = cbou
        else
           cboun = (alpcel/(alpcel-1.))*
     +              (sinp/sedtrw - sbou/sedtrw) / depth
        endif
c
c        Determine adapted courant number
c
        adcour = alphac * cboun * dtms / dx
c
c        Internal if | c+ | < 1/2
c
        intern = abs(adcour) .lt. .5

      endif

c
c     Calculate sediment transport on i+1/2 or n-1/2
c
      sedcel = (sbou+scel) / 2.

      if (intern) then
c
c        Determine adapted courant number
c
        adcour = .5 * alphac * (cbou+ccel)
     +            * dtms / dx

c
c        Determine predictor
c
         if (indir .eq. 1) then
c
c           Begin of branch
c
           predic = (adcour+.5)*sbou +
     +               (.5-adcour)*scel
         else
c
c           End of branch
c
           predic = (adcour+.5)*scel +
     +               (.5-adcour)*sbou
         endif
c
c        Determine I on halve point
c
        intcel = .5 * (sedcel + predic) * dtms
      else
c
c        External
c
        dta = .5 * ( dtms / abs(adcour) )

        if (iboun .gt. 0) then
           if (mbdpar(1,iboun) .eq. cmbsft) then
c
c              S = f(t) boundary, fetch boundary table number
c
              itab = mbdpar(3+isec,iboun)
c
c              Interpolate on time level time
c
              call inttab ( ntab(1,itab),
     +                       ntab(4,itab),
     +                       table(ntab(2,itab)),
     +                       table(ntab(3,itab)),
     +                       time,
     +                       sin1
     +                     )
c
c              Interpolate on time level time+dtm-dta
c
              call inttab ( ntab(1,itab),
     +                       ntab(4,itab),
     +                       table(ntab(2,itab)),
     +                       table(ntab(3,itab)),
     +                       time+dtm-dble(dta),
     +                       sin2
     +                     )
           else
c
c              S = f(Q) boundary
c
              sin1 = sinp
              sin2 = sinp
           endif
        else
c
c           Branch connected to an internal node
c
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
c
c        Determine I on halve point
c
        intcel = .5 * dta * ( sedcel + sbou) +
     +            .5 * ( dtms - dta ) * ( sin1 + sin2 )
      endif

      return
      end
