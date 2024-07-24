      subroutine sedspo (inode ,nnode  ,nbran  ,ngrid  ,maxtab ,ntabm  ,
ci1
     &                   nrout ,stotfr ,bgout  ,sedinf ,sdrdbf ,seddb  ,
cu   &                   nrout ,stot   ,bgout  ,sedinf ,sdrdbf ,seddb  ,
     &                   ntab  ,morcon ,q2     ,ws     ,table  ,dissed ,
ci1
     &                   trform, nfrac ,nfracb )
cu   &                   trform )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Modul :             SEDSPO (SEdiment Distr Sediment with POwer func)
c
c Module description: Calculate sediment transport according to a power
c                     distribution function.
c
c                     The total sediment transport is divided over all
c                     the outflowing branches (i.e. that are branches
c                     that recieve transport from the node) by using the
c                     discharges through the branches, and the widths
c                     and two user defined exponents.
c                     The user must define the exponents for each node.
c                     See Wang [ Stability of nodal point in 1D
c                     morphodynamic models, Journal Hydr Research, IAHR
c                     1995-4 ].
c                     After this routine SEDISE is called in case one or
c                     both branches are sedredge branches. Routine SEDI-
c                     SE distributes the sediment transport over the
c                     left and right channel.
c
c Precondition:       The total incoming transport is positive.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 bgout(3,nrout)    I  Contains info of every outflowing branch i
c                         from the current node:
c                         (1,i) = Branch number
c                         (2,i) = Grid point number
c                         (3,i) = Direction in branch:
c                                 +1 : First grid point is in node.
c                                 -1 : Last grid point is in node.
c 17 dissed(4,nbran)   IO Redistributed sediment transport at begin and
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
c  1 inode             I  Number of actual node.
c  5 maxtab            I  Maximum number of defined tables.
c 14 morcon(4,nmorcn)  I  Morphodynamic conditions in nodes:
c                         (1,i) = Branch number 1.
c                         (2,i) = Branch number 2.
c                         (3,i) = Table pointer to distribution function
c                                 (ratio) or coefficient ALPHA (linear) 
c                                 or exponent k (power).
c                         (4,i) = Undefined for ratio or coefficient
c                                 BETA (linear) or exponent m (power).
c  3 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  2 nnode             I  Number of nodes.
c 13 ntab              P  -
c  6 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 15 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c  7 qtot              I  Total discharge flowing towards node.
c 11 sdrdbf            P  -
c 12 seddb(2,nnode)    I  Sediment distribution as function of the di-
c                         scharge:
c                         (1,j) = Type of distribution function:
c                                 cdbpro (1) : Proportional
c                                 cdblin (2) : Linear
c                                 cdbrat (3) : Ratio
c                                 cdbpow (4) : Power
c                         Options 2 and 3 are only available for nodes
c                         with three connected branches.
c                         (2,j) = Starting index in morcon for distribu-
c                                 tion functions 2 and 3. If a starting
c                                 index has been specified a seddn(3,j)
c                                 pairs of branches will be found in
c                                 array morcon on position index, in-
c                                 dex+1 , index+2 etc.
c                         (3,j) = Number of pairs with defined linear or
c                                 ratio distribution function.
c 10 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c  8 stot              I  Total sediment transport going towards node.
c 16 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c equal   EQUAL test of two real variables
c sedise  SEdiment DIstribute SEdredge
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sedspo.pf,v $
c Revision 1.4  1996/12/03  08:24:15  kuipe_j
c calibration factor added in power distribution
c
c Revision 1.3  1996/09/03  15:21:03  kuipe_j
c comment leader
c
c Revision 1.2  1996/09/03  14:54:37  kuipe_j
c frequency time hist,etc
c
c Revision 1.1  1996/09/03  14:46:56  kuipe_j
c frequency time hist,Power distribution added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    inode ,nnode    ,nbran ,ngrid  ,maxtab ,ntabm ,
ci1
     &           nrout ,nfrac    ,nfracb
cu   &           nrout
      integer    sedinf(2,nbran) ,sdrdbf(2,*)   ,seddb(3,nnode),
     &           ntab  (4,maxtab),bgout (3,*)
cu    real       stot
ci3
      real       morcon(2+2*nfrac,*) ,ws(ngrid)     ,
     &           table (ntabm)       ,dissed(nfracb,2,nbran)       ,
     &           stotfr(nfrac)
cu    real       morcon(4,*)     ,q2    (ngrid) ,ws(ngrid)     ,
cu   &           table (ntabm)   ,dissed(4,nbran)
      real       trform(3,nbran)  

	double precision q2(ngrid)
c
c     Declaration of local parameters
c
      integer    ibr    ,igr    ,dir  ,ind   ,sedbr  ,i     ,iside,
     &           ifrac
cu    integer    ibr    ,igr    ,dir  ,ind   ,sedbr  ,i
      real       sum    ,expk   ,expm ,s     ,rdir
      real       dissdb (2)
      double precision q
c
c     Get exponents for this node
c
      ind  = seddb (2,inode)
ci3
      do 40 ifrac=1,nfrac
         expk = morcon(1+2*ifrac,ind)
         expm = morcon(2+2*ifrac,ind)

cu    expk = morcon(3,ind)
cu    expm = morcon(4,ind)
c
c     Calculate denominators of distribution relations
c
      sum = 0.
      do 10 i = 1,nrout
c
         ibr = bgout(1,i)
         igr = bgout(2,i)
         dir = bgout(3,i)
         rdir= dir
c        Calibration factor added / dd. 961014
         sum = sum + (q2(igr)*rdir)**expk * ws(igr)**expm *
     +               trform(3,ibr)
   10 continue
c
c     Calculate distributed sediment per branch with inflow.
c
      do 20 i = 1,nrout
c
         ibr = bgout(1,i)
         igr = bgout(2,i)
         dir = bgout(3,i)
         rdir= dir
ci1
         iside = (3-dir) / 2
        if (sum .gt. 1.0e-20) then
c           Calibration factor added / dd. 961014
ci1
            dissed(ifrac,iside,ibr) =
cu          dissed(2-dir,ibr) =
     &            ((q2(igr)*rdir)**expk * ws(igr)**expm) * trform(3,ibr)
ci1
     &            * stotfr(ifrac) / sum * rdir
cu   &            * stot / sum * rdir
        else
ci1
            dissed(ifrac,iside,ibr) = 0.0
cu          dissed(2-dir,ibr) = 0.0
        endif
   20 continue
c
c     Check for sedredge branches and if nessecary distribute sediment
c     over both channels.
c
      do 30 i = 1,nrout
c
         ibr = bgout(1,i)
         igr = bgout(2,i)
         dir = bgout(3,i)
ci1
         iside = (3-dir) / 2
c
c        Get sedredge branch number.
c
         sedbr = sedinf(1,ibr)
         if (sedbr .ne. 0) then
c
c           Get total sediment and total discharge of the sedredge
c           branch.
c
ci1
            s = dissed(1,iside,ibr)
cu          s = dissed(2-dir,ibr)
            q = q2(igr)
c
            call sedise (sedbr  ,maxtab ,ntabm  ,s   ,q   ,
     &                   sdrdbf ,ntab   ,table  ,dissdb )
c
ci2
            dissed(1,iside,ibr) = dissdb(1)
            dissed(2,iside,ibr) = dissdb(2)
cu          dissed(2-dir,ibr) = dissdb(1)
cu          dissed(3-dir,ibr) = dissdb(2)
c
         endif
c
  30  continue
c
ci2
  40  continue
c
      end
