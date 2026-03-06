subroutine sedspo (inode ,nnode  ,nbran  ,ngrid  ,maxtab ,ntabm  ,&
!i1
&nrout ,stotfr ,bgout  ,sedinf ,sdrdbf ,seddb  ,&
!u   &                   nrout ,stot   ,bgout  ,sedinf ,sdrdbf ,seddb  ,
&ntab  ,morcon ,q2     ,ws     ,table  ,dissed ,&
!i1
&trform, nfrac ,nfracb )
!u   &                   trform )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Modul :             SEDSPO (SEdiment Distr Sediment with POwer func)
!
! Module description: Calculate sediment transport according to a power
!                     distribution function.
!
!                     The total sediment transport is divided over all
!                     the outflowing branches (i.e. that are branches
!                     that recieve transport from the node) by using the
!                     discharges through the branches, and the widths
!                     and two user defined exponents.
!                     The user must define the exponents for each node.
!                     See Wang [ Stability of nodal point in 1D
!                     morphodynamic models, Journal Hydr Research, IAHR
!                     1995-4 ].
!                     After this routine SEDISE is called in case one or
!                     both branches are sedredge branches. Routine SEDI-
!                     SE distributes the sediment transport over the
!                     left and right channel.
!
! Precondition:       The total incoming transport is positive.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 bgout(3,nrout)    I  Contains info of every outflowing branch i
!                         from the current node:
!                         (1,i) = Branch number
!                         (2,i) = Grid point number
!                         (3,i) = Direction in branch:
!                                 +1 : First grid point is in node.
!                                 -1 : Last grid point is in node.
! 17 dissed(4,nbran)   IO Redistributed sediment transport at begin and
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
!  1 inode             I  Number of actual node.
!  5 maxtab            I  Maximum number of defined tables.
! 14 morcon(4,nmorcn)  I  Morphodynamic conditions in nodes:
!                         (1,i) = Branch number 1.
!                         (2,i) = Branch number 2.
!                         (3,i) = Table pointer to distribution function
!                                 (ratio) or coefficient ALPHA (linear)
!                                 or exponent k (power).
!                         (4,i) = Undefined for ratio or coefficient
!                                 BETA (linear) or exponent m (power).
!  3 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  2 nnode             I  Number of nodes.
! 13 ntab              P  -
!  6 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 15 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
!  7 qtot              I  Total discharge flowing towards node.
! 11 sdrdbf            P  -
! 12 seddb(2,nnode)    I  Sediment distribution as function of the di-
!                         scharge:
!                         (1,j) = Type of distribution function:
!                                 cdbpro (1) : Proportional
!                                 cdblin (2) : Linear
!                                 cdbrat (3) : Ratio
!                                 cdbpow (4) : Power
!                         Options 2 and 3 are only available for nodes
!                         with three connected branches.
!                         (2,j) = Starting index in morcon for distribu-
!                                 tion functions 2 and 3. If a starting
!                                 index has been specified a seddn(3,j)
!                                 pairs of branches will be found in
!                                 array morcon on position index, in-
!                                 dex+1 , index+2 etc.
!                         (3,j) = Number of pairs with defined linear or
!                                 ratio distribution function.
! 10 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
!  8 stot              I  Total sediment transport going towards node.
! 16 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! equal   EQUAL test of two real variables
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
! $Log: sedspo.pf,v $
! Revision 1.4  1996/12/03  08:24:15  kuipe_j
! calibration factor added in power distribution
!
! Revision 1.3  1996/09/03  15:21:03  kuipe_j
! comment leader
!
! Revision 1.2  1996/09/03  14:54:37  kuipe_j
! frequency time hist,etc
!
! Revision 1.1  1996/09/03  14:46:56  kuipe_j
! frequency time hist,Power distribution added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    inode ,nnode    ,nbran ,ngrid  ,maxtab ,ntabm ,&
!i1
   &nrout ,nfrac    ,nfracb
!u   &           nrout
   integer    sedinf(2,nbran) ,sdrdbf(2,*)   ,seddb(3,nnode),&
   &ntab  (4,maxtab),bgout (3,*)
!u    real       stot
!i3
   real       morcon(2+2*nfrac,*) ,ws(ngrid)     ,&
   &table (ntabm)       ,dissed(nfracb,2,nbran)       ,&
   &stotfr(nfrac)
!u    real       morcon(4,*)     ,q2    (ngrid) ,ws(ngrid)     ,
!u   &           table (ntabm)   ,dissed(4,nbran)
   real       trform(3,nbran)

   double precision q2(ngrid)
!
!     Declaration of local parameters
!
   integer    ibr    ,igr    ,dir  ,ind   ,sedbr  ,i     ,iside,&
   &ifrac
!u    integer    ibr    ,igr    ,dir  ,ind   ,sedbr  ,i
   real       sum    ,expk   ,expm ,s     ,rdir
   real       dissdb (2)
   double precision q
!
!     Get exponents for this node
!
   ind  = seddb (2,inode)
!i3
   do 40 ifrac=1,nfrac
      expk = morcon(1+2*ifrac,ind)
      expm = morcon(2+2*ifrac,ind)

!u    expk = morcon(3,ind)
!u    expm = morcon(4,ind)
!
!     Calculate denominators of distribution relations
!
      sum = 0.
      do 10 i = 1,nrout
!
         ibr = bgout(1,i)
         igr = bgout(2,i)
         dir = bgout(3,i)
         rdir= dir
!        Calibration factor added / dd. 961014
         sum = sum + (q2(igr)*rdir)**expk * ws(igr)**expm *&
         &trform(3,ibr)
10    continue
!
!     Calculate distributed sediment per branch with inflow.
!
      do 20 i = 1,nrout
!
         ibr = bgout(1,i)
         igr = bgout(2,i)
         dir = bgout(3,i)
         rdir= dir
!i1
         iside = (3-dir) / 2
         if (sum .gt. 1.0e-20) then
!           Calibration factor added / dd. 961014
!i1
            dissed(ifrac,iside,ibr) =&
!u          dissed(2-dir,ibr) =
            &((q2(igr)*rdir)**expk * ws(igr)**expm) * trform(3,ibr)&
!i1
            &* stotfr(ifrac) / sum * rdir
!u   &            * stot / sum * rdir
         else
!i1
            dissed(ifrac,iside,ibr) = 0.0
!u          dissed(2-dir,ibr) = 0.0
         endif
20    continue
!
!     Check for sedredge branches and if nessecary distribute sediment
!     over both channels.
!
      do 30 i = 1,nrout
!
         ibr = bgout(1,i)
         igr = bgout(2,i)
         dir = bgout(3,i)
!i1
         iside = (3-dir) / 2
!
!        Get sedredge branch number.
!
         sedbr = sedinf(1,ibr)
         if (sedbr .ne. 0) then
!
!           Get total sediment and total discharge of the sedredge
!           branch.
!
!i1
            s = dissed(1,iside,ibr)
!u          s = dissed(2-dir,ibr)
            q = q2(igr)
!
            call sedise (sedbr  ,maxtab ,ntabm  ,s   ,q   ,&
            &sdrdbf ,ntab   ,table  ,dissdb )
!
!i2
            dissed(1,iside,ibr) = dissdb(1)
            dissed(2,iside,ibr) = dissdb(2)
!u          dissed(2-dir,ibr) = dissdb(1)
!u          dissed(3-dir,ibr) = dissdb(2)
!
         endif
!
30    continue
!
!i2
40 continue
!
end
