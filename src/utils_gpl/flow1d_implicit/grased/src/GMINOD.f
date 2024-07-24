      subroutine GMINOD (inode  ,nnode  ,nbran  ,nbrnod ,ngrid  ,nfrac ,
     +                   dtm    ,alphac ,alphad ,alphae ,branch ,brnode,
     +                   bgout  ,disgse ,intgr  ,x      ,celer  ,celert,
     +                   source ,dfrac  ,ds     ,spredc ,cela1  ,flwdir,
     +                   sedtr  ,itot   ,stot   ,jugralg)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             GMINOD (Graded Morphology Integral for a NODe)
c
c Module description: Calculate integrals in a node depending on flow
c                     conditions.
c
c                     The integral for an outflowing branch (from node
c                     to branch) will be a fraction of the total inte-
c                     grals flowing to the node. After the calculation
c                     the sum of the integrals in a node will be zero.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 11 alphac            P  -
c 15 bgout(3,nrout)    IO Contains info for every outflowing branch i
c                         from the current node:
c                         (1,i) = Branch number
c                         (2,i) = Grid point number
c                         (3,i) = Direction in branch
c                                 +1: First point of branch
c                                 -1: Last point of branch
c 12 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 13 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c 24 celer             P  -
c 20 disgse(4,nbran)   I  Redistributed sediment transport at begin and
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
c 10 dtm               P  -
c 16 grid              P  -
c  1 inode             I  Node number to be processed
c 21 intgr(ngrid,*)    IO Integral values for grid
c 26 juer              P  -
c 27 ker               P  -
c  7 maxtab            I  Maximum number of defined tables.
c 18 mbdpar            P  -
c  6 nboun             I  Number of boundary nodes.
c  3 nbran             I  Number of branches.
c  4 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  5 ngrid             I  Number of grid points in network.
c  2 nnode             I  Number of nodes.
c 17 node              P  -
c 19 ntab              P  -
c  8 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 25 sedtr             P  -
c 23 table             P  -
c  9 time              P  -
c 14 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 22 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c moibou  MOrphology Integral BOUndary
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gminod.F,v $
c Revision 1.2  1995/09/27  10:11:37  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    inode  ,nnode  ,nbran  ,nbrnod ,ngrid  ,nfrac ,jugralg
      integer    branch (4,nbran)       ,
     +           flwdir (ngrid)         ,
     +           brnode (nbrnod+1,nnode),
     +           bgout  (3,nbrnod)       

      real       alphac ,alphad ,alphae
      real       disgse (nfrac,2,nbran) ,
     +           intgr  (nfrac,2,nbran) ,
     +           celer  (ngrid,nfrac,5) ,celert(ngrid)        ,
     +           sedtr (ngrid,nfrac+2)  ,source(ngrid,nfrac+2),
     +           x      (ngrid)         ,cela1 (nfrac,nfrac)  ,
     +           ds     (nfrac)         ,spredc(nfrac)        ,
     +           dfrac  (nfrac)
      double precision   dtm
c
c     Declaration of local parameters
c
      integer    ibr    ,nbn    ,igp    ,igpcel ,dir   ,ixdir  ,
     &           i      ,nrout  ,jf
      real       s      ,alpha  ,sib
      real       stot(nfrac)    ,itot(nfrac)
c
c     Include sobek constants
c
      nrout    = 0
      do 10 jf = 1,nfrac
         itot(jf) = 0.
  10  continue
c
      nbn = brnode(1,inode)
c
      do 30 i = 1, nbn
c
         ibr = brnode(i+1,inode)
c
c        Determine if node is at the beginning or end of the branch.
c
         if (branch (1,ibr) .eq. inode) then
c           Begin of branch
            igp   = branch (3,ibr)
            dir   = 1
            ixdir = 1
         else
c           End of branch
            igp   = branch (4,ibr)
            dir   = -1
            ixdir = 2
         endif
c
         s = real(dir)
c
         if (dir * flwdir(igp) .lt. 0. ) then
c
c           Inflow in node.
c
c           Calculate sediment integral at begin or end of branch
c
            igpcel = igp+dir
c
            call gmiflp (igp    ,igpcel ,ngrid ,nfrac  ,alphac ,alphad ,
     +                   alphae ,sngl(dtm)     ,celer  ,celert ,sedtr  ,
     +                   source ,x      ,dfrac ,ds     ,spredc ,cela1  ,
     +                   intgr(1,ixdir,ibr)    ,jugralg)
c
c           Calculate total integral
c
            do 20 jf=1,nfrac
               itot(jf) = itot(jf) + intgr(jf,ixdir,ibr) * s
   20       continue
c
         else
c
c           Outflow from node.
c
            nrout = nrout + 1
            bgout(1,nrout) = ibr
            bgout(2,nrout) = ixdir
            bgout(3,nrout) = dir
c
         endif
 30   continue
c
c     Now distribute over outflowing branches
c     Outflowing is determined by the sign of the celerity.
c     If the sign of the transport is opposite, this branch
c     will get a zero transport.
c
      do 40 jf = 1,nfrac
         stot(jf) = 0.
  40  continue

      do 60 i = 1, nrout
c
         ibr   = bgout(1,i)
         ixdir = bgout(2,i)
         dir   = bgout(3,i)
       s     = real(dir)
c
         do 50 jf = 1, nfrac
c
c           Calculate total sediment transport
c
            sib = disgse(jf,ixdir,ibr) * s
            if (sib .gt. 0.) then
               stot(jf) = stot(jf) + sib
          endif
   50    continue
   60 continue
c
c     Now distribute over outflowing branches
c
      do 80 i = 1, nrout
c
         ibr   = bgout(1,i)
         ixdir = bgout(2,i)
         dir   = bgout(3,i)
       s     = real(dir)
c
         do 70 jf = 1, nfrac
c
c           Calculate distribution factor alpha
c
		if (stot(jf) .gt. 1.e-20) then
               sib = disgse(jf,ixdir,ibr) * s        
               if (sib .gt. 0.) then
                  alpha = sib / stot(jf)
               else
				alpha = 0.
               endif
		else
               alpha = 1. / real(nrout)
          endif
c
c           Calculate integral value
c
            intgr(jf,ixdir,ibr) = -alpha * itot(jf) * s

 70      continue
c
 80   continue
c
      end
