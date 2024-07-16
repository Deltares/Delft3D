      subroutine MOINOD (inode  ,nnode  ,nbran  ,nbrnod ,ngrid ,dtm    ,
     +                   alphac,branch  ,brnode ,typcr  ,bgout ,grid   ,
     +                   dissed ,intgr  ,x      ,celer  ,
     +                   mopta  ,moptb  ,moptc  ,moptd  ,flwdir,alphad ,
     +                   sedtr  ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOINOD (MOrphology Integral for a NODe)
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
c 10 alphac            P  -
c 25 alphad            P  -
c 14 bgout(3,nrout)    IO Contains info for every outflowing branch i
c                         from the current node:
c                         (1,i) = Branch number
c                         (2,i) = Grid point number
c                         (3,i) = Direction in branch
c                                 +1: First point of branch
c                                 -1: Last point of branch
c 11 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 12 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c 19 celer             P  -
c 16 dissed(4,nbran)   I  Redistributed sediment transport at begin and
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
c  9 dtm               P  -
c 24 flwdir(ngrid)     I  Indicator for flow direction at each grid
c                         point      1 = positive flow
c                                    0 = zero flow
c                                   -1 = negative flow
c 15 grid              P  -
c  1 inode             I  Node number to be processed
c 17 intgr(ngrid,*)    IO Integral values for grid
c 27 juer              P  -
c 28 ker               P  -
c 20 mopta             P  -
c 21 moptb             P  -
c 22 moptc             P  -
c 23 moptd             P  -
c  3 nbran             I  Number of branches.
c  4 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  5 ngrid             I  Number of grid points in network.
c  2 nnode             I  Number of nodes.
c                         table).
c 26 sedtr             P  -
c 13 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 18 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c moitno  MOrphology InTegral at a NOde
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moinod.pf,v $
c Revision 1.10  1999/03/15  15:52:53  kuipe_j
c tabs removed
c
c Revision 1.9  1998/06/11  11:47:11  kuipe_j
c Estuary special integrated
c
c Revision 1.8  1996/12/04  12:01:05  kuipe_j
c declarations / undefined vars
c
c Revision 1.7  1996/10/31  10:31:55  kuipe_j
c Extra resistance finished
c
c Revision 1.6  1996/09/03  14:48:48  kuipe_j
c frequency time hist,Improved sed distribution at nodes
c
c Revision 1.5  1996/03/08  09:39:07  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.4  1996/03/07  10:44:16  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.3  1995/05/30  09:55:54  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:49  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:17  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:52:36  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:46  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    inode  ,nnode  ,nbran  ,nbrnod ,ngrid  ,
     +           juer   ,ker

      real       alphac ,alphad

      integer    branch (4,nbran)       ,
     +           flwdir (ngrid)         ,
     +           brnode (nbrnod+1,nnode),
     +           typcr  (nbran)         ,
     +           bgout  (3,nbrnod)      ,
     +           grid   (ngrid)

      real       dissed (4,nbran),
     +           intgr  (ngrid,2,*),
     +           x      (ngrid)  ,
     +           celer  (ngrid,*),
     +           sedtr  (ngrid,*)

      double     precision  dtm

      logical    mopta, moptb ,moptc ,moptd
c
c     Declaration of local parameters
c
      integer    ibr    ,nbn    ,igp    ,dir    ,i      ,nrout  ,
     +           isec   ,nsec
      real       stot   ,itot   ,itote  ,alpha  ,sib
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      nrout = 0
      itot  = 0.
      itote = 0.
c
      nbn = brnode(1,inode)
c
      do 20 i = 1, nbn
c
         ibr = brnode(i+1,inode)
c
c        Determine if node is at the beginning or end of the branch.
c
         if (branch (1,ibr) .eq. inode) then
c           Begin of branch
            igp = branch (3,ibr)
            dir = 1
         else
c           End of branch
            igp = branch (4,ibr)
            dir = -1
         endif
c
         if (dir * flwdir(igp) .lt. 0. ) then
c
c           Inflow in node.
c
            if (typcr(ibr) .eq. ccrsed) then
               nsec = 2
            else
               nsec = 1
            endif
c
c           Calculate total sediment transport and integrals
c
            do 10 isec = 1, nsec
c
c              Calculate total sediment transport
c
c              Calculate integral
c
               call MOITNO ( igp    ,isec   ,ngrid  ,x      ,
     +                       dtm    ,alphac ,inode  ,branch ,
     +                       ibr    ,nbran  ,grid   ,
     +                       mopta  ,moptb  ,moptc  ,moptd  ,
     +                       celer  ,sedtr  ,alphad ,flwdir ,
     +                       juer   ,ker    ,intgr(igp,1,isec))
c
c              Calculate extra integral
c
               call MOIEXT  (igp    ,dir    ,isec   ,ngrid  ,
     +                       dtm    ,x      ,celer  ,sedtr  ,
     +                       intgr(igp,2,isec) )
c
c              Calculate total integrals
c
               itot = itot + intgr(igp,1,isec)*real(dir)
               itote= itote+ intgr(igp,2,isec)*real(dir)
 10         continue
c
         else
c
c           Outflow from node.
c
            nrout = nrout + 1
            bgout(1,nrout) = ibr
            bgout(2,nrout) = igp
            bgout(3,nrout) = dir
c
         endif
 20   continue
c
c     Now distribute over outflowing branches
c     Outflowing is determined by the sign of the celerity.
c     If the sign of the transport is opposite, this branch
c     will get a zero transport.
c
      stot = 0.
      do 40 i = 1, nrout
c
         ibr = bgout(1,i)
         dir = bgout(3,i)
c
         if (typcr(ibr) .eq. ccrsed) then
            nsec = 2
         else
            nsec = 1
         endif
c
         do 30 isec = 1, nsec
c
            sib = dissed(1+isec-dir,ibr) * real(dir)
            if (sib .gt. 0.) then
               stot = stot + sib
            endif
c
 30      continue
 40   continue
c
c     Now distribute over outflowing branches
c
      do 60 i = 1, nrout
c
         ibr = bgout(1,i)
         igp = bgout(2,i)
         dir = bgout(3,i)
c
c        Hier stond (typecr(ibr) .ne. 0), gewijzigd op 27-9-93
c
         if (typcr(ibr) .eq. ccrsed) then
            nsec = 2
         else
            nsec = 1
         endif
c
         do 50 isec = 1, nsec
c
c           Calculate distribution factor alpha
c
            if (stot .gt. 1.e-20) then
               sib = dissed(1+isec-dir,ibr) * real(dir)
               if (sib .gt. 0.) then
                  alpha = sib / stot
               else
                  alpha = 0.
               endif
            else
               alpha = 1. / real(nrout)
            endif
c
c           Calculate integral values
c
            intgr(igp,1,isec) = -alpha * itot  * real(dir)
            intgr(igp,2,isec) = -alpha * itote * real(dir)

 50      continue
 60   continue
c
      end
