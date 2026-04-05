subroutine GMINOD (inode  ,nnode  ,nbran  ,nbrnod ,ngrid  ,nfrac ,&
&dtm    ,alphac ,alphad ,alphae ,branch ,brnode,&
&bgout  ,disgse ,intgr  ,x      ,celer  ,celert,&
&source ,dfrac  ,ds     ,spredc ,cela1  ,flwdir,&
&sedtr  ,itot   ,stot   ,jugralg)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             GMINOD (Graded Morphology Integral for a NODe)
!
! Module description: Calculate integrals in a node depending on flow
!                     conditions.
!
!                     The integral for an outflowing branch (from node
!                     to branch) will be a fraction of the total inte-
!                     grals flowing to the node. After the calculation
!                     the sum of the integrals in a node will be zero.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 alphac            P  -
! 15 bgout(3,nrout)    IO Contains info for every outflowing branch i
!                         from the current node:
!                         (1,i) = Branch number
!                         (2,i) = Grid point number
!                         (3,i) = Direction in branch
!                                 +1: First point of branch
!                                 -1: Last point of branch
! 12 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 13 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
! 24 celer             P  -
! 20 disgse(4,nbran)   I  Redistributed sediment transport at begin and
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
! 10 dtm               P  -
! 16 grid              P  -
!  1 inode             I  Node number to be processed
! 21 intgr(ngrid,*)    IO Integral values for grid
! 26 juer              P  -
! 27 ker               P  -
!  7 maxtab            I  Maximum number of defined tables.
! 18 mbdpar            P  -
!  6 nboun             I  Number of boundary nodes.
!  3 nbran             I  Number of branches.
!  4 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  5 ngrid             I  Number of grid points in network.
!  2 nnode             I  Number of nodes.
! 17 node              P  -
! 19 ntab              P  -
!  8 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 25 sedtr             P  -
! 23 table             P  -
!  9 time              P  -
! 14 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 22 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! moibou  MOrphology Integral BOUndary
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gminod.F,v $
! Revision 1.2  1995/09/27  10:11:37  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    inode  ,nnode  ,nbran  ,nbrnod ,ngrid  ,nfrac ,jugralg
   integer    branch (4,nbran)       ,&
   &flwdir (ngrid)         ,&
   &brnode (nbrnod+1,nnode),&
   &bgout  (3,nbrnod)

   real       alphac ,alphad ,alphae
   real       disgse (nfrac,2,nbran) ,&
   &intgr  (nfrac,2,nbran) ,&
   &celer  (ngrid,nfrac,5) ,celert(ngrid)        ,&
   &sedtr (ngrid,nfrac+2)  ,source(ngrid,nfrac+2),&
   &x      (ngrid)         ,cela1 (nfrac,nfrac)  ,&
   &ds     (nfrac)         ,spredc(nfrac)        ,&
   &dfrac  (nfrac)
   double precision   dtm
!
!     Declaration of local parameters
!
   integer    ibr    ,nbn    ,igp    ,igpcel ,dir   ,ixdir  ,&
   &i      ,nrout  ,jf
   real       s      ,alpha  ,sib
   real       stot(nfrac)    ,itot(nfrac)
!
!     Include sobek constants
!
   nrout    = 0
   do 10 jf = 1,nfrac
      itot(jf) = 0.
10 continue
!
   nbn = brnode(1,inode)
!
   do 30 i = 1, nbn
!
      ibr = brnode(i+1,inode)
!
!        Determine if node is at the beginning or end of the branch.
!
      if (branch (1,ibr) .eq. inode) then
!           Begin of branch
         igp   = branch (3,ibr)
         dir   = 1
         ixdir = 1
      else
!           End of branch
         igp   = branch (4,ibr)
         dir   = -1
         ixdir = 2
      endif
!
      s = real(dir)
!
      if (dir * flwdir(igp) .lt. 0. ) then
!
!           Inflow in node.
!
!           Calculate sediment integral at begin or end of branch
!
         igpcel = igp+dir
!
         call gmiflp (igp    ,igpcel ,ngrid ,nfrac  ,alphac ,alphad ,&
         &alphae ,sngl(dtm)     ,celer  ,celert ,sedtr  ,&
         &source ,x      ,dfrac ,ds     ,spredc ,cela1  ,&
         &intgr(1,ixdir,ibr)    ,jugralg)
!
!           Calculate total integral
!
         do 20 jf=1,nfrac
            itot(jf) = itot(jf) + intgr(jf,ixdir,ibr) * s
20       continue
!
      else
!
!           Outflow from node.
!
         nrout = nrout + 1
         bgout(1,nrout) = ibr
         bgout(2,nrout) = ixdir
         bgout(3,nrout) = dir
!
      endif
30 continue
!
!     Now distribute over outflowing branches
!     Outflowing is determined by the sign of the celerity.
!     If the sign of the transport is opposite, this branch
!     will get a zero transport.
!
   do 40 jf = 1,nfrac
      stot(jf) = 0.
40 continue

   do 60 i = 1, nrout
!
      ibr   = bgout(1,i)
      ixdir = bgout(2,i)
      dir   = bgout(3,i)
      s     = real(dir)
!
      do 50 jf = 1, nfrac
!
!           Calculate total sediment transport
!
         sib = disgse(jf,ixdir,ibr) * s
         if (sib .gt. 0.) then
            stot(jf) = stot(jf) + sib
         endif
50    continue
60 continue
!
!     Now distribute over outflowing branches
!
   do 80 i = 1, nrout
!
      ibr   = bgout(1,i)
      ixdir = bgout(2,i)
      dir   = bgout(3,i)
      s     = real(dir)
!
      do 70 jf = 1, nfrac
!
!           Calculate distribution factor alpha
!
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
!
!           Calculate integral value
!
         intgr(jf,ixdir,ibr) = -alpha * itot(jf) * s

70    continue
!
80 continue
!
end
