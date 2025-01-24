subroutine MOINOD (inode  ,nnode  ,nbran  ,nbrnod ,ngrid ,dtm    ,&
&alphac,branch  ,brnode ,typcr  ,bgout ,grid   ,&
&dissed ,intgr  ,x      ,celer  ,&
&mopta  ,moptb  ,moptc  ,moptd  ,flwdir,alphad ,&
&sedtr  ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOINOD (MOrphology Integral for a NODe)
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
! 10 alphac            P  -
! 25 alphad            P  -
! 14 bgout(3,nrout)    IO Contains info for every outflowing branch i
!                         from the current node:
!                         (1,i) = Branch number
!                         (2,i) = Grid point number
!                         (3,i) = Direction in branch
!                                 +1: First point of branch
!                                 -1: Last point of branch
! 11 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 12 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
! 19 celer             P  -
! 16 dissed(4,nbran)   I  Redistributed sediment transport at begin and
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
!  9 dtm               P  -
! 24 flwdir(ngrid)     I  Indicator for flow direction at each grid
!                         point      1 = positive flow
!                                    0 = zero flow
!                                   -1 = negative flow
! 15 grid              P  -
!  1 inode             I  Node number to be processed
! 17 intgr(ngrid,*)    IO Integral values for grid
! 27 juer              P  -
! 28 ker               P  -
! 20 mopta             P  -
! 21 moptb             P  -
! 22 moptc             P  -
! 23 moptd             P  -
!  3 nbran             I  Number of branches.
!  4 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  5 ngrid             I  Number of grid points in network.
!  2 nnode             I  Number of nodes.
!                         table).
! 26 sedtr             P  -
! 13 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 18 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! moitno  MOrphology InTegral at a NOde
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moinod.pf,v $
! Revision 1.10  1999/03/15  15:52:53  kuipe_j
! tabs removed
!
! Revision 1.9  1998/06/11  11:47:11  kuipe_j
! Estuary special integrated
!
! Revision 1.8  1996/12/04  12:01:05  kuipe_j
! declarations / undefined vars
!
! Revision 1.7  1996/10/31  10:31:55  kuipe_j
! Extra resistance finished
!
! Revision 1.6  1996/09/03  14:48:48  kuipe_j
! frequency time hist,Improved sed distribution at nodes
!
! Revision 1.5  1996/03/08  09:39:07  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.4  1996/03/07  10:44:16  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.3  1995/05/30  09:55:54  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:49  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:17  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:52:36  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:46  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    inode  ,nnode  ,nbran  ,nbrnod ,ngrid  ,&
   &juer   ,ker

   real       alphac ,alphad

   integer    branch (4,nbran)       ,&
   &flwdir (ngrid)         ,&
   &brnode (nbrnod+1,nnode),&
   &typcr  (nbran)         ,&
   &bgout  (3,nbrnod)      ,&
   &grid   (ngrid)

   real       dissed (4,nbran),&
   &intgr  (ngrid,2,*),&
   &x      (ngrid)  ,&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*)

   double     precision  dtm

   logical    mopta, moptb ,moptc ,moptd
!
!     Declaration of local parameters
!
   integer    ibr    ,nbn    ,igp    ,dir    ,i      ,nrout  ,&
   &isec   ,nsec
   real       stot   ,itot   ,itote  ,alpha  ,sib
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   nrout = 0
   itot  = 0.
   itote = 0.
!
   nbn = brnode(1,inode)
!
   do 20 i = 1, nbn
!
      ibr = brnode(i+1,inode)
!
!        Determine if node is at the beginning or end of the branch.
!
      if (branch (1,ibr) .eq. inode) then
!           Begin of branch
         igp = branch (3,ibr)
         dir = 1
      else
!           End of branch
         igp = branch (4,ibr)
         dir = -1
      endif
!
      if (dir * flwdir(igp) .lt. 0. ) then
!
!           Inflow in node.
!
         if (typcr(ibr) .eq. ccrsed) then
            nsec = 2
         else
            nsec = 1
         endif
!
!           Calculate total sediment transport and integrals
!
         do 10 isec = 1, nsec
!
!              Calculate total sediment transport
!
!              Calculate integral
!
            call MOITNO ( igp    ,isec   ,ngrid  ,x      ,&
            &dtm    ,alphac ,inode  ,branch ,&
            &ibr    ,nbran  ,grid   ,&
            &mopta  ,moptb  ,moptc  ,moptd  ,&
            &celer  ,sedtr  ,alphad ,flwdir ,&
            &juer   ,ker    ,intgr(igp,1,isec))
!
!              Calculate extra integral
!
            call MOIEXT  (igp    ,dir    ,isec   ,ngrid  ,&
            &dtm    ,x      ,celer  ,sedtr  ,&
            &intgr(igp,2,isec) )
!
!              Calculate total integrals
!
            itot = itot + intgr(igp,1,isec)*real(dir)
            itote= itote+ intgr(igp,2,isec)*real(dir)
10       continue
!
      else
!
!           Outflow from node.
!
         nrout = nrout + 1
         bgout(1,nrout) = ibr
         bgout(2,nrout) = igp
         bgout(3,nrout) = dir
!
      endif
20 continue
!
!     Now distribute over outflowing branches
!     Outflowing is determined by the sign of the celerity.
!     If the sign of the transport is opposite, this branch
!     will get a zero transport.
!
   stot = 0.
   do 40 i = 1, nrout
!
      ibr = bgout(1,i)
      dir = bgout(3,i)
!
      if (typcr(ibr) .eq. ccrsed) then
         nsec = 2
      else
         nsec = 1
      endif
!
      do 30 isec = 1, nsec
!
         sib = dissed(1+isec-dir,ibr) * real(dir)
         if (sib .gt. 0.) then
            stot = stot + sib
         endif
!
30    continue
40 continue
!
!     Now distribute over outflowing branches
!
   do 60 i = 1, nrout
!
      ibr = bgout(1,i)
      igp = bgout(2,i)
      dir = bgout(3,i)
!
!        Hier stond (typecr(ibr) .ne. 0), gewijzigd op 27-9-93
!
      if (typcr(ibr) .eq. ccrsed) then
         nsec = 2
      else
         nsec = 1
      endif
!
      do 50 isec = 1, nsec
!
!           Calculate distribution factor alpha
!
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
!
!           Calculate integral values
!
         intgr(igp,1,isec) = -alpha * itot  * real(dir)
         intgr(igp,2,isec) = -alpha * itote * real(dir)

50    continue
60 continue
!
end
