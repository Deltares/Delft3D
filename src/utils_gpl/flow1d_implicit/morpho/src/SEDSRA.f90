subroutine sedsra (inode  ,nnode ,nbran  ,ngrid  ,maxtab ,ntabm  ,&
!i3
&qtot   ,stotfr,bgout  ,sedinf ,sdrdbf ,seddb  ,&
&ntab   ,morcon,q2     ,table  ,dissed ,nfrac  ,&
&nfracb )
!u   &                   qtot   ,stot  ,bgout  ,sedinf ,sdrdbf ,seddb  ,
!u   &                   ntab   ,morcon,q2     ,table  ,dissed )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEDSRA (SEdiment Distribute Sediment RAtio)
!
! Module description: Calculate sediment transport according to a ratio
!                     distribution function.
!
!                     The sediment transport is divided over the two
!                     outflowing branches (i.e. that are branches that
!                     recieve transport from the node) by using the
!                     defined ratio table for Q1/Q2 and S1/S2. The user
!                     defines a table for each pair of outflowing bran-
!                     ches. For this purpose routine INTTAB is used. The
!                     arguments and function values are defined as posi-
!                     tive values.
!                     [ S-FO-002.2KV / Par 4.5 Method 3 ]
!                     If the definition is not present for the currently
!                     outflowing branches the proportional method will
!                     be used.
!                     [ S-FO-002.2KV / Eq. 4.8 ]
!                     After this routine SEDISE is called in case one or
!                     both branches are sedredge branches. Routine SEDI-
!                     SE distributes the sediment transport over the
!                     left and right channel.
!
! Precondition:       The total incoming discharge and transport in the
!                     node are positive.
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
!                                 (ratio) or coefficient ALPHA (linear).
!                         (4,i) = Undefined for ratio or coefficient
!                                 BETA (linear).
!  3 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  2 nnode             I  Number of nodes.
! 13 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
! 16 table(ntabm)      I  Contains all table values:
!                         An example for table 2 is given below:
!                         ntab (1,2)    <---->
!                         ntab (3,2)-------------+
!                         ntab (2,2)----+        |
!                                       |        |
!                                       v        v
!                         table | <1> | x2..x2 | y2..y2 | ... | <n> |
!                         -------------------------------------------
!                         - Fourier or tidal components:
!                         In this case the x-table with length ntab(1,k)
!                         contains the following information:
!                         1     : Avarage A0.
!                         2     : Number of harmonics  or tide frequen-
!                                 cies n.
!                         - For harmonics only:
!                         3     : Base frequency W0.
!                         - For every harmonic component:
!                         2+n*2 : Amplitude Ai.
!                         3+n*2 : Phase GAMMAi.
!                         - For every tidel components:
!                         n*3   : Frequency Wi.
!                         1+n*3 : Amplitude Ai.
!                         2+n*3 : Phase GAMMAi.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! equal   EQUAL test of two real variables
! inttab  INTerpolate in TABle
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
! $Log: sedsra.pf,v $
! Revision 1.3  1995/10/18  09:00:40  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:07:23  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:24  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:47:35  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:34:52  kuipe_j
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
!i2
   integer    inode ,nnode    ,nbran ,ngrid     ,maxtab ,ntabm ,&
   &nfrac ,nfracb
!u    integer    inode ,nnode    ,nbran ,ngrid     ,maxtab ,ntabm
   integer    sedinf(2,nbran) ,sdrdbf(2,*)      ,seddb(3,nnode),&
   &ntab  (4,maxtab),bgout (3,2)
!i4
   real       qtot
   real       morcon(2+2*nfrac,*) ,&
   &table (ntabm)       ,dissed(nfracb,2,nbran),&
   &stotfr(nfrac)
!u    real       qtot  ,stot
!u    real       morcon(4,*)     ,q2    (ngrid)    ,
!u   &           table (ntabm)   ,dissed(4,nbran)

   double precision q2(ngrid), q
   logical    equal
   external   equal
!
!     Declaration of local parameters
!
   integer    ibr    ,ibr1   ,ibr2   ,igr    ,igr1  ,igr2   ,dir  ,&
   &dir1   ,dir2   ,ibrmax ,ibrmin ,ind   ,ind1   ,ind2 ,&
!i1
   &indm   ,indmax ,sedbr  ,i      ,itab  ,iside  ,ifrac
!u   &           indm   ,indmax ,sedbr  ,i      ,itab
   real       qratio ,sratio ,sedt2  ,s
   real       dissdb (2)
!
!     Determine which branch is branch-1 and which is branch-2.
!     Also get the index in array Morcon that refers to the 2 branches
!     with incoming sediment transport.
!
   ibr1   = bgout(1,1)
   ibr2   = bgout(1,2)
   ibrmax = max  (ibr1,ibr2)
   ibrmin = min  (ibr1,ibr2)
   ind    = seddb(2,inode)
   indmax = ind + seddb(3,inode)
   i      = ind
   indm   = 0
!
! ----do while index in array Morcon found --->
10 continue
   if (i .lt. indmax) then
      if (max(int(morcon(1,i)),int(morcon(2,i))) .eq. ibrmax) then
         if (min(int(morcon(1,i)),int(morcon(2,i))) .eq. ibrmin) then
            if (int(morcon(1,i)) .eq. ibr1) then
               ind1 = 1
               ind2 = 2
               indm = i
            else
               ind1 = 2
               ind2 = 1
               indm = i
            endif
            i = indmax
         endif
      endif
      i = i+1
      goto 10
   endif
! <---while end ------------------------------
!
   if (indm .ne. 0) then
!
!        Pair of branches with user defined distribution found.
!
      ibr1  = bgout(1,ind1)
      ibr2  = bgout(1,ind2)
      igr1  = bgout(2,ind1)
      igr2  = bgout(2,ind2)
      dir1  = bgout(3,ind1)
      dir2  = bgout(3,ind2)
!
!        Index Ind1 points to branch 1
!        Index Ind2 points to branch 2
!        Now distribute incoming sediment over the 2 branches.
!
!i2
      do 12 ifrac=1,nfrac
         itab = int(morcon(1+2*ifrac,indm))
!u       itab = int(morcon(3,indm))
         if(equal(q2(igr2),0.)) then
!
!           Get S1/S2 for largest Q1/Q2 ratio
            if (table(ntab(2,itab)) .gt.&
            &table(ntab(2,itab)+ntab(1,itab)-1)) then
               sratio = table(ntab(3,itab))
            else
               sratio = table(ntab(3,itab)+ntab(1,itab)-1)
            endif
         else
            qratio = abs(q2(igr1) / q2(igr2))
!
!           Interpolate in table and get S1/S2 = f(Q1/Q2)
!
            call inttab (ntab (1,itab)      ,ntab(4,itab),&
            &table(ntab(2,itab)),&
            &table(ntab(3,itab)),&
            &dble(qratio),sratio)
!
         endif
!
!i4
         sedt2 = stotfr(ifrac) / (sratio + 1.)
         dissed(ifrac,(3-dir1)/2,ibr1) =&
         &(stotfr(ifrac) - sedt2) *real(dir1)
         dissed(ifrac,(3-dir2)/2,ibr2) = sedt2 * real(dir2)
!u       sedt2 = stot / (sratio + 1.)
!u       dissed(2-dir1,ibr1) = (stot - sedt2) *real(dir1)
!u       dissed(2-dir2,ibr2) = sedt2 * real(dir2)
!i1
12    continue
!
   else
!
!        No pair of branches with user defined distribution found.
!        The proportional method will be used yet.
!
      do 20 i = 1,2
!
         ibr = bgout(1,i)
         igr = bgout(2,i)
         dir = bgout(3,i)
         if (qtot .gt. 1.e-10) then
            qratio = abs(q2(igr)) / qtot
         else
            qratio = .5
         endif
!i4
         iside = (3-dir)/2
         do 18 ifrac=1,nfrac
            dissed(ifrac,iside,ibr) =&
            &qratio * stotfr(ifrac) * real(dir)
18       continue
!u          dissed(2-dir,ibr) = qratio * stot * real(dir)
20    continue
!
   endif
!
!     Check for sedredge branches and if nessecary distribute sediment
!     over both channels.
!
   do 30 i = 1,2
!
      ibr = bgout(1,i)
      igr = bgout(2,i)
      dir = bgout(3,i)
!
!        Get sedredge branch number
!
      sedbr = sedinf(1,ibr)
      if (sedbr .ne. 0) then
!
!           Get total sediment and total discharge of the sedredge
!           branch.
!
!i2
         iside = (3-dir)/2
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
30 continue
!
end
