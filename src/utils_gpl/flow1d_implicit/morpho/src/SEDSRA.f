      subroutine sedsra (inode  ,nnode ,nbran  ,ngrid  ,maxtab ,ntabm  ,
ci3
     &                   qtot   ,stotfr,bgout  ,sedinf ,sdrdbf ,seddb  ,
     &                   ntab   ,morcon,q2     ,table  ,dissed ,nfrac  ,
     &                   nfracb )
cu   &                   qtot   ,stot  ,bgout  ,sedinf ,sdrdbf ,seddb  ,
cu   &                   ntab   ,morcon,q2     ,table  ,dissed )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEDSRA (SEdiment Distribute Sediment RAtio)
c
c Module description: Calculate sediment transport according to a ratio
c                     distribution function.
c
c                     The sediment transport is divided over the two
c                     outflowing branches (i.e. that are branches that
c                     recieve transport from the node) by using the
c                     defined ratio table for Q1/Q2 and S1/S2. The user
c                     defines a table for each pair of outflowing bran-
c                     ches. For this purpose routine INTTAB is used. The
c                     arguments and function values are defined as posi-
c                     tive values.
c                     [ S-FO-002.2KV / Par 4.5 Method 3 ]
c                     If the definition is not present for the currently
c                     outflowing branches the proportional method will
c                     be used.
c                     [ S-FO-002.2KV / Eq. 4.8 ]
c                     After this routine SEDISE is called in case one or
c                     both branches are sedredge branches. Routine SEDI-
c                     SE distributes the sediment transport over the
c                     left and right channel.
c
c Precondition:       The total incoming discharge and transport in the
c                     node are positive.
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
c                                 (ratio) or coefficient ALPHA (linear).
c                         (4,i) = Undefined for ratio or coefficient
c                                 BETA (linear).
c  3 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  2 nnode             I  Number of nodes.
c 13 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c 16 table(ntabm)      I  Contains all table values:
c                         An example for table 2 is given below:
c                         ntab (1,2)    <---->
c                         ntab (3,2)-------------+
c                         ntab (2,2)----+        |
c                                       |        |
c                                       v        v
c                         table | <1> | x2..x2 | y2..y2 | ... | <n> |
c                         -------------------------------------------
c                         - Fourier or tidal components:
c                         In this case the x-table with length ntab(1,k)
c                         contains the following information:
c                         1     : Avarage A0.
c                         2     : Number of harmonics  or tide frequen-
c                                 cies n.
c                         - For harmonics only:
c                         3     : Base frequency W0.
c                         - For every harmonic component:
c                         2+n*2 : Amplitude Ai.
c                         3+n*2 : Phase GAMMAi.
c                         - For every tidel components:
c                         n*3   : Frequency Wi.
c                         1+n*3 : Amplitude Ai.
c                         2+n*3 : Phase GAMMAi.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c equal   EQUAL test of two real variables
c inttab  INTerpolate in TABle
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
c $Log: sedsra.pf,v $
c Revision 1.3  1995/10/18  09:00:40  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:07:23  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:24  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:47:35  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:34:52  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:21  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
ci2
      integer    inode ,nnode    ,nbran ,ngrid     ,maxtab ,ntabm ,
     &           nfrac ,nfracb
cu    integer    inode ,nnode    ,nbran ,ngrid     ,maxtab ,ntabm
      integer    sedinf(2,nbran) ,sdrdbf(2,*)      ,seddb(3,nnode),
     &           ntab  (4,maxtab),bgout (3,2)
ci4
      real       qtot
      real       morcon(2+2*nfrac,*) ,
     &           table (ntabm)       ,dissed(nfracb,2,nbran),
     &           stotfr(nfrac)
cu    real       qtot  ,stot
cu    real       morcon(4,*)     ,q2    (ngrid)    ,
cu   &           table (ntabm)   ,dissed(4,nbran)

      double precision q2(ngrid), q
      logical    equal
      external   equal
c
c     Declaration of local parameters
c
      integer    ibr    ,ibr1   ,ibr2   ,igr    ,igr1  ,igr2   ,dir  ,
     &           dir1   ,dir2   ,ibrmax ,ibrmin ,ind   ,ind1   ,ind2 ,
ci1
     &           indm   ,indmax ,sedbr  ,i      ,itab  ,iside  ,ifrac
cu   &           indm   ,indmax ,sedbr  ,i      ,itab
      real       qratio ,sratio ,sedt2  ,s
      real       dissdb (2)
c
c     Determine which branch is branch-1 and which is branch-2.
c     Also get the index in array Morcon that refers to the 2 branches
c     with incoming sediment transport.
c
      ibr1   = bgout(1,1)
      ibr2   = bgout(1,2)
      ibrmax = max  (ibr1,ibr2)
      ibrmin = min  (ibr1,ibr2)
      ind    = seddb(2,inode)
      indmax = ind + seddb(3,inode)
      i      = ind
      indm   = 0
c
c ----do while index in array Morcon found --->
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
c <---while end ------------------------------
c
      if (indm .ne. 0) then
c
c        Pair of branches with user defined distribution found.
c
         ibr1  = bgout(1,ind1)
         ibr2  = bgout(1,ind2)
         igr1  = bgout(2,ind1)
         igr2  = bgout(2,ind2)
         dir1  = bgout(3,ind1)
         dir2  = bgout(3,ind2)
c
c        Index Ind1 points to branch 1
c        Index Ind2 points to branch 2
c        Now distribute incoming sediment over the 2 branches.
c
ci2
         do 12 ifrac=1,nfrac
         itab = int(morcon(1+2*ifrac,indm))
cu       itab = int(morcon(3,indm))
         if(equal(q2(igr2),0.)) then
c
c           Get S1/S2 for largest Q1/Q2 ratio
            if (table(ntab(2,itab)) .gt.
     &          table(ntab(2,itab)+ntab(1,itab)-1)) then
               sratio = table(ntab(3,itab))
            else
               sratio = table(ntab(3,itab)+ntab(1,itab)-1)
            endif
         else
            qratio = abs(q2(igr1) / q2(igr2))
c
c           Interpolate in table and get S1/S2 = f(Q1/Q2)
c
            call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                   table(ntab(2,itab)),
     &                   table(ntab(3,itab)),
     &                   dble(qratio),sratio)
c
         endif
c
ci4
         sedt2 = stotfr(ifrac) / (sratio + 1.)
         dissed(ifrac,(3-dir1)/2,ibr1) =
     &                  (stotfr(ifrac) - sedt2) *real(dir1)
         dissed(ifrac,(3-dir2)/2,ibr2) = sedt2 * real(dir2)
cu       sedt2 = stot / (sratio + 1.)
cu       dissed(2-dir1,ibr1) = (stot - sedt2) *real(dir1)
cu       dissed(2-dir2,ibr2) = sedt2 * real(dir2)
ci1
   12    continue
c
      else
c
c        No pair of branches with user defined distribution found.
c        The proportional method will be used yet.
c
         do 20 i = 1,2
c
            ibr = bgout(1,i)
            igr = bgout(2,i)
            dir = bgout(3,i)
            if (qtot .gt. 1.e-10) then
               qratio = abs(q2(igr)) / qtot
            else
               qratio = .5
            endif
ci4
            iside = (3-dir)/2
            do 18 ifrac=1,nfrac
               dissed(ifrac,iside,ibr) =
     &                 qratio * stotfr(ifrac) * real(dir)
   18       continue
cu          dissed(2-dir,ibr) = qratio * stot * real(dir)
   20    continue
c
      endif
c
c     Check for sedredge branches and if nessecary distribute sediment
c     over both channels.
c
      do 30 i = 1,2
c
         ibr = bgout(1,i)
         igr = bgout(2,i)
         dir = bgout(3,i)
c
c        Get sedredge branch number
c
         sedbr = sedinf(1,ibr)
         if (sedbr .ne. 0) then
c
c           Get total sediment and total discharge of the sedredge
c           branch.
c
ci2         
            iside = (3-dir)/2
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
      end
