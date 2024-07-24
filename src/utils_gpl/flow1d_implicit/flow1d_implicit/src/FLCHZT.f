      subroutine FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,
     +                  bfrict ,bfricp ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,d90    ,
     +                  engpar ,isec   ,h      ,q      ,u      ,
     +                  r      ,c      )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLCHZT (FLow compute CHeZy Time dependent)
c
c Module description: In subroutine FLCHZT the Chezy coefficient will be
c                     computed for a certain grid point.
c
c                     Subroutine FLCHZT computes the Chezy coefficient
c                     in a grid point according to the user defined
c                     option for computing the Chezy coefficient.
c
c                     Chezy options are:
c
c                     -   Chezy for the main section may be defined as a
c                         constant, a function of the discharge, a
c                         function of the water level for each grid
c                         point and direction of flow.
c
c                     -   Chezy for each sub section may be defined as a
c                         constant for each grid point/section and di-
c                         rection of flow.
c
c                     -   Strickler/Manning coefficients may be defined
c                         for each grid point/section and direction of
c                         flow.
c
c                     -   Nikuradze coefficients are only defined for
c                         each section.
c
c                     -   The Engelund predictor is only used in the
c                         main section. The Engelund parameters are
c                         defined once for the whole model.
c
c                     Remark:
c
c                     Each branch may have a different Chezy formulation
c                     for main section, sub section 1 and sub section 2.
c                     The formulation for a section of a branch is
c                     fixed. The actual values for coefficient may dif-
c                     fer for each grid point.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 bfricp(6,ngrid)   I  Bed friction parameters:
c                         (1,i) = Parameter for positive flow direction
c                                 in main section (depending on friction
c                                 type):
c                                 =     Table pointer (Q or h table)
c                                 =     Chezy constant value
c                                 =     Nikuradse parameter kn for Ni-
c                                       kuradse formula
c                                 =     Manning parameter nm for Manning
c                                       formula
c                                 =     Strickler coefficient ks for
c                                       Strickler formula
c                         (2,i) = Parameter for negative flow direction
c                                 in main section (depending on friction
c                                 type) Same definitions as bfricp(1,i).
c                         (3,i) = Parameter for positive flow direction
c                                 in sub sec 1 (depending on friction
c                                 type) Same definitions as bfricp(1,i).
c                         (4,i) = Parameter for negative flow direction
c                                 in sub sec 1 (depending on friction
c                                 type) Same definition as bfricp (1,i):
c                         (5,i) = Parameter for positive flow direction
c                                 in sub sec 2 (depending on friction
c                                 type) Same definition as bfricp (1,i).
c                         (6,i) = Parameter for negative flow direction
c                                 in sub sec 2 (depending on friction
c                                 type) Same definition as bfricp (1,i).
c  5 bfrict(3,nbran)   I  Bed friction in sections of branch.
c                         For each branch and section a different fric-
c                         tion type can be defined.
c                         (1,i) = Friction type in main section in
c                                 branch i:
c                                 cfrchc (1) : Chezy constant
c                                 cfrchq (2) : Chezy function of Q
c                                 cfrchh (3) : Chezy function of h
c                                 cfrman (4) : Manning constant
c                                 cfrmaq (42): Manning function of Q
c                                 cfrmah (43): Manning function of h
c                                 cfrskn (5) : Strickler 1 constant Kn
c                                 cfrknq (52): Strickl 1 function of Q
c                                 cfrknh (53): Strickl 1 function of h
c                                 cfrsks (6) : Strickler 2 constant Ks
c                                 cfrksq (62): Strickl 2 function of Q
c                                 cfrksh (63): Strickl 2 function of h
c                                 cfrnik (7) : Nikuradze constant
c                                 cfrniq (72): Nikuradze function of Q
c                                 cfrnih (73): Nikuradze function of h
c                                 cfreng (8) : Engelund predictor
c                         (2,i) = Friction type in sub section 1 in
c                                 branch i:
c                                 Same definitions as bfrict(1,i)
c                         (3,i) = Friction type in sub section 2 in
c                                 branch i:
c                                 Same definitions as bfrict(1,i)
c 18 c(ngrid)          O  Actual Chezy coefficient for total channel in
c                         every grid point.
c 11 d90               P  -
c 12 engpar            P  -
c 14 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  2 i                 I  Gridpoint index in branch.
c  1 ibr               I  Branch number.
c 13 isec              I  Indicates section:
c                         0 = main section
c                         1 = sub section 1
c                         2 = sub section 2.
c  7 maxtab            I  Maximum number of defined tables.
c  3 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  9 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c  8 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 15 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c 17 r(ngrid)          I  Actual hydraulic radius for total channel in
c                         every grid point.
c 10 table             P  -
c 16 u                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c engrpr  ENGelund Roughness PRedictor
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
c $Log: flchzt.pf,v $
c Revision 1.6  1999/03/15  14:27:43  kuipe_j
c bed friction table general
c
c Revision 1.5  1997/08/08  10:54:29  kuipe_j
c minimum C=10
c
c Revision 1.4  1995/09/22  10:01:05  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:54:52  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:49  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:35  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:20  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:30:41  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer   maxtab, ntabm, ntab(4,maxtab)
      integer   nbran, ngrid, ibr, i, bfrict(3,nbran)
      integer   isec
      real      table(ntabm), engpar(9), bfricp(6,ngrid)
      real      d90, u, r, c
      double precision h, q
c
c     Declaration of local variables:
c
      integer   itab, kform, kvalue, pos, neg, isec1,
     +          juerd,kerd
      real      cpar, sixth, rad
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c      
      real    chlim(3)
      data    chlim/10.,1.,1./
c
c     Explanation:
c     -----------
c
c     1. Each Chezy formula, apart from Engelund bed friction, is defined
c        by 1 constant parameter. This constant is stored in bfricp.
c        An exception is the Engelund bed friction defined by 10 parameters.
c     2. For the Engelund bed friction the specific parameters are stored
c        in the array engpar.
c     3. Time independent Chezy coefficients are assigned at the start of
c        SOBEK time simulation.
c        As a matter of fact there is no need for adjustment, so this option
c        has been left out from this routine.
c
      sixth = 1./6.
c
c     Prevention against zero hydraulic radius
c
      rad =  max(r,1.e-6)
c
      isec1 = isec + 1
c
c     Formulation = .not. Engelund
c
      if (bfrict(isec1,ibr) .gt.      0  .and.
     +    bfrict(isec1,ibr) .ne. cfreng) then
c
c        Define constants
c
         pos    = isec*2 + 1
         neg    = isec*2 + 2
         kvalue = mod(bfrict(isec1,ibr),10)
         kform  = bfrict(isec1,ibr)
         if (kform.gt.10) then
            kform  = kform/10
         endif
c
c        Roughness function of discharge depending on flow direction
c
         if (kvalue .eq. cfrchq) then
            if (q .gt. 0D0) then
               itab = int(bfricp(pos,i))
            else
               itab = int(bfricp(neg,i))
            endif
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)), table(ntab(3,itab)),
     +                   q, cpar)
c
c        Roughness function of water level depending on flow direction
c
         else if (kvalue .eq. cfrchh) then
            if (q .gt. 0D0) then
               itab = int(bfricp(pos,i))
            else
               itab = int(bfricp(neg,i))
            endif
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)), table(ntab(3,itab)),
     +                   h, cpar)
c
c        Roughness constant depending on flow direction
c
         else
            if (q .gt. 0D0) then
               cpar = bfricp(pos,i)
            else
               cpar = bfricp(neg,i)
            endif
         endif
c
         if (kform .eq. cfrnik) then
c
c           Nikuradze-formula
c           [Doc. S-FO-001.5KV  Eq. 3-1]
c
            c = 18.0 * alog10 (12.*rad/cpar)

         else if (kform .eq. cfrman) then
c
c           Manning-formula
c           [Doc. S-FO-001.5KV  Eq. 3-2]
c
            c = rad**sixth / cpar

         else if (kform .eq. cfrskn) then
c
c           Strickler-1 formula
c           [Doc. S-FO-001.5KV  Eq. 3-3]
c
            c = 25.0 * (rad/cpar)**sixth

         else if (kform .eq. cfrsks) then
c
c           Strickler-2 formula
c           [Doc. S-FO-001.5KV  Eq. 3-4]
c
            c = cpar * rad**sixth

         else
c
c           Chezy value (kode 0 or 1)
c
            c = cpar

         endif
c
c        The obtained Chezy value may never be less then 10. in
c        the main channel (Civil engineers common sense)
c        and less then 1. in the flood plains
c
         if (c .lt. chlim(isec1)) then
            call flroulim (isec1,c,juerd,kerd)
            c = chlim(isec1)
         endif   
c
      else if (bfrict(isec1,ibr) .eq. cfreng) then
c
c        Engelund-like roughness predictor
c        [Doc. S-FO-001.5KV  Eq. 3-5]
c
         call engrpr(engpar ,d90  ,u  ,rad  ,c   )
c
      endif
c
      end
