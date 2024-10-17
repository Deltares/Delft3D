subroutine FLCHZT(ibr    ,i      ,nbran  ,ngrid  ,&
&bfrict ,bfricp ,&
&maxtab ,ntabm  ,ntab   ,table  ,d90    ,&
&engpar ,isec   ,h      ,q      ,u      ,&
&r      ,c      )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLCHZT (FLow compute CHeZy Time dependent)
!
! Module description: In subroutine FLCHZT the Chezy coefficient will be
!                     computed for a certain grid point.
!
!                     Subroutine FLCHZT computes the Chezy coefficient
!                     in a grid point according to the user defined
!                     option for computing the Chezy coefficient.
!
!                     Chezy options are:
!
!                     -   Chezy for the main section may be defined as a
!                         constant, a function of the discharge, a
!                         function of the water level for each grid
!                         point and direction of flow.
!
!                     -   Chezy for each sub section may be defined as a
!                         constant for each grid point/section and di-
!                         rection of flow.
!
!                     -   Strickler/Manning coefficients may be defined
!                         for each grid point/section and direction of
!                         flow.
!
!                     -   Nikuradze coefficients are only defined for
!                         each section.
!
!                     -   The Engelund predictor is only used in the
!                         main section. The Engelund parameters are
!                         defined once for the whole model.
!
!                     Remark:
!
!                     Each branch may have a different Chezy formulation
!                     for main section, sub section 1 and sub section 2.
!                     The formulation for a section of a branch is
!                     fixed. The actual values for coefficient may dif-
!                     fer for each grid point.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 bfricp(6,ngrid)   I  Bed friction parameters:
!                         (1,i) = Parameter for positive flow direction
!                                 in main section (depending on friction
!                                 type):
!                                 =     Table pointer (Q or h table)
!                                 =     Chezy constant value
!                                 =     Nikuradse parameter kn for Ni-
!                                       kuradse formula
!                                 =     Manning parameter nm for Manning
!                                       formula
!                                 =     Strickler coefficient ks for
!                                       Strickler formula
!                         (2,i) = Parameter for negative flow direction
!                                 in main section (depending on friction
!                                 type) Same definitions as bfricp(1,i).
!                         (3,i) = Parameter for positive flow direction
!                                 in sub sec 1 (depending on friction
!                                 type) Same definitions as bfricp(1,i).
!                         (4,i) = Parameter for negative flow direction
!                                 in sub sec 1 (depending on friction
!                                 type) Same definition as bfricp (1,i):
!                         (5,i) = Parameter for positive flow direction
!                                 in sub sec 2 (depending on friction
!                                 type) Same definition as bfricp (1,i).
!                         (6,i) = Parameter for negative flow direction
!                                 in sub sec 2 (depending on friction
!                                 type) Same definition as bfricp (1,i).
!  5 bfrict(3,nbran)   I  Bed friction in sections of branch.
!                         For each branch and section a different fric-
!                         tion type can be defined.
!                         (1,i) = Friction type in main section in
!                                 branch i:
!                                 cfrchc (1) : Chezy constant
!                                 cfrchq (2) : Chezy function of Q
!                                 cfrchh (3) : Chezy function of h
!                                 cfrman (4) : Manning constant
!                                 cfrmaq (42): Manning function of Q
!                                 cfrmah (43): Manning function of h
!                                 cfrskn (5) : Strickler 1 constant Kn
!                                 cfrknq (52): Strickl 1 function of Q
!                                 cfrknh (53): Strickl 1 function of h
!                                 cfrsks (6) : Strickler 2 constant Ks
!                                 cfrksq (62): Strickl 2 function of Q
!                                 cfrksh (63): Strickl 2 function of h
!                                 cfrnik (7) : Nikuradze constant
!                                 cfrniq (72): Nikuradze function of Q
!                                 cfrnih (73): Nikuradze function of h
!                                 cfreng (8) : Engelund predictor
!                         (2,i) = Friction type in sub section 1 in
!                                 branch i:
!                                 Same definitions as bfrict(1,i)
!                         (3,i) = Friction type in sub section 2 in
!                                 branch i:
!                                 Same definitions as bfrict(1,i)
! 18 c(ngrid)          O  Actual Chezy coefficient for total channel in
!                         every grid point.
! 11 d90               P  -
! 12 engpar            P  -
! 14 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  2 i                 I  Gridpoint index in branch.
!  1 ibr               I  Branch number.
! 13 isec              I  Indicates section:
!                         0 = main section
!                         1 = sub section 1
!                         2 = sub section 2.
!  7 maxtab            I  Maximum number of defined tables.
!  3 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  9 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  8 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 15 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
! 17 r(ngrid)          I  Actual hydraulic radius for total channel in
!                         every grid point.
! 10 table             P  -
! 16 u                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! engrpr  ENGelund Roughness PRedictor
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flchzt.pf,v $
! Revision 1.6  1999/03/15  14:27:43  kuipe_j
! bed friction table general
!
! Revision 1.5  1997/08/08  10:54:29  kuipe_j
! minimum C=10
!
! Revision 1.4  1995/09/22  10:01:05  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:54:52  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:49  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:35  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:20  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:30:41  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer   maxtab, ntabm, ntab(4,maxtab)
   integer   nbran, ngrid, ibr, i, bfrict(3,nbran)
   integer   isec
   real      table(ntabm), engpar(9), bfricp(6,ngrid)
   real      d90, u, r, c
   double precision h, q
!
!     Declaration of local variables:
!
   integer   itab, kform, kvalue, pos, neg, isec1,&
   &juerd,kerd
   real      cpar, sixth, rad
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
   real    chlim(3)
   data    chlim/10.,1.,1./
!
!     Explanation:
!     -----------
!
!     1. Each Chezy formula, apart from Engelund bed friction, is defined
!        by 1 constant parameter. This constant is stored in bfricp.
!        An exception is the Engelund bed friction defined by 10 parameters.
!     2. For the Engelund bed friction the specific parameters are stored
!        in the array engpar.
!     3. Time independent Chezy coefficients are assigned at the start of
!        SOBEK time simulation.
!        As a matter of fact there is no need for adjustment, so this option
!        has been left out from this routine.
!
   sixth = 1./6.
!
!     Prevention against zero hydraulic radius
!
   rad =  max(r,1.e-6)
!
   isec1 = isec + 1
!
!     Formulation = .not. Engelund
!
   if (bfrict(isec1,ibr) .gt.      0  .and.&
   &bfrict(isec1,ibr) .ne. cfreng) then
!
!        Define constants
!
      pos    = isec*2 + 1
      neg    = isec*2 + 2
      kvalue = mod(bfrict(isec1,ibr),10)
      kform  = bfrict(isec1,ibr)
      if (kform.gt.10) then
         kform  = kform/10
      endif
!
!        Roughness function of discharge depending on flow direction
!
      if (kvalue .eq. cfrchq) then
         if (q .gt. 0D0) then
            itab = int(bfricp(pos,i))
         else
            itab = int(bfricp(neg,i))
         endif
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)), table(ntab(3,itab)),&
         &q, cpar)
!
!        Roughness function of water level depending on flow direction
!
      else if (kvalue .eq. cfrchh) then
         if (q .gt. 0D0) then
            itab = int(bfricp(pos,i))
         else
            itab = int(bfricp(neg,i))
         endif
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)), table(ntab(3,itab)),&
         &h, cpar)
!
!        Roughness constant depending on flow direction
!
      else
         if (q .gt. 0D0) then
            cpar = bfricp(pos,i)
         else
            cpar = bfricp(neg,i)
         endif
      endif
!
      if (kform .eq. cfrnik) then
!
!           Nikuradze-formula
!           [Doc. S-FO-001.5KV  Eq. 3-1]
!
         c = 18.0 * log10 (12.*rad/cpar)

      else if (kform .eq. cfrman) then
!
!           Manning-formula
!           [Doc. S-FO-001.5KV  Eq. 3-2]
!
         c = rad**sixth / cpar

      else if (kform .eq. cfrskn) then
!
!           Strickler-1 formula
!           [Doc. S-FO-001.5KV  Eq. 3-3]
!
         c = 25.0 * (rad/cpar)**sixth

      else if (kform .eq. cfrsks) then
!
!           Strickler-2 formula
!           [Doc. S-FO-001.5KV  Eq. 3-4]
!
         c = cpar * rad**sixth

      else
!
!           Chezy value (kode 0 or 1)
!
         c = cpar

      endif
!
!        The obtained Chezy value may never be less then 10. in
!        the main channel (Civil engineers common sense)
!        and less then 1. in the flood plains
!
      if (c .lt. chlim(isec1)) then
         call flroulim (isec1,c,juerd,kerd)
         c = chlim(isec1)
      endif
!
   else if (bfrict(isec1,ibr) .eq. cfreng) then
!
!        Engelund-like roughness predictor
!        [Doc. S-FO-001.5KV  Eq. 3-5]
!
      call engrpr(engpar ,d90  ,u  ,rad  ,c   )
!
   endif
!
end
