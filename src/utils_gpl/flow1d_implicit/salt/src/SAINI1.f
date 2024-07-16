      subroutine saini1 (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid  ,maxtab,
     &                   ntabm  ,juer   ,g      ,rhow   ,time   ,tp    ,
     &                   le     ,dispf  ,mouth  ,branch ,bramrl ,ntab  ,
     &                   table  ,sbdpar ,moupar ,grid   ,x      ,af    ,
     &                   wf     ,c      ,q2     ,csa2   ,distmp ,disgr ,
     &                   csd2   ,rho    ,cdcdx0 ,cdcdx1 ,cdcdx2 ,thasca,
     &                   mouqpu ,tw     ,thcsum ,timout ,sbdscr ,ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAINI1 (SAlt INItialise subroutine 1)
c
c Module description: Set the initial salt concentration values for each
c                     gridpoint.
c
c                     The user specifies the initial concentrations for
c                     each gridpoint in the user interface. These con-
c                     centrations are needed for the advection diffusion
c                     equation. The initial diffusion (c's) must be cal-
c                     culated in every grid point. Also the salt concen-
c                     tration at outflow boundaries must be stored.
c
c                     In the case that the Thatcher-Harleman or Zwendl
c                     formulation has been chosen as the dispersion
c                     formulation a.o. the following will be calculated:
c                     -   The constant term of the Thatcher-Harleman
c                         formulation.
c                     -   The initial values for the fresh water di-
c                         scharge, the flood volume and maximum flood
c                         velocity.
c                     -   The initial value of the term <c/c0*dc/dx>.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 24 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 17 bramrl(nmouth+1,  I  Branch-Mouth relation table. The first index
c        ,nbran)          contains the number of related mouths (index
c                         1). The second index contains the first rela-
c                         ted mouth number etc.
c 16 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 26 c                 P  -
c 33 cdcdx0(ngrid)     O  Contains for every grid point on time t=n+1
c                         the sum of the term c/c0*dc/dx over the cur-
c                         rent tide.
c 34 cdcdx1(ngrid)     IO Contains for every grid point the averaged
c                         value of term <c/c0*dc/dx> over the last tide.
c 35 cdcdx2(ngrid)     O  Contains for every grid point the averaged
c                         value of term <c/c0*dc/dx> over the tide befo-
c                         re the last tide.
c 28 csa2(ngrid)       I  Salt concentration in every grid point at time
c                         t(n+1).
c 31 csd2(ngrid)       O  Diffusion (c s) in every grid point at time
c                         t(n+1).
c 30 disgr(ngrid)      I  Dispersion coefficient in every grid point at
c                         time t(n+1).
c 14 dispf             P  -
c 29 distmp(ngrid)     I  Scratch array for dispersion calculation.
c  1 dsopt             I  Option for dispersion for the whole network:
c                           cds1fu (1) : One function of place or time
c                           cds2fu (2) : Two functions of place or time
c                           cdsthh (3) : Thatcher-Harleman formulation
c                           cdsemp (4) : Empirical formulation
c  9 g                 I  Acceleration of gravity.
c 22 grid              P  -
c  8 juer              P  -
c 42 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 13 le                I  Estuary length (salt module).
c  6 maxtab            I  Maximum number of defined tables.
c 21 moupar(5,nmouth)  I  Mouth parameters (for Thatcher-Harleman or
c                         Empirical formulation).
c                         (1,i) = Reference salt concentration at mouth
c                                 i.
c                         (2,i) = Reference water depth at mouth i.
c                         (3,i) = Characteristic flood velocity at mouth
c                                 i.
c                         (4,i) = Density of sea water at mouth i.
c                         (5,i) = Initial fresh water discharge at mouth
c                                 i.
c 37 mouqpu(3,0:2,     IO Contains auxilliary data for the Thatcher
c        nmouth)          Harleman or ZWENDL dispersion formulation.
c                         - First index:
c                         (1,,) = Fresh water discharge.
c                         (2,,) = Flood volume.
c                         (3,,) = Maximum flood velocity.
c                         - Second index:
c                         (,0,) = For current tide. Mouqpu(i,0,j) con-
c                                 tains the actual sum or maximum on the
c                                 current time.
c                         (,1,) = For the last tide.
c                         (,2,) = For the tide before the last tide.
c                         - Third index:
c                         (,,i) = Mouth number.
c 15 mouth(2,nmouth)   I  Node numbers which are mouths:
c                         (1,i) = Node number j which is a mouth.
c                         (2,i) = Number of the branch that contains the
c                                 mouth.
c  3 nboun             I  Number of boundary nodes.
c  4 nbran             I  Number of branches.
c  5 ngrid             I  Number of grid points in network.
c  2 nmouth            I  Maximum number of mouths in the network.
c 18 ntab              P  -
c  7 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 27 q2                P  -
c 32 rho               P  -
c 10 rhow              I  Density of fresh water.
c 20 sbdpar(5,nboun)   I  Definition of salt boundary conditions:
c                         (1,i) = Option for boundary condition:
c                                 csbusr (1) : User specified concen-
c                                              tration at inflow
c                                 csbthh (2) : Thatcher-Harleman for-
c                                              mulation at inflow
c                                 csbflx (3) : Zero flux
c                         (2,i) = Location (node number).
c                         (3,i) = Branch number that is connected.
c                         (4,i) = Table number (Options 1 and 2).
c                         (5,i) = T0 period for option 2, else undefi-
c                                 ned.
c 41 sbdscr(3,nboun)   O  Intermediate results at salt boundaries:
c                         (1,i) = Last time of outflow (option = 2)
c                         (2,i) = Concentration at last time of outflow
c                                 (option = 2)
c                         (3,i) = Concentration at inflow (time n+1) if
c                                 option is 1 or 2
c 19 table             P  -
c 36 thasca(3)         O  Administration for the calculation of
c                         <c/c0*dc/dx>:
c                         (1) =   End time of current tide.
c                         (2) =   Number of time steps that contribute
c                                 in current sum.
c                         (3) =   0 : Frst tidal period not started yet.
c                                 1 : Fist tidal period has started.
c 39 thcsum(2,nbran)   O  Contains Thatcher-Harleman sum per branch:
c                         (1,i) = Constant part of Thatcher-Harleman
c                                 sum.
c                         (2,i) = Part of Thatcher-Harleman sum that is
c                                 constant in one tidal period.
c 11 time              I  Actual time level tn+1. in sec.
c 40 timout(2,nmouth)  O  Administration for the calculation of fresh
c                         water discharge, flood volume and maximum
c                         flood velocity for every mouth:
c                         (1,i) = Starting time of current tide.
c                         (2,i) = 0 : Fist tidal period not started yet.
c                                 1 : Fist tidal period has started.
c 12 tp                I  Tidal period   (salt module).
c 38 tw                P  -
c 25 wf                P  -
c 23 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c sadcdx  SAlt DC/DX calculation
c sadens  SAlt DENSity
c sadspc  SAlt DiSPersion Coefficient
c satbpa  SAlt Thatcher harleman Branch PArameters
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: saini1.pf,v $
c Revision 1.7  1999/06/01  13:42:36  kuipe_j
c names in messages substituted + message template
c
c Revision 1.6  1995/10/18  09:00:24  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.5  1995/08/30  12:37:19  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:42  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:56:11  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:06  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:48  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:13  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:46  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer dsopt ,nmouth  ,nboun ,nbran   ,ngrid    ,maxtab  ,ntabm ,
     &        juer  ,ker
      integer dispf (2,3)    ,mouth (2,*)       ,branch(4,nbran) ,
     &        bramrl(nmouth+1,nbran)            ,ntab(4,maxtab)
      integer grid  (ngrid)
      real    g     ,rhow      ,le
      real    table (ntabm)    ,sbdpar(5,nboun) ,moupar(5,*)   ,
     &        x     (ngrid)    ,af    (ngrid)   ,wf    (ngrid) ,
     &        c     (ngrid)    ,csa2  (ngrid)   ,
     &        distmp(ngrid)    ,disgr (ngrid)   ,csd2  (ngrid) ,
     &        cdcdx0(ngrid)    ,cdcdx1(ngrid)   ,cdcdx2(ngrid) ,
     &        rho   (ngrid)    ,
     &        thasca(3    )    ,mouqpu(3,0:2,*) ,
     &        tw    (nbran)    ,thcsum(2,nbran) ,
     &        timout(2,*)      ,sbdscr(3,nboun)
      double  precision  time  , tp, q2(ngrid) 
c
c     Declaration of local variables
c
      integer   ibr   ,igr    ,i1   ,i2    ,nmj    ,im     ,i   ,j   ,
     &          ib    ,lbrnam
      real      c0j   ,confac ,d0j  ,rhoj  ,uastj  ,castj  ,pi
      character*40     branam
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
c     Calculate dc/dx
c                                                               dc/dx
      call sadcdx (nbran  ,ngrid  ,branch ,grid   ,csa2   ,x   ,distmp)
c
c---- Initialize in case of Thatcher Harleman or ZWENDL dispersion ----
c     formulation.
c
      if (dsopt .eq. cdsthh .or. dsopt .eq. cdsemp) then
c
         pi = atan(1.)*4.
         do 10 im = 1,nmouth
c
c           Calculate grid point at mouth
c
            ibr = mouth(2,im)
            if (branch(1,ibr) .eq. mouth(1,im)) then
c
c              Mouth at begin of branch
               igr = branch(3,ibr)
            else
c
c              Mouth at end of branch
               igr = branch(4,ibr)
            endif
c
c           Initial fresh water discharge, flood volume, and maximum
c           flood velocity are determined.
c           [ Doc. S-FO-001.5KV / Eq. 19-'after 10' ]
c
            mouqpu(1,1,im) = moupar(5,im)
            mouqpu(2,1,im) = af(igr) * moupar(3,im) * sngl(tp) / pi
            mouqpu(3,1,im) = moupar(3,im)
   10    continue
c
         do 30 im=1,nmouth
            timout(1,im) = 0.
            timout(2,im) = 0.
            do 20 i = 1,3
               mouqpu(i,2,im) = mouqpu(i,1,im)
               mouqpu(i,0,im) = 0.
   20       continue
   30    continue
c
         do 40 i = 1,3
            thasca(i) = 0.
   40    continue
c
c        Calculate constant part of Thatcher Harleman sum.
c
         confac = le * le * (sngl(tp) * g / rhow ) **.25
         do 60 ibr = 1,nbran
            uastj = 0.
            castj = 0.
            d0j   = 0.
            rhoj  = 0.
            nmj   = bramrl(1,ibr)
            if (nmj .eq. 0) then
c           
c              No mouth defined for this branch
c           
               ker = fatal
               call getbrn (ibr,branam,lbrnam)
               call error (juer,'SAINI1 branch @'//branam(:lbrnam)//
     &                   '@',esabrm,ker)
               goto 1000           
            endif   
            do 50 i = 2,nmj+1
               im    = bramrl(i,ibr)
               uastj = uastj + moupar(3,im)
               castj = castj + moupar(1,im)
               d0j   = d0j   + moupar(2,im)
               rhoj  = rhoj  + moupar(4,im)
   50       continue
            d0j   = d0j  / real(nmj)
            rhoj  = rhoj / real(nmj)
            thcsum(1,ibr) = confac * uastj / castj *
     &                      (d0j * (rhoj - rhow)) ** .25
   60    continue
c
c        Calculate part of Thatcher Harleman sum that is constant in
c        a tidal period.
c
         call satbpa (nmouth ,nbran ,juer ,bramrl ,mouqpu ,thcsum ,ker)
         if (ker .eq. fatal) goto 1000
c
c        Calculate the initial value of <c/c0*dc/dx>
c
         do 90 ibr = 1,nbran
c
c           First c0(j,0) , which is constant in a branch,
c           will be calculated.
c
            nmj  = bramrl(1,ibr)
            c0j  = 0.
            do 70 i = 2,nmj+1
               im = bramrl(i,ibr)
               j  = mouth(2,im)
               if (branch(1,j) .eq. mouth(1,im)) then
c
c                 Mouth at begin of branch
                  igr = branch(3,j)
               else
c
c                 Mouth at end of branch
                  igr = branch(4,j)
               endif
               c0j = c0j + csa2(igr)
   70       continue
            c0j = c0j / real(nmj)
c
c           Check on zero concentrations at mouths
c
            if (c0j .lt. 1.e-10) then
               ker = fatal
               call getbrn (ibr,branam,lbrnam)
               call error (juer,'SAINI1 branch @'//branam(:lbrnam)//
     &                     '@',esacon,ker)
               goto 1000
            endif
c
c           Calculation per grid point.
c
            i1 = branch(3,ibr)
            i2 = branch(4,ibr)
            do 80 igr = i1,i2
c                                               dc/dx
               cdcdx1(igr) = csa2(igr) / c0j * abs(distmp(igr))
   80       continue
c
   90    continue
         do 100 igr = 1,ngrid
            cdcdx2(igr) = cdcdx1(igr)
            cdcdx0(igr) = 0.
  100    continue
      endif
c
c---- Calculate intial C-s ( i.e. A*D*dc/dx) and Rho -----------------
c     [ Doc. S-FO-001.5KV / Eq. 22-2 ]
c
      call sadspc (dsopt  ,nbran  ,ngrid  ,maxtab ,ntabm ,g      ,
     &             time   ,dispf  ,branch ,ntab   ,table ,thcsum ,
     &             grid   ,x      ,wf     ,q2     ,c      ,csa2  ,
     &             cdcdx1 ,cdcdx2 ,csd2   ,disgr  )
c                                 temporary
c
      do 110 igr = 1,ngrid
c                                       dc/dx
         csd2(igr) = af(igr)*disgr(igr)*distmp(igr)
  110 continue
c
      call sadens (nbran ,ngrid ,juer  ,branch ,tw   ,csa2   ,
     &             rho   ,ker   )
c
c---- Initialize array SBDSCR for the boundaries ---------------------
c
      do 130 ib = 1,nboun
         do 120 i = 1,3
            sbdscr(i,ib) = 0.
  120    continue
         if (int(sbdpar(1,ib)).eq.2) then
            sbdscr(1,ib) = sngl(time) - sbdpar(5,ib)
         endif
  130 continue
c
 1000 continue
c
      end
