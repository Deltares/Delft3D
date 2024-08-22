      subroutine FLGS (g      ,il     ,ir     ,iter   ,ngrid  ,istru  ,
     +                 lambda ,relstr ,dhstru ,strclo ,strpar ,h      ,
     +                 h1     ,q      ,q1     ,q2     ,af     ,wf     ,
     +                 lsalt  ,rho    ,strhis ,asde   ,bsde   ,csde   ,
     +                 dsde   ,esde   ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLGS (FLow General Structure)
c
c Module description: In subroutine FLGS the ABCDE coefficients are
c                     computed for a General Structure.
c
c                     In this subroutine the coefficients A2-E2 will be
c                     computed for the stage-discharge equation for this
c                     specific structure. In subroutine FLQHGS the Q-H
c                     relation for the general structure is defined.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 16 af                P  -
c 21 asde              IO a-coefficient in stage-discharge equation for
c                         this specific structure.
c 22 bsde              IO b-coefficient in stage-discharge equation for
c                         this specific structure.
c 23 csde              IO c-coefficient in stage-discharge equation for
c                         this specific structure.
c  9 dhstru            I  Delta h used for numerical differentation.
c 24 dsde              IO -
c 25 esde              O  e-coefficient in stage-discharge equation for
c                         this specific structure.
c  1 g                 I  Acceleration of gravity.
c 13 h1                P  -
c 12 h                 P  -
c  2 il                I  Grid point on left side of structure (lower
c                         index).
c  3 ir                P  -
c  6 istru             I  Number of structure.
c  4 iter              I  Iteration step.
c  7 lambda            I  Extra resistance in general structure.
c 18 lsalt             I  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c  5 ngrid             I  Number of grid points in network.
c 15 q1                P  -
c 14 q                 P  -
c  8 relstr            I  Under relaxation factor for structures.
c 19 rho(ngrid)        I  Density of diluted water per grid point.
c 10 strclo            P  -
c 20 strhis(10,nstru)  IO For each structure the discharge and the
c                         parameters to be controlled must be saved to
c                         be able to write to the output file. This will
c                         be done in array strhis(8,nstru). This array
c                         will also be used to check the values of the
c                         controlled parameters or to determine if
c                         increase(open) or decrease(close) of these
c                         parameters occurs. This array will also be
c                         part of the restart file.
c                         (1,i) = Gate height
c                         (2,i) = Crest height
c                         (3,i) = Crest width
c                         (4,i) = Discharge through structure
c                         (5,i) = Gate height at previous time step
c                         (6,i) = Crest height at previous time step
c                         (7,i) = Crest width at previous time step
c                         (8,i) = Flow condition of general structure:
c                                 formno = 0, closed or other structure
c                                 formno = 1, free weir
c                                 formno = 2, drowned weir
c                                 formno = 3, free gate
c                                 formno = 4, drowned gate
c                         (9,i) = coefficient Q-H-realtion asde
c                         (10,i)= coefficient Q-H-realtion bsde
c                         (11,i)= coefficient Q-H-realtion csde
c                         (12,i)= coefficient Q-H-realtion dsde
c 11 strpar(21,nstru)  I  Each structure is characterized by a number of
c                         specific parameters. strpar (i,j) = parameter
c                         i of structure j:
c                         - Simple weir:
c                         (1,j) = Crest height Zs.
c                         (2,j) = Crest width Ws.
c                              Positive flow:
c                         (3,j) = Correction coefficient cw.
c                         (4,j) = Submergence limit Slim.
c                         (5,j) = Table pointer for drowned reduction
c                                 curve f(h2/h1).
c                              Negative flow:
c                         (6,j) = Correction coefficient cw.
c                         (7,j) = Submergence limit Slim.
c                         (8,j) = Table pointer for drowned reduction
c                                 curve f(h2/h1).
c                         - Advanced weir:
c                         (1,j) = Crest height Zs.
c                         (2,j) = Total net width Wn.
c                         (3,j) = Number of piers N.
c                              Positive flow:
c                         (4,j) = Heigth of upstream face P.
c                         (5,j) = Design head H0 of the weir.
c                         (6,j) = Pier contraction coefficient Kp.
c                         (7,j) = Abutment contraction coefficient Ka.
c                              Negative flow:
c                         (8,j) = Heigth of upstream face P.
c                         (9,j) = Design head H0 of the weir.
c                         (10,j)= Pier contraction coefficient Kp.
c                         (11,j)= Abutment contraction coefficient Ka.
c                         - Pump:
c                         (1,j) = Control direction:
c                                 cpmpup (-1) : upward control
c                                 cpmpdw (+1) : downward control
c                         (2,j) = Table pointer for pump capacitity re-
c                                 duction factor.
c                         (3,j) = Capacity.
c                         (4,j) = Water level which starts pump.
c                         (5,j) = Water level which stops pump.
c                         - General structure:
c                         (1,j) = Width left side of structure W1.
c                         (2,j) = Bed level left side of structure Zb1.
c                         (3,j) = Width structure left side Wsdl.
c                         (4,j) = Bed left side of structure Zbsl.
c                         (5,j) = Width structure centre Ws.
c                         (6,j) = Bed level centre Zs.
c                         (7,j) = Width structure right side Wsdr.
c                         (8,j) = Bed right side of structure Zbsr.
c                         (9,j) = Width right side of structure W2.
c                         (10,j)= Bed level right side of structure Zb2.
c                         (11,j)= Gate opening heigth dg.
c                              Positive flow:
c                         (12,j)= Correction coefficient for free gate
c                                 flow cgf.
c                         (13,j)= Correction coefficient for drowned
c                                 gate flow cgd.
c                         (14,j)= Correction coefficient for free weir
c                                 flow cwf.
c                         (15,j)= Correction coefficient for drowned
c                                 weir flow cwd.
c                         (16,j)= Contraction coefficient for free gate
c                                 flow MU-gf.
c                              Negative flow:
c                         (17,j)= Correction coefficient for free gate
c                                 flow cgf.
c                         (18,j)= Correction coefficient for drowned
c                                 gate flow cgd.
c                         (19,j)= Correction coefficient for free weir
c                                 flow cwf.
c                         (20,j)= Correction coefficient for drowned
c                                 weir flow cwd.
c                         (21,j)= Contraction coefficient for free gate
c                                 flow MU-gf.
c 17 wf                P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flgtar  FLow get General sTructure ARguments
c flupdg  FLow UP/Downstream near General structure
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flgs.pf,v $
c Revision 1.14  1999/03/15  15:49:51  kuipe_j
c tabs removed
c
c Revision 1.13  1997/11/06  09:16:40  kuipe_j
c Linearization original again
c
c Revision 1.12  1997/10/03  06:38:31  kuipe_j
c linearization compounds improved, changed flow dir. crit
c
c Revision 1.11  1996/05/30  09:56:37  kuipe_j
c general structure dlim, controllers
c
c Revision 1.10  1996/04/11  08:23:18  kuipe_j
c Kalman module added
c
c Revision 1.9  1996/01/17  14:38:21  kuipe_j
c header update
c
c Revision 1.8  1995/11/21  11:07:49  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.7  1995/09/22  10:01:17  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:10:53  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:36:33  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:17  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:54:58  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:56  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:41  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:30:50  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of function:
c
      double precision FLQHGS
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
      include '../include/erradm.i'
c
c     Declaration of parameters:
c
      integer il, ir, istru, ngrid, iter, juer, ker
      logical lsalt, strclo(*)
      real    strpar(dmstrpar,*), strhis(dmstrh,*)
      double precision h(ngrid),h1(ngrid),q(ngrid),q1(ngrid),q2(ngrid)
      real    af(ngrid),wf(ngrid),  rho(ngrid)
      real    asde, bsde, csde, dsde, esde, g
      real    lambda ,relstr ,dhstru
c
c     Declaration of local variables:
c
      integer           iup, idown, formno, lstnam
      logical           strsta
      double precision  hunp1 ,hdnp1 ,hun   ,hdn   ,uu  ,ud ,qa ,qdhu ,
     +                  qdhd  ,teken ,wstr  ,zs    ,w2  ,zb2,dg ,
     +                  cgf   ,cgd   ,cwf   ,cwd   ,mugf,wsd,ds1,ds2  ,
     +                  rhoast,crest ,as    ,bs  ,cs  ,ds,
     +                  qun   ,qunp1 ,wu    ,au    ,qdqu,
     +                  gdub  ,lamdub,reldub,dhsdub,dum1
      real              astr, abran, hcrit, hstr
      character(len=8)  abrtxt, astrtxt
      character(len=40) strnam
c
      integer     qol
      parameter  (qol=4)

      LOGICAL uitput

      COMMON  /UITPUT/uitput

      include '../include/errcod.i'

      gdub   = g
      lamdub = lambda
      reldub = relstr
      dhsdub = dhstru
c
      crest = strpar(6 ,istru)
c
      call FLUPDG (gdub   ,il     ,ir     ,ngrid  ,h      ,h1     ,
     +             q      ,q1     ,q2     ,af     ,wf     ,rho    ,
     +             crest  ,hunp1  ,hdnp1  ,hun    ,hdn    ,uu     ,
     +             ud     ,qun    ,qunp1  ,au     ,wu     ,iup    ,
     +             idown  ,teken  )
c
      call FLGTAR (istru  ,strpar ,teken  ,zs     ,wstr   ,
     +             w2     ,wsd    ,zb2    ,dg     ,ds1    ,
     +             ds2    ,cgf    ,cgd    ,cwf    ,cwd    ,
     +             mugf   )
c
      if (iter.eq.1) then
         if (nrerperr(neragenstr).lt.20) then
            abran = min(af(il),af(ir))
            hcrit = 0.6666667*(hunp1 + uu*uu/g*0.5 - zs)
            hstr  = min(h1(il),h1(ir))-zs
            hstr  = max(hstr,hcrit)
            astr  = min(hstr,real(dg, kind=kind(hstr)))*wstr
            if (astr.gt.abran*1.01) then
c               nrerperr(neragenstr) = nrerperr(neragenstr) + 1 
c               call getstr(istru,strnam,lstnam)
c               ker = warnng
c               write (abrtxt,'(f8.0)') abran
c               write (astrtxt,'(f8.0)') astr
c               call sre_error (juer,'FLGS Area in gen structure @'//
c     &            strnam(:lstnam)//'@(@'//astrtxt//'@ > area branch (@'
c     &            //abrtxt//'@)',eflgsa, ker)
            endif
         endif   
      endif
      if (istru.eq.-10) then
CJK   if ((istru.eq.3 .or. istru.eq.6).and.iter.gt.90) then
CJK   if (istru.gt.0) then
      WRITE (99,*) '==== ITER',iter
      WRITE (99,*) 'STRUC=',istru,'B=',wstr,'H=',zs,'GP=',il,
     +'S=',teken
      WRITE (99,*) 'GATE =',dg
      uitput=.true.
      else
      uitput=.false.
      endif
c
      if ( lsalt ) then
         rhoast = rho(idown) / rho(iup)
      else
         rhoast = 1.0D0
      endif
c
      strsta = .true.
      qa = teken * FLQHGS (gdub   ,istru  ,strsta ,strclo ,hunp1  ,
     +                     hdnp1  ,uu     ,zs     ,wstr   ,w2     ,
     +                     wsd    ,zb2    ,dg     ,ds1    ,ds2    ,
     +                     rhoast ,cgf    ,cgd    ,cwf    ,cwd    ,
     +                     mugf   ,dum1   ,formno ,lamdub ,dhsdub )
      strsta = .false.
c
      qdhu = teken * FLQHGS(gdub  ,istru  ,strsta,strclo,hunp1+dhsdub ,
     +                      hdnp1 ,uu     ,zs    ,wstr  ,w2   ,
     +                      wsd   ,zb2    ,dg    ,ds1   ,ds2  ,rhoast ,
     +                      cgf   ,cgd    ,cwf   ,cwd   ,mugf ,dum1   ,
     +                      formno,lamdub,dhsdub )
      qdhu = ( qdhu - qa ) / dhsdub

      qdhd = teken * FLQHGS(gdub  ,istru  ,strsta,strclo,hunp1        ,
     +                      hdnp1-dhsdub  ,uu    ,zs    ,wstr ,w2     ,
     +                      wsd   ,zb2    ,dg    ,ds1   ,ds2  ,rhoast ,
     +                      cgf   ,cgd    ,cwf   ,cwd   ,mugf ,dum1   ,
     +                      formno,lamdub,dhsdub )
      qdhd = ( qa - qdhd ) / dhsdub

      qdqu = qdhu * uu/(au*gdub)
      qdhu = qdhu * (1.D0-uu**2*wu/(au*gdub))
c
      if (uitput) then
      WRITE (99  ,*) 'RHO-STER',istru,rhoast
      WRITE (99  ,*) 'Hu,Hd,U',hunp1,hdnp1,uu  
      WRITE (99  ,*) 'IN STRUC qa,afg-qdhu,qdhd,  ',istru,qa,qdhu,qdhd
      WRITE (99  ,*) 'FORMNO= ',formno
      endif
         
      if (teken .gt. 0) then
         as    = qdhu
         bs    = qdqu
         cs    = qdhd
         ds    = 0.
      else
         as    = qdhd
         bs    = 0.
         cs    = qdhu
         ds    = qdqu
      endif
c
c     Underrelaxation of Q                               
c
      if (iter.gt.1) then
         qa = reldub*qa+(1.0D0 - reldub)*dble(strhis(qol,istru))
      endif

      asde = real(as, kind=kind(asde))
      bsde = real(bs, kind=kind(bsde))
      csde = real(cs, kind=kind(ssde))
      dsde = real(ds, kind=kind(dsde))
      strhis(qol,istru) = real(qa, kind=kind(strhis))
c
      if (teken .gt. 0) then
         esde = real(-qa + (hunp1-hun) * as + (hdnp1-hdn) * cs
     +                   + (qunp1-qun) * (bs + ds), kind=kind(esde))

      else
         esde = real(-qa + (hdnp1-hdn) * as + (hunp1-hun) * cs
     +                   + (qunp1-qun) * (bs + ds), kind=kind(esde))
      endif
c
      strhis(8,istru)=formno
c
      end
