      subroutine FLSW (g      ,il     ,ir     ,ngrid  ,istru  ,strclo ,
     +                 strpar ,h      ,h1     ,q      ,q2     ,af     ,
     +                 maxtab ,ntabm  ,ntab   ,table  ,rho    ,strhis ,
     +                 iter   ,relstr ,asde   ,csde   ,esde   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer / J.Kuipers
c
c Module:             FLSW (FLow structure Simple Weir)
c
c Module description: In subroutine FLSW the ABCDE coefficients for the
c                     stage-discharge equation are computed for a simple
c                     weir.
c
c                     In subroutine FLQHSW the Q-H relation for the
c                     simple weir is defined.
c
c                     The following items can be controlled:
c
c                     Wstr = Width across flow section
c                     Zs   = Crest level of weir
c
c                     For the calculation of the velocity the flow from
c                     the previous iteration level and the flow area on
c                     level n+1/2 are used. This because differences
c                     will be small.
c
c                     The input parameters for the simple weir are the
c                     following:
c
c                     -   Level of crest Zs
c                     -   Crest width Wstr
c                     -   Correction coefficient cw
c                     -   Submergence limit sl
c                     -   Drowned flow reduction curve (stored in tabu-
c                         lar form)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 11 af                P  -
c 20 asde              IO a-coefficient in stage-discharge equation for
c                         this specific structure.
c 21 csde              IO c-coefficient in stage-discharge equation for
c                         this specific structure.
c 22 esde              O  e-coefficient in stage-discharge equation for
c                         this specific structure.
c  1 g                 P  -
c  9 h1                P  -
c  8 h                 P  -
c  2 il                P  -
c  3 ir                P  -
c  5 istru             I  Number of structure.
c 18 iter              I  Iteration step.
c 12 maxtab            I  Maximum number of defined tables.
c  4 ngrid             I  Number of grid points in network.
c 14 ntab              P  -
c 13 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 10 q                 P  -
c 19 relstr            I  Under relaxation factor for structures.
c 16 rho               P  -
c  6 strclo            P  -
c 17 strhis(10,nstru)  IO For each structure the discharge and the
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
c  7 strpar(21,nstru)  I  Each structure is characterized by a number of
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
c 15 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flswar  FLow get Simple Weir ARguments
c flupdo  FLow UP- and DOwnstream near structure
c=======================================================================
c
c     Declaration of function:
c
      real    FLQHSW
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      integer il, ir, istru, ngrid, maxtab, ntabm ,iter
      integer ntab(4,maxtab)
      logical strclo(*)
      real    strpar(dmstrpar,*), strhis(dmstrh,*)
      real    af(ngrid)
      real    rho(ngrid)
      real    table(ntabm)
      real    asde, csde, esde, g ,relstr
      real    wideff, hcrfre

      double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
      integer     qol
      parameter  (qol=4)
c
c     Declaration of local variables:
c
      integer iup, idown, itab
      logical strsta, wet, red, reda, redb
      real    hunp1, hdnp1, hun, hdn, uu, ud, dh, dq, qa,  teken
      real    zs, wstr, cw, slim ,relst1, dum1 ,qdh1, qdh2, qdhu, qdhd 
      real    crest, fred,fred1, fred2, hda, hdb, qdh1q, qdh2q 
      real    qq, dh2, theta, hua, hub, dh1, afactor, qtot
c
      crest = strpar(1 ,istru)
c
      call FLUPDO(g      ,il     ,ir     ,ngrid  ,h      ,h1     ,
     +            q      ,q2     ,af     ,rho    ,crest  ,hunp1  ,
     +            hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,
     +            idown  ,teken  )
c
      call FLSWAR(istru  ,strpar ,teken  ,zs     ,wstr   ,cw     ,
     +            slim   ,itab   )
c
      dh = 0.001
c
      strsta = .true.
      qa = teken * FLQHSW (g      ,istru  ,strsta ,strclo ,hunp1  ,
     +                     hdnp1  ,uu     ,ud     ,zs     ,wstr   ,
     +                     cw     ,slim   ,itab   ,maxtab ,ntabm  ,
     +                     ntab   ,table  ,fred   )
c
      strsta = .false.
c
c     Numerical differentiation by incrementing in waterlevels.
c
      qdhu = FLQHSW (g       ,istru  ,strsta ,strclo ,hunp1+dh,
     +               hdnp1   ,uu     ,ud     ,zs     ,wstr    ,cw    ,
     +               slim    ,itab   ,maxtab ,ntabm  ,ntab    ,table ,
     +               dum1    )
c
      qdhd = FLQHSW (g       ,istru  ,strsta ,strclo ,hunp1   ,
     +               hdnp1-dh,uu     ,ud     ,zs     ,wstr    ,cw    ,
     +               slim    ,itab   ,maxtab ,ntabm  ,ntab    ,table ,
     +               dum1    )
c
      if (teken .gt. 0) then
         qdh1 = ( qdhu - qa ) / dh
         qdh2 = ( qa - qdhd ) / dh
      else
         qdh1 = (  qdhd + qa ) / dh
         qdh2 = ( -qdhu - qa ) / dh 
      endif
c
      fred1 = .10
      fred2 = .05
      if (fred.lt.fred1) then 
c
c        Degeneration ahead (reduction factor --> 0), so       
c        numerical differentiation will be done by incrementing in 
c        discharge.
c
         dq = .1
c        ARS 7170 / Use discharge of this structure only.
c                   Change is made for compound structures.
         qtot = abs((q(il)+q(ir))) *.5 
c
c        Discharge thru all simple weirs
         qq = max(qtot - abs(strhis(10,istru)),0.)
c
c        Ratio between wet area of this structure to wet area of
c        all simple weirs
         if (strhis(11,istru).gt.1.e-6) then 
            afactor = strpar(10,istru)/strhis(11,istru)
         else
            afactor = 1.
         endif   
c
c        Discharge thru this weir 
         qq = qq * afactor
c
         dq = max(1.e-4*qq,0.1) 
         hua = hunp1
         hub = hunp1
c         
c        Numerical differentiation to downwards waterlevel
c
         call FLQHSWQ(g      ,hua    ,hda    ,qq     ,uu     , 
     +                ud     ,zs     ,wstr   ,cw     ,wet    ,
     +                itab   ,maxtab ,ntabm  ,ntab   ,table  ,
     +                reda   )
c
         if (wet) then
  10        continue 

               call FLQHSWQ(g      ,hub    ,hdb    ,qq+dq  ,uu     , 
     +                      ud     ,zs     ,wstr   ,cw     ,wet    ,
     +                      itab   ,maxtab ,ntabm  ,ntab   ,table  ,
     +                      redb   )
               dh2 = hda - hdb
               if (redb) then 
                  if (dh2 .lt. 1.e-4) then
                     dq = dq * 10.
            goto 10
                  endif   
               endif
         endif
c 
         if (wet) then  
            red = redb
            if (.not.redb.and.reda) then
c
               hua = hub           
               hda = hdb
c
c              Reduction factor in one point < 1, so force in both
c              points the reduction factor = 1 by increasing the
c              discharge.
c
               call FLQHSWQ(g      ,hub    ,hdb    ,qq+dq*2,uu     , 
     +                      ud     ,zs     ,wstr   ,cw     ,wet    ,
     +                      itab   ,maxtab ,ntabm  ,ntab   ,table  ,
     +                      red    )        
            endif
            
            if (red) then
c            
c               Reduction factor < 1, so numerical differentiation
c               to upwards waterlevel            
c
                qdhu = FLQHSW (g     ,istru ,strsta ,strclo ,hua+dh2 ,
     +                        hda   ,uu    ,ud     ,zs     ,wstr    ,
     +                        cw    ,slim  ,itab   ,maxtab ,ntabm   ,
     +                        ntab  ,table , dum1  )
               if (teken.gt.0.) then
                  qdh1q = ( qdhu - qq ) / dh2
                  qdh2q = -dq / dh2
               else
                  qdh1q = dq / dh2
                  qdh2q = ( qq - qdhu ) / dh2
               endif
            else
c
c              Reduction factor = 1, So the downwards differentiation
c              coefficients are zero. 
c              This will very seldom occur.
c
               dh1 = hub - hua         
               if (teken.gt.0.) then
                  qdh1q = dq / dh1
                  qdh2q = 0.
               else
                  qdh1q = 0.
                  qdh2q = -dq / dh1
               endif
            endif   
            if (fred.lt.fred2) then 
c
c              Only differentiation by incrementing in discharge.
c
               qdh1  = qdh1q
               qdh2  = qdh2q
               qa    = qq * teken
               hunp1 = hua
               hdnp1 = hda
            else
c
c              Transition zone 
c
               theta = (fred -fred1) / (fred2 - fred1)
               qdh1  = (1.-theta) * qdh1  + theta * qdh1q
               qdh2  = (1.-theta) * qdh2  + theta * qdh2q
               qa    = (1.-theta) * qa    + theta * qq * teken
               hunp1 = (1.-theta) * hunp1 + theta * hua
               hdnp1 = (1.-theta) * hdnp1 + theta * hda
            endif  
         endif             
      endif 
c 
c     Calculate if structure is drowned or free
c
      wideff = wstr * cw
      if (wideff .le. 1.e-6) then
         strclo(istru) = .true.
      endif
      if (strclo(istru)) then
         strhis(8,istru) = 0.
      else
         hcrfre = (hunp1 +uu**2/2./g - zs) * .6666666
         if (hcrfre .lt. (hdnp1-zs)) then
c            drowned weir flow
             strhis(8,istru) = 2.
         else 
c            free weir flow
             strhis(8,istru) = 1.
         endif
      endif
c
      asde = qdh1
      csde = qdh2
c
c     Underrelaxation of the A, C and E-coefficient
c
      if (iter.ne.1) then
         relst1 = 1.0 - relstr
         qa   = relstr*qa  +relst1*strhis(qol,istru)
      endif
      strhis(qol,istru) = qa
c
      if (teken .gt. 0) then
         esde = -qa + (hunp1-hun) * asde + (hdnp1-hdn) * csde
      else
         esde = -qa + (hdnp1-hdn) * asde + (hunp1-hun) * csde
      endif
c
      end
      
      subroutine flswarea (g      ,il     ,ir     ,ngrid  ,istru  ,
     +                     strpar ,h      ,h1     ,q      ,q2     ,
     +                     af     ,rho    )
c
c     Calculate wetted area in simple weir above crest
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      integer il, ir, istru, ngrid
      real    strpar(dmstrpar,*)
      double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
      real    af(ngrid)
      real    rho(ngrid)
      real    g 
c
c     Declaration of local variables:
c
      integer iup,   idown, itab
      real    hunp1, hdnp1, hun, hdn,  uu, ud, teken
      real    crest, wstr,  cw,  slim, a
c
      crest = strpar(1 ,istru)
c
      call FLUPDO(g      ,il     ,ir     ,ngrid  ,h      ,h1     ,
     +            q      ,q2     ,af     ,rho    ,crest  ,hunp1  ,
     +            hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,
     +            idown  ,teken  )
c
      call FLSWAR(istru  ,strpar ,teken  ,crest  ,wstr   ,cw     ,
     +            slim   ,itab   )    
     
      a                = (max(hunp1,hdnp1)-crest)*wstr*cw
      
      strpar(10,istru) = max(a,0.)
      
      end
