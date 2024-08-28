      function FLQH04 (teken , width , le    , z     , cullen,
     &                 heidia, applen, ksa   , ks    , rloc  ,
     &                 wloc  , theta , alpha , beta  , hu    ,
     &                 hd    , uu    , qin   , g     , cc    ,
     &                 bot   , strsta, strclo, istru , nstru ,
     &                 rle   )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLow QH relation type 04 (culvert)
c
c Module description: Calculate discharge through structure. This 
c                     routine is taken from WENDY and altered to suit
c                     SOBEK. Functionality is unchanged. WENDY history
c                     is:
c                        Project: Construction-Module
c                        Programmer: G. van Driel / H. van Zanten
c                        Function: Discharge through structure type 4
c                        Updates: None
c                     Note: due to array length limitations, part of
c                     the functionality has been left out. This is to
c                     included later with the introduction of dynamic
c                     array allocation. See documentation for details.
c
c
c an initial value is required for yco0 (this is not in the
c original WENDY code and needs to be checked!!!
c
c-----------------------------------------------------------------------
c Subprogram calls:
c
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flqh04.pf,v $
c Revision 1.2  1998/06/08  12:35:38  kuipe_j
c log added
c
c
c
c***********************************************************************

*  Description array COEF :
*  ------------------------
*     coef (1) : culver width (rectangular case) (W)
*     coef (2) : bottom level at the entrance (Le)
*     coef (3) : difference between entrance and outlet bottom levels(z)
*     coef (4) : length of open culvert (L)
*     coef (5) : height cq. diameter of the culvert (D)
*     coef (6) : length of approach section (Lw) (+ default)
*     coef (7) : roughness of approach section (ksa) (+ default)
*     coef (8) : roughness of flume section (ks) (+ default)
*     coef (9) : entrance rounding (r) (+ default)
*     coef (10): entrance beveling (w) (+ default)
*     coef (11): angle of beveling (theta) (+ default)
*     coef (12): coefficient C4 (+ default)
*     coef (13): coefficient C5 (+ default)
*     coef (14): coefficient C6 (+ default)
*     coef (15): ratio width of approach section vs. W (+ default)
*     coef (16): coefficient C1 (+ default)
*     coef (17): coefficient C2 (+ default)
*     coef (18): coefficient C3 (+ default)
*     coef (19): number of identical culverts (+ default)
*     coef (20): entrance beveling (w) (+ default)      (reverse flow)
*     coef (21): angle of beveling (theta) (+ default)  (reverse flow)
*     coef (22): entrance rounding (r) (+ default)      (reverse flow)
*     coef (23): coefficient C1 (+ default)             (reverse flow)
*     coef (24): coefficient C2 (+ default)             (reverse flow)
*     coef (25): coefficient C3 (+ default)             (reverse flow)
*     coef (26): coefficient C4 (+ default)             (reverse flow)
*     coef (27): coefficient C5 (+ default)             (reverse flow)
*     coef (28): coefficient C6 (+ default)             (reverse flow)
*     coef (29): beta = correction factor for the hydr. radius from the
*                       approach section
*     coef (30): 1 = rectangular cross section of the culvert
*                2 = circular    cross section of the culvert
*     coef (31): 1 = beveled entrance
*                2 = rounded entrance
*     coef (32): 1 = beveled outlet
*                2 = rounded outlet
*
************************************************************************

c
c declare arguments
      integer teken , istru  , nstru
      logical strsta, strclo(nstru)
      real    width , le     , z     , cullen, heidia, applen,
     &        ksa   , ks     , rloc  , wloc  , theta , alpha ,
     &        beta  , hu     , hd    , uu    , qin   , g     ,
     &        bot   , rle
      real    cc(6)

c
c declare variables
      logical lround, lbevup, lbevdn
      integer nculv , i     , iflow , kco1  , kco2  
      real    slope , hnrg  , rlemax, delh1 , delhz , ro    ,
     &        delh2 , term  , dif   , yi0   , x     , ai0   ,
     &        x2    , x3    , yi1   , yi    , ri    , ai    ,
     &        r1i   , a1i   , r1    , r11   , a1    , a11   ,
     &        rmax1 , sk11  , xmaxi , sk1i  , hf1i  , yci   ,
     &        xcrtm , ycrtm , xmaxc , rc    , ac    , yc    ,
     &        ymaxc , hulp  , froot , rnc   , hh1d  , hh2d  ,
     &        h2yci , rnoem , c0    , ckr   , w     , w2    ,
     &        ckw   , y     , y2    , codef , c51   , c52   ,
     &        rco   , rco0  , aco0  , xmaxo , sk1o  , hfio  ,
     &        rco1  , aco1  , ymaxo , arg2  , arg3  , pi    ,
     &        c42   , qstr  , arg6  , hule  , ymaxi , scrit ,
     &        aci   , q1    , yco   , yco0  , aco   , yo
      real    ao    , ro0   , ao0   , r     , a     , rn    ,
     &        arg4  , q4   

c
c declare functions
      real    FLWAPI, FLHRPI, FLSQRT, FLFRIK, FLQH04
      logical  EQUAL
      external EQUAL

c
c set round or rectangular culvert
      lround  = .true.
      if (width .gt. 0.) then
        lround = .false.
      endif

c
c set beveling or rounding at entrance and exit
c currently only the beveling option is available
      lbevup = .true.
      lbevdn = .true.

c
c set number of culverts to one
      nculv   = 1

      slope   = z / cullen
      hnrg = hu + uu * uu / (2. * g)
      rlemax  = MAX(le,(le-z))
c
c check if energy level is lower dan MAX(inlet level,outlet level)
      if (hnrg .le. rlemax) then
        if (strsta) strclo(istru) = .true.
        qstr = 0.
        goto 10
      else
        if (strsta) strclo(istru) = .false.
      endif

      hule   = hu - rle
      delh1  = hnrg - rle
      delhz  = hnrg - rle + z
      delh2  = hd - rle + z

c--De eerste keer per iteratie wordt het stroomtype bepaald (lange lus!)
c this loop is no longer used
c      if (k .eq. 0) then

c
c determine scrit for each culvert type
c round culvert
        if (lround) then

c iteratief yi bepalen uit yi=H1-Le-Q*Q/2g/Ai/Ai (1x per iter.)
c divergentie als :
c    |(deltayi(i+1)/deltayi(i))| > 1.
c             of
c    |(deltayi(i+1)/yi(i+1))| > 1.e-02 na 10 iteratie stappen
c met deltayi(i+1) = yi(i+1) - yi(i)
c in geval van divergentie kies yi = yci (met verwaarlozing van hf1i)

          term = qin * qin / (2. * g)

c
c free or drowned flow upstream?
          if (hule .lt. heidia) then
            yi1  = hule
            if (hule .lt. 0.01) goto 102
            dif = 9999.
            do 101 i = 1,10
              yi0 = yi1
              x   = yi0 / heidia
              ai0 = FLWAPI(x,heidia)
              if (EQUAL(ai0,0.)) goto 1015
              yi1 = delh1 - term / (ai0 * ai0)

c
c check for divergence
              if (ABS((yi1 - yi0) / dif ) .gt. 1.) goto 1015
              dif = yi1 - yi0
              if (ABS((yi1-yi0)/yi1) .lt. 5.e-04) goto 102
  101       continue

 1015       x = delh1  / heidia
            if (x .ge. 2.96) x = 2.96
            x2 = x * x
            x3 = x2 * x
            yi1 = .021308 * x3 * x2 - 0.089157 * x2 * x2 + 0.040374
     &            * x3 - 0.0296 * x2 + 0.744717 * x
            yi1 = yi1 * heidia
        
  102       yi  = yi1
            x   = yi/heidia
            ri  = FLHRPI(x,heidia)
            ai  = FLWAPI(x,heidia)
          else
            yi = heidia
            ri = yi/4.
            ai = 4.*ATAN(1.)*ri*yi
          endif

c
c calculate friction terms
          r1i = ri
          a1i = ai
          r1  = beta * (hu-bot)
          r11 = r1
          a1  = r1 * alpha * heidia
          a11 = a1

          rmax1= MAX(0.1,10.*ksa)
          if (r11 .lt. rmax1) then
            r11 = rmax1
            a11 = (alpha * heidia * heidia * r11) /
     &            (heidia - 2. * r11)
          endif

          sk11 = FLFRIK(r11,ksa,a11)

          xmaxi= MAX(0.025,10.*ks/heidia)
          x    = yi/heidia
          if (x .lt. xmaxi) then
            x   = xmaxi
            r1i = FLHRPI(x,heidia)
            a1i = FLWAPI(x,heidia)
          endif

          sk1i = FLFRIK(r1i,ks,a1i)

          hf1i = applen * qin * qin / (sk11 * sk1i)

c
c determine x and yci
          x = (delh1 - hf1i) / heidia
          if ( x .le. 0.) then
c            if (itel .eq. 1) kflow(id) = 7
            yci   = 1.0e-4
            qstr  = 0.
            goto 10
          else
            if (x .ge. 2.96) x = 2.96
            x2 = x * x
            x3 = x2 * x
            yci = .021308 * x3 * x2 - 0.089157 * x2 * x2 + 0.040374
     &            * x3 - 0.0296 * x2 + 0.744717 * x
            yci = yci * heidia
          endif

c
c define xcrtm and ycrtm to determine criterium
          if (z .lt. 0.) then
            xcrtm = (delh1 - hf1i + z) / heidia
          else
            xcrtm = (delh1 - hf1i) / heidia
          endif

          if (xcrtm .le. 0.) then
            ycrtm   = 1.0e-4
          else
            x = xcrtm
            if (x .ge. 2.96) x = 2.96
            x2 = x * x
            x3 = x2 * x
            ycrtm = .021308 * x3 * x2 - 0.089157 * x2 * x2 + 0.040374
     &            * x3 - 0.0296 * x2 + 0.744717 * x
            ycrtm = ycrtm * heidia
          endif

          yc   = yci
          x    = yc / heidia
          xmaxc= MAX(0.025,10.*ks/heidia)
          if (x .lt. xmaxc) x = xmaxc
          rc   = FLHRPI(x,heidia)
          ac   = FLWAPI(x,heidia)

        else

c
c rectangular culvert

c iteratief yi uit yi=H1-Le-Q*Q/2g/(yi*W)/(yi*W)  (1x per iter.)
c divergentie als :
c    |(deltayi(i+1)/deltayi(i))| > 1.
c             of
c    |(deltayi(i+1)/yi(i+1))| > 1.e-02 na 10 iteratie stappen
c met deltayi(i+1) = yi(i+1) - yi(i)
c in geval van divergentie kies yi = yci (met verwaarlozing van hf1i)

          term = qin * qin / (2. * g * width * width)

c
c free or drowned flow upstream?
          if (hule .lt. heidia) then
            yi1  = hule
            dif = 9999.
            if (hule .lt. 0.01) goto 104
            do 103 i = 1,10
              yi0 = yi1
              if (EQUAL(yi0,0.)) goto 1035
              yi1 = delh1 - term / (yi0 * yi0)

c
c check for divergence
              if (ABS((yi1 - yi0) / dif ) .gt. 1.) goto 1035
              dif = yi1 - yi0
              if (ABS((yi1-yi0)/yi1) .lt. 5.e-04) goto 104
  103       continue

 1035       yi1 = delh1 * 2./3.
  104       yi  = yi1

            ai  = yi * width
            ri  = ai / (2.*yi + width)
          else
            yi = heidia
            ai = yi*width
            ri = ai/(2.*(width+yi))
          endif

c
c calculate the friction terms
          r1i = ri
          a1i = ai
          r1  = beta * (hu-bot)
          r11 = r1
          a1  = r1 * alpha * width
          a11 = a1

          rmax1= MAX(0.1,10.*ksa)
          if (r11 .lt. rmax1) then
            r11 = rmax1
            a11 = (alpha * width * width * r11) /
     &            (width - 2. * r11)
          endif
          sk11 = FLFRIK(r11,ksa,a11)

          ymaxi= MAX(0.1,10.*ks)
          if (yi .lt. ymaxi) then
            yi  = ymaxi
            a1i = width * yi
            r1i = a1i/(width + 2.*yi)
          endif
          sk1i = FLFRIK(r1i,ks,a1i)
          hf1i = applen * qin * qin / (sk11 * sk1i)
          yci  = (delh1 - hf1i) * 2./3.

c
c define ycrtm to determine criterium
          if (z .lt. 0.) then
            ycrtm = yci + 2./3 * z
          else
            ycrtm = yci
          endif

          if (ycrtm .le. 0.) ycrtm = 1.0e-4

          if (yci .le. 0.) then
c            if (itel .eq. 1) kflow(id) = 7
            yci   = 1.0e-4
            qstr  = 0.
            goto 10
          endif

          yc   = yci
          ymaxc= MAX(0.1,10.*ks)
          if (yc .lt. ymaxc) yc = ymaxc
          ac   = width * yc
          rc   = ac / (width + 2.*yc)
        endif

c
c end of square culvert

        hulp  = 12. * rc / ks
        froot = 1. / (2. * LOG10(hulp))
        rnc   = rc**(1./6.) * froot / SQRT(8.*9.81)

        scrit = (rnc*qin / (ac*rc**(2./3.)))**2

c
c determine flowtype
c        if (itel .eq. 1) then

          hh1d  = delh1 / heidia
          hh2d  = delh2 / heidia
       
          if (z .lt. 0.) then
            h2yci = delh2 / ycrtm
          else
            h2yci = delh2 / (ycrtm + z)
          endif

          if (hh2d .gt. 1.) then
            iflow = 4
          else
            if (hh1d .gt. 1.5 ) then
              if (cullen .ge. 5 * heidia) then
                iflow = 6
              else
                iflow = 5
              endif
            else
              if (h2yci .ge. 1.) then
                iflow = 3
              else
                if (slope .gt. scrit) then
                  iflow = 1
                else
                  iflow = 2
                endif
              endif
            endif
          endif

          if (iflow  .eq. 5) then
            if ( z .lt. -0.5 * heidia) iflow = 6
          endif

          if ( ((iflow .eq. 6) .or. (iflow .eq. 5))
     &         .and. (delhz .lt. 1.5 * heidia)) iflow = 2
         
c        endif

c
c calculate values for c1-6
c within 1 iteration step the coefficients are kept constant
c c1,c2,c3
        kco1 = 1
        do 20 i=1,3
          if (EQUAL(cc(i), 0.)) then
            if (kco1 .eq. 0) then
              cc(i) = codef
            else
              if (lround) then
                rnoem = heidia
              else 
                rnoem = width
              endif

              x       = delh1 / rnoem
              x2      = x * x

              if (x .le. 0.4) then
                c0 = 0.93
              else
                if (x .gt. 2.) then
                  c0 = 0.7054
                else
                  c0  = -.0694*x2 + .0265*x + .93
                endif
              endif

              if ((teken .eq.  1 .and. lbevup)  .or.
     &            (teken .eq. -1 .and. lbevdn)) then
                ckr = 1.
                w   = wloc / rnoem
                if (w .gt. 0.1192) w = .1192
                w2  = w * w
                ckw  = -6.25*w2 + 1.49*w
                ckw  = ckw * (theta / 30) ** 1.7 + 1.
              else
                y    = rloc / rnoem
                if (y .gt. 0.1645) y = .1645
                y2   = y * y
                ckr  = -7.813*y2 + 2.57* y + 1.
                ckw  = 1.
              endif

              cc(i) = c0 * ckr * ckw
              codef = cc(i)
              kco1  = 0
            endif
          endif
   20   continue

c
c c4 & c6
        kco2 = 1
        do 30 i=4,6,2
          if (EQUAL(cc(i),0.)) then
            if (kco2 .eq. 0) then
              cc(i) = codef
            else
              if ((teken .eq.  1 .and. lbevup)  .or.
     &            (teken .eq. -1 .and. lbevdn)) then
                if (lround) then
                  x = wloc / heidia
                else
                  x = wloc / width
                endif
              else
                if (lround) then
                  x = rloc / heidia
                else
                  x = rloc / width
                endif
              endif

              if (x .gt. 0.13222) x=0.13222
              cc(i) = -8.036*x*x + 2.125*x + 0.84
              codef = cc(i)
              kco2  = 0
            endif
          endif
   30   continue

c
c c5
        if (EQUAL(cc(5),0.)) then
          x = delh1 / heidia

          if (x .le. 0.) then
            c51 = 0.34
          else
            c51 = 0.11*SQRT(x) + 0.34
          endif

          if ((teken .eq.  1 .and. lbevup)  .or.
     &        (teken .eq. -1 .and. lbevdn)) then
            if (lround) then
              x = wloc / heidia
            else
              x = wloc / width
            endif
          else
            if (lround) then
              x = rloc / heidia
            else
              x = rloc / width
            endif
          endif

          if (x .gt. 0.25) x=0.25

          if (x .le. 0.00) then
            c52 = 1.00
          else
            c52 = 1.00 + 0.88*(SQRT(x) - x)
          endif

          cc(5) = c51 * c52
        endif
c
c end of long (not used) loop
c      endif
c
c flowtype 1
      if (iflow <= 1 .or. iflow > 7) then
         hf1i = applen * qin * qin / (sk11 * sk1i)

         if (lround) then
           x    = yci / heidia
           aci  = FLWAPI(x,heidia)
         else
           aci  = width * yci
         endif

         if ((delh1 - yci - hf1i) .le. 0.) then
           q1 = 0.
         else
           q1 = nculv * cc(1) * aci * SQRT(19.62 * (delh1 - yci - hf1i))
         endif

         qstr = q1
c
c flowtype 2
      else if ( iflow == 2 ) then
         if (lround) then
           hf1i = applen * qin * qin / (sk11 * sk1i)

c-- iteratief bepalen van hfio, start met beginwaarde voor yco, waarbij
c---  hfiostart = l*q*q/ki/ki
c
c determne hfio by iteration
           hfio = cullen * qin * qin / (sk1i*sk1i)
           x    = (delhz - hf1i - hfio) / heidia

           if ( x .le. 0.) then
             yco = 0
           else
             if (x .ge. 2.96) x = 2.96
             x2 = x * x
             x3 = x2 * x
             yco = .021308 * x3 * x2 - 0.089157 * x2 * x2 + 0.040374
     &         * x3 - 0.0296 * x2 + 0.744717 * x
             yco = yco * heidia
           endif

           x    = yco / heidia
           rco  = FLHRPI(x,heidia)
           rco0 = rco
           aco0 = FLWAPI(x,heidia)

c
c determine rco by iteration
           dif = 9999.
           do 111 i = 1,10

c
c correct for friction
             xmaxo= MAX(0.025,10.*ks/heidia)
             x    = yco/heidia

             if (x .lt. xmaxo) then
               x   = xmaxo
               rco0 = FLHRPI(x,heidia)
               aco0 = FLWAPI(x,heidia)
             endif

             sk1o = FLFRIK(rco0,ks,aco0)
             hfio = cullen * qin * qin / (sk1i*sk1o)
             x    = (delhz - hf1i - hfio) / heidia

             if ( x .le. 0.) then
               yco = 0.
             else
               if (x .ge. 2.96) x = 2.96
               x2 = x * x
               x3 = x2 * x
               yco = .021308 * x3 * x2 - 0.089157 * x2 * x2 + 0.040374
     &            * x3 - 0.0296 * x2 + 0.744717 * x
               yco = yco * heidia
             endif

             x    = yco / heidia
             if (x .lt. xmaxo) x = xmaxo
             rco1 = FLHRPI(x,heidia)
             aco1 = FLWAPI(x,heidia)

c
c check for divergence
c yco0 is not yet defined. The assumption is that the 
c following IF statement is not true for the first
c iteration.
             if (ABS((rco1 - rco) / dif ) .gt. 1.) then
               yco = yco0
               rco = rco0
               aco = aco0
               goto 1125
             endif

             dif = rco1 - rco
             if (ABS((rco1-rco)/rco1) .lt. 5.e-04) goto 112
             yco0 = yco
             rco  = rco1
             rco0 = rco1
             aco0 = aco1

  111      continue
  112      continue

           rco = rco1
           aco = aco1
c
c this is an odd statement! Is it correct?
           yco = yco
 1125      continue

         else
           hf1i = applen * qin * qin / (sk11 * sk1i)

           if (ABS(qin) .lt. 1.0e-10) then
             hfio = 0
             yco  = (delhz-hf1i-hfio)*2./3.
             aco  = yco*width
           else

c---         iteratief bepalen van hfio start met beginwaarde voor yco,
c---         waarbij hfiostart = l*q*q/ki/ki
c
c determine hfio by iteration
             hfio = cullen* qin * qin / (sk1i*sk1i)
             yco  = (delhz-hf1i-hfio)*2./3.
             rco  = yco*width/(2.*yco+width)
             rco0 = rco
             aco0 = yco*width

c
c determine rco by iteration
             dif  = 9999.
             do 115 i = 1,10

c
c correct for friction
               ymaxo= MAX(0.1,10.*ks)

               if (yco .lt. ymaxo) then
                 yco  = ymaxo
                 aco0 = width * yco
                 rco0 = aco0/(width + 2.*yco)
               endif

               sk1o = FLFRIK(rco0,ks,aco0)
               hfio = cullen * qin * qin / (sk1i*sk1o)
               yco  = (delhz-hf1i-hfio)*2./3.
               rco1 = yco*width/(2.*yco+width)
               aco1 = yco*width

c
c check for divergence
               if (ABS((rco1 - rco) / dif ) .gt. 1.) then
                 yco = yco0
                 rco = rco0
                 aco = aco0
                 goto 1165
               endif

               dif = rco1 - rco
               if (ABS((rco1-rco)/rco1) .lt. 5.e-04) goto 116
               rco  = rco1
               rco0 = rco1
               aco0 = aco1
  115        continue

  116        continue
             rco = rco1
             aco = aco1
             yco = yco
 1165        continue
           endif
         endif
         arg2 = delhz - yco - hf1i- hfio
         if (arg2 .le. 0.) arg2 = 1.0e-6

         qstr = nculv * cc(2) * aco * SQRT(2 * g * arg2)
c
c flowtype 3
      else if ( iflow == 3 ) then
      if (lround) then
        hf1i = applen * qin * qin / (sk11 * sk1i)
        yo   = delh2
        x    = yo/heidia
        ro   = FLHRPI(x,heidia)
        ao   = FLWAPI(x,heidia)
        ro0  = ro
        ao0  = ao

c
c correct for friction
        xmaxo= MAX(0.025,10.*ks/heidia)

        if (x .lt. xmaxo) then
          x   = xmaxo
          ro0 = FLHRPI(x,heidia)
          ao0 = FLWAPI(x,heidia)
        endif

        sk1o = FLFRIK(ro0,ks,ao0)
        hfio = cullen * qin * qin / (sk1i*sk1o)
      else
        hf1i = applen * qin * qin / (sk11 * sk1i)
        yo   = delh2
        ro   = delh2*width/(2.*delh2+width)
        ao   = delh2*width
        ro0  = ro
        ao0  = ao

c
c correct for friction
        ymaxo= MAX(0.1,10.*ks)

        if (yo .lt. ymaxo) then
          yo  = ymaxo
          ao0 = width * yo
          ro0 = ao0/(width + 2.*yo)
        endif

        sk1o = FLFRIK(ro0,ks,ao0)
        hfio = cullen * qin * qin / (sk1i*sk1o)
      endif

      arg3 = hnrg - hd - hf1i- hfio

      qstr =  nculv * cc(3)*ao*FLSQRT( 2 * g * arg3 )
c
c flowtype 4
      else if ( iflow == 4 ) then
      if (lround) then
        pi = 4.*ATAN(1.)
        r  = heidia / 4.
        a  = r*pi*heidia
      else
        a = width * heidia
        r = a / (2.*(width + heidia))
      endif

      hulp  = 12*r/ks
      froot = 1. / (2 * LOG10(hulp))
      rn    = r**(1./6) * froot / 8.86
      c42   = cc(4) * cc(4)
      rnoem = 1. + 19.62*c42*rn*rn*cullen/r**(4./3.)

      arg4  = (hnrg - hd) / rnoem
      if (arg4 .le. 0.) arg4 = 1.0e-6
      q4    = SQRT(19.62 * arg4)
      qstr  = nculv * q4 * cc(4) * a
c
c flowtype 5
      else if ( iflow == 5 ) then
      if (lround) then
        pi = 4.*ATAN(1.)
        r  = heidia / 4.
        a  = r*pi*heidia
      else
        a = width * heidia
      endif

      qstr = nculv * cc(5) * a * SQRT(2. * g * delh1)
c
c flowtype 6
      else if ( iflow == 6 ) then
      if (lround) then
        pi = 4.*ATAN(1.)
        ri = heidia / 4.
        ai = ri*pi*heidia
        ao = ai
        ro = ri
        a  = ai
      else
        ai = width * heidia
        ao = ai
        ri = ai / (2. * (width + heidia))
        ro = ri
        a  = ai
      endif

      hf1i = applen * qin * qin / (sk11 * sk1i)

      sk1o = FLFRIK(ro,ks,ao)
      hfio = cullen * qin * qin / (sk1i*sk1o)

      arg6 = (delhz-hf1i- hfio - heidia)
      if (arg6 .le. 0.) arg6 = 1.0e-6
      qstr = nculv * cc(6) * a * SQRT(2. * g * arg6)
c
c flowtype 7
      else if ( iflow == 7 ) then
      qstr = 0.
      end if
 10   continue
      FLQH04 = qstr

      return
      end
