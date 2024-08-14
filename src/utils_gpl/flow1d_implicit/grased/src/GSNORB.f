      subroutine gsnorb (nbran  ,ngrid  ,maxlev ,nfrac  ,nlayer ,heiopt,
     &                   lathic ,rouopt ,ibr    ,juer   ,jugralg,fdi   ,
     &                   g      ,pacfac ,relden ,kinvis ,redfac ,nvast ,
     &                   alffl  ,nchfla ,nunlay ,duncof ,pdiacc ,dfrac ,
     &                   branch ,duneh  ,dunel  ,nucoef ,uscoef ,trform,
     &                   prslot ,psltvr ,af     ,afs    ,wf     ,wfs   ,
     &                   nlev   ,hlev   ,wft    ,ws     ,secths ,rs    ,
     &                   cs     ,alfab  ,asubsc ,q2     ,qs     ,h2    ,
     &                   grsize ,dmed0  ,x      ,source ,ptrla2 ,pexla2,
     &                   p0la   ,deff2  ,forcon ,duncon ,sedtra ,dsdu  ,
     &                   pdspdd ,cela   ,celb   ,celc   ,sedtr  ,depos ,
     &                   cela0  ,cela1a ,cela1b ,cela2  ,cela3  ,celert,
     &                   nrdzdl ,sedexp ,zbave  ,zbfl   ,sedtrp ,ker   )
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsnorb.F,v $
c Revision 1.5  1996/06/07  11:56:35  kuipe_j
c multi  +  fixed layer
c
c Revision 1.4  1996/01/08  13:30:02  kuipe_j
c Multi layer option for under layer added
c
c Revision 1.2  1995/09/27  10:12:44  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c Module:      Graded Sediment  transport in NORmal Branch
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    nbran   ,ngrid  ,maxlev ,nfrac ,nlayer ,juer  ,ibr   ,
     &           heiopt  ,lathic ,rouopt ,ker   ,nunlay ,nvast ,nchfla,
     &           jugralg ,nucoef
      integer    branch(4,nbran) ,nlev   (ngrid),nrdzdl (ngrid)
      real       g     ,pacfac   ,relden ,kinvis,fdi    ,redfac,
     &           alffl ,depthl
      real       uscoef(nrcoefs,nucoef)  ,trform(3,nbran)        ,
     &           duncof(2)               ,prslot(3,nbran)        ,
     &           af    (ngrid)           ,afs   (ngrid,2 )       ,
     &           wf    (ngrid)           ,wfs   (ngrid,2)        ,
     &           rs    (ngrid,3)         ,cs    (ngrid,3)        ,
     &           qs    (ngrid,2)         ,alfab (ngrid)          ,
     &           wft   (ngrid,maxlev)    ,
     &           ws    (ngrid)           ,x     (ngrid)          ,
     &           secths(ngrid)           ,dmed0 (ngrid)          ,
     &           duneh (ngrid)           ,dunel (ngrid)          ,
     &           deff2 (ngrid)           ,asubsc(ngrid)          ,
     &           zbave (ngrid)           ,zbfl  (ngrid)          ,
     &           sedexp(ngrid)           ,celert(ngrid) 
      real       grsize(4,ngrid,nlayer+1),forcon(nfrac+1,4,nbran),
     &           source(ngrid,nfrac+2)   ,sedtr (ngrid,nfrac+2)  ,
     &           ptrla2(ngrid,nfrac)     ,pexla2(ngrid,nfrac)    ,
     &           cela0 (ngrid,nfrac)     ,
     &           p0la  (ngrid,nfrac,nunlay)     ,
     &           cela1a(ngrid,nfrac)     ,cela1b(ngrid,nfrac)    ,
     &           cela2 (ngrid,nfrac)     ,cela3 (ngrid,nfrac)    ,
     &           dfrac (nfrac)           ,dsdu  (nfrac)          ,
     &           pdspdd(nfrac)           ,sedtra(nfrac)          ,
     &           cela  (nfrac)           ,celb  (nfrac)          ,
     &           celc  (nfrac)           ,pdiacc(nfrac)          ,
     &           sedtrp(nfrac+2)         ,
     &           duncon(*)               ,
     &           psltvr(7,ngrid)
      double precision hlev  (ngrid,maxlev)
      double precision q2(ngrid), h2(ngrid)
      logical    depos (ngrid) 
c
c     Declaration of local variables
c
      integer    igr   ,ibrl  ,ind    ,nsect  ,jf    ,exl   ,nml   ,
     &           igrl  ,igrr  ,jl     ,jr
      real       depth ,u     ,sum    ,sumsd  ,width ,
     &           velo  ,frou2 ,
     &           sj    ,ddefdh,ddefdd ,ddefdu ,duda  ,dhda,
     &           delta ,deltb ,deltc  ,cy     ,cym   ,pdiadi
      logical    lwet  ,incall, equal
      external   equal
c
c     Constants
c
      integer    d90      ,dmed
      parameter  (d90=3   ,dmed=4)
c
c     Include sobek error code file
c
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
c     trl    = 1  
c JK 24-11-2000 De dmed van de exchange laag was fout.
      exl    = 2
      nml    = nlayer+1
      incall = .false.
      ind    = max(int(trform(2,ibr)),1)
      ibrl   = ibr
      igrl   = branch(3,ibr)
      igrr   = branch(4,ibr)
      pdiadi = 0.
      do 5 jf=1,nfrac
        pdiadi = pdiadi + pdiacc(jf) * dfrac(jf)
  5   continue 

      do 50 igr = igrl,igrr
c
         if (int(prslot(1,ibr)) .eq. cslena) then
            if (h2(igr) .lt. psltvr(5,igr) ) then
c
c              Dry point in case of Preisman slot.
c
               do 10 jf=1,nfrac+2
                  sedtr(igr,jf) = 0.
   10          continue
               do 20 jf=1,nfrac
                  cela0 (igr,jf) = 0.
                  cela1a(igr,jf) = 0.
                  cela1b(igr,jf) = 0.
                  cela2 (igr,jf) = 0.
                  cela3 (igr,jf) = 0.
   20          continue
               lwet         = .false.
            else
               lwet         = .true.
            endif
         else
            lwet = .true.
         endif
c
         if (lwet) then
c
c           Wet point.
c           Calculate velocity and depth.
c
            u     = qs(igr,1) / afs(igr,1)
            depth = afs(igr,1) / wfs(igr,1)
            depthl = h2(igr) - zbave(igr)
            velo  = abs(u)
c
c Statements op 30-11-99 weggecommentatieerd (JK)
c Deze test is tussen okt 95 en febr 96 toegevoegd. In
c okt 96 is behandeling van structures verbeterd (I i.p.v. A
c doorgeven). Daarom nu wellicht(?) niet meer nodig.
c
c begin i.v.m. aanpassing voor structures
c            if (u .lt. 0.) then
c            write(juer,*) ' run afgebroken: u<0 '
c            write(juer,*) ' u>0 moet vanwege structure implementatie '
c            STOP ' u<0, structure implementatie onjuist '
c            endif
c einde i.v.m. aanpassing voor structures
c
            frou2 = velo**2 / (g*depth)
c
c           Calculate roughness
c
            call gsroug (rouopt     ,grsize(d90,igr,1) ,rs(igr,1) ,
     &                   duneh(igr) ,dunel (igr)       ,cs(igr,1) )
c
c           Calculate sediment transport.
c
            call gstrfo
     &          (incall  ,nfrac ,g ,pacfac   ,relden   ,kinvis    ,
     &           grsize(1,igr,1),cs(igr,1)   ,u ,depthl,rs(igr,1) ,
     &           uscoef(1,ind)  ,trform(1,ibr)  ,dfrac ,forcon(1,1,ibr),
     &           sedtra  ,sedexp(igr),nvast  ,alffl    ,zbave(igr),
     &           zbfl(igr) ,nchfla   ,ngrid  ,igr      ,ptrla2    )
c
c           Calculate the sediment transport width
c
            if (wfs(igr,1) .gt. ws(igr)) then
c              if flow widht > sediment transporting width use ws
               width = ws(igr)
            else
               width = wfs(igr,1)
            endif
c
c           Multiply transport/unit-width with probabilty per fraction
c           and width
c
            sum   = 0.
            sumsd = 0.
            do 30 jf=1,nfrac
               cy             = sedtra(jf) * width
               sedtrp(jf)     = cy
               sj             = cy * ptrla2(igr,jf)
               sum            = sum + sj
               sumsd          = sumsd + sj * dfrac(jf)
               sedtra(jf)     = sj
               sedtr (igr,jf) = sj
   30       continue
            sedtr (igr,nfrac+1) = sum
            sedtr (igr,nfrac+2) = sumsd
c 
c           Calculate celerity by numerical differentiation,
c           if transport is not zero.
c
            if (.not.equal(sum,0.)) then
c
c              Derivatives of transport
c
               call gsdsd(nbran  ,ngrid  ,nfrac  ,igr   ,ibr    ,g     ,
     &                    pacfac ,relden ,kinvis ,depth ,width  ,u     ,
     &                    cs(igr,1)      ,nvast  ,alffl ,nchfla ,dfrac ,
     &                    nucoef ,uscoef ,trform ,rs    ,grsize ,forcon,
     &                    ptrla2 ,sedtra ,dsdu   ,pdspdd ,zbave ,zbfl  )
c
c              Derivatives of layer thickness
c
               call gsddef(heiopt ,lathic ,g      ,relden ,kinvis ,
     &                     dmed0  (igr)   ,cs(igr,1)      ,u      , 
     &                     depth  ,frou2  ,duncof ,trform(1,ibr)  , 
     &                     duncon ,sedexp(igr)    ,deff2(igr)     , 
     &                     ddefdh ,ddefdu ,ddefdd ,redfac )
c
c              Derivatives to area
c              The actual number of sections are 
c
               nsect = int(asubsc(igr)) + 1

               call gsduhd(g      ,igr    ,nsect  ,u     ,
     &                     depth  ,ibrl   ,maxlev ,ngrid ,nlev  ,hlev  ,
     &                     wft    ,ws     ,secths ,h2    ,q2    ,qs    ,
     &                     wf     ,wfs    ,af     ,afs   ,rs    ,alfab ,
     &                     juer   ,duda   ,dhda   ,ker   )

               if (ker .ge. warnng) then
                  if (ker .eq. fatal) then
                     goto 100
                  else
                     ibrl = 0
                  endif
               endif
c
c              Calculate if it is sedimentation or erosion.
c              Calculate coefficients DELTA.. in d(deff)/dt
c
               jl = max(igr-1,igrl)
               jr = min(igr+1,igrr)
               call gscond(nlayer ,nfrac   ,ngrid,igr   ,jl    ,jr    ,
     &                     width  ,deff2   (igr) ,dunel (igr)  ,
     &                     grsize (dmed,igr,1)   ,grsize(dmed,igr,exl),
     &                     grsize (dmed,igr,nml) ,dhda  ,duda  ,ddefdh,
     &                     ddefdu ,ddefdd  ,fdi  ,pdiadi,x     ,sedtr ,
     &                     source ,depos   (igr) ,delta ,deltb ,deltc ,
     &                     jugralg)
c
c              Calculate coefficients for celerity.
c
               call gsacel(nlayer ,nfrac  ,ngrid ,igr   ,width  ,duda  ,
     &                     delta  ,deltb  ,deltc ,fdi   ,depos  ,ptrla2,
     &                     pdiacc ,pexla2 ,p0la  ,sedtr ,dunel  ,deff2 ,
     &                     dfrac  ,pdspdd ,dsdu  ,cela0 ,cela1a ,cela1b,
     &                     cela2  ,cela3  ,cela  ,celb  ,celc   ,
     &                     nunlay ,nrdzdl ,sedtrp)
c
c              Determine maximum dS/du(j) for celerity calculation
c
               cym   = 0.
               do 40 jf=1,nfrac
                 cym = max (cym,abs(dsdu(jf)))
   40          continue
c
c              Calculate total estimated celerity
        
               celert (igr) = max (cym * duda, abs(sedtr(igr,nfrac+1)) /
     &                                            (deff2(igr)*width))
            else
c
c              No sediment transport occured.
c
               lwet = .false.
            endif   
         endif
c
c        Set celerities to zero if transport is zero or if gridpoint
c        is dry
c
         if (.not. lwet) then
            do jf=1,nfrac
                cela0 (igr,jf) = 0.
                cela1a(igr,jf) = 0.
                cela1b(igr,jf) = 0.
                cela2 (igr,jf) = 0.
                cela3 (igr,jf) = 0.
            enddo            
            celert (igr) = 0.
         endif   
c
  50  continue
      return
c
 100  continue
c
      end
