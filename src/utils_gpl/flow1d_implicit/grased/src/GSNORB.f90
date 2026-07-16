subroutine gsnorb (nbran  ,ngrid  ,maxlev ,nfrac  ,nlayer ,heiopt,&
&lathic ,rouopt ,ibr    ,juer   ,jugralg,fdi   ,&
&g      ,pacfac ,relden ,kinvis ,redfac ,nvast ,&
&alffl  ,nchfla ,nunlay ,duncof ,pdiacc ,dfrac ,&
&branch ,duneh  ,dunel  ,nucoef ,uscoef ,trform,&
&prslot ,psltvr ,af     ,afs    ,wf     ,wfs   ,&
&nlev   ,hlev   ,wft    ,ws     ,secths ,rs    ,&
&cs     ,alfab  ,asubsc ,q2     ,qs     ,h2    ,&
&grsize ,dmed0  ,x      ,source ,ptrla2 ,pexla2,&
&p0la   ,deff2  ,forcon ,duncon ,sedtra ,dsdu  ,&
&pdspdd ,cela   ,celb   ,celc   ,sedtr  ,depos ,&
&cela0  ,cela1a ,cela1b ,cela2  ,cela3  ,celert,&
&nrdzdl ,sedexp ,zbave  ,zbfl   ,sedtrp ,ker   )
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsnorb.F,v $
! Revision 1.5  1996/06/07  11:56:35  kuipe_j
! multi  +  fixed layer
!
! Revision 1.4  1996/01/08  13:30:02  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.2  1995/09/27  10:12:44  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
! Module:      Graded Sediment  transport in NORmal Branch
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    nbran   ,ngrid  ,maxlev ,nfrac ,nlayer ,juer  ,ibr   ,&
   &heiopt  ,lathic ,rouopt ,ker   ,nunlay ,nvast ,nchfla,&
   &jugralg ,nucoef
   integer    branch(4,nbran) ,nlev   (ngrid),nrdzdl (ngrid)
   real       g     ,pacfac   ,relden ,kinvis,fdi    ,redfac,&
   &alffl ,depthl
   real       uscoef(nrcoefs,nucoef)  ,trform(3,nbran)        ,&
   &duncof(2)               ,prslot(3,nbran)        ,&
   &af    (ngrid)           ,afs   (ngrid,2 )       ,&
   &wf    (ngrid)           ,wfs   (ngrid,2)        ,&
   &rs    (ngrid,3)         ,cs    (ngrid,3)        ,&
   &qs    (ngrid,2)         ,alfab (ngrid)          ,&
   &wft   (ngrid,maxlev)    ,&
   &ws    (ngrid)           ,x     (ngrid)          ,&
   &secths(ngrid)           ,dmed0 (ngrid)          ,&
   &duneh (ngrid)           ,dunel (ngrid)          ,&
   &deff2 (ngrid)           ,asubsc(ngrid)          ,&
   &zbave (ngrid)           ,zbfl  (ngrid)          ,&
   &sedexp(ngrid)           ,celert(ngrid)
   real       grsize(4,ngrid,nlayer+1),forcon(nfrac+1,4,nbran),&
   &source(ngrid,nfrac+2)   ,sedtr (ngrid,nfrac+2)  ,&
   &ptrla2(ngrid,nfrac)     ,pexla2(ngrid,nfrac)    ,&
   &cela0 (ngrid,nfrac)     ,&
   &p0la  (ngrid,nfrac,nunlay)     ,&
   &cela1a(ngrid,nfrac)     ,cela1b(ngrid,nfrac)    ,&
   &cela2 (ngrid,nfrac)     ,cela3 (ngrid,nfrac)    ,&
   &dfrac (nfrac)           ,dsdu  (nfrac)          ,&
   &pdspdd(nfrac)           ,sedtra(nfrac)          ,&
   &cela  (nfrac)           ,celb  (nfrac)          ,&
   &celc  (nfrac)           ,pdiacc(nfrac)          ,&
   &sedtrp(nfrac+2)         ,&
   &duncon(*)               ,&
   &psltvr(7,ngrid)
   double precision hlev  (ngrid,maxlev)
   double precision q2(ngrid), h2(ngrid)
   logical    depos (ngrid)
!
!     Declaration of local variables
!
   integer    igr   ,ibrl  ,ind    ,nsect  ,jf    ,exl   ,nml   ,&
   &igrl  ,igrr  ,jl     ,jr
   real       depth ,u     ,sum    ,sumsd  ,width ,&
   &velo  ,frou2 ,&
   &sj    ,ddefdh,ddefdd ,ddefdu ,duda  ,dhda,&
   &delta ,deltb ,deltc  ,cy     ,cym   ,pdiadi
   logical    lwet  ,incall, equal
   external   equal
!
!     Constants
!
   integer    d90      ,dmed
   parameter  (d90=3   ,dmed=4)
!
!     Include sobek error code file
!
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
!     trl    = 1
! JK 24-11-2000 De dmed van de exchange laag was fout.
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
5  continue

   do 50 igr = igrl,igrr
!
      if (int(prslot(1,ibr)) .eq. cslena) then
         if (h2(igr) .lt. psltvr(5,igr) ) then
!
!              Dry point in case of Preisman slot.
!
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
!
      if (lwet) then
!
!           Wet point.
!           Calculate velocity and depth.
!
         u     = qs(igr,1) / afs(igr,1)
         depth = afs(igr,1) / wfs(igr,1)
         depthl = h2(igr) - zbave(igr)
         velo  = abs(u)
!
! Statements op 30-11-99 weggecommentatieerd (JK)
! Deze test is tussen okt 95 en febr 96 toegevoegd. In
! okt 96 is behandeling van structures verbeterd (I i.p.v. A
! doorgeven). Daarom nu wellicht(?) niet meer nodig.
!
! begin i.v.m. aanpassing voor structures
!            if (u .lt. 0.) then
!            write(juer,*) ' run afgebroken: u<0 '
!            write(juer,*) ' u>0 moet vanwege structure implementatie '
!            STOP ' u<0, structure implementatie onjuist '
!            endif
! einde i.v.m. aanpassing voor structures
!
         frou2 = velo**2 / (g*depth)
!
!           Calculate roughness
!
         call gsroug (rouopt     ,grsize(d90,igr,1) ,rs(igr,1) ,&
         &duneh(igr) ,dunel (igr)       ,cs(igr,1) )
!
!           Calculate sediment transport.
!
         call gstrfo&
         &(incall  ,nfrac ,g ,pacfac   ,relden   ,kinvis    ,&
         &grsize(1,igr,1),cs(igr,1)   ,u ,depthl,rs(igr,1) ,&
         &uscoef(1,ind)  ,trform(1,ibr)  ,dfrac ,forcon(1,1,ibr),&
         &sedtra  ,sedexp(igr),nvast  ,alffl    ,zbave(igr),&
         &zbfl(igr) ,nchfla   ,ngrid  ,igr      ,ptrla2    )
!
!           Calculate the sediment transport width
!
         if (wfs(igr,1) .gt. ws(igr)) then
!              if flow widht > sediment transporting width use ws
            width = ws(igr)
         else
            width = wfs(igr,1)
         endif
!
!           Multiply transport/unit-width with probabilty per fraction
!           and width
!
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
!
!           Calculate celerity by numerical differentiation,
!           if transport is not zero.
!
         if (.not.equal(sum,0.)) then
!
!              Derivatives of transport
!
            call gsdsd(nbran  ,ngrid  ,nfrac  ,igr   ,ibr    ,g     ,&
            &pacfac ,relden ,kinvis ,depth ,width  ,u     ,&
            &cs(igr,1)      ,nvast  ,alffl ,nchfla ,dfrac ,&
            &nucoef ,uscoef ,trform ,rs    ,grsize ,forcon,&
            &ptrla2 ,sedtra ,dsdu   ,pdspdd ,zbave ,zbfl  )
!
!              Derivatives of layer thickness
!
            call gsddef(heiopt ,lathic ,g      ,relden ,kinvis ,&
            &dmed0  (igr)   ,cs(igr,1)      ,u      ,&
            &depth  ,frou2  ,duncof ,trform(1,ibr)  ,&
            &duncon ,sedexp(igr)    ,deff2(igr)     ,&
            &ddefdh ,ddefdu ,ddefdd ,redfac )
!
!              Derivatives to area
!              The actual number of sections are
!
            nsect = int(asubsc(igr)) + 1

            call gsduhd(g      ,igr    ,nsect  ,u     ,&
            &depth  ,ibrl   ,maxlev ,ngrid ,nlev  ,hlev  ,&
            &wft    ,ws     ,secths ,h2    ,q2    ,qs    ,&
            &wf     ,wfs    ,af     ,afs   ,rs    ,alfab ,&
            &juer   ,duda   ,dhda   ,ker   )

            if (ker .ge. warnng) then
               if (ker .eq. fatal) then
                  goto 100
               else
                  ibrl = 0
               endif
            endif
!
!              Calculate if it is sedimentation or erosion.
!              Calculate coefficients DELTA.. in d(deff)/dt
!
            jl = max(igr-1,igrl)
            jr = min(igr+1,igrr)
            call gscond(nlayer ,nfrac   ,ngrid,igr   ,jl    ,jr    ,&
            &width  ,deff2   (igr) ,dunel (igr)  ,&
            &grsize (dmed,igr,1)   ,grsize(dmed,igr,exl),&
            &grsize (dmed,igr,nml) ,dhda  ,duda  ,ddefdh,&
            &ddefdu ,ddefdd  ,fdi  ,pdiadi,x     ,sedtr ,&
            &source ,depos   (igr) ,delta ,deltb ,deltc ,&
            &jugralg)
!
!              Calculate coefficients for celerity.
!
            call gsacel(nlayer ,nfrac  ,ngrid ,igr   ,width  ,duda  ,&
            &delta  ,deltb  ,deltc ,fdi   ,depos  ,ptrla2,&
            &pdiacc ,pexla2 ,p0la  ,sedtr ,dunel  ,deff2 ,&
            &dfrac  ,pdspdd ,dsdu  ,cela0 ,cela1a ,cela1b,&
            &cela2  ,cela3  ,cela  ,celb  ,celc   ,&
            &nunlay ,nrdzdl ,sedtrp)
!
!              Determine maximum dS/du(j) for celerity calculation
!
            cym   = 0.
            do 40 jf=1,nfrac
               cym = max (cym,abs(dsdu(jf)))
40          continue
!
!              Calculate total estimated celerity

            celert (igr) = max (cym * duda, abs(sedtr(igr,nfrac+1)) /&
            &(deff2(igr)*width))
         else
!
!              No sediment transport occured.
!
            lwet = .false.
         endif
      endif
!
!        Set celerities to zero if transport is zero or if gridpoint
!        is dry
!
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
!
50 continue
   return
!
100 continue
!
end
