      subroutine gstrfo (initra ,nfrac  ,g      ,pacfac ,relden ,kinvis,
     &                   grn    ,chezy  ,u      ,depth  ,hrad   ,uscofb,
     &                   trforb ,dfrac  ,frcnfr ,sedtra ,sedexp ,nvast ,
     &                   alffl  ,zbave  ,zbfl   ,nchfla ,ngrid  ,igr   ,
     &                   p      )
c
c     Graded Sediment Transport Formulas

c
c     Declaration of parameters
c
      integer    nfrac  ,nvast    ,nchfla  ,ngrid     ,igr
      real       g      ,pacfac   ,relden  ,kinvis    ,chezy  ,hrad  ,
     &           u      ,depth    ,sedexp  ,alffl     ,zbave  ,zbfl
      real       grn(4) ,uscofb(*),frcnfr  (nfrac+1,*),
     &           trforb (3)       ,dfrac   (nfrac)    ,sedtra (nfrac),
     &           p      (ngrid,nfrac) 
      logical    initra
c
c     Declaration of local parameters
c
      integer    i
      real       velo   ,factor   ,distnc  ,daythn   ,disrat 
      real       hidexpf(nfrac)
c
c     Constants
c
      integer     d35   , d50   , d90   , dmed  
      parameter  (d35=1 , d50=2 , d90=3 , dmed=4)
c
      velo  = abs(u)
c
      goto (10,20,30,199,199,199,70,80,90,100) int(trforb(1))
c
c        Engelund and Hansen
c
   10    continue
         call gstfeh (initra ,nfrac ,g      ,pacfac ,relden ,chezy ,
     &                velo   ,dfrac ,frcnfr(1,1)    ,sedtra )
      goto 200
c
c        Meyer-Peter and Muller
c
   20    continue
         call gshidexp (initra ,nfrac ,ngrid  ,grn     ,dfrac  ,relden,
c                                              hidexpp
     &                  chezy  ,velo  ,p      ,uscofb(11)      ,igr   ,
     &                  hidexpf)
c
         call gstfmm   (initra ,nfrac  ,g     ,pacfac ,relden ,grn(d90), 
     &                  chezy  ,velo   ,hrad  ,dfrac  ,frcnfr(1,1)     , 
     &                  sedtra ,uscofb ,hidexpf       )
      goto 200
c
c        Ackers and White
c
   30    continue
         call gshidexp (initra ,nfrac ,ngrid  ,grn     ,dfrac  ,relden,
c                                              hidexpp
     &                  chezy  ,velo  ,p      ,uscofb(11)      ,igr   ,
     &                  hidexpf )
c
         call gstfaw   (initra ,nfrac  ,g      ,pacfac ,relden ,kinvis,
     &                  chezy  ,velo   ,depth  ,dfrac  ,frcnfr(1,1)   ,
     &                  frcnfr(1,2)    ,frcnfr(1,3)    ,frcnfr(1,4)   ,
     &                  sedtra ,hidexpf)
      goto 200
c
c        Van Rijn
c
c  40    call gstfvr
c     goto 200
c
c        Parker and Klingeman
c
c  50    call gstfpk
c    &       (initra   ,g        ,pacfac   ,relden ,grn(d50) ,chezy ,
c    &        velo     ,forcng(1),forcng(2),sedtra )
c     goto 200
c
c        User defined formula
c
c  60    call gstfud
c     goto 200
c
c        Special formulae for Waalbocht
c
   70    continue
         call gstfwb (initra ,nfrac  ,pacfac ,grn(dmed) ,velo  ,uscofb ,
     &                sedtra, sedexp )
      goto 200
c
c        Meyer-Peter and Muller (special version)
c
   80    continue
         call gshidexp (initra ,nfrac ,ngrid  ,grn     ,dfrac  ,relden,
c                                              hidexpp
     &                  chezy  ,velo  ,p      ,uscofb(11)      ,igr   ,
     &                  hidexpf )
c
         call gstfm1   (initra ,nfrac  ,g     ,pacfac  ,relden ,chezy  ,
     &                  velo   ,dfrac  ,frcnfr(1,1)    ,sedtra ,uscofb ,
     &                  hidexpf) 
      goto 200
c      
c        Ashida & Michiue for graded material
c
   90    continue
         call gshidexp (initra ,nfrac ,ngrid  ,grn     ,dfrac  ,relden,
c                                              hidexpp
     &                  chezy  ,velo  ,p      ,uscofb(11)      ,igr   ,
     &                  hidexpf )
c
         call gstfashida (initra ,nfrac  ,g      ,pacfac ,relden     ,
     &                    grn(dmed)      ,chezy  ,velo   ,hrad       ,
     &                    dfrac  ,frcnfr(1,1)    ,frcnfr(1,2)        ,
     &                    sedtra ,uscofb ,hidexpf)      
      goto 200
c      
c        Wu, Wang and Jia
c
  100    continue
         call gshidexp (initra ,nfrac ,ngrid  ,grn     ,dfrac  ,relden ,
c                                              hidexpp
     &                  chezy  ,velo  ,p      ,uscofb(11)      ,igr    ,
     &                  hidexpf )
c
         call gstwwj   (initra  ,nfrac  ,g     ,pacfac ,relden ,kinvis ,
     &                  grn(d50),chezy  ,velo  ,hrad   ,dfrac  ,
     &                  frcnfr(1,1)     ,frcnfr(1,2)   ,frcnfr(1,3)    ,
     &                  sedtra  ,uscofb ,hidexpf       )
      goto 200
c
c        No formula known, exit with transport = 0
c
  199    continue
         do i=1,nfrac
            sedtra(i) = 0.
         enddo
  200 continue
c
c  alffl coefficient om grootte ongestoorde laag te bepalen (=alffl * h )
c  zbave breedte-gemiddelde bodemligging volgens lagen model.
c  zbfl  ligging van de vaste laag
c  nchfla =1 lineair verloop =2 sinus verloop
c  nvast =1 vaste laag optie , = 0 geen vaste laag
c
      factor = trforb(3)
      if (nvast.eq.1) then
         distnc = alffl * depth
         daythn = amax1(zbave - zbfl, 0.0)
         if (daythn.lt.distnc) then
            disrat = daythn /distnc
            if (nchfla.eq.1) then
               factor= factor * disrat
            else
               factor= factor * sin(1.57079633 * disrat)
            endif
         endif
      endif
c
      do i=1,nfrac
         sedtra(i) = sign(sedtra(i),u) * factor
      enddo
c
      end
