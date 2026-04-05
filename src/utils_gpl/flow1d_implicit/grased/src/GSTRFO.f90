subroutine gstrfo (initra ,nfrac  ,g      ,pacfac ,relden ,kinvis,&
&grn    ,chezy  ,u      ,depth  ,hrad   ,uscofb,&
&trforb ,dfrac  ,frcnfr ,sedtra ,sedexp ,nvast ,&
&alffl  ,zbave  ,zbfl   ,nchfla ,ngrid  ,igr   ,&
&p      )
!
!     Graded Sediment Transport Formulas

!
!     Declaration of parameters
!
   integer    nfrac  ,nvast    ,nchfla  ,ngrid     ,igr
   real       g      ,pacfac   ,relden  ,kinvis    ,chezy  ,hrad  ,&
   &u      ,depth    ,sedexp  ,alffl     ,zbave  ,zbfl
   real       grn(4) ,uscofb(*),frcnfr  (nfrac+1,*),&
   &trforb (3)       ,dfrac   (nfrac)    ,sedtra (nfrac),&
   &p      (ngrid,nfrac)
   logical    initra
!
!     Declaration of local parameters
!
   integer    i
   real       velo   ,factor   ,distnc  ,daythn   ,disrat
   real       hidexpf(nfrac)
!
!     Constants
!
   integer     d35   , d50   , d90   , dmed
   parameter  (d35=1 , d50=2 , d90=3 , dmed=4)
!
   velo  = abs(u)
!
   goto (10,20,30,199,199,199,70,80,90,100) int(trforb(1))
!
!        Engelund and Hansen
!
10 continue
   call gstfeh (initra ,nfrac ,g      ,pacfac ,relden ,chezy ,&
   &velo   ,dfrac ,frcnfr(1,1)    ,sedtra )
   goto 200
!
!        Meyer-Peter and Muller
!
20 continue
   call gshidexp (initra ,nfrac ,ngrid  ,grn     ,dfrac  ,relden,&
!                                              hidexpp
   &chezy  ,velo  ,p      ,uscofb(11)      ,igr   ,&
   &hidexpf)
!
   call gstfmm   (initra ,nfrac  ,g     ,pacfac ,relden ,grn(d90),&
   &chezy  ,velo   ,hrad  ,dfrac  ,frcnfr(1,1)     ,&
   &sedtra ,uscofb ,hidexpf       )
   goto 200
!
!        Ackers and White
!
30 continue
   call gshidexp (initra ,nfrac ,ngrid  ,grn     ,dfrac  ,relden,&
!                                              hidexpp
   &chezy  ,velo  ,p      ,uscofb(11)      ,igr   ,&
   &hidexpf )
!
   call gstfaw   (initra ,nfrac  ,g      ,pacfac ,relden ,kinvis,&
   &chezy  ,velo   ,depth  ,dfrac  ,frcnfr(1,1)   ,&
   &frcnfr(1,2)    ,frcnfr(1,3)    ,frcnfr(1,4)   ,&
   &sedtra ,hidexpf)
   goto 200
!
!        Van Rijn
!
!  40    call gstfvr
!     goto 200
!
!        Parker and Klingeman
!
!  50    call gstfpk
!    &       (initra   ,g        ,pacfac   ,relden ,grn(d50) ,chezy ,
!    &        velo     ,forcng(1),forcng(2),sedtra )
!     goto 200
!
!        User defined formula
!
!  60    call gstfud
!     goto 200
!
!        Special formulae for Waalbocht
!
70 continue
   call gstfwb (initra ,nfrac  ,pacfac ,grn(dmed) ,velo  ,uscofb ,&
   &sedtra, sedexp )
   goto 200
!
!        Meyer-Peter and Muller (special version)
!
80 continue
   call gshidexp (initra ,nfrac ,ngrid  ,grn     ,dfrac  ,relden,&
!                                              hidexpp
   &chezy  ,velo  ,p      ,uscofb(11)      ,igr   ,&
   &hidexpf )
!
   call gstfm1   (initra ,nfrac  ,g     ,pacfac  ,relden ,chezy  ,&
   &velo   ,dfrac  ,frcnfr(1,1)    ,sedtra ,uscofb ,&
   &hidexpf)
   goto 200
!
!        Ashida & Michiue for graded material
!
90 continue
   call gshidexp (initra ,nfrac ,ngrid  ,grn     ,dfrac  ,relden,&
!                                              hidexpp
   &chezy  ,velo  ,p      ,uscofb(11)      ,igr   ,&
   &hidexpf )
!
   call gstfashida (initra ,nfrac  ,g      ,pacfac ,relden     ,&
   &grn(dmed)      ,chezy  ,velo   ,hrad       ,&
   &dfrac  ,frcnfr(1,1)    ,frcnfr(1,2)        ,&
   &sedtra ,uscofb ,hidexpf)
   goto 200
!
!        Wu, Wang and Jia
!
100 continue
   call gshidexp (initra ,nfrac ,ngrid  ,grn     ,dfrac  ,relden ,&
!                                              hidexpp
   &chezy  ,velo  ,p      ,uscofb(11)      ,igr    ,&
   &hidexpf )
!
   call gstwwj   (initra  ,nfrac  ,g     ,pacfac ,relden ,kinvis ,&
   &grn(d50),chezy  ,velo  ,hrad   ,dfrac  ,&
   &frcnfr(1,1)     ,frcnfr(1,2)   ,frcnfr(1,3)    ,&
   &sedtra  ,uscofb ,hidexpf       )
   goto 200
!
!        No formula known, exit with transport = 0
!
199 continue
   do i=1,nfrac
      sedtra(i) = 0.
   enddo
200 continue
!
!  alffl coefficient om grootte ongestoorde laag te bepalen (=alffl * h )
!  zbave breedte-gemiddelde bodemligging volgens lagen model.
!  zbfl  ligging van de vaste laag
!  nchfla =1 lineair verloop =2 sinus verloop
!  nvast =1 vaste laag optie , = 0 geen vaste laag
!
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
!
   do i=1,nfrac
      sedtra(i) = sign(sedtra(i),u) * factor
   enddo
!
end
