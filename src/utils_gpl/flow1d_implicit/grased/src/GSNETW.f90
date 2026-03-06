subroutine gsnetw (nbran  ,ngrid  ,maxlev ,nfrac  ,nlayer ,nunlay,&
&heiopt ,lathic ,rouopt ,juer   ,jugralg,fdi   ,&
&g      ,pacfac ,relden ,kinvis ,redfac ,nvast ,&
&alffl  ,nonall ,branch ,sedinf ,duncof ,pdiacc,&
&dfrac  ,duneh  ,dunel  ,nucoef ,uscoef ,&
&trform ,prslot ,psltvr ,af     ,afs    ,wf    ,&
&wfs    ,nlev   ,hlev   ,wft    ,ws     ,secths,&
&rs     ,cs     ,alfab  ,q2     ,qs     ,h2    ,&
&grsize ,dmed0  ,x      ,source ,ptrla2 ,pexla2,&
&p0la   ,deff2  ,forcon ,duncon ,tmpfr  ,sedtr ,&
&depos  ,celer  ,celert ,asubsc ,nrdzdl ,sedexp,&
&zbave  ,zbfl   ,ker    )

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsnetw.F,v $
! Revision 1.4  1996/06/07  11:56:33  kuipe_j
! multi  +  fixed layer
!
! Revision 1.3  1996/01/08  13:29:58  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.2  1995/09/27  10:12:37  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
   include '..\include\sobdim.i'
!     Declaration of parameters
!
   integer    nbran   ,ngrid   ,maxlev  ,nfrac   ,nlayer  ,&
   &heiopt  ,rouopt  ,lathic  ,juer    ,ker     ,&
   &nunlay  ,nvast   ,jugralg ,nucoef
   integer    branch(4,nbran)  ,sedinf(2,nbran)  ,nlev    (ngrid)  ,&
   &nrdzdl(ngrid)    ,nonall(3,nbran)
   real       g     ,pacfac  ,relden  ,kinvis  ,fdi     ,&
   &alffl ,redfac
   real       uscoef(nrcoefs,nucoef)             ,trform(3,nbran)  ,&
   &duncof(2)        ,prslot(3,nbran)  ,&
   &af    (ngrid)    ,afs   (ngrid,2 ) ,secths(ngrid)    ,&
   &wf    (ngrid)    ,wfs   (ngrid,2)  ,ws    (ngrid)    ,&
   &wft   (ngrid,maxlev)               ,&
   &alfab (ngrid)    ,rs    (ngrid,3)  ,x     (ngrid)    ,&
   &cs    (ngrid,3)  ,qs    (ngrid,2)  ,grsize(4,ngrid,*),&
   &sedtr (ngrid,nfrac+2)   ,celer (ngrid,nfrac,5)       ,&
   &source(ngrid,nfrac+2)   ,ptrla2(ngrid,nfrac)         ,&
   &pexla2(ngrid,nfrac)     ,p0la  (ngrid,nfrac,nunlay)  ,&
   &forcon(nfrac+1,4,nbran) ,duncon(*)                   ,&
   &duneh (ngrid)           ,dunel (ngrid)               ,&
   &deff2 (ngrid)           ,dmed0 (ngrid)               ,&
   &celert(ngrid)           ,asubsc(ngrid)               ,&
   &zbave (ngrid)           ,zbfl  (ngrid)               ,&
   &sedexp(ngrid)           ,&
   &dfrac (nfrac)           ,pdiacc(nfrac)               ,&
   &tmpfr (nfrac+2,*)       ,&
   &psltvr(7,ngrid)
!
   double precision hlev  (ngrid,maxlev), h2(ngrid), q2(ngrid)
!
   logical    depos (ngrid)
!
!     Declaration of local parameters
!
   integer    ibr
!
   do 10 ibr = 1, nbran
!
      if (sedinf(1,ibr) .gt. 0) then
!
!           Sedredge branch
!
      else
!
!           Normal branch
!
         call gsnorb (nbran  ,ngrid  ,maxlev ,nfrac  ,nlayer ,heiopt,&
         &lathic ,rouopt ,ibr    ,juer   ,jugralg,fdi   ,&
         &g      ,pacfac ,relden ,kinvis ,redfac ,nvast ,&
         &alffl  ,nonall(2,ibr)  ,nunlay ,duncof ,pdiacc,&
         &dfrac  ,branch ,duneh  ,dunel  ,nucoef ,uscoef,&
         &trform ,prslot ,psltvr ,af     ,afs    ,wf    ,&
         &wfs    ,nlev   ,&
         &hlev   ,wft    ,ws     ,secths ,rs     ,cs    ,&
         &alfab  ,asubsc ,q2     ,qs     ,h2     ,grsize,&
         &dmed0,x,source ,ptrla2 ,pexla2 ,p0la   ,deff2 ,&
!                                       <sedtra>        <dsdu>
         &forcon ,duncon ,tmpfr(1,1)     ,tmpfr(1,2)    ,&
!                       <pdspdd>        <cela>          <celb>
         &tmpfr(1,3)     ,tmpfr(1,4)     ,tmpfr(1,5)    ,&
!                       <celc>                          <cela0>
         &tmpfr(1,6)     ,sedtr  ,depos  ,celer(1,1,1)  ,&
!                       <cela1a>        <cela1b>        <cela2>
         &celer(1,1,2)   ,celer(1,1,3)   ,celer(1,1,4)  ,&
!                       <cela3>
         &celer(1,1,5)   ,celert ,nrdzdl ,sedexp ,zbave ,&
         &zbfl           ,tmpfr(1,7)     ,ker    )
      endif
10 continue
!
end
