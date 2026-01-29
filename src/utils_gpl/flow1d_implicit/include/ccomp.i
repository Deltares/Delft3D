integer       istep ,mstep ,ostep ,it ,steps ,                              &
&              nreduc ,ptim(2),                                              &
&              filstp,cpredn,nostep,Iwqin
real          thetau
double precision     time  ,timeps,dtm   ,fp ,time0
logical       tmpkal,tmpsed,tmpmor,tmpstd,tmpriv,                           &
&              fflow ,fkalm ,fsalt ,fsedt ,fmorp ,fgrad ,                    &
&              lastts,ffp
!                   mozart declaration
logical       lmozad, lestmorf
integer       nstepd, nstart
!
!     Pointers to arrays and single integer values
!
integer       flwpar ,branch, nbran,  ngrid,  hpack,  qpack
!
!     Additional Pointers for SRW
!
integer       h2 ,q2 , hstat, nhstat, qstat, nqstat ,                        &
&              nqlat, qlat, storWidth

common /ccomp/ time  ,timeps,dtm ,fp,time0,istep,mstep ,                     &
&               ostep ,it    ,steps ,nreduc ,ptim,                            &
&               filstp,cpredn,nostep,Iwqin,nstepd,                            &
&               nstart, thetau, tmpkal,tmpsed,flwpar,                         &
&               tmpmor,tmpstd,tmpriv,fflow,fkalm,                             &
&               fsalt ,fsedt ,                                                &
&               fmorp ,fgrad ,lastts,ffp, lmozad, lestmorf
