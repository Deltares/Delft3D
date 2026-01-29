subroutine KAFRER(fd_nefis_res, nfremn ,nkfrmp ,nkfrtm ,nsamp  ,&
&itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,&
&juer   ,kfrmap ,kfrtim ,res    ,scares ,rescov ,&
&ncelm  ,ncelh  ,frecpr ,buf    ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAFRER (KAlman Filter REsidual Results)
!
! Module description: Write the user selected Kalman module results to
!                     the result file. The results processed in this
!                     module are scaled residuals and covariances of
!                     residuals.
!
!                     The stored data can be processed further by the
!                     User Interface. The user can select functions of
!                     place or functions of time. See [S-DD-002.1MR] for
!                     a specification of the nefis names.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 22 buf(ngrid)        O  Buffer for results to be written to NEFIS.
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 10 first             I  True in case of first call.
! 21 frecpr(nfremn)    I  frecpr(i) = index in block table (1...nentri)
!                         for main code i of the residuals.
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
! 13 juer              P  -
! 23 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 14 kfrmap(*)         I  Parameter list for residuals MAP block
!                         (1/2/3) Report step of begin, end, resp.
!                         increment.
!                         (4) Main code parameter 1.
!                         (5) Sub code parameter 1.
!                         (.) Etc.
! 15 kfrtim(*)         I  Parameter list for  residuals HIST
!                         block
!                         (1) Number of places.
!                         (2) Location 1.
!                         (i) Location n.
!                         (i+1) Main code parameter 1.
!                         (i+2) Sub code parameter 1.
!                         (.) Etc.
! 12 lfilt             I  = True if a filter step must be performed.
! 20 ncelh             I  Actual cell number of a history block of re-
!                         sult file.
! 19 ncelm             I  Actual cell number of a map block of result
!                         file.
!  3 nfremn            I  Number of main codes of residuals.
!  4 nkfrmp            I  Number of entries in kfrmap.
!  5 nkfrtm            I  Number of entries in kfrtim.
!  6 nsamp             I  Number of hydrodynamic samples (measurements)
!  9 nstep             I  Last time step number in simulation.
! 16 res(nsamp)        I  Residual vector
! 18 rescov(nsamp,     I  Matrix with covariances of residuals.
!      ,nsamp)
! 17 scares(nsamp)     I  Scaled residual vector
! 11 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flsdat  FLuSh buffers of DATa file
! putrel  PUT Real ELement to a nefis file
! resadm  RESults; make ADMinistration for element selection
! resdes  RESults; DEScription group is defined
! resini  RESults; writing is INItialized
! restim  RESults; writing of current TIMe
! yesmap  YES MAP results are written
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kafrer.pf,v $
! Revision 1.5  1999/03/15  15:51:50  kuipe_j
! tabs removed
!
! Revision 1.4  1996/12/03  07:59:06  kuipe_j
! dimension of element names improved
!
! Revision 1.3  1996/09/03  14:54:24  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1996/04/12  13:04:56  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:34  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nfremn ,nkfrmp ,nkfrtm ,nsamp  ,juer   ,istep  ,&
   &nstep  ,ncelm  ,ncelh  ,ker
   integer      fd_nefis_res, kfrmap(nkfrmp)  ,&
   &kfrtim(nkfrtm) ,frecpr(nfremn),itim  (2)
   real         buf(nsamp)     ,res(nsamp)    ,scares(nsamp)   ,&
   &rescov(nsamp,nsamp)
   logical      first  ,writim ,lfilt
!
!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=3)
   integer      errr  ,i ,j ,ie ,nrerr, ismp, nsk, nlc, lastcod
   integer      code  (nentri)   ,usrord(1),&
   &uindex(3)
   character*16 grnamm          ,grnamh          ,grnamd         ,&
   &name
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*8  txt
   logical      llog, new    ,newuit
!
!     Declaration of external functions
!
   integer      putrel ,flsdat
   logical      yesmap
   external     putrel ,flsdat ,yesmap
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   data  usrord /1/
!
!     Definition of elements.
!
   data (code(i) ,descel(i) ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
   &1 ,'residual'            ,'HIS_rs' ,'rs', '-' ,&
   &1 ,'Scaled residual'     ,'HIS_sr' ,'sr', '-' ,&
   &1 ,'Residual covariance' ,'HIS_rc' ,'rc', '-' /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'KFRES-MAP-GROUP' ,&
   &'KFRES-HIS-GROUP' ,&
   &'KFRES-DES-GROUP' /
!
   lastcod = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = ekaboo
!
      call resadm (nentri ,code   ,frecpr )
!
      call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KFRES',&
      &nentri ,nkfrmp ,nkfrtm ,nsamp  ,itim   ,nameel ,&
      &quanel ,unitel ,descel ,frecpr ,kfrmap ,kfrtim ,&
      &ncelm  ,ncelh  ,nameac ,lastcod,errr   )
!
      if (errr.ne.0) goto 1000
   endif
!
   if (lfilt) then
!
!       Write Map results
!
      if (nkfrmp .gt. 3) then
         if (yesmap(kfrmap(1),kfrmap(2),kfrmap(3),istep)) then
            nrerr = ekamap
!
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,&
            &nameac ,errr   )
            if (errr.ne.0) goto 1000
!
            call resdes ('KFRES',fd_nefis_res, grnamd ,1 ,writim ,&
            &kfrtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
!
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
!
            do 50 i = 4,nkfrmp,2
               ie = frecpr(kfrmap(i))+kfrmap(i+1)
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
               name  = 'MAP'//nameel(ie)(4:)
               goto (10,20,30) , ie
10             continue
               do 15 j = 1, nsamp
                  buf(j) = res(j)
15             continue
               goto 40
20             continue
               do 25 j = 1, nsamp
                  buf(j) = scares(j)
25             continue
               goto 40
30             continue
               do 35 j = 1, nsamp
                  buf(j) = rescov(j,j)
35             continue
               goto 40
40             continue
               errr = putrel (fd_nefis_res, grnamm ,name   ,&
               &uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
50          continue
         endif
      endif
!
!       Write History results.
!
      nlc = kfrtim(1)
      new = mod(nkfrtm-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
!
      if (nkfrtm .gt. 1+nsk) then
         if (new) then
            newuit = yesmap(kfrtim(nlc+2),kfrtim(nlc+3),kfrtim(nlc+4),&
            &istep)
         else
            newuit = .false.
         endif
         if ( (kfrtim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = ekahis
!
            call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,&
            &nameac ,errr   )
            if (errr.ne.0) goto 1000
!
            call resdes ('KFRES',fd_nefis_res, grnamd ,2 ,writim ,&
            &kfrtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
!
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
!
            do 100 i = kfrtim(1)+2+nsk,nkfrtm,2
               ie = frecpr(kfrtim(i))+kfrtim(i+1)
               goto (60,70,80) ,ie
60             continue
               do 65 j=1,kfrtim(1)
                  ismp = kfrtim(j+1)
                  buf(j) = res(ismp)
65             continue
               goto 90
70             continue
               do 75 j=1,kfrtim(1)
                  ismp = kfrtim(j+1)
                  buf(j) = scares(ismp)
75             continue
               goto 90
80             continue
               do 85 j=1,kfrtim(1)
                  ismp = kfrtim(j+1)
                  buf(j) = rescov(ismp,ismp)
85             continue
               goto 90
90             continue
               errr = putrel (fd_nefis_res, grnamh ,nameel(ie),&
               &uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
100         continue
         endif
      endif
   endif
!
!     Be sure that at the last step the number of cells has been
!     written correctly.
!
   if (istep.ge.nstep) then
      nrerr = ekaeoo
      llog  = .true.
!
      call resdes ('KFRES' ,fd_nefis_res, grnamd ,3 ,llog ,&
      &kfrtim ,ncelm  ,ncelh  ,errr   )
      if (errr.ne.0) goto 1000
!
      errr = flsdat (fd_nefis_res)
      if (errr.ne.0) goto 1000
   endif
!
   return
!
1000 continue
!
   ker = fatal
   write (txt,'(i8)') errr
   call error (juer ,'KAFRER @'//txt//'@' ,nrerr ,ker)
!
end
