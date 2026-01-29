subroutine KAFPAR(fd_nefis_res, nfpamn ,nkfpmp ,nkfptm ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,&
&juer   ,kfpmap ,kfptim ,nnf    ,pfa    ,nnmu   ,&
&pmua   ,pw     ,np     ,p1     ,nkapar ,&
&ncelm  ,ncelh  ,fpacpr ,buf    ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAFPAR (KAlman Filtered PArameter Results)
!
! Module description: Write the user selected Kalman module results to
!                     the result file. The results processed in this
!                     module are filtered mean and covariances of the
!                     correction parameters.
!
!                     The stored data can be processed further by the
!                     User Interface. The user can select functions of
!                     place or functions of time. See [S-DD-002.1MR] for
!                     a specification of the nefis names.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 27 buf(ngrid)        O  Buffer for results to be written to NEFIS.
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 10 first             I  True in case of first call.
! 26 fpacpr(nfpamn)    I  fpacpr(i) = index in block table (1...nentri)
!                         for main code i of the parameter filter
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
! 13 juer              P  -
! 28 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 14 kfpmap(*)         I  Parameter list for residuals (MAP block).
!                         (1/2/3) Report step of begin, end, resp.
!                         increment.
!                         (4) Main code parameter 1.
!                         (5) Sub code parameter 1.
!                         (.) Etc.
! 15 kfptim(*)         I  Parameter list for residuals (HIST block).
!                         (1) Number of places.
!                         (2) Location 1.
!                         (i) Location n.
!                         (i+1) Main code parameter 1.
!                         (i+2) Sub code parameter 1.
!                         (.) Etc.
! 12 lfilt             I  = True if a filter step must be performed.
! 25 ncelh             I  Actual cell number of a history block of re-
!                         sult file.
! 24 ncelm             I  Actual cell number of a map block of result
!                         file.
!  3 nfpamn            I  Number of main codes of parameter filter
!  6 ngrid             I  Number of grid points in network.
! 23 nkapar            I  = nnf + nnmu + 1
!  4 nkfpmp            I  Number of entries in kfpmap.
!  5 nkfptm            I  Number of entries in kfptim.
! 16 nnf               I  Number of uncertain bed friction parameters.
! 18 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
! 21 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  9 nstep             I  Last time step number in simulation.
! 22 p1(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n+1 (filtered
!                         values) or n|n (previous time step).
! 17 pfa(nnf)          I  Uncertain bed friction parameters of all
! 19 pmua(nnmu)        I  Uncertain energy loss parameters in case of
! 20 pw                I  Uncertain wind stress parameter.
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
! $Log: kafpar.pf,v $
! Revision 1.5  1999/03/15  15:51:48  kuipe_j
! tabs removed
!
! Revision 1.4  1996/12/03  07:59:05  kuipe_j
! dimension of element names improved
!
! Revision 1.3  1996/09/03  14:54:23  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1996/04/12  13:04:55  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:32  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nfpamn ,nkfpmp ,nkfptm ,ngrid  ,juer   ,istep  ,&
   &nstep  ,ncelm  ,ncelh  ,ker    ,&
   &nnf    ,nnmu   ,nkapar ,np
   integer      fd_nefis_res, kfpmap(nkfpmp),&
   &kfptim(nkfptm) ,fpacpr(nfpamn),itim  (2)
   real         pfa(nnf)       ,pmua(nnmu)    ,pw
   real         p1(np,np)      ,buf(nkapar)
   logical      first   ,writim,lfilt
!
!
!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=2)
   integer      errr  ,i ,j ,k ,ie ,nrerr, igr, nlc, nsk, lastcod
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
   &1 ,'Correction parameter'          ,'HIS_pf ' ,'pf ', '-' ,&
   &1 ,'Filtered parameter covariance' ,'HIS_pcf' ,'pcf', '-' /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'KFPAR-MAP-GROUP' ,&
   &'KFPAR-HIS-GROUP' ,&
   &'KFPAR-DES-GROUP' /
!
   lastcod = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = ekaboo
!
      call resadm (nentri ,code   ,fpacpr )
!
      call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KFPAR',&
      &nentri ,nkfpmp ,nkfptm ,nkapar ,itim   ,nameel ,&
      &quanel ,unitel ,descel ,fpacpr ,kfpmap ,kfptim ,&
      &ncelm  ,ncelh  ,nameac ,lastcod,errr   )
!
      if (errr.ne.0) goto 1000
   endif
!
   if (lfilt) then
!
!       Write Map results
!
      if (nkfpmp .gt. 3) then
         if (yesmap(kfpmap(1),kfpmap(2),kfpmap(3),istep)) then
            nrerr = ekamap
!
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,&
            &nameac ,errr   )
            if (errr.ne.0) goto 1000
!
            call resdes ('KFPAR',fd_nefis_res, grnamd ,1 ,writim ,&
            &kfptim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
!
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
!
            do 40 i = 4,nkfpmp,2
               ie = fpacpr(kfpmap(i))+kfpmap(i+1)
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
               name  = 'MAP'//nameel(ie)(4:)
               goto (10,20) , ie
10             continue
               k = 1
               do 12 j = 1, nnf
                  buf(k) = pfa(j)
                  k = k + 1
12             continue
               do 14 j = 1, nnmu
                  buf(k) = pmua(j)
                  k = k + 1
14             continue
               buf(k) = pw
               goto 30
20             continue
               k = 1
               do 25 j = ngrid*2+1, np
                  buf(k) = p1(j,j)
                  k = k + 1
25             continue
               goto 30
30             continue
               errr = putrel (fd_nefis_res, grnamm ,name   ,&
               &uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
40          continue
         endif
      endif
!
!       Write History results.
!
      nlc = kfptim(1)
      new = mod(nkfptm-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
!
      if (nkfptm .gt. 1+nsk) then
         if (new) then
            newuit = yesmap(kfptim(nlc+2),kfptim(nlc+3),kfptim(nlc+4),&
            &istep)
         else
            newuit = .false.
         endif
         if ( (kfptim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = ekahis
!
            call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,&
            &nameac ,errr   )
            if (errr.ne.0) goto 1000
!
            call resdes ('KFPAR',fd_nefis_res, grnamd ,2 ,writim ,&
            &kfptim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
!
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
!
            do 80 i = kfptim(1)+2+nsk,nkfptm,2
               ie = fpacpr(kfptim(i))+kfptim(i+1)
               goto (50,60) ,ie
50             continue
               do 55 j=1,kfptim(1)
                  igr = kfptim(j+1)
                  if (igr .ge. 1 .and. igr .le. nnf) then
                     buf(j) = pfa(igr)
                  else if (igr .gt. nnf .and. igr .lt. nkapar) then
                     buf(j) = pmua(igr-nnf)
                  else
                     buf(j) = pw
                  endif
55             continue
               goto 70
60             continue
               do 65 j=1,kfptim(1)
                  igr = kfptim(j+1) + 2*ngrid
                  buf(j) = p1(igr,igr)
65             continue
               goto 70
70             continue
               errr = putrel (fd_nefis_res, grnamh ,nameel(ie),&
               &uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
80          continue
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
      call resdes ('KFPAR' ,fd_nefis_res, grnamd ,3 ,llog ,&
      &kfptim ,ncelm  ,ncelh  ,errr   )
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
   call error (juer ,'KAFPAR @'//txt//'@' ,nrerr ,ker)
!
end
