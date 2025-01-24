subroutine KAFPAH(nfpamn ,nkfpmp ,nkfptm ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,lfilt  ,&
&kfpmap ,kfptim ,nnf    ,pfa    ,nnmu   ,&
&pmua   ,pw     ,np     ,p1     ,nkapar ,&
&fpacpr ,buffer ,corrnm ,kalini ,dt     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAFPAH (KAlman Filtered PArameter results to
!                             HIS file)
!
! Module description: Write the user selected Kalman module results to
!                     the HIS file. The results processed in this
!                     module are filtered mean and covariances of the
!                     correction parameters.
!
!                     The stored data can be processed further by the
!                     User Interface. The user can select functions of
!                     place or functions of time.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 27 buffer(ngrid)     O  Buffer for results to be written to NEFIS.
! 10 first             I  True in case of first call.
! 26 fpacpr(nfpamn)    I  fpacpr(i) = index in block table (1...nentri)
!                         for main code i of the parameter filter
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
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
!  3 nfpamn            I  Number of main codes of parameter filter
!  6 ngrid             I  Number of grid points in network.
! 23 nkapar            I  = nnf + nnmu + 1
!  4 nkfpmp            I  Number of entries in kfpmap.
!  5 nkfptm            I  Number of entries in kfptim.
! 16 nnf               I  Number of uncertain bed friction parameters.
! 18 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
! 21 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 22 p1(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n+1 (filtered
!                         values) or n|n (previous time step).
! 17 pfa(nnf)          I  Uncertain bed friction parameters of all
! 19 pmua(nnmu)        I  Uncertain energy loss parameters in case of
! 20 pw                I  Uncertain wind stress parameter.
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
! $Log: kafpah.pf,v $
! Revision 1.2  1998/02/13  12:12:41  kuipe_j
! Adapt to CMT
!
! Revision 1.1  1997/06/17  11:26:47  kuipe_j
! output in history format
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
   include '..\include\filsim.i'
!
!     Declaration of parameters
!
   integer      nentri
   parameter   (nentri=2)
   integer      nfpamn ,nkfpmp ,nkfptm ,ngrid  ,istep  ,nstep ,&
   &nnf    ,nnmu   ,nkapar ,np
   integer      kfpmap(nkfpmp),&
   &kfptim(nkfptm) ,fpacpr(nfpamn),itim  (2)     ,&
   &kalini(*)      ,code(2)
   real         pfa(nnf)       ,pmua(nnmu)    ,pw
   real         p1(np,np)      ,buffer(nentri,ngrid)
   logical      first          ,lfilt
   double precision             dt
!
!     Declaration of local variables
!
   integer      i ,j ,k ,ie , igr, nlc, nsk
   integer      ijaar ,imaand ,idag ,iuur ,imin ,isec, iscu
   integer      lun   ,ivar   ,nvar ,igrid,ifil ,istepf, istphf
   integer      ipar
   double precision    scudt
   character*40 idmap(4) , idhis(4)
   character*40 corrnm(*)
   character*20 parnam(3)
   logical      new      , newuit
!
!     Declaration of external functions
!
   logical      yesmap
   external     yesmap
   save         istepf, istphf, scudt
!
   data (code(i),i=1,nentri) / 1, 1/
!
!     Initialize.
!
   if (first) call resadm (nentri ,code   ,fpacpr )
!
   idmap(1)  = 'SOBEK                                   '
   idmap(2)  = 'Maps results at gridpoints              '
   idmap(3)  = '                                        '
   idmap(4)  = '                                        '
   parnam(1) = 'Correction param.   '
   parnam(2) = 'Filt. param. var.   '
   idhis(1)  = 'SOBEK                                   '
   idhis(2)  = 'History results at gridpoints           '
   idhis(3)  = '                                        '
   idhis(4)  = '                                        '
!
   if (lfilt) then
!
!       Write Map results
!
      if (nkfpmp .gt. 3) then
         if (yesmap(kfpmap(1),kfpmap(2),kfpmap(3),istep)) then
            nvar = nint(0.5*(nkfpmp-3))
            ifil = 7
            lun  = 66
            if ( kalini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = fipmap , form = 'binary')
#else
#if defined (USE_HPUX)
               open(lun , file = fipmap , form = 'unformatted')
#else
               open(lun , file = fipmap , form = 'unformatted')
#endif
#endif
               iscu = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu)
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idmap(4),1000 ) ijaar,imaand,idag,&
               &iuur,imin,isec,iscu
               write(lun) (idmap(i) , i = 1,4)
               write(lun) nvar,nkapar
               write(lun) ( parnam(fpacpr(kfpmap(i))+&
               &kfpmap(i+1)) ,  i = 4,nkfpmp,2 )
               write(lun) (ipar , corrnm(ipar)(:20),&
               &ipar = 1 , nkapar)
               kalini(ifil) = 1
               istepf = istep
            endif
!
            ivar = 0
            do 40 i = 4,nkfpmp,2
               ivar = ivar + 1
               ie = fpacpr(kfpmap(i))+kfpmap(i+1)
               if (ie.eq.1) then
                  k = 1
                  do 12 j = 1, nnf
                     buffer(ivar,k) = pfa(j)
                     k = k + 1
12                continue
                  do 14 j = 1, nnmu
                     buffer(ivar,k) = pmua(j)
                     k = k + 1
14                continue
                  buffer(ivar,k) = pw
               elseif (ie.eq.2) then
                  k = 1
                  do 25 j = ngrid*2+1, np
                     buffer(ivar,k) = p1(j,j)
                     k = k + 1
25                continue
               endif
40          continue
            write(lun) nint((istep-istepf)*scudt),&
            &((buffer(ivar,ipar),&
            &ivar = 1 , nvar),&
            &ipar = 1 , nkapar)
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
            nvar = nint(.5*(nkfptm-kfptim(1)-2-nsk+1))
            ifil = 8
            lun  = 67
            if ( kalini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = fiphis , form = 'binary')
#else
#if defined (USE_HPUX)
               open(lun , file = fiphis , form = 'unformatted')
#else
               open(lun , file = fiphis , form = 'unformatted')
#endif
#endif
               iscu = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu)
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idhis(4),1000 ) ijaar,imaand,idag ,&
               &iuur,imin,isec,iscu
               write(lun) (idhis(i) , i = 1,4)
               write(lun) nvar , kfptim(1)
               write(lun) (parnam(fpacpr(kfptim(i))+kfptim(i+1)),&
               &i = kfptim(1)+2+nsk,nkfptm,2)
               write(lun) (kfptim(igrid),corrnm(kfptim(igrid))(:20),&
               &igrid = 2 , kfptim(1)+1)
               kalini(ifil) = 1
               istphf       = istep
            endif
!
            ivar = 0
            do 80 i = kfptim(1)+2+nsk,nkfptm,2
               ivar = ivar + 1
               ie = fpacpr(kfptim(i))+kfptim(i+1)
               if      (ie.eq.1) then
                  do 55 j=1,kfptim(1)
                     igr = kfptim(j+1)
                     if (igr .ge. 1 .and. igr .le. nnf) then
                        buffer(ivar,j) = pfa(igr)
                     else if (igr .gt. nnf .and. igr .lt. nkapar) then
                        buffer(ivar,j) = pmua(igr-nnf)
                     else
                        buffer(ivar,j) = pw
                     endif
55                continue
               elseif (ie.eq.2) then
                  do 65 j=1,kfptim(1)
                     igr = kfptim(j+1) + 2*ngrid
                     buffer(ivar,j) = p1(igr,igr)
65                continue
               endif
80          continue
            write(lun) nint((istep-istphf)*scudt),&
            &((buffer(ivar,j),&
            &ivar = 1,nvar),&
            &j    = 1,kfptim(1))
         endif
      endif
   endif
!
1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,&
   &'  (scu=',i8,'s)')
!
end
