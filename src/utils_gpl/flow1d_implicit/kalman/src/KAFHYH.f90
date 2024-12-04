subroutine KAFHYH(nfhymn ,nkfhmp ,nkfhtm ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,lfilt  ,&
&kfhmap ,kfhtim ,h      ,q      ,np     ,&
&p1     ,fhycpr ,buffer ,gridnm ,kalini ,dt     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         A.W.J.Koster
!
! Module:             KAFHYH (KAlman Filtered HYdraulic Results to
!                             HIS file)
!
! Module description: Write the user selected Kalman module results to
!                     the result file. The results processed in this
!                     module are filtered mean and covariances of water
!                     levels and discharges.
!
!                     The stored data can be processed further by the
!                     User Interface. The user can select functions of
!                     place or functions of time. See [S-DD-002.1MR] for
!                     a specification of the nefis names.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 23 buffer(ngrid)     O  Buffer for results to be written to NEFIS.
! 22 fhycpr(nfhymn)    I  fhycpr(i) = index in block table (1...nentri)
!                         for main code i of the hydrodynamic filter
! 10 first             I  True in case of first call.
! 16 h(ngrid)          I  Contains water levels in every grid point. It is
!                         stored on index 2 of the packed array hpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
! 14 kfhmap(*)         I  Parameter list for filter results of water
!                         levels and discharges of MAP block.
!                         (1/2/3) Report step of begin, end, resp.
!                         increment.
!                         (4) Main code parameter 1.
!                         (5) Sub code parameter 1.
!                         (.) Etc.
! 15 kfhtim(*)         I  Parameter list for filter results of water
!                         levels and discharges of HIST block.
!                         (1) Number of places.
!                         (2) Location 1.
!                         (i) Location n.
!                         (i+1) Main code parameter 1.
!                         (i+2) Sub code parameter 1.
!                         (.) Etc.
! 12 lfilt             I  = True if a filter step must be performed.
!  3 nfhymn            I  Number of main codes of hydrodynamic filter
!  6 ngrid             I  Number of grid points in network.
!  4 nkfhmp            I  Number of entries in kfhmap.
!  5 nkfhtm            I  Number of entries in kfhtim.
! 18 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 19 p1(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n+1 (filtered
!                         values) or n|n (previous time step).
! 17 q(ngrid)          I  Contains discharges in every grid point. It is
!                         stored on index 2 of the packed array qpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
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
! $Log: kafhyh.pf,v $
! Revision 1.3  1998/06/24  11:10:28  kuipe_j
! Try direct solver if BICGST fails
!
! Revision 1.2  1998/02/13  12:12:40  kuipe_j
! Adapt to CMT
!
! Revision 1.1  1997/06/17  11:26:46  kuipe_j
! output in history format
!
! Revision 1.4  1996/12/03  07:59:04  kuipe_j
! dimension of element names improved
!
! Revision 1.3  1996/09/03  14:54:22  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1996/04/12  13:04:51  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:28  kuipe_j
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
   parameter   (nentri=4)
   integer      nfhymn ,nkfhmp ,nkfhtm ,ngrid  ,istep  ,nstep ,&
   &np
   integer      kfhmap(nkfhmp),&
   &kfhtim(nkfhtm) ,fhycpr(nfhymn),itim  (2)     ,&
   &kalini(*)
   real         p1(np,np),  buffer(nentri,ngrid)
   logical      first    , lfilt
   double       precision dt, h(ngrid), q(ngrid)
!
!     Declaration of local variables
!
   integer      i ,j ,ie  , igr ,nsk ,nlc
   integer      ijaar ,imaand ,idag ,iuur ,imin ,isec,iscu
   integer      lun   ,ivar   ,nvar ,igrid,ifil ,istepf, istphf
   integer      code(nentri)
   character*40 idmap(nentri) , idhis(nentri)
   character*40 gridnm(*)
   character*20 parnam(nentri)
   logical      new           , newuit
   double       precision scudt
!
!     Declaration of external functions
!
   logical      yesmap
   external     yesmap
   save         istepf, istphf,scudt
!
   data (code(i) ,i=1,nentri) / 1, 1, 2, 2/
!
!     Initialize.
!
   if (first) call resadm (nentri ,code   ,fhycpr )
!
   idmap(1)  = 'SOBEK                                   '
   idmap(2)  = 'Maps results at gridpoints              '
   idmap(3)  = '                                        '
   idmap(4)  = '                                        '
   parnam(1) = 'Filt. water level   '
   parnam(2) = 'Filt. wat. lev. var.'
   parnam(3) = 'Filt. discharge     '
   parnam(4) = 'Filt. disch. var.   '
   idhis(1)  = 'SOBEK                                   '
   idhis(2)  = 'History results at gridpoints           '
   idhis(3)  = '                                        '
   idhis(4)  = '                                        '
!
   if (lfilt) then
!
!       Write Map results
!
      if (nkfhmp .gt. 3) then
         if (yesmap(kfhmap(1),kfhmap(2),kfhmap(3),istep)) then
            nvar = nint(0.5*(nkfhmp-3))
            ifil = 5
            lun  = 49
! In Microsoft Powerstation Unit 64 does not work!
            if ( kalini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = fihmap , form = 'binary')
#else
#if defined (USE_HPUX)
               open(lun , file = fihmap , form = 'unformatted')
#else
               open(lun , file = fihmap , form = 'unformatted')
#endif
#endif
               iscu   = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu)
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idmap(4),1000 ) ijaar,imaand,idag,&
               &iuur,imin,isec,iscu
               write(lun) (idmap(i) , i = 1,4)
               write(lun) nvar,ngrid
               write(lun) ( parnam(fhycpr(kfhmap(i))+&
               &kfhmap(i+1)) ,  i = 4,nkfhmp,2 )
               write(lun) (igrid , gridnm(igrid)(:20),&
               &igrid = 1 , ngrid )
               kalini(ifil) = 1
               istepf = istep
            endif
!
            ivar = 0
            do 60 i = 4,nkfhmp,2
               ivar = ivar+1
               ie   = fhycpr(kfhmap(i))+kfhmap(i+1)
               if (ie.eq.1) then
                  do 20 igr = 1, ngrid
                     buffer(ivar,igr) = sngl( h(igr) )
20                continue
               elseif (ie.eq.2) then
                  do 25 igr = 1, ngrid
                     buffer(ivar,igr) = p1(igr,igr)
25                continue
               elseif (ie.eq.3) then
                  do 30 igr = 1, ngrid
                     buffer(ivar,igr) = sngl( q(igr) )
30                continue
               elseif (ie.eq.4) then
                  do 45 igr = 1, ngrid
                     buffer(ivar,igr) = p1(ngrid+igr,ngrid+igr)
45                continue
               endif
60          continue
            write(lun) nint((istep-istepf)*scudt),&
            &((buffer(ivar,igrid),&
            &ivar  = 1 , nvar),&
            &igrid = 1 , ngrid)
         endif
      endif
!
!       Write History results.
!
      nlc = kfhtim(1)
      new = mod(nkfhtm-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
!
      if (nkfhtm .gt. 1+nsk) then
         if (new) then
            newuit = yesmap(kfhtim(nlc+2),kfhtim(nlc+3),kfhtim(nlc+4),&
            &istep)
         else
            newuit = .false.
         endif
         if ( (kfhtim(1).gt.0 .and. .not. new) .or. newuit ) then
            nvar = nint(.5*(nkfhtm-kfhtim(1)-2-nsk+1))
            ifil = 6
            lun  = 65
            if ( kalini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = fihhis , form = 'binary')
#else
#if defined (USE_HPUX)
               open(lun , file = fihhis , form = 'unformatted')
#else
               open(lun , file = fihhis , form = 'unformatted')
#endif
#endif
               iscu   = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu)
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idhis(4),1000 ) ijaar,imaand,idag,&
               &iuur,imin,isec,iscu
               write(lun) (idhis(i) , i = 1,4)
               write(lun) nvar , kfhtim(1)
               write(lun) (parnam(fhycpr(kfhtim(i))+kfhtim(i+1)),&
               &i = kfhtim(1)+2+nsk,nkfhtm,2)
               write(lun) (kfhtim(igrid),gridnm(kfhtim(igrid))(:20),&
               &igrid = 2 , kfhtim(1)+1)
               kalini(ifil) = 1
               istphf       = istep
            endif
!
            ivar = 0
            do 120 i = kfhtim(1)+2+nsk,nkfhtm,2
               ivar = ivar + 1
               ie = fhycpr(kfhtim(i))+kfhtim(i+1)
               if (ie.eq.1) then
                  do 75 j=1,kfhtim(1)
                     igr = kfhtim(j+1)
                     buffer(ivar,j) = sngl( h(igr) )
75                continue
               else if (ie.eq.2) then
                  do 85 j=1,kfhtim(1)
                     igr = kfhtim(j+1)
                     buffer(ivar,j) = p1(igr,igr)
85                continue
               else if (ie.eq.3) then
                  do 95 j=1,kfhtim(1)
                     igr = kfhtim(j+1)
                     buffer(ivar,j) = sngl( q(igr) )
95                continue
               else if (ie.eq.4) then
                  do 105 j=1,kfhtim(1)
                     igr = kfhtim(j+1) + ngrid
                     buffer(ivar,j) = p1(igr,igr)
105               continue
               endif
120         continue
            write(lun) nint((istep-istphf)*scudt),&
            &((buffer(ivar,j),&
            &ivar = 1,nvar),&
            &j    = 1,kfhtim(1))
         endif
      endif
   endif
!
1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,&
   &'  (scu=',i8,'s)')
!
end
