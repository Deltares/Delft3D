      Subroutine MozWriteh(itmstp ,ngrid  ,nqlat ,qltpar,
     +                     qlatid ,tempuse,h     )  
c
c     **************************************
c     * write water levels on q-lat points *
c     **************************************
c
      integer       nqlat, ngrid, itmstp
      real          qltpar(9,nqlat), tempuse(ngrid)
      double precision h(ngrid, 3)
      character*40  qlatid(nqlat)
      
      integer       jopt, igr, iout, nfm, i, ii, istat, iopt, 
     +              nloca, nhis, iloc   
      character*120 name, path
      character*20  idpara(1), idq
      character*1   quote, header*160
        
      quote = char(39)
      idpara(1) = 'Water Level'

      do 30 igr = 1, ngrid
        tempuse(igr) = 0.
   30 continue
c
c open his-file
c
      jopt = 0
c     name = 'mozarth.his'
      iout = 120
      nfm = 0
      open (iout, file = 'Sbmzkop.fnm')
   10 read (iout,'(a)') path
        if(path(1:1) .eq. '*') then
          goto 10
        else
          nfm = nfm + 1
      if (nfm .lt. 6) goto 10
          read (path,*) name
        endif
      close (iout)
      call mozopenhs (iout, NAME, jOPT)
c
c bepaal aantal locaties
c
      ii = 0
      do istat = 1, nqlat
         iopt = int(qltpar(2,istat))
         if (iopt .eq. 8) then        
            igr = int(qltpar(5,istat))
            ii = ii + 1
            tempuse(ii) = h(igr, 3)
c           id(ii) = qlatid(istat)
         endif
      end do                  
c
c schrijf waterstanden naar HIS-file
c
      header = 'Sobek-Mozart'
      nloca  = ii
      nhis   = 1
      ii = 1
      IF (jOPT .EQ. 1) THEN
         WRITE(IOUT,*)  NLOCA
         DO I=1,NLOCA
            do istat = ii, nqlat
               iopt = int(qltpar(2,istat))
               if (iopt .eq. 8) then        
                  igr = int(qltpar(5,istat))
                      idq = qlatid(istat)(1:20)
                  ii = istat + 1
                  goto 15
               endif
            end do                  
   15       WRITE(IOUT,*)  QUOTE, IDq, QUOTE
         ENDDO
         WRITE(IOUT,*)  NHIS
         DO I=1,NHIS
            WRITE(IOUT,*)  QUOTE, IDPARA(I), QUOTE
         ENDDO
      ELSE
         WRITE(IOUT) Header
         WRITE(IOUT) NHIS, NLOCA
         WRITE(IOUT) (IDPARA(I)(1:20),I=1,NHIS)
         DO I=1,NLOCA
            do istat = ii, nqlat
               iopt = int(qltpar(2,istat))
               if (iopt .eq. 8) then        
                  igr = int(qltpar(5,istat))
                      idq = qlatid(istat)(1:20)
                  ii = istat +1
                  goto 20
               endif
            end do                  
   20       WRITE(IOUT)  i, IDq
         ENDDO
c        WRITE(IOUT) (I, ID(I)(1:20),I=1,NLOCA)
      ENDIF   
      IF (jOPT .EQ. 1) THEN
        WRITE (IOUT,*)
     *         (tempuse(ILOC), ILOC=1,NLOCA)
      ELSE
        WRITE (IOUT)  ITMSTP,
     *         (tempuse(ILOC),ILOC=1,NLOCA)
      ENDIF
      close (iout)
      end
