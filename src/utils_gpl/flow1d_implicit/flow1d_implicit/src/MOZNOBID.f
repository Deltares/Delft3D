      Integer function MozSobNodId (cNodeId, nqlat, qltpar, qlatid) 
c
c           **********************************************
c           * Find Sobek lateral node id                 *
c           **********************************************
      integer      nqlat
      real         qltpar(9,*)
      character(len=40) qlatid(*), cNodeId
      
      integer      istat ,iopt
c
      do istat = 1, nqlat
c
         iopt = int(qltpar(2,istat))
         if (iopt .eq. 8) then
           if ( qlatid (istat) .eq. cNodeId) then
             MozSobNodId = istat
             return
           endif
         endif
      end do             
      MozSobNodId = 0
c
      end
