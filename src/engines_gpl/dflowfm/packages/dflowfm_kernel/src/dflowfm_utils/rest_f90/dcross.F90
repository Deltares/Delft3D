!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
! 
! 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
      SUBROUTINE dCROSS(X1,Y1,X2,Y2,X3,Y3,X4,Y4,JACROS,SL,SM,XCR,YCR,CRP) ! liggen 3 en 4 aan weerszijden van lijn 12
      use m_sferic
      use geometry_module, only: getdxdy, sphertoCart3D, Cart3Dtospher, crossinbox
      use m_missing, only: dmiss
      implicit none
      double precision           :: det
      double precision           :: eps
      DOUBLE PRECISION           :: X1,Y1,X2,Y2,X3,Y3,X4,Y4,SL,SM,XCR,YCR,CRP
      INTEGER                    :: JACROS

      DOUBLE PRECISION           :: X21, Y21, X43, Y43, X31, Y31
      double precision           :: xx1, yy1, zz1
      double precision           :: xx2, yy2, zz2
      double precision           :: xx3, yy3, zz3
      double precision           :: xx4, yy4, zz4
      double precision           :: xx21, yy21, zz21
      double precision           :: xx43, yy43, zz43
      double precision           :: xx31, yy31, zz31
      double precision           :: xxn, yyn, zzn
      double precision           :: det2
      double precision           :: xxcr, yycr, zzcr


      JACROS = 0
      EPS    = 0.00001d0
!     SL     = LABDA TUSSEN 0 EN 1 OP EERSTE PAAR
!     Sm     = LABDA TUSSEN 0 EN 1 OP TWEEDE PAAR

      if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
         call sphertoCart3D(x1,y1,xx1,yy1,zz1)
         call sphertoCart3D(x2,y2,xx2,yy2,zz2)
         call sphertoCart3D(x3,y3,xx3,yy3,zz3)
         call sphertoCart3D(x4,y4,xx4,yy4,zz4)

         xx21 = xx2 - xx1
         yy21 = yy2 - yy1
         zz21 = zz2 - zz1

         xx43 = xx4 - xx3
         yy43 = yy4 - yy3
         zz43 = zz4 - zz3

         xx31 = xx3 - xx1
         yy31 = yy3 - yy1
         zz31 = zz3 - zz1

         xxn = yy43*zz21 - zz43*yy21
         yyn = zz43*xx21 - xx43*zz21
         zzn = xx43*yy21 - yy43*xx21

         det2 = xxn**2 + yyn**2 + zzn**2
         det  = sqrt(det2)

         if ( det.lt.eps ) then
            return
         else
            SL = ( xxn*(yy43*zz31-zz43*yy31) + yyn*(zz43*xx31-xx43*zz31) + zzn*(xx43*yy31-yy43*xx31) ) / det2
            SM = ( xxn*(yy21*zz31-zz21*yy31) + yyn*(zz21*xx31-xx21*zz31) + zzn*(xx21*yy31-yy21*xx31) ) / det2

            xxcr = 0.5d0*(xx1 + SL*xx21 + xx3 + SM*xx43)
            yycr = 0.5d0*(yy1 + SL*yy21 + yy3 + SM*yy43)
            zzcr = 0.5d0*(zz1 + SL*zz21 + zz3 + SM*zz43)

            call Cart3Dtospher(xxcr,yycr,zzcr,xcr,ycr,maxval((/x1,x2,x3,x4/)))
!            CRP = -DET
            crp = -(xxn*xxcr + yyn*yycr + zzn*zzcr) / sqrt( xxcr**2 + yycr**2 + zzcr**2)
            IF (SM >= 0d0 .AND. SM <= 1d0) then
               JACROS = 1
            ENDIF
         end if

      else
         call getdxdy(x1,y1,x2,y2,x21,y21,jsferic)
         call getdxdy(x3,y3,x4,y4,x43,y43,jsferic)
         call getdxdy(x1,y1,x3,y3,x31,y31,jsferic)

         !X21 =  getdx(x1,y1,x2,y2)
         !Y21 =  getdy(x1,y1,x2,y2)
         !X43 =  getdx(x3,y3,x4,y4)
         !Y43 =  getdy(x3,y3,x4,y4)
         !X31 =  getdx(x1,y1,x3,y3)
         !Y31 =  getdy(x1,y1,x3,y3)
         DET    = X43*Y21 - Y43*X21
         IF (ABS(DET) .LT. EPS) THEN
            RETURN
         ELSE
            SM = (Y31*X21 - X31*Y21) / DET
            IF (ABS(X21) .GT. EPS) THEN
               SL = (SM*X43 + X31) / X21
            ELSE IF (ABS(Y21) .GT. EPS) THEN
               SL = (SM*Y43 + Y31) / Y21
            ELSE
               SL   = 0d0
            ENDIF
            XCR = X1 + SL*(X2-X1)
            YCR = Y1 + SL*(Y2-Y1)
            CRP = -DET
            IF (SM >= 0d0 .AND. SM <= 1d0) then
               JACROS = 1
            ENDIF
         ENDIF
      end if

      RETURN
      END subroutine dcross
