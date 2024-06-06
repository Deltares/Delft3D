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
module unstruc_opengl
   use precision
#ifdef HAVE_OPENGL
   use IFWINA, only: HANDLE
   use IFOPNGL
#endif
   implicit none
   private

#ifdef HAVE_OPENGL
   integer(HANDLE) :: HWND = 0
   integer(HANDLE) :: MEMDC = 0
   integer(HANDLE) :: HBITMAP = 0
   integer         :: OldBitmap = 0
   integer(HANDLE) :: HDC = 0
   integer(HANDLE) :: HRC = 0
   integer, public :: currentWidth, currentHeight
   integer         :: lastNCol = -1
   integer         :: lastWidth, lastHeight
   integer, public :: jaOpenGL = 1   !< use OpenGL (1) or not (0)
! curvigrid is not being displayed with OpenGl
#else
   integer, public :: jaOpenGL = 0   !< use OpenGL (1) or not (0)
#endif
   logical, public :: InOpenGLRendering = .false.
   integer, parameter, public :: FontSize = 28
   real(kind=sp), public :: xlast, ylast

   public :: BeginRender, EndRender, RenderText, SetLineWidth, LineTo, MoveTo, FillPolygon, DrawPoint, SetColorFromColorNR, SetTextHeight

contains

 !todo: optimize by checking for triangle (n=3)/quads (n=4) first, don't switch mode while in those
 SUBROUTINE FillPolygon(xs, ys, n)
    implicit none
    real(kind=sp), intent(in) :: xs(n)
    real(kind=sp), intent(in) :: ys(n)
    integer,       intent(in) :: n !< num vertices
    integer :: i

#ifdef HAVE_OPENGL
    !draw solid polygon
   CALL fglPolygonMode( GL_FRONT, GL_FILL ) !todo: do we have to call this every time?
   CALL fglBegin( GL_POLYGON )
    do i=1,n
        CALL fglVertex2f(xs(i),ys(i))
    end do
   CALL fglEnd()
#endif

 END SUBROUTINE

  SUBROUTINE SetLineWidth(w) ! pixels
    implicit none
    integer, intent(in) :: w
#ifdef HAVE_OPENGL
    CALL fglLineWidth(real(w))
#endif
  END SUBROUTINE


  SUBROUTINE SetPointSize(r) ! pixels
    implicit none
    real(kind=sp), intent(in) :: r
#ifdef HAVE_OPENGL
    CALL fglPointSize(r)
#endif
  END SUBROUTINE


 !todo: optimize all these Begin/End calls, only call when switching modes
 SUBROUTINE DrawLine(x1, y1, x2, y2) ! world coordinates
    implicit none
    real(kind=sp), intent(in) :: x1
    real(kind=sp), intent(in) :: x2
    real(kind=sp), intent(in) :: y1
    real(kind=sp), intent(in) :: y2

#ifdef HAVE_OPENGL
    CALL fglBegin(GL_LINES)
    CALL fglVertex2f(x1, y1)
    CALL fglVertex2f(x2, y2)
    CALL fglEnd()
#endif

 END SUBROUTINE


 SUBROUTINE DrawPoint(x,y) ! world coordinates
    implicit none
    real(kind=sp), intent(in) :: x
    real(kind=sp), intent(in) :: y

#ifdef HAVE_OPENGL
    CALL fglBegin(GL_POINTS)
    CALL fglVertex2f(x, y)
    CALL fglEnd()
#endif
    xlast = x
    ylast = y
 END SUBROUTINE


SUBROUTINE RenderText(x,y,txt) ! world coordinates
  implicit none
  real(kind=sp),    intent(in) :: x
  real(kind=sp),    intent(in) :: y
  CHARACTER(len=*), intent(in) :: txt

#ifdef HAVE_OPENGL
  CALL fglRasterPos2f(x,y)
  CALL fglCallLists (LEN(txt), GL_UNSIGNED_BYTE, LOC(txt)) ! now draw the characters in a string
#endif

END SUBROUTINE


 SUBROUTINE SetColorFromColorNr(ncol) !interacter color nr.
    implicit none
    integer, intent(in) :: ncol
    integer :: rgb, r,g,b
    real(kind=sp) :: rr, gg, bb
    INTEGER, external :: InfoGrPalette !access interacter palette info

#ifdef HAVE_OPENGL
    IF (lastNCol .EQ. ncol) THEN
        RETURN ! no change..
    ENDIF

    lastNCol = ncol

    ! grab the rgb value of the color nr
    rgb = InfoGrPalette(NCOL)

    ! split into separate r, g, b channel values (0.0 - 1.0)
    r = IAND(rgb,z'ff')
    g = IAND(ISHFT(rgb,-8),z'ff')
    b = IAND(ISHFT(rgb,-16),z'ff')
    rr = real(r) / 255.0
    gg = real(g) / 255.0
    bb = real(b) / 255.0

    ! set OpenGl color
    CALL fglColor3f(rr,gg,bb) ! values from 0 - 255
#endif

 END SUBROUTINE



 SUBROUTINE MoveTo(x,y)
  implicit none
  real(kind=hp), intent(in) :: x
  real(kind=hp), intent(in) :: y

#ifdef HAVE_OPENGL
  xlast = x
  ylast = y
#endif
 END SUBROUTINE


 SUBROUTINE LineTo(xd,yd)
  implicit none
  real(kind=hp), intent(in) :: xd
  real(kind=hp), intent(in) :: yd

  real(kind=sp) :: x,y
  x = xd
  y = yd

#ifdef HAVE_OPENGL
  CALL DrawLine(xlast, ylast, x, y)
  xlast = x ! also set xlast & ylast: a LineTo may follow directly without MoveTo in between
  ylast = y
#endif
 END SUBROUTINE


 SUBROUTINE BeginRender
#ifdef HAVE_OPENGL
    use M_DEVICES
    use m_WEARELT
    use unstruc_colors

    implicit none
    integer :: infoscreen
    real(kind=sp) :: r,g,b
    integer :: ndraw
    COMMON /DRAWTHIS/  ndraw(50)

    if ( jaOpenGL.eq.0 ) then
      InOpenGLRendering = .false.
      return
    end if

    currentWidth  = INFOSCREEN(4) ! npx & npy are not always up-to-date it seems..
    currentHeight = INFOSCREEN(5)

    IF (HWND .le. 0) THEN ! first time only
        CALL InitializeOpenGL()
    END IF

    IF (lastWidth .ne. currentWidth .or. lastHeight .ne. currentHeight) THEN
        CALL ReInitializeBackBuffer()
    END IF

    ! screen coordinates extend
    CALL fglViewPort(0,0,currentWidth,currentHeight)
    CALL fglDisable(GL_DEPTH_TEST) ! no depth

    ! switch to 2d projection (world coordinates)
    CALL fglMatrixMode (GL_PROJECTION)
    CALL fglLoadIdentity()

    CALL fglOrtho (X1, X2, Y1, Y2, 0, 1) ! world coordinates extent
    CALL fglMatrixMode (GL_MODELVIEW)

    ! clear the screen
    ! CALL fglClearColor(.4, .4, .4, 0) ! gray background

    if (ndraw(10) == 0) then
       r = nreds/255d0
       g = ngreens/255d0
       b = nblues/255d0
    else
       r = nredp/255d0
       g = ngreenp/255d0
       b = nbluep/255d0
    endif

    CALL fglClearColor(r,g,b, 0) ! screen background

    CALL fglClear(GL_COLOR_BUFFER_BIT)

    InOpenGLRendering = .true.
#endif
 END SUBROUTINE


 SUBROUTINE EndRender
#ifdef HAVE_OPENGL
    use, intrinsic :: ISO_C_BINDING
    use IFWINA
    use M_DEVICES
    use user32
    implicit none
    logical(c_bool) :: res

    if ( jaOpenGL.eq.0 ) then
      return
    end if

    ! Ignored if single-buffered
    !res = SwapBuffers(hDC) !note: non-blocking
    CALL fglFinish() ! force completion before continuing with non-opengl stuff

    res = BitBlt(HDC,0,0,currentWidth,currentHeight,MEMDC,0,0,SRCCOPY)

    InOpenGLRendering = .false.
#endif
 END SUBROUTINE


 SUBROUTINE ReInitializeBackBuffer
#ifdef HAVE_OPENGL
    use, intrinsic :: ISO_C_BINDING
    use IFWINA ! renamed symbols to avoid conflicts
    use M_DEVICES
    implicit none
    integer(HANDLE) :: ptr_bytes
    logical(c_bool) :: res
    type(T_BITMAPINFO) bmi

    ! We render into a bitmap because not all video cards support mixing gdi / opengl directly. This may mean
    ! we loose hardware acceleration, but even without this it's still significantly faster than rendering
    ! using gdi (interacter).

    if (hbitmap .ne. 0) then
        res = DeleteObject(hbitmap)
    endif

    lastWidth = currentWidth
    lastHeight = currentHeight

    ! create bitmap
    call ZeroMemory(loc(bmi%bmiHeader),sizeof(bmi%bmiHeader))
    bmi%bmiHeader%biSize = sizeof(bmi)
    bmi%bmiHeader%biWidth = currentWidth
    bmi%bmiHeader%biHeight = currentHeight
    bmi%bmiHeader%biPlanes = 1
    bmi%bmiHeader%biBitCount = 32
    bmi%bmiHeader%biCompression = BI_RGB
    bmi%bmiHeader%biSizeImage = 0
    bmi%bmiHeader%biXPelsPerMeter = 0
    bmi%bmiHeader%biYPelsPerMeter = 0
    bmi%bmiHeader%biClrUsed = 0
    bmi%bmiHeader%biClrImportant = 0

    ! bmi%bmicolors(1) = 0

    ptr_bytes = 0

    hBitmap = CreateDIBSection(memDC, bmi, DIB_RGB_COLORS, ptr_bytes, NULL, 0) ! kleur green is -15?

    oldBitmap = SelectObject(memDC, hBitmap)

#endif
 END SUBROUTINE


 SUBROUTINE InitializeOpenGl
#ifdef HAVE_OPENGL
    use, intrinsic :: ISO_C_BINDING
    use IFWINA ! renamed symbols to avoid conflicts
    implicit none
    logical(c_bool) :: res
    integer :: pixelFormat, error_code
    type(T_PixelFormatDescriptor) pfd

    HWND = GetActiveWindow()
    HDC = GetDC(HWND) ! get device context of entire screen
    memDC = CreateCompatibleDC(HDC)

    CALL ReInitializeBackBuffer()

    ! set pixel format
    call ZeroMemory (loc(pfd), sizeof(pfd))
    pfd%nSize = sizeof(pfd)
    pfd%nVersion = 1
    pfd%dwFlags = ior(PFD_DRAW_TO_BITMAP,PFD_SUPPORT_OPENGL)
    pfd%dwFlags = ior(pfd%dwFlags,PFD_SUPPORT_GDI)
    pfd%iPixelType = PFD_TYPE_RGBA
    pfd%cColorBits = 32
    pixelFormat = ChoosePixelFormat (memDC, pfd)
    res = SetPixelFormat (memDC, pixelFormat, pfd)

    ! create render context
    HRC = fwglCreateContext(memDC)
    if (HRC .eq. 0) then
        error_code = GetLastError()
        if (error_code .gt. 0) then
            write (*,*) 'error initialiseopengl:', error_code
        endif
    endif
    res = fwglMakeCurrent(memDC, HRC)

    CALL SetTextHeight(FontSize)

#endif
 END SUBROUTINE

SUBROUTINE SetTextHeight(height)
#ifdef HAVE_OPENGL
 use, intrinsic :: ISO_C_BINDING
 use IFWINA ! renamed symbols to avoid conflicts
#endif
 implicit none

 integer, intent(in) :: height

#ifdef HAVE_OPENGL
 integer(HANDLE) :: font, prevFont
 logical(c_bool) :: res

    ! prepare the font to render text in
    font = CreateFont( height, 0, 0, 0, & ! font size
        FW_NORMAL, & ! bold
        .FALSE., & ! italic
        .FALSE., & ! underline
        .FALSE., & ! strikout
        ANSI_CHARSET, &
        OUT_TT_PRECIS, &
        CLIP_DEFAULT_PRECIS, &
        ANTIALIASED_QUALITY, &
        ior(FF_DONTCARE,DEFAULT_PITCH), &
        'Arial') !font name

!    prevFont = SelectObject (hdc, font)
    res = fwglUseFontBitmaps (hdc, 0, 255, 0) ! create the bitmap display lists, we're making images of glyphs 0 thru 254
!    res = SelectObject(hdc, prevFont) ! select old font again
    res = DeleteObject(font) ! delete temporary font

#endif
 END SUBROUTINE

 SUBROUTINE DeInitializeOpenGl
#ifdef HAVE_OPENGL
    use IFWINA
    use iso_c_binding, only: c_bool
    implicit none
    logical(c_bool) :: res

    res = DeleteObject(hbitmap)
    res = fwglMakeCurrent(NULL,NULL)
    res = fwglDeleteContext (HRC)
    res = DeleteDC(memDC)
    res = ReleaseDC(HWND, HDC)
#endif
 END SUBROUTINE

end module unstruc_opengl


