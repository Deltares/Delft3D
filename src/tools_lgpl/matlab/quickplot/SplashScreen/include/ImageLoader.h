#pragma once
#include "wincodec.h"

class CImageLoader
{
protected:
	HBITMAP CreateHBITMAP(IWICBitmapSource * ipBitmap);
	IWICBitmapSource *LoadImageFromDecoder( IWICBitmapDecoder * ipDecoder) ;
	virtual IWICBitmapSource *LoadImage()=0;

public:
	CImageLoader(void);
	~CImageLoader(void);

	
	HBITMAP LoadSplashImage();
	
};
