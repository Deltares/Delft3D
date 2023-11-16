#pragma once
#include "imageloader.h"
#include <iostream>
#include <string>

class CResourceImageLoader :
	public CImageLoader
{
private:
	LPCTSTR m_strName;
	std::basic_string <TCHAR> m_strType;
protected:
	IStream *CreateStreamOnResource(LPCTSTR lpName, LPCTSTR lpType);
	IWICBitmapSource *LoadBitmapFromStream(IStream * ipImageStream);
	virtual IWICBitmapSource *LoadImage() ;
public:
	CResourceImageLoader(LPCTSTR lpName, LPCTSTR lpType);
	~CResourceImageLoader(void);
};
