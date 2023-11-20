#pragma once
#include "imageloader.h"
#include <iostream>
#include <string>

class CFileImageLoader :
	public CImageLoader
{
protected:
	std::basic_string <TCHAR> m_strFileName;
	virtual IWICBitmapSource *LoadImage() ;
public:
	CFileImageLoader(LPCTSTR lpszFileName);
	~CFileImageLoader(void);
};
