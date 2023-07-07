#pragma once
#include "keyvaluell.h"

class IXmlTree {
public:
	virtual void AddAttrib( const char * name, const char * value) = 0;

	virtual void AddChild( IXmlTree * child ) = 0;

	virtual void ExpandEnvironmentVariables( void ) = 0;

	virtual void ExpandEnvironmentVariables( int instance ) = 0;

	virtual bool GetBoolAttrib(	const char * name ) = 0;

	virtual long int GetIntegerAttrib( const char * name ) = 0;

	virtual double GetFloatAttrib( const char * name ) = 0;

	virtual IXmlTree * Lookup( const char * pathname ) = 0;

	virtual IXmlTree * Lookup( const char * pathname, int instance ) = 0;

	virtual int Lookup(	const char * pathname,	int instance, keyValueLL * &kvlist ) = 0; // key-value pairs linked list

	virtual const char * GetAttrib(	const char * name ) = 0;

	virtual const char * GetElement( const char * name ) = 0;

	virtual bool GetBoolElement( const char * name,	bool defaultValue ) = 0;

	virtual void Print(	void ) = 0;
};