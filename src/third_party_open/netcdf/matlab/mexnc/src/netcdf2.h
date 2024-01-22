
# define		calloc(A, B)	mxCalloc(A, B)
# define		free(A)			mxFree(A)

# if defined	__STDC__
# if !defined	HAS_VOID
# define		HAS_VOID
# endif
# endif

# if defined	HAS_VOID
# define		VOID	void
# define		VOIDP	void *
# define		VOIDPP	void **
# else
# define		VOID
# define		VOIDP	char *
# define		VOIDPP	char **
# endif

# if !defined	DOUBLE
# define		DOUBLE	double
# define		INT		int
# endif

# if !defined	VERBOSE
# define		VERBOSE		0	/*	Verbosity.	*/
# endif

# if !defined	MEXCDF_4
# define		MEXCDF_4				0
# define		MEXCDF_5				1
# define        Matrix                  mxArray
# define        COMPLEX                 mxCOMPLEX
# define        REAL                    mxREAL
# define        INT                     int
# else
# define		MEXCDF_5				0
# endif

# define		MAX_BUFFER	32
