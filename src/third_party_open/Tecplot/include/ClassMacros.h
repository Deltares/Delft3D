#ifndef CLASSMACROS_H
#define CLASSMACROS_H

/**
 * Convenience macro to declare that a class is not copyable.
 */

// (jenya) 2011/12/11: Undefing another definition of UNCOPYABLE_CLASS from toolbox.
// Proper fix is to remove Uncopable.h from toolbox.
#undef UNCOPYABLE_CLASS

#define UNCOPYABLE_CLASS(CLASS_NAME) \
    private:\
      CLASS_NAME(const CLASS_NAME &);\
      CLASS_NAME & operator = (const CLASS_NAME &)

#endif
