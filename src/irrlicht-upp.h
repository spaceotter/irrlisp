#include "irrlicht.h"
// These constructors don't normally get instantiated, but are handy for conversion from lisp
// strings
template irr::core::string<char>::string(const char *const c, irr::u32 length);
template irr::core::string<char>::string(const wchar_t *const c, irr::u32 length);
template irr::core::string<wchar_t>::string(const char *const c, irr::u32 length);
template irr::core::string<wchar_t>::string(const wchar_t *const c, irr::u32 length);
