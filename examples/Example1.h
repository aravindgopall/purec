#ifndef Example1_FFI_H
#define Example1_FFI_H

#include "runtime/purescript.h"

PURS_FFI_FUNC_1(Example1_runGC, _, {
	GC_gcollect();
	return NULL;
})

PURS_FFI_FUNC_2(Example1_xpure, _a, _, {
	return _a;
})

PURS_FFI_FUNC_3(Example1_xdiscard, x, f, _, {
	const purs_any_t * _ = purs_any_app(x, NULL);
	const purs_any_t * y = purs_any_app(purs_any_app(f, NULL), NULL);
	return y;
})

PURS_FFI_FUNC_3(Example1_xbind, _f, _k, _, {
	const purs_any_t * x = purs_any_app(_f, NULL);
	const purs_any_t * y = purs_any_app(purs_any_app(_k, x), NULL);
	return y;
})

#endif // Example1_FFI_H
