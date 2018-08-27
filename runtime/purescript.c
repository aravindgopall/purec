#include <gc/gc.h>
#include "Block.h"
#include "runtime/purescript.h"

// -----------------------------------------------------------------------------
// managed data: garbage collected data
// -----------------------------------------------------------------------------

const managed_t * managed_new (const void * data, const managed_release_func release) {
	managed_t * managed = purs_new(managed_block_t);
	managed->data = data;
	GC_register_finalizer(
		managed,
		(GC_finalization_proc) release,
		0, 0, 0);
	return managed;
}

// -----------------------------------------------------------------------------
// managed blocks
// -----------------------------------------------------------------------------

void managed_block_release (managed_t * managed) {
	Block_release(managed->data);
}

const managed_block_t * managed_block_new (const void * block) {
	return managed_new(block, managed_block_release);
}

// -----------------------------------------------------------------------------
// managed utf8 strings
// -----------------------------------------------------------------------------
void managed_utf8str_release (managed_t * managed) {
	free((void *) managed->data);
}

const managed_utf8str_t * managed_utf8str_new (const void * data) {
	return managed_new(data, managed_utf8str_release);
}

// -----------------------------------------------------------------------------
// misc
// -----------------------------------------------------------------------------

inline const void * purs_assert_not_null(const void * data, const char * message) {
	char * message_ = afmt(message);
	purs_assertf(data != NULL, message_);
	free (message_);
	return data;
}

// -----------------------------------------------------------------------------
// any: dynamically typed values
// -----------------------------------------------------------------------------

inline const purs_any_t * purs_any_unthunk (const purs_any_t * x) {
	while (x != NULL && x->tag == THUNK) {
		x = x->value.fn(NULL);
	}
	return x;
}

inline const purs_any_tag_t * purs_any_get_tag_maybe (const purs_any_t * x) {
	if      (x->tag == INT)	return &x->tag;
	else if (x->tag == NUMBER)	return &x->tag;
	else if (x->tag == ABS)	return &x->tag;
	else if (x->tag == ABS_BLOCK)	return &x->tag;
	else if (x->tag == CONS)	return &x->tag;
	else if (x->tag == RECORD)	return &x->tag;
	else if (x->tag == STRING)	return &x->tag;
	else if (x->tag == ARRAY)	return &x->tag;
	else if (x->tag == THUNK)	return &x->tag;
	else if (x->tag == FOREIGN)	return &x->tag;
	else return NULL;
}

inline const char * purs_any_tag_str (const purs_any_tag_t tag) {
	static const char * tags[10] =
		{ "INT",
		  "NUMBER",
		  "ABS",
		  "ABS_BLOCK",
		  "CONS",
		  "RECORD",
		  "STRING",
		  "ARRAY",
		  "THUNK",
		  "FOREIGN" };
	return tags[tag];
}

const purs_cons_t * purs_any_get_cons_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == CONS) {
		return & x->value.cons;
	} else {
		return NULL;
	}
}

const abs_t purs_any_get_abs_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == ABS) {
		return (abs_t)(x->value.fn);
	} else {
		return NULL;
	}
}

const int * purs_any_get_int_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == INT) {
		return &x->value.integer;
	} else {
		return NULL;
	}
}

const float * purs_any_get_number_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == NUMBER) {
		return &x->value.number;
	} else {
		return NULL;
	}
}

const managed_block_t * purs_any_get_abs_block_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == ABS_BLOCK) {
		return (managed_block_t *) x->value.block;
	} else {
		return NULL;
	}
}

const managed_utf8str_t * purs_any_get_string_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == STRING) {
		return (const managed_block_t *) x->value.string;
	} else {
		return NULL;
	}
}

const purs_record_t * purs_any_get_record_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == RECORD) {
		return (const purs_record_t *) x->value.record;
	} else {
		return NULL;
	}
}

const purs_vec_t * purs_any_get_array_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == ARRAY) {
		return (const purs_vec_t *) x->value.array;
	} else {
		return NULL;
	}
}

void * purs_any_get_foreign_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == FOREIGN) {
		return x->value.foreign;
	} else {
		return NULL;
	}
}

#define PURS_ANY_GET_IMPL(T, X)\
	T purs_any_get_##X (const purs_any_t * x) {\
		x = purs_any_unthunk(x);\
		purs_assert_not_null(x, "(purs_any_get_" #X ") expected: " #X);\
		char * msg = afmt(\
			"(purs_any_get_" #X ") (got: %s)",\
			purs_any_tag_str(\
				* (const int *) purs_assert_not_null(\
					purs_any_get_tag_maybe(x),\
					"(purs_any_get_" #X ") expected purs_any_t"\
				)\
			)\
		);\
		T r = (T) purs_assert_not_null(\
			purs_any_get_##X##_maybe(x),\
			msg\
		);\
		free(msg);\
		return r;\
	}

PURS_ANY_GET_IMPL(const purs_cons_t *, cons);
PURS_ANY_GET_IMPL(const abs_t, abs);
PURS_ANY_GET_IMPL(const int *, int);
PURS_ANY_GET_IMPL(const float *, number);
PURS_ANY_GET_IMPL(const managed_block_t *, abs_block);
PURS_ANY_GET_IMPL(const managed_utf8str_t *, string);
PURS_ANY_GET_IMPL(const purs_record_t *, record);
PURS_ANY_GET_IMPL(const purs_vec_t *, array);
PURS_ANY_GET_IMPL(void *, foreign);

#define PURS_ANY_INIT_IMPL(NAME, TYPE, TAG, KEY)\
	inline purs_any_t * NAME (purs_any_t * any, TYPE val) {\
		any->tag = TAG;\
		any->value.KEY = val;\
		return any;\
	}

PURS_ANY_INIT_IMPL(purs_any_init_abs, const abs_t, ABS, fn)
PURS_ANY_INIT_IMPL(purs_any_init_abs_block, const managed_block_t *, ABS_BLOCK, block)
PURS_ANY_INIT_IMPL(purs_any_init_number, float, NUMBER, number)
PURS_ANY_INIT_IMPL(purs_any_init_int, int, INT, integer)
PURS_ANY_INIT_IMPL(purs_any_init_cons, purs_cons_t, CONS, cons)
PURS_ANY_INIT_IMPL(purs_any_init_string, const managed_utf8str_t *, STRING, string)
PURS_ANY_INIT_IMPL(purs_any_init_record, const purs_record_t *, RECORD, record)
PURS_ANY_INIT_IMPL(purs_any_init_array, const purs_vec_t *, ARRAY, array)
PURS_ANY_INIT_IMPL(purs_any_init_foreign, void *, FOREIGN, foreign)

// XXX: for convenient emitting only (might be removed)
int purs_cons_get_tag (const purs_cons_t * cons) {
	return cons->tag;
}

inline const purs_any_t * purs_any_app (const purs_any_t * x, const purs_any_t * arg) {
	const void * f;
	const managed_block_t * b;

	x = purs_any_unthunk(x);

	b = purs_any_get_abs_block_maybe(x);
	if (b != NULL) {
		return ((abs_block_t) b->data)(arg);
	}

	f = purs_any_get_abs_maybe(x);
	if (f != NULL) {
		return ((abs_t) f)(arg);
	}

	char * msg = afmt("expected function (got: %s)", purs_any_tag_str(x->tag));
	purs_assertf(0, msg);
	free(msg);
}

int purs_any_eq_string (const purs_any_t * x, const void * str) {
	const managed_utf8str_t * a = purs_any_get_string(x);
	return utf8cmp(a->data, str) == 0;

}

int purs_any_eq_int (const purs_any_t * x, int y) {
	const int * a = purs_any_get_int(x);
	return *a == y;
}

int purs_any_eq_number (const purs_any_t * x, float y) {
	const float * a = purs_any_get_number(x);
	return *a == y;
}

/**
 Concatenate two dyanmic values into a new dynamic value
*/
const purs_any_t * purs_any_concat(const purs_any_t * x, const purs_any_t * y) {
	x = purs_any_unthunk(x);
	y = purs_any_unthunk(y);

	if (x->tag != y->tag) {
		char * msg = afmt("cannot concat %s with %s",
				  purs_any_tag_str(x->tag),
				  purs_any_tag_str(y->tag));
		purs_assertf(0, msg);
		free(msg);
	} else {
		switch(x->tag) {
		/* XXX this will falsly capture chars */
		case STRING: {
			const managed_utf8str_t * x_utf8str = purs_any_get_string(x);
			const managed_utf8str_t * y_utf8str = purs_any_get_string(y);
			return PURS_ANY_STRING(afmt("%s%s",
						    x_utf8str->data,
						    y_utf8str->data));
		}
		case ARRAY: {
			const purs_vec_t * x_vec = purs_any_get_array(x);
			const purs_vec_t * y_vec = purs_any_get_array(y);
			if (x_vec->length == 0) {
				return y;
			} else if (y_vec->length == 0) {
				return x;
			} else {
				printf("x before: %d %d (%p)\n", x_vec->length, x_vec->capacity, x_vec->data);
				printf("y before: %d %d (%p)\n", y_vec->length, y_vec->capacity, y_vec->data);
				purs_vec_t * out_vec = (purs_vec_t *) purs_vec_copy(x_vec);
				vec_pusharr(out_vec, y_vec->data, y_vec->length);
				printf("x after: %d %d (%p)\n", x_vec->length, x_vec->capacity, x_vec->data);
				printf("y after: %d %d (%p)\n", y_vec->length, y_vec->capacity, y_vec->data);
				return PURS_ANY_ARRAY((const purs_vec_t *) out_vec);
			}
		}
		default: {
			char * msg = afmt("cannot concat %s",
					purs_any_tag_str(x->tag));
			purs_assertf(0, msg);
			free(msg);
		}
		}
	}
}

// -----------------------------------------------------------------------------
// strings (via managed_utf8str_t)
// -----------------------------------------------------------------------------

const void * purs_string_copy (const void * source) {
	size_t sz = utf8size(source);
	void * dest = purs_malloc(sz);
	memcpy(dest, source, sz);
	return (const void*) dest;
}

// -----------------------------------------------------------------------------
// arrays (via vectors)
// -----------------------------------------------------------------------------

void purs_vec_release (purs_vec_t * vec) {
	vec_deinit(vec);
}

const purs_vec_t * purs_vec_new (const purs_any_t ** items, int count) {
	purs_vec_t * v = purs_new(purs_vec_t);
	GC_register_finalizer(v,
			      (GC_finalization_proc) purs_vec_release,
			      0, 0, 0);
	vec_init(v);
	vec_pusharr(v, items, count);
	return (const purs_vec_t *) v;
}

const purs_vec_t * purs_vec_new_va (int count, ...) {
	int i;
	const purs_any_t ** xs = malloc(sizeof (purs_any_t *) * count);
	va_list args;
	va_start(args, count);
	for (i = 0; i < count; i++) {
		xs[i] = va_arg(args, const purs_any_t *);
	}
	const purs_vec_t * o = purs_vec_new(xs, count);
	free(xs);
	return o;
}

const purs_vec_t * purs_vec_copy (const purs_vec_t * vec) {
	if (vec->data == NULL) {
		return (purs_vec_t *) purs_vec_new(NULL, 0);
	} else {
		purs_vec_t * copy = (purs_vec_t *) purs_vec_new(NULL, 0);
		copy->length = vec->length;
		copy->capacity = vec->capacity;
		vec_expand_((char**)&copy->data,
			    (int *)&copy->length,
			    (int *)&copy->capacity,
			    sizeof(*copy->data));
		memcpy(copy, vec, sizeof *copy);
		memcpy(copy->data,
		    vec->data,
		    sizeof (*copy->data) * vec->capacity);
		return (const purs_vec_t *) copy;
	}
}

const purs_vec_t * purs_vec_insert(const purs_vec_t * vec,
				   int idx,
				   const purs_any_t * val) {
	if (vec == NULL) {
		return purs_vec_new_va(1, val);
	} else {
		purs_vec_t * out = (purs_vec_t *) purs_vec_copy(vec);
		vec_insert(out, idx, val);
		return (const purs_vec_t *) out;
	}
}

// -----------------------------------------------------------------------------
// records (via hash tables)
// -----------------------------------------------------------------------------

PURS_ANY_THUNK_DEF(purs_record_empty, PURS_ANY_RECORD(NULL));

const purs_record_t * purs_record_copy_shallow(const purs_record_t * source) {
	const purs_record_t * current_entry, * tmp;
	purs_record_t * entry_copy;
	purs_record_t * record = NULL;
	HASH_ITER(hh, source, current_entry, tmp) {
		entry_copy = purs_new(purs_record_t);
		memcpy(entry_copy, current_entry, sizeof(purs_record_t));
		HASH_ADD_KEYPTR(
			hh,
			record,
			entry_copy->key->data,
			utf8size(entry_copy->key->data),
			entry_copy
		);
	}
	return (const purs_record_t *) record;
}

const purs_record_t * purs_record_add_multi(const purs_record_t * source, size_t count, ...) {
	if (count == 0) {
		return source;
	}

	purs_record_t * copy = (purs_record_t *) purs_record_copy_shallow(source);

	va_list args;
	va_start(args, count);

	for (size_t i = 0; i < count; i++) {
		const void * key = va_arg(args, const void *);
		const purs_any_t * value = va_arg(args, const purs_any_t *);
		purs_record_t * entry = purs_new(purs_record_t);
		entry->key = managed_utf8str_new(key);
		entry->value = value;
		HASH_ADD_KEYPTR(
			hh,
			copy,
			entry->key->data,
			utf8size(entry->key->data),
			entry
		);
	}

	va_end(args);

	return (const purs_record_t *) copy;
}

const purs_record_t * purs_record_remove(const purs_record_t * source,
					 const void * key) {
	purs_record_t * copy = (purs_record_t *) purs_record_copy_shallow(source);
	purs_record_t * v = (purs_record_t *) purs_record_find_by_key(source, key);
	if (v != NULL) {
		HASH_DEL(copy, (purs_record_t *) v);
	}
	return (const purs_record_t *) copy;
}

const purs_record_t * purs_record_find_by_key(const purs_record_t * record,
					      const void * key) {
	purs_record_t * result;
	size_t len = utf8size(key);
	HASH_FIND(hh, record, key, len, result);
	return result;
}

// -----------------------------------------------------------------------------
// Built-ins
// -----------------------------------------------------------------------------

PURS_ANY_THUNK_DEF(purs_any_true, PURS_ANY_INT(1));
PURS_ANY_THUNK_DEF(purs_any_false, PURS_ANY_INT(0));

const purs_any_t * purs_any_eq(const purs_any_t * x, const purs_any_t * y) {
	x = purs_any_unthunk(x);
	y = purs_any_unthunk(y);

	if (x == y) {
		return purs_any_true;
	} else if (x == NULL || y == NULL) {
		return purs_any_false;
	} else if (x->tag == y->tag) {
		switch (x->tag) {
		case INT:
			if (*purs_any_get_int(x) == *purs_any_get_int(y)) {
				return purs_any_true;
			} else {
				return purs_any_false;
			}
		case NUMBER:
			if (*purs_any_get_number(x) == *purs_any_get_number(y)) {
				return purs_any_true;
			} else {
				return purs_any_false;
			}
		case STRING:
			if (utf8cmp(purs_any_get_string(x)->data,
				    purs_any_get_string(y)->data) == 0) {
				return purs_any_true;
			} else {
				return purs_any_false;
			}
		default:
			return purs_any_false;
		}
	} else {
		return purs_any_false;
	}
}
