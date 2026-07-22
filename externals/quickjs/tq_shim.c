/*
 * tq_shim.c - ABI-safe C shim over quickjs-ng for Free Pascal.
 *
 * quickjs-ng's JSValue is a 16-byte struct on 64-bit targets. Passing or
 * returning that by value across an FPC <-> C cdecl boundary depends on
 * per-platform struct classification rules (SysV splits it across two
 * registers; Win64 returns it via a hidden pointer). Rather than rely on
 * FPC and GCC agreeing, every JSValue crosses this boundary by pointer:
 *
 *   - a JSValue argument   becomes  const JSValue *
 *   - a JSValue result     becomes  a leading  JSValue *out  parameter
 *
 * Callbacks run the same hazard in reverse: quickjs calls a function that
 * must RETURN a JSValue struct. Pascal never does that here. Instead a
 * single C trampoline receives the call and forwards it to one Pascal
 * dispatch routine, passing the result slot as an out-pointer. quickjs's
 * `magic` int selects which registered Pascal callback to run.
 */

#include <stdint.h>
#include <string.h>
#include "quickjs.h"

#ifdef _WIN32
#define TQ_API __declspec(dllexport)
#else
#define TQ_API __attribute__((visibility("default")))
#endif

/* ------------------------------------------------------------------ */
/* Sanity: let the Pascal side verify the layout it was compiled for.  */
/* ------------------------------------------------------------------ */

TQ_API int tq_sizeof_jsvalue(void) { return (int)sizeof(JSValue); }
TQ_API int tq_abi_version(void)    { return 1; }

/* ------------------------------------------------------------------ */
/* Value construction                                                  */
/* ------------------------------------------------------------------ */

TQ_API void tq_undefined(JSValue *out)     { *out = JS_UNDEFINED; }
TQ_API void tq_null(JSValue *out)          { *out = JS_NULL; }
TQ_API void tq_true(JSValue *out)          { *out = JS_TRUE; }
TQ_API void tq_false(JSValue *out)         { *out = JS_FALSE; }
TQ_API void tq_exception(JSValue *out)     { *out = JS_EXCEPTION; }
TQ_API void tq_uninitialized(JSValue *out) { *out = JS_UNINITIALIZED; }

TQ_API void tq_new_bool(JSValue *out, JSContext *ctx, int v)      { *out = JS_NewBool(ctx, v != 0); }
TQ_API void tq_new_int32(JSValue *out, JSContext *ctx, int32_t v) { *out = JS_NewInt32(ctx, v); }
TQ_API void tq_new_int64(JSValue *out, JSContext *ctx, int64_t v) { *out = JS_NewInt64(ctx, v); }
TQ_API void tq_new_float64(JSValue *out, JSContext *ctx, double v){ *out = JS_NewFloat64(ctx, v); }

TQ_API void tq_new_bigint64(JSValue *out, JSContext *ctx, int64_t v) { *out = JS_NewBigInt64(ctx, v); }

/* JS_NewString is a static inline in the header - not exported. Route to the
   real symbol so the Pascal side has something to link against. */
TQ_API void tq_new_string(JSValue *out, JSContext *ctx, const char *s)
{
    *out = JS_NewStringLen(ctx, s, s ? strlen(s) : 0);
}
TQ_API void tq_new_string_len(JSValue *out, JSContext *ctx, const char *s, size_t len)
{
    *out = JS_NewStringLen(ctx, s, len);
}

TQ_API void tq_new_object(JSValue *out, JSContext *ctx) { *out = JS_NewObject(ctx); }
TQ_API void tq_new_array(JSValue *out, JSContext *ctx)  { *out = JS_NewArray(ctx); }
TQ_API void tq_new_object_class(JSValue *out, JSContext *ctx, JSClassID id)
{
    *out = JS_NewObjectClass(ctx, id);
}

/* ------------------------------------------------------------------ */
/* Value inspection                                                    */
/* ------------------------------------------------------------------ */

TQ_API int     tq_tag(const JSValue *v)         { return JS_VALUE_GET_TAG(*v); }
TQ_API int     tq_get_int(const JSValue *v)     { return JS_VALUE_GET_INT(*v); }
TQ_API int     tq_get_bool(const JSValue *v)    { return JS_VALUE_GET_BOOL(*v); }
TQ_API double  tq_get_float64(const JSValue *v) { return JS_VALUE_GET_FLOAT64(*v); }
TQ_API void   *tq_get_ptr(const JSValue *v)     { return JS_VALUE_GET_PTR(*v); }

TQ_API int tq_is_undefined(const JSValue *v) { return JS_IsUndefined(*v) ? 1 : 0; }
TQ_API int tq_is_null(const JSValue *v)      { return JS_IsNull(*v)      ? 1 : 0; }
TQ_API int tq_is_exception(const JSValue *v) { return JS_IsException(*v) ? 1 : 0; }
TQ_API int tq_is_bool(const JSValue *v)      { return JS_IsBool(*v)      ? 1 : 0; }
TQ_API int tq_is_number(const JSValue *v)    { return JS_IsNumber(*v)    ? 1 : 0; }
TQ_API int tq_is_string(const JSValue *v)    { return JS_IsString(*v)    ? 1 : 0; }
TQ_API int tq_is_object(const JSValue *v)    { return JS_IsObject(*v)    ? 1 : 0; }
TQ_API int tq_is_array(const JSValue *v)     { return JS_IsArray(*v)     ? 1 : 0; }
TQ_API int tq_is_error(const JSValue *v)     { return JS_IsError(*v)     ? 1 : 0; }
TQ_API int tq_is_function(JSContext *ctx, const JSValue *v) { return JS_IsFunction(ctx, *v) ? 1 : 0; }

/* ------------------------------------------------------------------ */
/* Lifetime                                                            */
/* ------------------------------------------------------------------ */

TQ_API void tq_free_value(JSContext *ctx, const JSValue *v)   { JS_FreeValue(ctx, *v); }
TQ_API void tq_free_value_rt(JSRuntime *rt, const JSValue *v) { JS_FreeValueRT(rt, *v); }
TQ_API void tq_dup_value(JSValue *out, JSContext *ctx, const JSValue *v) { *out = JS_DupValue(ctx, *v); }

/* ------------------------------------------------------------------ */
/* Properties                                                          */
/* ------------------------------------------------------------------ */

TQ_API void tq_get_property_str(JSValue *out, JSContext *ctx, const JSValue *obj, const char *prop)
{
    *out = JS_GetPropertyStr(ctx, *obj, prop);
}
TQ_API void tq_get_property_uint32(JSValue *out, JSContext *ctx, const JSValue *obj, uint32_t idx)
{
    *out = JS_GetPropertyUint32(ctx, *obj, idx);
}
/* NOTE: val is consumed (moved) by quickjs, matching JS_SetPropertyStr semantics. */
TQ_API int tq_set_property_str(JSContext *ctx, const JSValue *obj, const char *prop, const JSValue *val)
{
    return JS_SetPropertyStr(ctx, *obj, prop, *val);
}
TQ_API int tq_set_property_uint32(JSContext *ctx, const JSValue *obj, uint32_t idx, const JSValue *val)
{
    return JS_SetPropertyUint32(ctx, *obj, idx, *val);
}
TQ_API int tq_get_own_property_names(JSContext *ctx, JSPropertyEnum **ptab, uint32_t *plen,
                                     const JSValue *obj, int flags)
{
    return JS_GetOwnPropertyNames(ctx, ptab, plen, *obj, flags);
}
TQ_API int tq_set_opaque(const JSValue *obj, void *opaque) { return JS_SetOpaque(*obj, opaque); }

/* ------------------------------------------------------------------ */
/* Conversion                                                          */
/* ------------------------------------------------------------------ */

TQ_API const char *tq_to_cstring(JSContext *ctx, const JSValue *v)
{
    return JS_ToCStringLen2(ctx, NULL, *v, false);
}
TQ_API const char *tq_atom_to_cstring(JSContext *ctx, JSAtom atom)
{
    return JS_AtomToCStringLen(ctx, NULL, atom);
}
TQ_API int tq_to_bool(JSContext *ctx, const JSValue *v)             { return JS_ToBool(ctx, *v); }
TQ_API int tq_to_int32(JSContext *ctx, int32_t *pres, const JSValue *v) { return JS_ToInt32(ctx, pres, *v); }
TQ_API int tq_to_int64(JSContext *ctx, int64_t *pres, const JSValue *v) { return JS_ToInt64(ctx, pres, *v); }
TQ_API int tq_to_float64(JSContext *ctx, double *pres, const JSValue *v){ return JS_ToFloat64(ctx, pres, *v); }

/* ------------------------------------------------------------------ */
/* Eval / call                                                         */
/* ------------------------------------------------------------------ */

TQ_API void tq_eval(JSValue *out, JSContext *ctx, const char *input, size_t len,
                    const char *filename, int flags)
{
    *out = JS_Eval(ctx, input, len, filename, flags);
}
TQ_API void tq_call(JSValue *out, JSContext *ctx, const JSValue *func,
                    const JSValue *this_obj, int argc, JSValue *argv)
{
    *out = JS_Call(ctx, *func, *this_obj, argc, argv);
}
TQ_API void tq_get_exception(JSValue *out, JSContext *ctx)     { *out = JS_GetException(ctx); }
TQ_API void tq_get_global_object(JSValue *out, JSContext *ctx) { *out = JS_GetGlobalObject(ctx); }
TQ_API void tq_new_promise_capability(JSValue *out, JSContext *ctx, JSValue *resolving_funcs)
{
    *out = JS_NewPromiseCapability(ctx, resolving_funcs);
}

/* ------------------------------------------------------------------ */
/* Callback trampolines                                                */
/* ------------------------------------------------------------------ */

/*
 * The Pascal side registers exactly one dispatch routine. `magic` identifies
 * which of its registered callbacks to invoke - Trndi already keeps a named
 * registry, so magic is just that registry's index.
 *
 * func_data is passed through as a raw pointer to the JSValue array plus its
 * length, so the Pascal side can read it without any struct crossing the ABI.
 */
typedef void (*tq_dispatch_fn)(JSValue *out, JSContext *ctx, const JSValue *this_val,
                               int argc, JSValue *argv, int magic,
                               JSValue *func_data, int func_data_len);

static tq_dispatch_fn g_dispatch = NULL;

TQ_API void tq_set_dispatch(tq_dispatch_fn fn) { g_dispatch = fn; }

static JSValue tq_tramp_data(JSContext *ctx, JSValueConst this_val, int argc,
                             JSValueConst *argv, int magic, JSValue *func_data)
{
    JSValue out = JS_UNDEFINED;
    if (g_dispatch)
        g_dispatch(&out, ctx, &this_val, argc, argv, magic, func_data, 1);
    return out;
}

static JSValue tq_tramp_magic(JSContext *ctx, JSValueConst this_val, int argc,
                              JSValueConst *argv, int magic)
{
    JSValue out = JS_UNDEFINED;
    if (g_dispatch)
        g_dispatch(&out, ctx, &this_val, argc, argv, magic, NULL, 0);
    return out;
}

/* Register a plain JS-callable function; `magic` selects the Pascal callback. */
TQ_API void tq_new_function(JSValue *out, JSContext *ctx, const char *name,
                            int length, int magic)
{
    *out = JS_NewCFunction2(ctx, (JSCFunction *)tq_tramp_magic, name, length,
                            JS_CFUNC_generic_magic, magic);
}

/* Register a constructor (used for the Trndi class). */
TQ_API void tq_new_constructor(JSValue *out, JSContext *ctx, const char *name,
                               int length, int magic)
{
    *out = JS_NewCFunction2(ctx, (JSCFunction *)tq_tramp_magic, name, length,
                            JS_CFUNC_constructor_magic, magic);
}

/* Register a function carrying bound data (used for async/promise plumbing). */
TQ_API void tq_new_function_data(JSValue *out, JSContext *ctx, int length, int magic,
                                 int data_len, JSValue *data)
{
    *out = JS_NewCFunctionData(ctx, tq_tramp_data, length, magic, data_len, data);
}

/* ------------------------------------------------------------------ */
/* Promise rejection tracker                                           */
/* ------------------------------------------------------------------ */

typedef void (*tq_rejection_fn)(JSContext *ctx, const JSValue *promise,
                                const JSValue *reason, int is_handled, void *opaque);

static tq_rejection_fn g_rejection = NULL;

static void tq_rejection_thunk(JSContext *ctx, JSValueConst promise,
                               JSValueConst reason, bool is_handled, void *opaque)
{
    if (g_rejection)
        g_rejection(ctx, &promise, &reason, is_handled ? 1 : 0, opaque);
}

TQ_API void tq_set_rejection_tracker(JSRuntime *rt, tq_rejection_fn fn, void *opaque)
{
    g_rejection = fn;
    JS_SetHostPromiseRejectionTracker(rt, fn ? tq_rejection_thunk : NULL, opaque);
}
