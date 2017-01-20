/* Copyright (C) 2013, 2014, 2015, 2016, 2017 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * GetData is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include "php_getdata.h"
#include "php_ini.h"

/* corresponding type of the PHP integer */
#if SIZEOF_LONG == 4
#define GDPHP_LONG_TYPE GD_INT32
typedef int32_t gdphp_long_t;
#elif SIZEOF_LONG == 8
#define GDPHP_LONG_TYPE GD_INT64
typedef int64_t gdphp_long_t;
#endif

/* PHP7 malarkey */
#if ZEND_MODULE_API_NO >= 20151012
/* PHP7 */

/* Resources are now bona fide zvals */
#define GDPHP_RESOURCE zend_resource
#define GDPHP_RET_RES(r,le) RETVAL_RES(zend_register_resource((r),(le)))
#define gdphp_fetch_resource(z) zend_fetch_resource2_ex((z), "Dirfile", \
    le_gdphp_dirfile, le_gdphp_dirfile_persist)
#define gdphp_list_close(z) zend_list_close(Z_RES_P(z))

/* Strings now always duplicate */
#define gdphp_add_index_string add_index_string
#define GDPHP_RET_STRL_COPY RETURN_STRINGL
#define GDPHP_RET_STR_COPY(s) GDPHP_RET_STRL_COPY((s),strlen(s))
#define GDPHP_STRINGL ZVAL_STRINGL
#define GDPHP_COPY_DATA(z,s,l,d) do { \
  ZVAL_STRINGL((z),(s),(l)); \
  if (!(d)) efree(s); \
} while(0)
#define GDPHP_RETVAL_STRING RETVAL_STRING
#define gdphp_add_assoc_string add_assoc_string

/* Hashes basically suck */
#define GDPHP_HASH_FINDL(v,ht,key,len) \
  (v = zend_hash_str_find((ht), (key), len))
#define GDPHP_HASH_ADD(ht,key,len,data) \
  zend_hash_str_update_mem((ht), (key), (len), &(data), sizeof(data))
#define GDPHP_FIND_RES(v,ht,key,len) \
  (v = zend_hash_str_find_ptr((ht), (key), len))
#define GDPHP_HASH_INDEX_FIND(v,ht,h) (v = zend_hash_index_find((ht),(h)))
#define GDPHP_HASH_KEY_DECL(n) zend_string* n
#define GDPHP_HASH_GET_CURRENT_KEY(ht,key,idx,pos) \
  zend_hash_get_current_key_ex(ht, &key, &idx, &pos)
#define GDPHP_HASH_GET_CURRENT_DATA(v,ht,pos) \
  (v = zend_hash_get_current_data_ex(ht, &pos))
#define GDPHP_HASH_FIND(v,ht,key) GDPHP_HASH_FINDL((v),(ht),(key),strlen(key))

/* Booleans are gone */
#define Z_BVAL_P(z) ((Z_TYPE_P(z) == IS_TRUE) ? 1 : 0)
#define BOOL_CASE /**/

/* Most things that used to use zval** now just uses zval* */
#define GDPHP_ZVALP zval*
#define ZP(z) z
#define ZR(z) z

/* We now can allocate zvals directly on the stack */
#define GDPHP_ZVAL zval
#define GDPHP_ZVAL_NULL(z) zval z
#define GDPHP_INIT_ZVAL(z) /**/
#define RZ(z) &z
#define GDPHP_PTR_DTOR(z) /**/

/* Changes to zend_parse_parameters */
#define GDPHP_LONG zend_long
#define GDPHP_SLEN size_t
#define ZPP_Z "z"

#else
/* PHP5 */

#define gdphp_fetch_resource(z) zend_fetch_resource(&(z) TSRMLS_CC, -1, \
    "Dirfile", NULL, 2, le_gdphp_dirfile, le_gdphp_dirfile_persist)
#define gdphp_add_index_string(a,i,s) add_index_string((a),(i),(s),1)
#define GDPHP_RET_STRL_COPY(s,l) RETURN_STRINGL((s), (l), 1)
#define GDPHP_RET_STR_COPY(s) RETURN_STRING((s), 1)
#define GDPHP_RET_RES(r,le) ZEND_REGISTER_RESOURCE(return_value, (r), (le))
#define GDPHP_HASH_FINDL(v,ht,key,len) \
  (zend_hash_find((ht), (key), (len), (void**)&(v)) == SUCCESS)
#define GDPHP_RESOURCE zend_rsrc_list_entry
#define GDPHP_HASH_ADD(ht,key,len,data) \
  zend_hash_add((ht), (key), (len), &(data), sizeof(data), NULL)
#define GDPHP_FIND_RES GDPHP_HASH_FINDL
#define GDPHP_RETVAL_STRING(s) RETVAL_STRING(s, 1)
#define gdphp_list_close(z) zend_list_delete(Z_LVAL_P(z))
#define GDPHP_STRINGL(z,s,l) ZVAL_STRINGL((z),(s),(l),1)
#define GDPHP_COPY_DATA(z,s,l,d) do { \
  if (!d) { /* trim excess */ \
    void *ptr = erealloc((s), (l)); \
    if (ptr) data = ptr; \
  } \
  ZVAL_STRINGL((z),(s),(l),1); \
} while(0);
#define GDPHP_ZVALP zval**
#define ZP(z) *z
#define ZR(z) &z
#define GDPHP_HASH_INDEX_FIND(v,ht,h) \
  (zend_hash_index_find((ht),(h), (void**)&(v)) == SUCCESS)
#define GDPHP_HASH_KEY_DECL(n) char*n; unsigned int n ## _len
#define GDPHP_HASH_GET_CURRENT_KEY(ht,key,idx,pos) \
  zend_hash_get_current_key_ex(ht, &key, &key ## _len, &idx, 0, &pos)
#define GDPHP_HASH_GET_CURRENT_DATA(v,ht,pos) \
  (zend_hash_get_current_data_ex(ht, (void*)&v, &pos) == SUCCESS)
#define BOOL_CASE case IS_BOOL:
#define gdphp_add_assoc_string(a,k,s) add_assoc_string((a),(k),(s),1)
#define GDPHP_HASH_FIND(v,ht,key) GDPHP_HASH_FINDL((v),(ht),(key),strlen(key)+1)

#define GDPHP_ZVAL zval*
#define GDPHP_ZVAL_NULL(z) zval* z = NULL
#define GDPHP_INIT_ZVAL ALLOC_INIT_ZVAL
#define RZ(z) z
#define GDPHP_PTR_DTOR(z) zval_ptr_dtor(&z)

#define GDPHP_LONG long
#define GDPHP_SLEN int
#define ZPP_Z "Z"

#endif


/* PHP globals */
ZEND_BEGIN_MODULE_GLOBALS(getdata)
  zend_bool degrade_complex;
  zend_bool unpack;
ZEND_END_MODULE_GLOBALS(getdata)

ZEND_DECLARE_MODULE_GLOBALS(getdata)

#ifdef ZTS
# define GDPHP_G(v) TSRMG(getdata_globals_id, zend_getdata_globals *, v)
# define dtracetsrm(fmt, ...)  dtrace(fmt ", %p", __VA_ARGS__, tsrm_ls)
#else
# define GDPHP_G(v) (getdata_globals.v)
# define dtracetsrm dtrace
#endif

/* PHP INI entries */
PHP_INI_BEGIN()
  STD_PHP_INI_ENTRY("getdata.degrade_complex", "true", PHP_INI_ALL,
      OnUpdateBool, degrade_complex, zend_getdata_globals, getdata_globals)
  STD_PHP_INI_ENTRY("getdata.unpack", "false", PHP_INI_ALL, OnUpdateBool,
      unpack, zend_getdata_globals, getdata_globals)
PHP_INI_END()

/* Common idioms */

#define GDPHP_FETCH_DIRFILE(r, z) do { \
  r = (gdphp_dirfile*)gdphp_fetch_resource(z); \
  if (!r) { GDPHP_RETURN_F; } \
} while(0)

#define GDPHP_RETURN_T do { dreturn("%s", "TRUE");  RETURN_TRUE; } while(0)
#define GDPHP_RETURN_F do { dreturn("%s", "FALSE"); RETURN_FALSE; } while(0)
#define GDPHP_CHECK_ERROR(D) do { if (gd_error(D)) GDPHP_RETURN_F; } while(0)
#define GDPHP_RETURN_ERROR(D) do { \
  if (gd_error(D)) GDPHP_RETURN_F; \
  else GDPHP_RETURN_T; \
} while(0) 
#define GDPHP_RETURN_BOOL(e) do { \
  if (e) GDPHP_RETURN_F; else GDPHP_RETURN_T; } while (0)

#define GDPHP_PARSE(...) do { \
  if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, __VA_ARGS__) \
      != SUCCESS) { \
    GDPHP_RETURN_F; \
  } \
} while(0)

#define GDPHP_PARSED(vars, ...) do { \
  zval *gdphp_dirfile_zval; \
  gdphp_dirfile *gdphp_dirfile_rsrc; \
  GDPHP_PARSE("r" vars, &gdphp_dirfile_zval, __VA_ARGS__); \
  GDPHP_FETCH_DIRFILE(gdphp_dirfile_rsrc, gdphp_dirfile_zval); \
  D = gdphp_dirfile_rsrc->D; \
} while(0)

#define GDPHP_PARSED_ONLY() do { \
  zval *gdphp_dirfile_zval; \
  gdphp_dirfile *gdphp_dirfile_rsrc; \
  GDPHP_PARSE("r", &gdphp_dirfile_zval); \
  GDPHP_FETCH_DIRFILE(gdphp_dirfile_rsrc, gdphp_dirfile_zval); \
  D = gdphp_dirfile_rsrc->D; \
} while(0)

#define dtracephp() dtrace("..., %p, ...", return_value);

struct gdphp_din {
  void *data;
  gd_type_t type;
  int free_din;
  size_t ns;
};

/* error reporting */
struct gdphp_context_t {
  int p;
  const char *k;
  int i;
};

#define dtracectx(fmt, ...) dtrace(fmt " {%i,%s,%i}", __VA_ARGS__, ctx->p, \
    ctx->k ? ctx->k : "-", ctx->i)
#define GDPHP_CONTEXT(v) struct gdphp_context_t v = { 0, NULL, -1 };
#define GDPHP_CONTEXTp(v,p) struct gdphp_context_t v = { p, NULL, -1 };
#define GDPHP_DIE_BUFFER_LEN 1000
#define GDPHP_DIE(x,fmt) do { \
  char gdphp_die_buffer[GDPHP_DIE_BUFFER_LEN]; \
  zend_error(E_ERROR,fmt " in %s", gdphp_context(gdphp_die_buffer,x)); \
} while (0)
#define GDPHP_DIE2(x,fmt,...) do { \
  char gdphp_die_buffer[GDPHP_DIE_BUFFER_LEN]; \
  zend_error(E_ERROR,fmt " in %s", __VA_ARGS__, \
      gdphp_context(gdphp_die_buffer,x)); \
} while (0)

/* report the context (for error messages) */
static char *gdphp_context(char *buffer, const struct gdphp_context_t *ctx)
{
  dtrace("%p, %p", buffer, ctx);

  if (ctx->k) {
    if (ctx->i >= 0)
      snprintf(buffer, GDPHP_DIE_BUFFER_LEN,
          "element %i of '%s' of parameter %i", ctx->i, ctx->k, ctx->p + 1);
    else 
      snprintf(buffer, GDPHP_DIE_BUFFER_LEN, "element '%s' of parameter %i",
          ctx->k, ctx->p + 1);
  } else if (ctx->i >= 0)
    snprintf(buffer, GDPHP_DIE_BUFFER_LEN, "element %i of parameter %i", ctx->i,
        ctx->p + 1);
  else
    snprintf(buffer, GDPHP_DIE_BUFFER_LEN, "parameter %i", ctx->p + 1);

  dreturn("\"%s\"", buffer);
  return buffer;
}

/* the dirfile resource */
typedef struct gdphp_dirfile_ {
  DIRFILE *D;
  char *callback;
  int callback_len;
  GDPHP_ZVALP callback_data;
  char *key;
  int key_len;

  /* Persistent resource stuff */
  int persist;
} gdphp_dirfile;

int le_gdphp_dirfile;
int le_gdphp_dirfile_persist;

static void gdphp_dirfile_dtor(GDPHP_RESOURCE *z TSRMLS_DC)
{
  dtracetsrm("%p", z);

  gdphp_dirfile *r = (gdphp_dirfile*)(z->ptr);

  gd_discard(r->D);

  if (r->callback)
    efree(r->callback);
  if (r->callback_data && ZP(r->callback_data))
    Z_DELREF_P(ZP(r->callback_data));
  efree(r->key);
  efree(r);

  dreturnvoid();
}

static void gdphp_dirfile_pdtor(GDPHP_RESOURCE *z TSRMLS_DC)
{
  dtracetsrm("%p", z);

  gdphp_dirfile *r = (gdphp_dirfile*)(z->ptr);

  gd_discard(r->D);

  if (r->callback)
    pefree(r->callback, 1);
  if (r->callback_data)
    Z_DELREF_P(ZP(r->callback_data));
  pefree(r->key, 1);
  pefree(r, 1);

  dreturnvoid();
}

/* callback wrapper */
static int gdphp_callback(gd_parser_data_t *pdata, void *extra)
{
  int sem = GD_SYNTAX_ABORT;
  int new_line = 0;
  char *ptr;
  gdphp_dirfile *r = (gdphp_dirfile*)extra;
  zval *zpdata;
  GDPHP_ZVAL function_name;
  GDPHP_ZVAL response;
#if ZEND_MODULE_API_NO >= 20151012
  zval params[2];
#else
  zval **params[2] = { &zpdata, r->callback_data };
#endif

  dtrace("%p, %p", pdata, r->callback_data);

#if ZEND_MODULE_API_NO >= 20151012
  zpdata = &params[0];
  if (r->callback_data == NULL) 
    ZVAL_UNDEF(params + 1);
  else
    ZVAL_COPY_VALUE(params + 1, r->callback_data);
#endif

  if (r->callback == NULL) { /* nothing to do */
    dreturn("%i", GD_SYNTAX_ABORT);
    return GD_SYNTAX_ABORT;
  }

  TSRMLS_FETCH();

  /* make a zval for the function name */
  GDPHP_INIT_ZVAL(function_name);
  GDPHP_STRINGL(RZ(function_name), r->callback, r->callback_len);

  /* make a hashtable for the parser data */
  GDPHP_INIT_ZVAL(zpdata);
  array_init(zpdata);
  add_assoc_long(zpdata, "suberror", pdata->suberror);
  add_assoc_long(zpdata, "linenum", pdata->linenum);
  gdphp_add_assoc_string(zpdata, "line", pdata->line);
  gdphp_add_assoc_string(zpdata, "filename", (char*)pdata->filename);

  /* call the callback */
  if (call_user_function_ex(CG(function_table), NULL, RZ(function_name),
        &response, 2, params, 0, NULL TSRMLS_CC) != SUCCESS)
  {
    zend_error(E_ERROR, "Unable to execute GetData callback");
  }

  GDPHP_PTR_DTOR(function_name);

  /* interpret the response */
  switch (Z_TYPE_P(RZ(response))) {
    case IS_LONG:
      sem = (int)Z_LVAL_P(RZ(response));
      break;
    case IS_STRING: /* this means rescan */
      ptr = strdup(Z_STRVAL_P(RZ(response)));
      if (ptr == NULL)
        zend_error(E_ERROR, "Out of memory");
      pdata->line = ptr;
      sem = GD_SYNTAX_RESCAN;
      new_line = 1;
      break;
    default:
      zend_error(E_ERROR, "Bad response from GetData callback");
  }

  /* copy the line out of the zpdata hash, if necessary and possible */
  if (sem == GD_SYNTAX_RESCAN && new_line == 0) {
    zval *line;
    if (GDPHP_HASH_FIND(line, Z_ARRVAL_P(zpdata), "line")) {
      if (Z_TYPE_P(line) == IS_STRING) {
        ptr = strdup(Z_STRVAL_P(RZ(response)));
        if (ptr == NULL)
          zend_error(E_ERROR, "Out of memory");
        pdata->line = ptr;
      }
    }
  }

  zval_ptr_dtor(ZR(zpdata));

  dreturn("%i", sem);
  return sem;
}

/* create a dirfile resource */
static gdphp_dirfile *gdphp_open(const char *dirfilename, int len, long flags,
    const char *callback, int callback_len, GDPHP_ZVALP callback_data,
    int persist)
{
  gdphp_dirfile *r;
  DIRFILE *D;

  dtrace("\"%s\", %i, 0x%lX, \"%s\", %i, %p, %i", dirfilename, len, flags,
      callback, callback_len, callback_data, persist);

  /* create the resource */
  r = pemalloc(sizeof(gdphp_dirfile), persist);
  memset(r, 0, sizeof(gdphp_dirfile));

  r->callback_len = callback_len;
  if (callback)
    r->callback = pestrdup(callback, persist);

  r->key_len = len;
  if (len > 0) {
    r->key = pemalloc(len, persist);
    memcpy(r->key, dirfilename, len);
  }

  if (callback_data) {
    Z_ADDREF_P(ZP(callback_data));
    r->callback_data = callback_data;
  }

  /* open */
  if (callback)
    D = gd_cbopen(dirfilename, (unsigned)flags, gdphp_callback, r);
  else
    D = gd_open(dirfilename, (unsigned)flags);

  /* record the dirfile */
  r->D = D;
  r->persist = persist;

  dreturn("%p", r);
  return r;
}

/* convert a complex pair to a 2-element array */
static zval *gdphp_from_complex(zval *z, double r, double i)
{
  dtrace("%p, %g, %g", z, r, i);

  TSRMLS_FETCH();

  if (z == NULL)
    GDPHP_INIT_ZVAL(z);

  if (i == 0 && GDPHP_G(degrade_complex)) {
    ZVAL_DOUBLE(z, r);
  } else {
    array_init(z);
    add_index_double(z, 0, r);
    add_index_double(z, 1, i);
  }

  dreturn("%p", z);
  return z;
}

/* add a complex scalar to a hash */
static void gdphp_add_assoc_complex(zval *z, const char *key, double r,
    double i)
{
  GDPHP_ZVAL_NULL(tmp);

  dtrace("%p, \"%s\", %g, %g", z, key, r, i);

  add_assoc_zval(z, key, gdphp_from_complex(RZ(tmp), r, i));

  dreturnvoid();
}

/* add an array of complex data to a hash */
static void gdphp_add_assoc_cmparray(zval *z, const char *key, double *l,
    int n)
{
  GDPHP_ZVAL a;
  int i;
  GDPHP_ZVAL_NULL(tmp);

  dtrace("%p, \"%s\", %p, %i", z, key, l, n);

  GDPHP_INIT_ZVAL(a);
  array_init(RZ(a));

  for (i = 0; i < n; ++i)
    add_index_zval(RZ(a), i, gdphp_from_complex(RZ(tmp), l[i * 2],
          l[i * 2 + 1]));

  add_assoc_zval(z, key, RZ(a));

  dreturnvoid();
}

/* add an array of strings to a hash */
static void gdphp_add_assoc_string_arr(zval *z, const char *key, char **l,
    int n)
{
  GDPHP_ZVAL a;
  int i;

  dtrace("%p, \"%s\", %p, %i", z, key, l, n);

  GDPHP_INIT_ZVAL(a);
  array_init(RZ(a));

  for (i = 0; i < n; ++i)
    gdphp_add_index_string(RZ(a), i, l[i]);

  add_assoc_zval(z, key, RZ(a));

  dreturnvoid();
}

/* add scalars to a entry hash */
static void gdphp_add_assoc_scalars(zval *z, gd_entry_t *E, unsigned mask)
{
  int i;

  GDPHP_ZVAL scalar;
  GDPHP_ZVAL lm;

  dtrace("%p, %p, 0x%X", z, E, mask);

  GDPHP_INIT_ZVAL(scalar);
  array_init(RZ(scalar));

  for (i = 0; i <= GD_MAX_POLYORD; ++i)
    if (mask & (1 << i))
      if (E->scalar[i]) {
        GDPHP_INIT_ZVAL(lm);
        array_init(RZ(lm));
        gdphp_add_index_string(RZ(lm), 0, E->scalar[i]);
        add_index_long(RZ(lm), 1, E->scalar_ind[i]);
        add_index_zval(RZ(scalar), i, RZ(lm));
      }

  add_assoc_zval(z, "scalar", RZ(scalar));

  dreturnvoid();
}

/* convert from a gd_entry_t to a hash */
static void gdphp_from_entry(zval *z, gd_entry_t *E)
{
  dtrace("%p, %p", z, E);
  
  array_init(z);

  gdphp_add_assoc_string(z, "field", E->field);
  add_assoc_long(z, "field_type", E->field_type);
  add_assoc_long(z, "fragment_index", E->fragment_index);
  switch (E->field_type) {
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      gdphp_add_assoc_string_arr(z, "in_fields", E->in_fields, 1);
      add_assoc_long(z, "bitnum", E->EN(bit,bitnum));
      add_assoc_long(z, "numbits", E->EN(bit,numbits));
      gdphp_add_assoc_scalars(z, E, 0x3);
      break;
    case GD_SARRAY_ENTRY:
      add_assoc_long(z, "array_len", E->EN(scalar,array_len));
      break;
    case GD_CARRAY_ENTRY:
      add_assoc_long(z, "array_len", E->EN(scalar,array_len));
      /* fallthrough */
    case GD_CONST_ENTRY:
      add_assoc_long(z, "const_type", E->EN(scalar,const_type));
      break;
    case GD_LINCOM_ENTRY:
      add_assoc_long(z, "n_fields", E->EN(lincom,n_fields));
      gdphp_add_assoc_string_arr(z, "in_fields", E->in_fields,
          E->EN(lincom,n_fields));
      gdphp_add_assoc_cmparray(z, "m", (double*)E->EN(lincom,cm),
          E->EN(lincom,n_fields));
      gdphp_add_assoc_cmparray(z, "b", (double*)E->EN(lincom,cb),
          E->EN(lincom,n_fields));
      gdphp_add_assoc_scalars(z, E, ((1 << E->EN(lincom,n_fields)) - 1) * 9);
      break;
    case GD_LINTERP_ENTRY:
      gdphp_add_assoc_string_arr(z, "in_fields", E->in_fields, 1);
      gdphp_add_assoc_string(z, "table", E->EN(linterp,table));
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      gdphp_add_assoc_string_arr(z, "in_fields", E->in_fields, 2);
      break;
    case GD_PHASE_ENTRY:
      gdphp_add_assoc_string_arr(z, "in_fields", E->in_fields, 1);
      add_assoc_long(z, "shift", E->EN(phase,shift));
      gdphp_add_assoc_scalars(z, E, 0x1);
      break;
    case GD_POLYNOM_ENTRY:
      gdphp_add_assoc_string_arr(z, "in_fields", E->in_fields, 1);
      add_assoc_long(z, "poly_ord", E->EN(polynom,poly_ord));
      gdphp_add_assoc_cmparray(z, "a", (double*)E->EN(polynom,ca),
          E->EN(polynom,poly_ord) + 1);
      gdphp_add_assoc_scalars(z, E, (1 << (E->EN(polynom,poly_ord) + 1)) - 1);
      break;
    case GD_RECIP_ENTRY:
      gdphp_add_assoc_string_arr(z, "in_fields", E->in_fields, 1);
      gdphp_add_assoc_complex(z, "dividend", creal(E->EN(recip,cdividend)),
          cimag(E->EN(recip,cdividend)));
      gdphp_add_assoc_scalars(z, E, 0x1);
      break;
    case GD_RAW_ENTRY:
      add_assoc_long(z, "spf", E->EN(raw,spf));
      add_assoc_long(z, "data_type", E->EN(raw,data_type));
      gdphp_add_assoc_scalars(z, E, 0x1);
      break;
    case GD_WINDOW_ENTRY:
      gdphp_add_assoc_string_arr(z, "in_fields", E->in_fields, 2);
      add_assoc_long(z, "windop", E->EN(window,windop));
      switch (E->EN(window,windop)) {
        case GD_WINDOP_EQ:
        case GD_WINDOP_NE:
          add_assoc_long(z, "threshold", E->EN(window,threshold.u));
          break;
        case GD_WINDOP_SET:
        case GD_WINDOP_CLR:
          add_assoc_long(z, "threshold", E->EN(window,threshold.i));
          break;
        default:
          add_assoc_double(z, "threshold", E->EN(window,threshold.r));
          break;
      }
      gdphp_add_assoc_scalars(z, E, 0x1);
      break;
    case GD_MPLEX_ENTRY:
      gdphp_add_assoc_string_arr(z, "in_fields", E->in_fields, 2);
      add_assoc_long(z, "count_val", E->EN(mplex,count_val));
      add_assoc_long(z, "period", E->EN(mplex,period));
      gdphp_add_assoc_scalars(z, E, 0x3);
      break;
    case GD_INDEX_ENTRY:
    case GD_STRING_ENTRY:
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
      break;
  }

  dreturnvoid();
}

/* interpret a long as a gd_type_t symbol */
static gd_type_t gdphp_type_from_long(zval *z, struct gdphp_context_t *ctx)
{
  gd_type_t t = GD_UNKNOWN;
  dtracectx("%p", z);

  if (Z_TYPE_P(z) != IS_LONG)
    GDPHP_DIE(ctx, "Bad type code");

  switch (Z_LVAL_P(z)) {
    case GD_NULL:
    case GD_UINT8:
    case GD_INT8:
    case GD_UINT16:
    case GD_INT16:
    case GD_UINT32:
    case GD_INT32:
    case GD_UINT64:
    case GD_INT64:
    case GD_FLOAT32:
    case GD_FLOAT64:
    case GD_COMPLEX64:
    case GD_COMPLEX128:
      t = Z_LVAL_P(z);
  }

  dreturn("0x%X", t);
  return t;
}

static int gdphp_to_datum(void *dst, gd_type_t type, zval *src, int complain,
    struct gdphp_context_t *ctx);

/* convert an array to a complex number -- z is known to be an array; returns
 * zero on success, non-zero (or doesn't return) on error */
static int gdphp_to_complex(double *r, double *i, zval *z, int complain,
    struct gdphp_context_t *ctx)
{
  int n, converted = 0;

  dtracectx("%p, %p, %p, %i", r, i, z, complain);

  HashTable *a = Z_ARRVAL_P(z);

  n = zend_hash_num_elements(a);

  if (n == 2) {
    GDPHP_ZVALP d;

    if (GDPHP_HASH_INDEX_FIND(d, a, 0))
      if (gdphp_to_datum(r, GD_FLOAT64, ZP(d), 0, ctx) == 0)
        converted++;

    if (GDPHP_HASH_INDEX_FIND(d, a, 1))
      if (gdphp_to_datum(i, GD_FLOAT64, ZP(d), 0, ctx) == 0)
        converted++;
  }

  if (converted < 2) {
    if (complain)
      GDPHP_DIE(ctx, "Bad numeric data");
    dreturn("%i", 1);
    return 1;
  }

  dreturn("%i (%g, %g)", 0, *r, *i);
  return 0;
}

/* return a GD type based on a PHP type */
static gd_type_t gdphp_get_type(zval *z, struct gdphp_context_t *ctx)
{
  gd_type_t t = GD_UNKNOWN;
  double r, i;

  dtracectx("%p", z);

  switch (Z_TYPE_P(z)) {
    case IS_NULL:
      t = GD_NULL;
      break;
    case IS_LONG: BOOL_CASE
      t = GD_INT64;
      break;
    case IS_DOUBLE:
      t = GD_FLOAT64;
      break;
    case IS_ARRAY: /* check for complex by tring to convert */
      if (gdphp_to_complex(&r, &i, z, 0, ctx) == 0)
        t = GD_COMPLEX128;
  }

  dreturn("0x%X", t);
  return t;
}

#define CCONVERT_ZVAL(t) \
  do { \
    switch (Z_TYPE_P(src)) { \
      double r, i; \
      case IS_NULL:               ((t*)dst)[0] = ((t*)dst)[1] = 0; break; \
      case IS_LONG: BOOL_CASE     ((t*)dst)[0] = Z_LVAL_P(src); \
                                  ((t*)dst)[1] = 0; break; \
      case IS_DOUBLE:             ((t*)dst)[0] = Z_DVAL_P(src); \
                                  ((t*)dst)[1] = 0; break; \
      case IS_ARRAY: gdphp_to_complex(&r, &i, src, 1, ctx); \
                     ((t*)dst)[0] = r; ((t*)dst)[1] = i; break; \
      default: if (complain) GDPHP_DIE(ctx, "bad numeric type"); r = 1; \
    } \
  } while (0)

#define CONVERT_ZVAL(t) \
  do { \
    switch (Z_TYPE_P(src)) { \
      double r, i; \
      case IS_LONG: BOOL_CASE     *((t*)dst) = Z_LVAL_P(src); break; \
      case IS_DOUBLE:             *((t*)dst) = Z_DVAL_P(src); break; \
      case IS_ARRAY: gdphp_to_complex(&r, &i, src, 1, ctx); \
                     *((t*)dst) = r; break; \
      default: if (complain) GDPHP_DIE(ctx, "bad numeric type"); r = 1; \
    } \
  } while (0)

/* convert a zval to a C numeric type */
static int gdphp_to_datum(void *dst, gd_type_t type, zval *src, int complain,
    struct gdphp_context_t *ctx)
{
  int r = 0;
  dtracectx("%p, 0x%X, %p, %i", dst, type, src, complain);

  switch (type) {
    case GD_NULL: case GD_UNKNOWN:               break;
    case GD_UINT8:       CONVERT_ZVAL( uint8_t); break;
    case GD_INT8:        CONVERT_ZVAL(  int8_t); break;
    case GD_UINT16:      CONVERT_ZVAL(uint16_t); break;
    case GD_INT16:       CONVERT_ZVAL( int16_t); break;
    case GD_UINT32:      CONVERT_ZVAL(uint32_t); break;
    case GD_INT32:       CONVERT_ZVAL( int32_t); break;
    case GD_UINT64:      CONVERT_ZVAL(uint64_t); break;
    case GD_INT64:       CONVERT_ZVAL( int64_t); break;
    case GD_FLOAT32:     CONVERT_ZVAL(   float); break;
    case GD_FLOAT64:     CONVERT_ZVAL(  double); break;
    case GD_COMPLEX64:  CCONVERT_ZVAL(   float); break;
    case GD_COMPLEX128: CCONVERT_ZVAL(  double); break;
  }

  dreturn("%i", r);
  return r;
}

static gd_type_t gdphp_to_datum_and_type(void *datum, zval *z,
    struct gdphp_context_t *ctx)
{
  gd_type_t t = GD_UNKNOWN;

  dtracectx("%p, %p", datum, z);

  switch (Z_TYPE_P(z)) {
    case IS_LONG:
      t = GDPHP_LONG_TYPE;
      *((gdphp_long_t*)datum) = Z_LVAL_P(z);
      break;
    case IS_DOUBLE:
      t = GD_FLOAT64;
      *((double*)datum) = Z_DVAL_P(z);
      break;
    case IS_ARRAY:
      /* check if it is a complex array */
      if (gdphp_to_complex(datum, ((double*)datum) + 1, z, 0, ctx) == 0) {
        t = GD_COMPLEX128;
        break;
      }
      /* FALLTHROUGH */
    default:
      GDPHP_DIE(ctx, "expected numeric scalar");
  }

  dreturn("0x%X", t);
  return t;
}

gd_static_inline_ char *gdphp_check_null_string(char *v)
{
  dtrace("\"%s\"", v);
  if (v == NULL || v[0] == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("\"%s\"", v);
  return v;
}

static void gdphp_to_threshold(gd_triplet_t *t, gd_windop_t windop, zval *z,
    struct gdphp_context_t *ctx)
{
  dtracectx("%p, %i, %p", t, windop, z);

  switch (windop) {
    case GD_WINDOP_EQ:
    case GD_WINDOP_NE:
      gdphp_to_datum(&t->i, GD_INT64, z, 1, ctx);
      break;
    case GD_WINDOP_SET:
    case GD_WINDOP_CLR:
      gdphp_to_datum(&t->u, GD_UINT64, z, 1, ctx);
      break;
    default:
      gdphp_to_datum(&t->r, GD_FLOAT64, z, 1, ctx);
      break;
  }

  dreturnvoid();
}

/* doesn't return on error */
static void gdphp_validate_type(gd_type_t t, struct gdphp_context_t *ctx) {
  dtracectx("0x%X", t);

  if (GD_SIZE(t) == 0)
    GDPHP_DIE(ctx, "bad data type");

  dreturnvoid();
}

/* convert a PHP array ot an array of complex data; returns length */
static int gdphp_convert_cmparray(double *out, zval *z, int min, int max,
    unsigned mask, struct gdphp_context_t *ctx)
{
  HashTable *a = Z_ARRVAL_P(z);
  HashPosition i;
  GDPHP_ZVALP d = NULL;
  int n = -1;
  GDPHP_HASH_KEY_DECL(key);
  unsigned long index;

  int *have;

  dtracectx("%p, %p, %i, %i, 0x%X", out, z, min, max, mask);

  /* remember which elements we've seen */
  have = emalloc(sizeof(int) * max);
  memset(have, 0, sizeof(int) * max);

  for (index = 0; index < max; ++index)
    if (mask & (1 << index))
      have[index] = 1;

  /* populate the C array */
  for (zend_hash_internal_pointer_reset_ex(a, &i);
      GDPHP_HASH_GET_CURRENT_DATA(d, a, i);
      zend_hash_move_forward_ex(a, &i))
  {
    /* check key */
    if (GDPHP_HASH_GET_CURRENT_KEY(a, key, index, i) == HASH_KEY_IS_STRING) {
      GDPHP_DIE(ctx, "cannot use associative array");
    } else if (index >= max)
      GDPHP_DIE2(ctx, "bad array index (%lu/%lu)", index, max);

    if (!have[index]) {
      gdphp_to_datum(out + index * 2, GD_COMPLEX128, ZP(d), 1, ctx);
      have[index] = 1;
    }
  }

  /* check for holes and calculate n */
  index = max;
  while (index-- > 0) {
    if (have[index] == 0 && n != -1)
      GDPHP_DIE(ctx, "uninitialised data in numeric array");
    else if (have[index] && n == -1)
      n = index + 1;
  }

  if (n < min || n > max)
    GDPHP_DIE(ctx, "bad array count");

  efree(have);

  dreturn("%i", n);
  return n;
}

/* convert a PHP array to an array of strings; returns the number of elements */
static size_t gdphp_convert_sarray(const char ***out, const zval *z, int p)
{
  HashTable *a = Z_ARRVAL_P(z);
  HashPosition i;
  GDPHP_ZVALP d = NULL;
  size_t j, n = 0;
  GDPHP_HASH_KEY_DECL(key);
  unsigned long index;

  GDPHP_CONTEXTp(ctx,p);

  dtrace("%p, %p, %i", out, z, p);

  /* count and check */
  for (zend_hash_internal_pointer_reset_ex(a, &i);
      GDPHP_HASH_GET_CURRENT_DATA(d, a, i);
      zend_hash_move_forward_ex(a, &i))
  {
    /* check key */
    if (GDPHP_HASH_GET_CURRENT_KEY(a, key, index, i) == HASH_KEY_IS_STRING)
      GDPHP_DIE(&ctx, "cannot use associative array");

    if (Z_TYPE_P(ZP(d)) != IS_STRING)
      GDPHP_DIE(&ctx, "string array required");

    if (n < index + 1)
      n = index + 1;
  }

  if (n == 0) {
    *out = NULL;
    dreturn("%i", 0);
    return 0;
  }

  /* allocate and zero */
  *out = emalloc(sizeof(**out) * n);
  for (j = 0; j < n; ++j)
    (*out)[j] = NULL;

  /* convert */
  for (zend_hash_internal_pointer_reset_ex(a, &i);
			GDPHP_HASH_GET_CURRENT_DATA(d, a, i);
      zend_hash_move_forward_ex(a, &i))
  {
    GDPHP_HASH_GET_CURRENT_KEY(a, key, index, i);

    (*out)[index] = Z_STRVAL_P(ZP(d));
  }

  /* look for holes */
  for (j = 0; j < n; ++j)
    if ((*out)[j] == NULL)
      GDPHP_DIE2(&ctx, "missing array index (%li)", j);

  dreturn("%zu", n);
  return n;
}

/* convert a PHP array to a bounded array of strings; returns the number of
 * elements */
static int gdphp_convert_nsarray(char **out, zval *z, int min, int max,
    struct gdphp_context_t *ctx)
{
  HashTable *a = Z_ARRVAL_P(z);
  HashPosition i;
  GDPHP_ZVALP d = NULL;
  int n = -1;
  GDPHP_HASH_KEY_DECL(key);
  unsigned long index;

  dtracectx("%p, %p, %i, %i", out, z, min, max);

  /* santitise */
  memset(out, 0, sizeof(char*) * max);

  /* populate the C array */
  for (zend_hash_internal_pointer_reset_ex(a, &i);
      GDPHP_HASH_GET_CURRENT_DATA(d, a, i);
      zend_hash_move_forward_ex(a, &i))
  {
    /* check key */
    if (GDPHP_HASH_GET_CURRENT_KEY(a, key, index, i) == HASH_KEY_IS_STRING)
      GDPHP_DIE(ctx, "cannot use associative array");
    else if (index >= max)
      GDPHP_DIE2(ctx, "bad array index (%lu/%i)", index, max);

    if (Z_TYPE_P(ZP(d)) != IS_STRING)
      GDPHP_DIE(ctx, "string array required");

    out[index] = Z_STRVAL_P(ZP(d));
  }

  /* check for holes and calculate n */
  index = max;
  while (index-- > 0) {
    if (out[index] == NULL && n != -1)
      GDPHP_DIE(ctx, "uninitialised data in string array");
    else if (out[index] && n == -1)
      n = index + 1;
  }

  if (n < min || n > max)
    GDPHP_DIE(ctx, "bad string array count");

  dreturn("%i", n);
  return n;
}

/* convert an array to data */
static void gdphp_convert_array(struct gdphp_din *din, zval *zdata,
    struct gdphp_context_t *ctx)
{
  HashTable *a = Z_ARRVAL_P(zdata);
  HashPosition i;
  GDPHP_ZVALP d = NULL;
  GDPHP_HASH_KEY_DECL(key);
  unsigned long index;

  dtracectx("%p, %p", din, zdata);

  din->ns = 0;

  /* pass 1: validate data and determine ns */
  for (zend_hash_internal_pointer_reset_ex(a, &i);
      GDPHP_HASH_GET_CURRENT_DATA(d, a, i);
      zend_hash_move_forward_ex(a, &i))
  {
    /* make sure this isn't an associative array */
    if (GDPHP_HASH_GET_CURRENT_KEY(a, key, index, i) == HASH_KEY_IS_STRING)
      GDPHP_DIE(ctx, "cannot use associative arrays");

    if (index + 1 > din->ns)
      din->ns = index + 1;

    if (din->type == GD_UNKNOWN) {
        din->type = gdphp_get_type(ZP(d), ctx);
        if (din->type == GD_UNKNOWN)
          GDPHP_DIE(ctx, "bad numeric type");
        else if (din->type == GD_NULL)
          din->type = GD_UNKNOWN;
    }
  }

  if (din->ns == 0 || din->type == GD_UNKNOWN) {
    din->type = GD_NULL;
    din->ns = 0;
  } else {
    /* allocate the buffer */
    din->free_din = 1;
    din->data = emalloc(din->ns * GD_SIZE(din->type));

    /* zero */
    if (din->type == GD_FLOAT64 || din->type == GD_COMPLEX128) {
      size_t j, ns = din->ns;

      if (din->type == GD_COMPLEX128)
        ns *= 2;

      for (j = 0; j < ns; ++j)
        ((double*)(din->data))[j] = NAN;
    } else if (din->type == GD_FLOAT32 || din->type == GD_COMPLEX64) {
      size_t j, ns = din->ns;

      if (din->type == GD_COMPLEX128)
        ns *= 2;

      for (j = 0; j < ns; ++j)
        ((float*)(din->data))[j] = (float)NAN;
    } else
      memset(din->data, 0, din->ns * GD_SIZE(din->type));

    /* pass 2: convert the data */
    for (zend_hash_internal_pointer_reset_ex(a, &i);
        GDPHP_HASH_GET_CURRENT_DATA(d, a, i);
        zend_hash_move_forward_ex(a, &i))
    {
      GDPHP_HASH_GET_CURRENT_KEY(a, key, index, i);

      gdphp_to_datum(((char*)(din->data)) + index * GD_SIZE(din->type),
          din->type, ZP(d), 1, ctx);
    }
  }

  dreturn("0x%X, %zu", din->type, din->ns);
}

/* convert input data */
static struct gdphp_din gdphp_convert_data(zval *zdata1, zval *zdata2, int p1,
    int p2)
{
  struct gdphp_din din;

  dtrace("%p, %p, %i, %i", zdata1, zdata2, p1, p2);

  if (zdata2 == NULL) {
    GDPHP_CONTEXTp(ctx,p1);

    if (Z_TYPE_P(zdata1) == IS_ARRAY) {
      din.type = GD_UNKNOWN;
      gdphp_convert_array(&din, zdata1, &ctx);
    } else
      GDPHP_DIE(&ctx, "bad input data: expected array or type code");
  } else {
    /* in the two argument case, the first argument is taken to be a type code
     */
    GDPHP_CONTEXTp(ctx,p1);

    din.type = gdphp_type_from_long(zdata1, &ctx); /* doesn't return on error */

    if (din.type == GD_NULL) { /* Weird, but we'll allow it */
      din.data = NULL;
      din.ns = 0;
      din.free_din = 0;
    } else {
      ctx.p = p2;

      switch (Z_TYPE_P(zdata2)) {
        case IS_STRING: /* packed data -- just use it in place */
          din.data = Z_STRVAL_P(zdata2);
          din.ns = Z_STRLEN_P(zdata2) / GD_SIZE(din.type);
          din.free_din = 0;
          break;
        case IS_ARRAY:
          gdphp_convert_array(&din, zdata2, &ctx);
          break;
        default:
          GDPHP_DIE(&ctx, "bad input data: expected array or string");
      }
    }
  }

  dreturn("{%p, 0x%X, %i, %zu}", din.data, din.type, din.free_din, din.ns);
  return din;
}

static long gdphp_convert_long(zval *z, struct gdphp_context_t *ctx)
{
  long l;

  dtracectx("%p", z);

  if (Z_TYPE_P(z) != IS_LONG && Z_TYPE_P(z) != IS_DOUBLE)
    GDPHP_DIE(ctx, "expected number");

  convert_to_long(z); /* coerce */
  l = Z_LVAL_P(z);
  
  dreturn("%li", l);
  return l;
}

static double gdphp_convert_double(zval *z, struct gdphp_context_t *ctx)
{
  double d;

  dtracectx("%p", z);

  if (Z_TYPE_P(z) != IS_LONG && Z_TYPE_P(z) != IS_DOUBLE)
    GDPHP_DIE(ctx, "expected number");

  convert_to_double(z); /* coerce */
  d = Z_DVAL_P(z);
  
  dreturn("%g", d);
  return d;
}

/* convert PHP string with error checking */
static char *gdphp_convert_string(zval *z, struct gdphp_context_t *ctx)
{
  char *s;

  dtracectx("%p", z);

  if (Z_TYPE_P(z) != IS_STRING)
    GDPHP_DIE(ctx, "expected string");

  s = Z_STRVAL_P(z);
  
  dreturn("\"%s\"", s);
  return s;
}

static void gdphp_entry_complex(double *v, HashTable *a, const char *key,
    int partial, struct gdphp_context_t *ctx)
{
  GDPHP_ZVALP z;

  dtracectx("%p, %p, \"%s\", %i", v, a, key, partial);

  if (GDPHP_HASH_FIND(z, a, key)) {
    ctx->k = key;
    gdphp_to_datum(v, GD_COMPLEX128, ZP(z), 1, ctx);
    ctx->k = NULL;
  } else if (!partial)
    GDPHP_DIE2(ctx, "required key '%s' not found", key);

  dreturnvoid();
}

static long gdphp_entry_long(HashTable *a, const char *key, int *missing,
    int partial, struct gdphp_context_t *ctx)
{
  long r = 0;
  GDPHP_ZVALP z;

  dtracectx("%p, \"%s\", %p, %i", a, key, missing, partial);

  if (GDPHP_HASH_FIND(z, a, key)) {
    ctx->k = key;
    r = gdphp_convert_long(ZP(z), ctx);
    ctx->k = NULL;
    if (missing != NULL)
      *missing = 0;
  } else if (missing != NULL)
    *missing = 1;
  else if (!partial)
    GDPHP_DIE2(ctx, "required key '%s' not found", key);

  dreturn("%li", r);
  return r;
}

static double gdphp_entry_double(HashTable *a, const char *key, int partial,
    struct gdphp_context_t *ctx)
{
  double r = 0;
  GDPHP_ZVALP z;

  dtracectx("%p, \"%s\", %i", a, key, partial);

  if (GDPHP_HASH_FIND(z, a, key)) {
    ctx->k = key;
    r = gdphp_convert_double(ZP(z), ctx);
    ctx->k = NULL;
  } else if (!partial)
    GDPHP_DIE2(ctx, "required key '%s' not found", key);

  dreturn("%g", r);
  return r;
}

static int gdphp_entry_infields(char **l, HashTable *a, int min, int max,
    int partial, struct gdphp_context_t *ctx)
{
  int n = 0;
  GDPHP_ZVALP z;

  dtracectx("%p, %p, %i, %i, %i", l, a, min, max, partial);

  if (GDPHP_HASH_FIND(z, a, "in_fields")) {
    ctx->k = "in_fields";
    if (Z_TYPE_P(ZP(z)) == IS_ARRAY)
      n = gdphp_convert_nsarray(l, ZP(z), min, max, ctx);
    else if (Z_TYPE_P(ZP(z)) == IS_STRING) {
      if (min > 1)
        GDPHP_DIE(ctx, "bad string array count");
      n = 1;
      l[0] = gdphp_convert_string(ZP(z), ctx);
    } else
      GDPHP_DIE(ctx, "expected string or string array");
    ctx->k = NULL;
  } else if (!partial)
    GDPHP_DIE(ctx, "required key 'in_fields' not found");

  dreturn("%i", n);
  return n;
}

static int gdphp_entry_cmparray(double *l, HashTable *a, char *key, int min,
    int max, unsigned mask, int partial, struct gdphp_context_t *ctx)
{
  int n = 0;
  GDPHP_ZVALP z;

  dtracectx("%p, %p, \"%s\", %i, %i, 0x%X", l, a, key, min, max, mask);

  if (GDPHP_HASH_FIND(z, a, key)) {
    ctx->k = key;
    if (Z_TYPE_P(ZP(z)) == IS_ARRAY)
      n = gdphp_convert_cmparray(l, ZP(z), min, max, mask, ctx);
    else if (Z_TYPE_P(ZP(z)) == IS_DOUBLE) {
      if (min > 1)
        GDPHP_DIE(ctx, "bad array count");
      n = 1;
      l[0] = Z_DVAL_P(ZP(z));
    } else
      GDPHP_DIE(ctx, "expected array or float");
    ctx->k = NULL;
  } else if (!partial)
    GDPHP_DIE2(ctx, "required key '%s' not found", key);

  dreturn("%i", n);
  return n;
}

/* populate an entry from the scalar array in an entry array; returns a
 * bitmask of the elements set */
static unsigned gdphp_entry_scalars(char** scalar, int *scalar_ind,
    HashTable *a, unsigned mask, struct gdphp_context_t *ctx)
{
  unsigned mask_out = 0;
  GDPHP_ZVALP z;
  int i;

  dtracectx("%p, %p, %p, 0x%X", scalar, scalar_ind, a, mask);
  
  /* find 'scalar' in the entry array */
  if (!GDPHP_HASH_FIND(z, a, "scalar")) {
    dreturn("%i", 0);
    return 0;
  }

  ctx->k = "scalar";

  /* 'scalar' must be an array (or null) */
  if (z == NULL || Z_TYPE_P(ZP(z)) == IS_NULL) {
    ctx->k = NULL;
    dreturn("%i", 0);
    return 0;
  } else if (Z_TYPE_P(ZP(z)) != IS_ARRAY)
    GDPHP_DIE(ctx, "expected array");

  /* loop through the array of scalars */
  a = Z_ARRVAL_P(ZP(z));
  for (i = 0; i < 2 * GD_MAX_LINCOM; ++i) {
    if (!(mask & (1 << i)))
      continue;

    /* get the i'th scalar array element */
    ctx->i = i;
    if (GDPHP_HASH_INDEX_FIND(z, a,i)) {
      HashTable *sa;

      if (Z_TYPE_P(ZP(z)) == IS_NULL)
        continue;
      if (Z_TYPE_P(ZP(z)) != IS_ARRAY)
        GDPHP_DIE(ctx, "expected array");

      sa = Z_ARRVAL_P(ZP(z));

      /* element zero should be the name of a scalar field. */
      if (GDPHP_HASH_INDEX_FIND(z, sa, 0)) {
        if (Z_TYPE_P(ZP(z)) != IS_STRING)
          GDPHP_DIE(ctx, "expected scalar field name");
        scalar[i] = Z_STRVAL_P(ZP(z));
      } else
        GDPHP_DIE(ctx, "element zero should be scalar field name");

      mask_out |= 1 << i;

      /* element one should be the index; if missing zero is assumed */
      if (GDPHP_HASH_INDEX_FIND(z, sa, 1)) {
        convert_to_long(ZP(z)); /* coerce */
        scalar_ind[i] = Z_LVAL_P(ZP(z));
      } else
        scalar_ind[i] = 0;
    }
  }

  ctx->k = NULL;
  ctx->i = -1;
  dreturn("0x%02X", mask_out);
  return mask_out;
}

static char *gdphp_entry_string(HashTable *a, const char *key, int partial,
    struct gdphp_context_t *ctx)
{
  char *s = NULL;
  GDPHP_ZVALP z;
  
  dtracectx("%p, \"%s\", %i", a, key, partial);

  if (GDPHP_HASH_FIND(z, a, key)) {
    ctx->k = key;
    s = gdphp_convert_string(ZP(z), ctx);
    ctx->k = NULL;
  } else if (!partial)
    GDPHP_DIE2(ctx, "required key '%s' not found", key);

  dreturn("\"%s\"", s);
  return s;
}

/* convert an entry array to a gd_entry_t; z is known to be an array */
static void gdphp_to_entry(gd_entry_t *E, zval *z, const gd_entry_t *old_E,
    int no_fragment, struct gdphp_context_t *ctx)
{
  /* lincom scalar masks */
  const unsigned lincom_mask[4] = { 0x00, 0x09, 0x1B, 0x3F };
  const int partial = (old_E != NULL);

  int n, missing = 0, min = 0, max = 0;
  unsigned mask, tmask;
  HashTable *a = Z_ARRVAL_P(z);

  dtracectx("%p, %p, %p, %i", E, z, old_E, no_fragment);

  if (old_E)
    memcpy(E, old_E, sizeof(gd_entry_t));
  else
    memset(E, 0, sizeof(gd_entry_t));

  if (!partial)
    E->field = gdphp_entry_string(a, "field", partial, ctx);

  E->field_type = gdphp_entry_long(a, "field_type", NULL, partial, ctx);

  if (no_fragment)
    E->fragment_index = 0;
  else
    E->fragment_index = gdphp_entry_long(a, "fragment_index", NULL, partial,
        ctx);


  switch (E->field_type) {
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      gdphp_entry_infields((char**)E->in_fields, a, 1, 1, partial, ctx);

      mask = gdphp_entry_scalars(E->scalar, E->scalar_ind, a, 3, ctx);

      if (!(mask & 1))
        E->EN(bit,bitnum) = gdphp_entry_long(a, "bitnum", NULL, partial, ctx);

      if (!(mask & 2)) {
        E->EN(bit,numbits) = gdphp_entry_long(a, "numbits", &missing, partial,
            ctx);

        if (missing && !partial)
          E->EN(bit,numbits) = 1;
      }
      break;
    case GD_SARRAY_ENTRY:
      E->EN(scalar,array_len) = gdphp_entry_long(a, "array_len", NULL, partial,
          ctx);
			break;
    case GD_CARRAY_ENTRY:
      E->EN(scalar,array_len) = gdphp_entry_long(a, "array_len", NULL, partial,
          ctx);
      /* FALLTHROUGH */
    case GD_CONST_ENTRY:
      E->EN(scalar,const_type) = gdphp_entry_long(a, "const_type", NULL,
          partial, ctx);
      break;
    case GD_LINCOM_ENTRY:
      /* honour n_fields, if given */
      n = gdphp_entry_long(a, "n_fields", &missing, partial, ctx);
      if (missing) {
        min = 1;
        max = GD_MAX_LINCOM;
      } else if (n < 1 || n > GD_MAX_LINCOM) {
        GDPHP_DIE2(ctx, "bad value for 'n_fields' (%i)", n);
      } else
        min = max = n;

      E->EN(lincom,n_fields) = gdphp_entry_infields((char**)E->in_fields, a,
          min, max, partial, ctx);
      
      if (E->EN(lincom,n_fields) != 0)
        min = max = E->EN(lincom,n_fields);

      mask = gdphp_entry_scalars(E->scalar, E->scalar_ind, a, lincom_mask[max],
          ctx);

      E->flags |= GD_EN_COMPSCAL;
      tmask = (1 << max) - 1;

      if ((mask & tmask) != tmask)
        gdphp_entry_cmparray((double*)E->EN(lincom,cm), a, "m", min, max, mask,
            partial, ctx);

      if (((mask >> GD_MAX_LINCOM) & tmask) != tmask)
        gdphp_entry_cmparray((double*)E->EN(lincom,cb), a, "b", min, max,
            mask >> GD_MAX_LINCOM, partial, ctx);
      else
        E->EN(lincom,n_fields) = max;
      break;
    case GD_LINTERP_ENTRY:
      gdphp_entry_infields((char**)E->in_fields, a, 1, 1, partial, ctx);
      E->EN(linterp,table) = gdphp_entry_string(a, "table", partial, ctx);
      break;
    case GD_MPLEX_ENTRY:
      gdphp_entry_infields((char**)E->in_fields, a, 2, 2, partial, ctx);
      mask = gdphp_entry_scalars(E->scalar, E->scalar_ind, a, 3, ctx);

      if (!(mask & 1))
        E->EN(mplex,count_val) = gdphp_entry_long(a, "count_val", NULL, partial,
            ctx);
      
      if (!(mask & 2)) {
        E->EN(mplex,period) = gdphp_entry_long(a, "period", &missing, partial,
            ctx);

        if (missing)
          E->EN(mplex,period) = 0;
      }
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
		case GD_INDIR_ENTRY:
		case GD_SINDIR_ENTRY:
      gdphp_entry_infields((char**)E->in_fields, a, 2, 2, partial, ctx);
      break;
    case GD_PHASE_ENTRY:
      gdphp_entry_infields((char**)E->in_fields, a, 1, 1, partial, ctx);
      mask = gdphp_entry_scalars(E->scalar, E->scalar_ind, a, 1, ctx);
      if (!(mask & 1))
        E->EN(phase,shift) = gdphp_entry_long(a, "shift", NULL, partial, ctx);
      break;
    case GD_POLYNOM_ENTRY:
      /* honour poly_ord, if given */
      n = gdphp_entry_long(a, "poly_ord", &missing, partial, ctx);
      if (missing) {
        min = 2;
        max = GD_MAX_POLYORD + 1;
      } else if (n < 1 || n > GD_MAX_POLYORD) {
        GDPHP_DIE2(ctx, "bad value for 'poly_ord' (%i)", n);
      } else
        min = max = n + 1;

      gdphp_entry_infields((char**)E->in_fields, a, 1, 1, partial, ctx);

      mask = gdphp_entry_scalars(E->scalar, E->scalar_ind, a, (1 << max) - 1,
          ctx);

      E->flags |= GD_EN_COMPSCAL;
      tmask = (1 << max) - 1;
      if ((mask & tmask) != tmask)
        E->EN(polynom,poly_ord) =
          gdphp_entry_cmparray((double*)E->EN(polynom,ca), a, "a", min, max,
              mask, partial, ctx) - 1;
      else
        E->EN(polynom,poly_ord) = max - 1;
      break;
    case GD_RAW_ENTRY:
      E->EN(raw,data_type) = gdphp_entry_long(a, "data_type", NULL, partial,
          ctx);
      E->EN(raw,spf) = gdphp_entry_long(a, "spf", NULL, partial, ctx);
      break;
    case GD_RECIP_ENTRY:
      gdphp_entry_infields((char**)E->in_fields, a, 1, 1, partial, ctx);
      mask = gdphp_entry_scalars(E->scalar, E->scalar_ind, a, 1, ctx);
      E->flags |= GD_EN_COMPSCAL;
      if (!(mask & 1))
        gdphp_entry_complex((double*)gd_csp_(E->EN(recip,cdividend)), a,
            "dividend", partial, ctx);
      break;
    case GD_STRING_ENTRY:
      /* nothing to do */
      break;
    case GD_WINDOW_ENTRY:
      gdphp_entry_infields((char**)E->in_fields, a, 2, 2, partial, ctx);
      mask = gdphp_entry_scalars(E->scalar, E->scalar_ind, a, 1, ctx);
      E->EN(window,windop) = gdphp_entry_long(a, "windop", NULL, partial, ctx);
      if (!(mask & 1))
        switch (E->EN(window,windop)) {
          case GD_WINDOP_EQ:
          case GD_WINDOP_NE:
            E->EN(window,threshold.i) = gdphp_entry_long(a, "threshold", NULL,
                partial, ctx);
            break;
          case GD_WINDOP_SET:
          case GD_WINDOP_CLR:
            E->EN(window,threshold.u) = gdphp_entry_long(a, "threshold", NULL,
                partial, ctx);
            break;
          default:
            E->EN(window,threshold.r) = gdphp_entry_double(a, "threshold",
                partial, ctx);
            break;
        }
      break;
    default:
      GDPHP_DIE2(ctx, "bad field type (%i)", E->field_type);
  }

  dreturnvoid();
}

/* convert a C string list to a array of strings; if n is zero, the list is
 * NULL terminated */
static zval *gdphp_to_string_array(zval *z, const char **l, int n)
{
  int i;

  dtrace("%p, %p, %i", z, l, n);

  if (z == NULL)
    GDPHP_INIT_ZVAL(z);

  array_init(z);

  for (i = 0; (n == 0) ? l[i] != NULL : i < n; ++i)
    if (l[i] == NULL)
      add_index_null(z, i);
    else
      gdphp_add_index_string(z, i, l[i]);

  dreturn("%p", z);
  return z;
}

/* convert a datum to a PHP value */
static void gdphp_from_datum(zval *z, void *datum, gd_type_t type)
{
  dtrace("%p, %p, 0x%X", z, datum, type);

  switch (type) {
    case GD_UINT8:
      ZVAL_LONG(z, *((uint8_t*)datum));
      break;
    case GD_INT8:
      ZVAL_LONG(z, *((int8_t*)datum));
      break;
    case GD_UINT16:
      ZVAL_LONG(z, *((uint16_t*)datum));
      break;
    case GD_INT16:
      ZVAL_LONG(z, *((int16_t*)datum));
      break;
    case GD_UINT32:
      ZVAL_LONG(z, *((uint32_t*)datum));
      break;
    case GD_INT32:
      ZVAL_LONG(z, *((int32_t*)datum));
      break;
    case GD_UINT64:
      ZVAL_LONG(z, *((uint64_t*)datum));
      break;
    case GD_INT64:
      ZVAL_LONG(z, *((int64_t*)datum));
      break;
    case GD_FLOAT32:
      ZVAL_DOUBLE(z, *((float*)datum));
      break;
    case GD_FLOAT64:
      ZVAL_DOUBLE(z, *((double*)datum));
      break;
    case GD_COMPLEX64:
      gdphp_from_complex(z, ((float*)datum)[0], ((float*)datum)[1]);
      break;
    case GD_COMPLEX128:
      gdphp_from_complex(z, ((double*)datum)[0], ((double*)datum)[1]);
      break;
    default:
      ZVAL_NULL(z);
      break;
  }

  dreturnvoid();
}

/* convert a data vector to a PHP array -- <a> must already be initialised as
 * an array */
static void gdphp_data_to_array(zval *a, const void *data, gd_type_t type,
    size_t n)
{
  size_t i;

  dtrace("%p, %p, 0x%X, %zu", a, data, type, n);

  switch (type) {
    case GD_UINT8:
      for (i = 0; i < n; ++i)
        add_index_long(a, i, ((uint8_t*)(data))[i]);
      break;
    case GD_INT8:
      for (i = 0; i < n; ++i)
        add_index_long(a, i, ((int8_t*)(data))[i]);
      break;
    case GD_UINT16:
      for (i = 0; i < n; ++i)
        add_index_long(a, i, ((uint16_t*)(data))[i]);
      break;
    case GD_INT16:
      for (i = 0; i < n; ++i)
        add_index_long(a, i, ((int16_t*)(data))[i]);
      break;
    case GD_UINT32:
      for (i = 0; i < n; ++i)
        add_index_long(a, i, ((uint32_t*)(data))[i]);
      break;
    case GD_INT32:
      for (i = 0; i < n; ++i)
        add_index_long(a, i, ((int32_t*)(data))[i]);
      break;
    case GD_UINT64:
      for (i = 0; i < n; ++i)
        add_index_long(a, i, ((uint64_t*)(data))[i]);
      break;
    case GD_INT64:
      for (i = 0; i < n; ++i)
        add_index_long(a, i, ((int64_t*)(data))[i]);
      break;
    case GD_FLOAT32:
      for (i = 0; i < n; ++i)
        add_index_double(a, i, ((float*)(data))[i]);
      break;
    case GD_FLOAT64:
      for (i = 0; i < n; ++i)
        add_index_double(a, i, ((double*)(data))[i]);
      break;
    case GD_COMPLEX64:
      for (i = 0; i < n; ++i) {
        GDPHP_ZVAL_NULL(tmp);
        add_index_zval(a, i, gdphp_from_complex(RZ(tmp),
              ((float*)(data))[i * 2], ((float*)(data))[i * 2 + 1]));
        }
      break;
    case GD_COMPLEX128:
      for (i = 0; i < n; ++i) {
        GDPHP_ZVAL_NULL(tmp);
        add_index_zval(a, i, gdphp_from_complex(RZ(tmp),
              ((double*)(data))[i * 2], ((double*)(data))[i * 2 + 1]));
      }
      break;
    default:
      /* fill with nulls */
      for (i = 0; i < n; ++i)
        add_index_null(a, i);
      break;
  }

  dreturnvoid();
}

static zval *gdphp_from_data(zval *z, size_t n, gd_type_t data_type, void *data,
    int duplicate, int unpack)
{
  dtrace("%p, %zu, 0x%X, %p, %i, %i", z, n, data_type, data, duplicate, unpack);

  if (z == NULL)
    GDPHP_INIT_ZVAL(z);

  if (unpack) {
    array_init(z);
    gdphp_data_to_array(z, data, data_type, n);
    if (!duplicate)
      efree(data);
  } else { /* packed data */
    if (n == 0) {
      if (!duplicate)
        efree(data);
      ZVAL_EMPTY_STRING(z);
    } else
      GDPHP_COPY_DATA(z, data, n * GD_SIZE(data_type), duplicate);
  }

  dreturn("%p", z);
  return z;
}

/* convert a zval to a long, with null checking - unfortunately PHP
 * automatically converts nulls passed as longs into zero */
static long gdphp_long_from_zval_null(zval *z, long dflt)
{
  long v = dflt;

  dtrace("%p, %li", z, dflt);

  if (z != NULL && Z_TYPE_P(z) != IS_NULL) {
    convert_to_long(z); /* coerce */
    v = Z_LVAL_P(z);
  }

  dreturn("%li", v);
  return v;
}

/* get the unpack value */
static int gdphp_unpack(zval *z)
{
  int unpack;

  dtrace("%p", z);

  TSRMLS_FETCH();

  if (z != NULL && Z_TYPE_P(z) != IS_NULL) {
    convert_to_boolean(z); /* coerce */
    unpack = Z_BVAL_P(z);
  } else
    unpack = GDPHP_G(unpack);

  dreturn("%i", unpack);
  return unpack;
}

static void gdphp_init_globals(zend_getdata_globals *g)
{
  dtrace("%p", g);

  g->unpack = 0;
  g->degrade_complex = 1;

  dreturnvoid();
}

/* module init function */
PHP_MINIT_FUNCTION(getdata)
{
  dtracetsrm("%i, %i", type, module_number);

  /* initialise globals */
  ZEND_INIT_MODULE_GLOBALS(getdata, gdphp_init_globals, NULL);

  /* INI entries */
  REGISTER_INI_ENTRIES();

  /* Register constants */
  gdphp_register_constants(module_number);

  /* conveniences */
  GDPHP_REGISTER_LONG_CONSTANT("GD_FLOAT", GD_FLOAT64, module_number);
  GDPHP_REGISTER_LONG_CONSTANT("GD_INT", GDPHP_LONG_TYPE, module_number);

  /* dirfile resource */
  le_gdphp_dirfile = zend_register_list_destructors_ex(gdphp_dirfile_dtor, NULL,
      "Dirfile", module_number);
  le_gdphp_dirfile_persist = zend_register_list_destructors_ex(NULL,
      gdphp_dirfile_pdtor, "Dirfile", module_number);

  dreturn("%i", 0);
  return 0;
}

PHP_MSHUTDOWN_FUNCTION(getdata)
{
  dtracetsrm("%i, %i", type, module_number);

  UNREGISTER_INI_ENTRIES();

  dreturn("%i", SUCCESS);
  return SUCCESS;
}

PHP_MINFO_FUNCTION(getdata)
{
  dtracetsrm("%p", zend_module);

  php_info_print_table_start();
  php_info_print_table_header(2, "GetData support", "enabled");
  php_info_print_table_row(2, "Version", GD_GETDATA_VERSION);
  php_info_print_table_end();

  dreturnvoid();
}


/* BINDINGS */
PHP_FUNCTION(gd_add)
{
  zval *z;

  gd_entry_t E;
  DIRFILE *D;

  GDPHP_CONTEXTp(ctx,2);

  dtracephp();

  GDPHP_PARSED("a", &z);

  gdphp_to_entry(&E, z, NULL, 0, &ctx);

  /* no need to free entry strings */
  GDPHP_RETURN_BOOL(gd_add(D, &E));
}

PHP_FUNCTION(gd_add_alias)
{
  char *field_code, *target;
  GDPHP_SLEN field_code_len, target_len;
  GDPHP_LONG index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ss|l", &field_code, &field_code_len, &target, &target_len,
      &index);

  GDPHP_RETURN_BOOL(gd_add_alias(D, field_code, target, index));
}

PHP_FUNCTION(gd_add_bit)
{
  char *field_code, *in_field;
  GDPHP_SLEN field_code_len, in_field_len;
  GDPHP_LONG bitnum, index = 0;
  zval *znumbits = NULL;

  int numbits;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ssl|zl", &field_code, &field_code_len, &in_field, &in_field_len,
      &bitnum, &znumbits, &index);

  numbits = gdphp_long_from_zval_null(znumbits, 1);

  GDPHP_RETURN_BOOL(gd_add_bit(D, field_code, in_field, bitnum, numbits,
        index));
}

PHP_FUNCTION(gd_add_carray)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG data_type;
  zval *z1, *z2 = NULL, *z3 = NULL;
  DIRFILE *D;

  int r;
  struct gdphp_din din;
  int index = 0;

  dtracephp();

  GDPHP_PARSED("slz|zz", &field_code, &field_code_len, &data_type, &z1, &z2,
      &z3);

  /* allowed types for the last three parameters:
   *
   * 1: anything: data
   * 2: array, anything: data, index
   * 3: other, anything: type, data
   * 4: other, anything, anything: type, data, index
   */
  if (z2 == NULL) /* 1 */
    din = gdphp_convert_data(z1, NULL, 3, 4);
  else if (Z_TYPE_P(z1) == IS_ARRAY) { /* 2 */
    din = gdphp_convert_data(z1, NULL, 3, 3);
    convert_to_long(z2); /* coerce */
    index = Z_LVAL_P(z2);
  } else if (z3 == NULL) /* 3 */
    din = gdphp_convert_data(z1, z2, 3, 4);
  else { /* 4 */
    din = gdphp_convert_data(z1, z2, 3, 4);
    convert_to_long(z3); /* coerce */
    index = Z_LVAL_P(z3);
  }

  r = gd_add_carray(D, field_code, data_type, din.ns, din.type, din.data,
      index);

  if (din.free_din)
    efree(din.data);

  GDPHP_RETURN_BOOL(r);
}

PHP_FUNCTION(gd_add_sarray)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  zval *zdata;
  DIRFILE *D;

  int r;
  size_t n;
  const char **d;
  long index = 0;

  dtracephp();

  GDPHP_PARSED("szl", &field_code, &field_code_len, &zdata, &index);

  n = gdphp_convert_sarray(&d, zdata, 1);

  r = gd_add_sarray(D, field_code, n, d, index);
  efree(d);

  GDPHP_RETURN_BOOL(r);
}


PHP_FUNCTION(gd_add_const)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG data_type, index = 0;
  zval *z;

  DIRFILE *D;
  char data[16];
  GDPHP_CONTEXTp(ctx,3);

  dtracephp();

  GDPHP_PARSED("slz|l", &field_code, &field_code_len, &data_type, &z, &index);

  /* might as well convert to the storage type now */
  gdphp_to_datum(data, data_type, z, 1, &ctx);

  GDPHP_RETURN_BOOL(gd_add_const(D, field_code, data_type, data_type, data,
        index));
}

PHP_FUNCTION(gd_add_divide)
{
  char *field_code, *in1, *in2;
  GDPHP_SLEN field_code_len, in1_len, in2_len;
  GDPHP_LONG index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sss|l", &field_code, &field_code_len, &in1, &in1_len, &in2,
      &in2_len, &index);

  GDPHP_RETURN_BOOL(gd_add_divide(D, field_code, in1, in2, index));
}

PHP_FUNCTION(gd_add_indir)
{
  char *field_code, *in1, *in2;
  GDPHP_SLEN field_code_len, in1_len, in2_len;
  GDPHP_LONG index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sss|l", &field_code, &field_code_len, &in1, &in1_len, &in2,
      &in2_len, &index);

  GDPHP_RETURN_BOOL(gd_add_indir(D, field_code, in1, in2, index));
}

PHP_FUNCTION(gd_add_sindir)
{
  char *field_code, *in1, *in2;
  GDPHP_SLEN field_code_len, in1_len, in2_len;
  GDPHP_LONG index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sss|l", &field_code, &field_code_len, &in1, &in1_len, &in2,
      &in2_len, &index);

  GDPHP_RETURN_BOOL(gd_add_sindir(D, field_code, in1, in2, index));
}

PHP_FUNCTION(gd_add_lincom)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG index = 0;
  zval *zin, *zm, *zb;

  char *in[GD_MAX_LINCOM];
  double m[GD_MAX_LINCOM * 2];
  double b[GD_MAX_LINCOM * 2];
  int n;

  DIRFILE *D;
  GDPHP_CONTEXT(ctx);

  dtracephp();

  GDPHP_PARSED("saaa|l", &field_code, &field_code_len, &zin, &zm, &zb, &index);

  /* these don't return on error */
  ctx.p = 2;
  n = gdphp_convert_nsarray((char**)in, zin, 1, GD_MAX_LINCOM, &ctx);
  ctx.p = 3;
  gdphp_convert_cmparray(m, zm, n, n, 0, &ctx);
  ctx.p = 4;
  gdphp_convert_cmparray(b, zb, n, n, 0, &ctx);

  GDPHP_RETURN_BOOL(gd_add_clincom(D, field_code, n, (const char**)in,
        (GD_DCOMPLEXP_t)m, (GD_DCOMPLEXP_t)b, index));
}

PHP_FUNCTION(gd_add_linterp)
{
  char *field_code, *in_field, *table;
  GDPHP_SLEN field_code_len, in_field_len, table_len;
  GDPHP_LONG index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sss|l", &field_code, &field_code_len, &in_field, &in_field_len,
      &table, &table_len, &index);

  GDPHP_RETURN_BOOL(gd_add_linterp(D, field_code, in_field, table, index));
}

PHP_FUNCTION(gd_add_mplex)
{
  char *field_code, *in1, *in2;
  GDPHP_SLEN field_code_len, in1_len, in2_len;
  GDPHP_LONG count, period = 0, index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sssl|ll", &field_code, &field_code_len, &in1, &in1_len, &in2,
      &in2_len, &count, &period, &index);

  GDPHP_RETURN_BOOL(gd_add_mplex(D, field_code, in1, in2, count, period,
        index));
}

PHP_FUNCTION(gd_add_multiply)
{
  char *field_code, *in1, *in2;
  GDPHP_SLEN field_code_len, in1_len, in2_len;
  GDPHP_LONG index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sss|l", &field_code, &field_code_len, &in1, &in1_len, &in2,
      &in2_len, &index);

  GDPHP_RETURN_BOOL(gd_add_multiply(D, field_code, in1, in2, index));
}

PHP_FUNCTION(gd_add_phase)
{
  char *field_code, *in_field;
  GDPHP_SLEN field_code_len, in_field_len;
  GDPHP_LONG shift, index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ssl|l", &field_code, &field_code_len, &in_field, &in_field_len,
      &shift, &index);

  GDPHP_RETURN_BOOL(gd_add_phase(D, field_code, in_field, shift, index));
}

PHP_FUNCTION(gd_add_polynom)
{
  char *field_code, *in_field;
  GDPHP_SLEN field_code_len, in_field_len;
  GDPHP_LONG index = 0;
  zval *za;

  double a[2 * (GD_MAX_POLYORD + 1)];
  int o;

  DIRFILE *D;
  GDPHP_CONTEXTp(ctx,3);

  dtracephp();

  GDPHP_PARSED("ssa|l", &field_code, &field_code_len, &in_field, &in_field_len,
      &za, &index);

  /* doesn't return on error */
  o = gdphp_convert_cmparray(a, za, 2, GD_MAX_POLYORD + 1, 0, &ctx) - 1;

  GDPHP_RETURN_BOOL(gd_add_cpolynom(D, field_code, o, in_field,
        (GD_DCOMPLEXP_t)a, index));
}

PHP_FUNCTION(gd_add_raw)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG data_type, spf, index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sll|l", &field_code, &field_code_len, &data_type, &spf,
      &index);

  GDPHP_RETURN_BOOL(gd_add_raw(D, field_code, data_type, spf, index));
}

PHP_FUNCTION(gd_add_recip)
{
  char *field_code, *in_field;
  GDPHP_SLEN field_code_len, in_field_len;
  zval *zdividend;
  GDPHP_LONG index = 0;

  double dividend[2];
  DIRFILE *D;
  GDPHP_CONTEXTp(ctx,3);

  dtracephp();

  GDPHP_PARSED("ssz|l", &field_code, &field_code_len, &in_field, &in_field_len,
      &zdividend, &index);

  gdphp_to_datum(dividend, GD_COMPLEX128, zdividend, 1, &ctx);

  GDPHP_RETURN_BOOL(gd_add_crecip89(D, field_code, in_field, dividend, index));
}

PHP_FUNCTION(gd_add_sbit)
{
  char *field_code, *in_field;
  GDPHP_SLEN field_code_len, in_field_len;
  GDPHP_LONG bitnum, index = 0;
  zval *znumbits = NULL;

  int numbits;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ssl|zl", &field_code, &field_code_len, &in_field, &in_field_len,
      &bitnum, &znumbits, &index);

  numbits = gdphp_long_from_zval_null(znumbits, 1);

  GDPHP_RETURN_BOOL(gd_add_sbit(D, field_code, in_field, bitnum, numbits,
        index));
}

PHP_FUNCTION(gd_add_spec)
{
  char *spec;
  GDPHP_SLEN spec_len;
  GDPHP_LONG index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s|l", &spec, &spec_len, &index);

  GDPHP_RETURN_BOOL(gd_add_spec(D, spec, index));
}

PHP_FUNCTION(gd_add_string)
{
  char *field_code, *value;
  GDPHP_SLEN field_code_len, value_len;
  GDPHP_LONG index = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ss|l", &field_code, &field_code_len, &value, &value_len,
      &index);

  GDPHP_RETURN_BOOL(gd_add_string(D, field_code, value, index));
}

PHP_FUNCTION(gd_add_window)
{
  char *field_code, *in1, *in2;
  GDPHP_SLEN field_code_len, in1_len, in2_len;
  GDPHP_LONG windop, index = 0;
  zval *zthreshold;

  gd_triplet_t threshold;
  DIRFILE *D;
  GDPHP_CONTEXTp(ctx,5);

  dtracephp();

  GDPHP_PARSED("ssslz|l", &field_code, &field_code_len, &in1, &in1_len, &in2,
      &in2_len, &windop, &zthreshold, &index);

  gdphp_to_threshold(&threshold, windop, zthreshold, &ctx);

  GDPHP_RETURN_BOOL(gd_add_window(D, field_code, in1, in2, windop, threshold,
        index));
}

PHP_FUNCTION(gd_alias_target)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  const char *s;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  s = gd_alias_target(D, field_code);

  if (s == NULL)
    GDPHP_RETURN_F;

  dreturn("\"%s\"", s);
  GDPHP_RET_STR_COPY(s);
}

PHP_FUNCTION(gd_aliases)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  const char **fl;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  fl = gd_aliases(D, field_code);

  if (fl == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, fl, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_alter_affixes)
{
  zval *zprefix, *zsuffix;
  GDPHP_LONG index;
  
  char *prefix = NULL, *suffix = NULL;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("lzz", &index, &zprefix, &zsuffix);

  if (Z_TYPE_P(zprefix) != IS_NULL) {
    convert_to_string(zprefix); /* coerce */
    prefix = Z_STRVAL_P(zprefix);
  }

  if (Z_TYPE_P(zsuffix) != IS_NULL) {
    convert_to_string(zsuffix); /* coerce */
    suffix = Z_STRVAL_P(zsuffix);
  }

  GDPHP_RETURN_BOOL(gd_alter_affixes(D, index, prefix, suffix));
}

PHP_FUNCTION(gd_alter_bit)
{
  char *field_code, *in_field = NULL;
  GDPHP_SLEN field_code_len, in_field_len;
  GDPHP_LONG numbits = 0;
  zval *zbitnum = NULL;

  DIRFILE *D;
  char *in_fieldp;
  int bitnum;

  dtracephp();

  GDPHP_PARSED("s|szl", &field_code, &field_code_len, &in_field, &in_field_len,
      &zbitnum, &numbits);

  in_fieldp = gdphp_check_null_string(in_field);

  bitnum = gdphp_long_from_zval_null(zbitnum, -1);

  GDPHP_RETURN_BOOL(gd_alter_bit(D, field_code, in_fieldp, bitnum, numbits));
}

PHP_FUNCTION(gd_alter_carray)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG type = GD_NULL, len = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s|ll", &field_code, &field_code_len, &type, &len);

  GDPHP_RETURN_BOOL(gd_alter_carray(D, field_code, type, len));
}

PHP_FUNCTION(gd_alter_sarray)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG len = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s|l", &field_code, &field_code_len, &len);

  GDPHP_RETURN_BOOL(gd_alter_sarray(D, field_code, len));
}

PHP_FUNCTION(gd_alter_const)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG type = GD_NULL;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s|l", &field_code, &field_code_len, &type);

  GDPHP_RETURN_BOOL(gd_alter_const(D, field_code, type));
}

PHP_FUNCTION(gd_alter_encoding)
{
  GDPHP_LONG e, i;
  zend_bool recode = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ll|b", &e, &i, &recode);

  GDPHP_RETURN_BOOL(gd_alter_encoding(D, e, i, recode));
}

PHP_FUNCTION(gd_alter_endianness)
{
  GDPHP_LONG e, i;
  zend_bool recode = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ll|b", &e, &i, &recode);

  GDPHP_RETURN_BOOL(gd_alter_endianness(D, e, i, recode));
}

PHP_FUNCTION(gd_alter_entry)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  zend_bool recode = 0;
  zval *z;

  gd_entry_t E, old_E;
  DIRFILE *D;
  GDPHP_CONTEXTp(ctx,2);

  dtracephp();

  GDPHP_PARSED("sa|b", &field_code, &field_code_len, &z, &recode);

  /* find the old entry */
  if (gd_entry(D, field_code, &old_E))
    GDPHP_RETURN_F;

  gd_free_entry_strings(&old_E); /* don't need these */

  gdphp_to_entry(&E, z, &old_E, 1, &ctx);

  GDPHP_RETURN_BOOL(gd_alter_entry(D, field_code, &E, recode));
}

PHP_FUNCTION(gd_alter_frameoffset)
{
  GDPHP_LONG i, o;
  zend_bool recode = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ll|b", &o, &i, &recode);

  GDPHP_RETURN_BOOL(gd_alter_frameoffset64(D, o, i, recode));
}

PHP_FUNCTION(gd_alter_divide)
{
  char *field_code, *in_field1 = NULL, *in_field2 = NULL;
  GDPHP_SLEN field_code_len, in_field1_len, in_field2_len;
  
  DIRFILE *D;
  char *in_field1p, *in_field2p;

  dtracephp();

  GDPHP_PARSED("s|ss", &field_code, &field_code_len, &in_field1,
      &in_field1_len, &in_field2, &in_field2_len);

  in_field1p = gdphp_check_null_string(in_field1);
  in_field2p = gdphp_check_null_string(in_field2);
  
  GDPHP_RETURN_BOOL(gd_alter_divide(D, field_code, in_field1p, in_field2p));
}

PHP_FUNCTION(gd_alter_indir)
{
  char *field_code, *in_field1 = NULL, *in_field2 = NULL;
  GDPHP_SLEN field_code_len, in_field1_len, in_field2_len;
  
  DIRFILE *D;
  char *in_field1p, *in_field2p;

  dtracephp();

  GDPHP_PARSED("s|ss", &field_code, &field_code_len, &in_field1,
      &in_field1_len, &in_field2, &in_field2_len);

  in_field1p = gdphp_check_null_string(in_field1);
  in_field2p = gdphp_check_null_string(in_field2);
  
  GDPHP_RETURN_BOOL(gd_alter_indir(D, field_code, in_field1p, in_field2p));
}

PHP_FUNCTION(gd_alter_sindir)
{
  char *field_code, *in_field1 = NULL, *in_field2 = NULL;
  GDPHP_SLEN field_code_len, in_field1_len, in_field2_len;
  
  DIRFILE *D;
  char *in_field1p, *in_field2p;

  dtracephp();

  GDPHP_PARSED("s|ss", &field_code, &field_code_len, &in_field1,
      &in_field1_len, &in_field2, &in_field2_len);

  in_field1p = gdphp_check_null_string(in_field1);
  in_field2p = gdphp_check_null_string(in_field2);
  
  GDPHP_RETURN_BOOL(gd_alter_sindir(D, field_code, in_field1p, in_field2p));
}

PHP_FUNCTION(gd_alter_lincom)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG n = 0;
  zval *zin = NULL, *zm = NULL, *zb = NULL;

  DIRFILE *D;
  int min, max, nout;
  const char *in_fields[GD_MAX_LINCOM];
  const char **in_fieldsp = in_fields;
  double m[GD_MAX_LINCOM * 2];
  double b[GD_MAX_LINCOM * 2];
  GD_DCOMPLEXP_t mp = (GD_DCOMPLEXP_t)m;
  GD_DCOMPLEXP_t bp = (GD_DCOMPLEXP_t)b;
  GDPHP_CONTEXT(ctx);

  dtracephp();

  GDPHP_PARSED("s|lzzz", &field_code, &field_code_len, &n, &zin, &zm, &zb);

  /* handle passed nulls */
  if (n == 0) {
    min = 1;
    max = GD_MAX_LINCOM;
  } else
    min = max = n;

  if (zin == NULL || Z_TYPE_P(zin) == IS_NULL) {
    in_fieldsp = NULL;
  } else {
    int nout;
    min = n, max = n;
    ctx.p = 2;

    nout = gdphp_convert_nsarray((char**)in_fields, zin, 1, GD_MAX_LINCOM,
        &ctx);

    if (n == 0)
      min = max = n = nout;
  }

  if (zm == NULL || Z_TYPE_P(zm) == IS_NULL)
    mp = NULL;
  else {
    ctx.p = 3;

    nout = gdphp_convert_cmparray(m, zm, min, max, 0, &ctx);

    if (n == 0)
      min = max = n = nout;
  }

  if (zb == NULL || Z_TYPE_P(zb) == IS_NULL)
    bp = NULL;
  else {
    ctx.p = 4;
    nout = gdphp_convert_cmparray(b, zb, min, max, 0, &ctx);

    if (n == 0)
      n = nout;
  }

  GDPHP_RETURN_BOOL(gd_alter_clincom(D, field_code, n, in_fieldsp, mp, bp));
}

PHP_FUNCTION(gd_alter_linterp)
{
  char *field_code, *in_field = NULL, *table = NULL;
  GDPHP_SLEN field_code_len, in_field_len, table_len;
  zend_bool rename = 0;
  
  DIRFILE *D;
  char *in_fieldp, *tablep;

  dtracephp();

  GDPHP_PARSED("s|ssb", &field_code, &field_code_len, &in_field, &in_field_len,
      &table, &table_len, &rename);

  in_fieldp = gdphp_check_null_string(in_field);
  tablep = gdphp_check_null_string(table);
  
  GDPHP_RETURN_BOOL(gd_alter_linterp(D, field_code, in_fieldp, tablep, rename));
}

PHP_FUNCTION(gd_alter_mplex)
{
  char *field_code, *in_field1 = NULL, *in_field2 = NULL;
  GDPHP_SLEN field_code_len, in_field1_len, in_field2_len;
  zval *zcount_val = NULL, *zperiod = NULL;
  
  DIRFILE *D;
  char *in_field1p, *in_field2p;
  int count_val, period;

  dtracephp();

  GDPHP_PARSED("s|sszz", &field_code, &field_code_len, &in_field1,
      &in_field1_len, &in_field2, &in_field2_len, &zcount_val, &zperiod);

  in_field1p = gdphp_check_null_string(in_field1);
  in_field2p = gdphp_check_null_string(in_field2);
  period = gdphp_long_from_zval_null(zperiod, -1);
  
  /* there's no way to indicate no change to count_val in the C API */
  if (zcount_val == NULL || Z_TYPE_P(zcount_val) == IS_NULL) {
    gd_entry_t E;
    if (gd_entry(D, field_code, &E))
      GDPHP_RETURN_F;
    count_val = E.EN(mplex,count_val);
    gd_free_entry_strings(&E);
  } else {
    convert_to_long(zcount_val); /* coerce */
    count_val = Z_LVAL_P(zcount_val);
  }

  GDPHP_RETURN_BOOL(gd_alter_mplex(D, field_code, in_field1p, in_field2p,
        count_val, period));
}

PHP_FUNCTION(gd_alter_multiply)
{
  char *field_code, *in_field1 = NULL, *in_field2 = NULL;
  GDPHP_SLEN field_code_len, in_field1_len, in_field2_len;
  
  DIRFILE *D;
  char *in_field1p, *in_field2p;

  dtracephp();

  GDPHP_PARSED("s|ss", &field_code, &field_code_len, &in_field1,
      &in_field1_len, &in_field2, &in_field2_len);

  in_field1p = gdphp_check_null_string(in_field1);
  in_field2p = gdphp_check_null_string(in_field2);
  
  GDPHP_RETURN_BOOL(gd_alter_multiply(D, field_code, in_field1p, in_field2p));
}

PHP_FUNCTION(gd_alter_phase)
{
  char *field_code, *in_field = NULL;
  GDPHP_SLEN field_code_len, in_field_len;
  zval *zshift = NULL;
  
  DIRFILE *D;
  char *in_fieldp;
  gd_shift_t shift;

  dtracephp();

  GDPHP_PARSED("s|sz", &field_code, &field_code_len, &in_field, &in_field_len,
      &zshift);

  /* handle null shift */
  if (zshift == NULL || Z_TYPE_P(zshift) == IS_NULL) {
    /* there's no way to get specify no change in shift, so fetch the current
     * value */
    gd_entry_t E;
    if (gd_entry(D, field_code, &E))
      GDPHP_RETURN_F;
    shift = E.EN(phase,shift);
    gd_free_entry_strings(&E);
  } else {
    convert_to_long(zshift); /* coerce */
    shift = Z_LVAL_P(zshift);
  }

  in_fieldp = gdphp_check_null_string(in_field);
  
  GDPHP_RETURN_BOOL(gd_alter_phase(D, field_code, in_fieldp, shift));
}

PHP_FUNCTION(gd_alter_polynom)
{
  char *field_code, *in_field = NULL;
  GDPHP_SLEN field_code_len, in_field_len;
  GDPHP_LONG o = 0;
  zval *za = NULL;

  DIRFILE *D;
  int min, max, oout;
  char *in_fieldp;
  double a[(GD_MAX_POLYORD + 1) * 2];
  GD_DCOMPLEXP_t ap = (GD_DCOMPLEXP_t)a;
  GDPHP_CONTEXTp(ctx,4);

  dtracephp();

  GDPHP_PARSED("s|lsz", &field_code, &field_code_len, &o, &in_field,
      &in_field_len, &za);

  /* handle nulls */
  in_fieldp = gdphp_check_null_string(in_field);

  if (o == 0) {
    min = 2;
    max = GD_MAX_POLYORD + 1;
  } else
    min = max = o + 1;

  if (za == NULL || Z_TYPE_P(za) == IS_NULL)
    ap = NULL;
  else {
    oout = gdphp_convert_cmparray(a, za, min, max, 0, &ctx);

    if (o == 0)
      o = oout;
  }

  GDPHP_RETURN_BOOL(gd_alter_cpolynom(D, field_code, o, in_fieldp, ap));
}

PHP_FUNCTION(gd_alter_protection)
{
  GDPHP_LONG p, i;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ll", &p, &i);

  GDPHP_RETURN_BOOL(gd_alter_protection(D, p, i));
}

PHP_FUNCTION(gd_alter_raw)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG type = GD_NULL, spf = 0;
  zend_bool recode = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s|llb", &field_code, &field_code_len, &type, &spf, &recode);

  GDPHP_RETURN_BOOL(gd_alter_raw(D, field_code, type, spf, recode));
}

PHP_FUNCTION(gd_alter_recip)
{
  char *field_code, *in_field = NULL;
  GDPHP_SLEN field_code_len, in_field_len;
  zval *zdividend = NULL;

  DIRFILE *D;
  char *in_fieldp;
  double dividend[2];
  GDPHP_CONTEXTp(ctx,3);

  dtracephp();

  GDPHP_PARSED("s|sz", &field_code, &field_code_len, &in_field, &in_field_len,
      &zdividend);

  /* handle nulls */
  in_fieldp = gdphp_check_null_string(in_field);
  if (zdividend == NULL || Z_TYPE_P(zdividend) == IS_NULL)
    dividend[0] = dividend[1] = 0;
  else
    gdphp_to_datum(dividend, GD_COMPLEX128, zdividend, 1, &ctx);

  GDPHP_RETURN_BOOL(gd_alter_crecip89(D, field_code, in_fieldp, dividend));
}

PHP_FUNCTION(gd_alter_sbit)
{
  char *field_code, *in_field = NULL;
  GDPHP_SLEN field_code_len, in_field_len;
  GDPHP_LONG numbits = 0;
  zval *zbitnum = NULL;

  DIRFILE *D;
  char *in_fieldp;
  int bitnum;

  dtracephp();

  GDPHP_PARSED("s|szl", &field_code, &field_code_len, &in_field, &in_field_len,
      &zbitnum, &numbits);

  in_fieldp = gdphp_check_null_string(in_field);

  bitnum = gdphp_long_from_zval_null(zbitnum, -1);

  GDPHP_RETURN_BOOL(gd_alter_sbit(D, field_code, in_fieldp, bitnum, numbits));
}

PHP_FUNCTION(gd_alter_spec)
{
  char *spec;
  GDPHP_SLEN spec_len;
  zend_bool recode = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s|b", &spec, &spec_len, &recode);

  GDPHP_RETURN_BOOL(gd_alter_spec(D, spec, recode));
}

PHP_FUNCTION(gd_alter_window)
{
  char *field_code, *in1 = NULL, *in2 = NULL;
  GDPHP_SLEN field_code_len, in1_len, in2_len;
  GDPHP_LONG windop = GD_WINDOP_UNK;
  zval *zthreshold = NULL;

  gd_triplet_t threshold;
  char *in1p, *in2p;
  DIRFILE *D;
  GDPHP_CONTEXTp(ctx,5);

  dtracephp();

  GDPHP_PARSED("s|sslz", &field_code, &field_code_len, &in1, &in1_len, &in2,
      &in2_len, &windop, &zthreshold);

  in1p = gdphp_check_null_string(in1);
  in2p = gdphp_check_null_string(in2);

  /* there is no way to specify no change to the threshold in the C API */
  if (zthreshold == NULL || Z_TYPE_P(zthreshold) == IS_NULL) {
    gd_entry_t E;
    if (gd_entry(D, field_code, &E))
      GDPHP_RETURN_F;
    memcpy(&threshold, &E.EN(window,threshold), sizeof(threshold));
    gd_free_entry_strings(&E);
  } else
    gdphp_to_threshold(&threshold, windop, zthreshold, &ctx);

  GDPHP_RETURN_BOOL(gd_alter_window(D, field_code, in1p, in2p, windop,
        threshold));
}

PHP_FUNCTION(gd_bof)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  long bof;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  bof = (long)gd_bof64(D, field_code);

  if (bof < 0)
    GDPHP_RETURN_F;

  dreturn("%li", bof);
  RETURN_LONG(bof);
}

PHP_FUNCTION(gd_carray_len)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  size_t n;

  dtracephp();

  zend_error(E_DEPRECATED,
      "gd_carray_len() is deprecated.  Use gd_array_len() instead.");

  GDPHP_PARSED("s", &field_code, &field_code_len);

  n = gd_array_len(D, field_code);

  if (n == 0)
    GDPHP_RETURN_F;

  dreturn("%zu", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_array_len)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  size_t n;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  n = gd_array_len(D, field_code);

  if (n == 0)
    GDPHP_RETURN_F;

  dreturn("%zu", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_carrays)
{
  zval *zunpack = NULL;
  GDPHP_LONG data_type;
  GDPHP_ZVAL_NULL(tmp);

  DIRFILE *D;
  int unpack, i;
  const gd_carray_t *c;

  dtracephp();

  GDPHP_PARSED("l|z", &data_type, &zunpack);

  unpack = gdphp_unpack(zunpack);

  c = gd_carrays(D, data_type);

  if (c == NULL)
    GDPHP_RETURN_F;

  /* convert */
  array_init(return_value);

  for (i = 0; c[i].n; ++i)
    add_index_zval(return_value, i, gdphp_from_data(RZ(tmp), c[i].n, data_type,
          c[i].d, 1, unpack));

  dreturnvoid();
}

PHP_FUNCTION(gd_sarrays)
{
  DIRFILE *D;
  size_t i;
  const char ***s;
	GDPHP_ZVAL_NULL(tmp);

  dtracephp();

  GDPHP_PARSED_ONLY();

  s = gd_sarrays(D);

  if (s == NULL)
    GDPHP_RETURN_F;

  /* convert */
  array_init(return_value);

  for (i = 0; s[i]; ++i)
    add_index_zval(return_value, i, gdphp_to_string_array(RZ(tmp), s[i], 0));

  dreturnvoid();
}

PHP_FUNCTION(gd_close)
{
  zval *z;
  gdphp_dirfile *r;

  dtracephp();

  GDPHP_PARSE("r", &z);
  GDPHP_FETCH_DIRFILE(r, z);

  if (!r->persist) {
    if (gd_close(r->D))
      GDPHP_RETURN_F;

    /* delete the resource on success */
    r->D = NULL; /* avoid double close */
    gdphp_list_close(z);
  }

  GDPHP_RETURN_T;
}

PHP_FUNCTION(gd_constants)
{
  GDPHP_LONG data_type;
  zval *zunpack = NULL;

  void *data;
  unsigned n;
  DIRFILE *D;
  zend_bool unpack;
  GDPHP_CONTEXTp(ctx,2);

  dtracephp();

  GDPHP_PARSED("l|z", &data_type, &zunpack);

  unpack = gdphp_unpack(zunpack);
  gdphp_validate_type(data_type, &ctx);

  data = (void*)gd_constants(D, data_type);
  
  GDPHP_CHECK_ERROR(D);

  n = gd_nfields_by_type(D, GD_CONST_ENTRY);

  GDPHP_CHECK_ERROR(D);

  gdphp_from_data(return_value, n, data_type, data, 1, unpack);
  dreturn("%i", n);
}

PHP_FUNCTION(gd_delete)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG flags = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s|l", &field_code, &field_code_len, &flags);

  GDPHP_RETURN_BOOL(gd_delete(D, field_code, flags));
}

PHP_FUNCTION(gd_desync)
{
  GDPHP_LONG flags = 0;

  int r;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("|l", &flags);

  r = gd_desync(D, flags);

  if (r < 0)
    RETURN_NULL();

  GDPHP_RETURN_BOOL(!r);
}

PHP_FUNCTION(gd_dirfile_standards)
{
  zval *zversion = NULL;

  long version;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("|z", &zversion);

  version = gdphp_long_from_zval_null(zversion, GD_VERSION_CURRENT);

  version = gd_dirfile_standards(D, version);

  if (version < 0)
    GDPHP_RETURN_F;

  dreturn("%li", version);
  RETURN_LONG(version);
}

PHP_FUNCTION(gd_dirfilekey)
{
  zval *z;
  gdphp_dirfile *r;

  dtracephp();

  GDPHP_PARSE("r", &z);
  GDPHP_FETCH_DIRFILE(r, z);

  dreturn("\"%s\"", r->key);
  GDPHP_RET_STRL_COPY(r->key, r->key_len);
}

PHP_FUNCTION(gd_dirfilename)
{
  const char *s;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED_ONLY();

  s = gd_dirfilename(D);
  dreturn("\"%s\"", s);
  GDPHP_RET_STR_COPY(s);
}

PHP_FUNCTION(gd_discard)
{
  zval *z;
  gdphp_dirfile *r;

  dtracephp();

  GDPHP_PARSE("r", &z);
  GDPHP_FETCH_DIRFILE(r, z);

  if (!r->persist) {
    if (gd_discard(r->D))
      GDPHP_RETURN_F;

    /* delete the resource on success */
    r->D = NULL; /* avoid double close */
    gdphp_list_close(z);
  }

  GDPHP_RETURN_T;
}

PHP_FUNCTION(gd_encoding)
{
  GDPHP_LONG i = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("|l", &i);

  i = gd_encoding(D, i);

  if (i == 0)
    GDPHP_RETURN_F;

  dreturn("%lx", i);
  RETURN_LONG(i);
}

PHP_FUNCTION(gd_endianness)
{
  GDPHP_LONG i = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("|l", &i);

  i = gd_endianness(D, i);

  if (i == 0)
    GDPHP_RETURN_F;

  dreturn("%lx", i);
  RETURN_LONG(i);
}

PHP_FUNCTION(gd_entry)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  gd_entry_t E;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  if (gd_entry(D, field_code, &E))
    GDPHP_RETURN_F;

  gdphp_from_entry(return_value, &E);
  gd_free_entry_strings(&E);
  dreturn("%p", return_value);
}

PHP_FUNCTION(gd_entry_list)
{
  char *parent;
  GDPHP_SLEN parent_len;
  GDPHP_LONG type = 0, flags = 0;
  DIRFILE *D;

  const char **fl;
  char *parentp;
  dtracephp();

  GDPHP_PARSED("|sll", &parent, &parent_len, &type, &flags);

  parentp = gdphp_check_null_string(parent);

  fl = gd_entry_list(D, parentp, type, flags);

  if (fl == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, fl, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_match_entries)
{
  char *regex;
  GDPHP_SLEN regex_len;
  GDPHP_LONG type = 0, flags = 0, fragment = GD_ALL_FRAGMENTS;
  DIRFILE *D;

  const char **fl = NULL;
  char *regexp;
  dtracephp();

  GDPHP_PARSED("|slll", &regex, &regex_len, &fragment, &type, &flags);

  regexp = gdphp_check_null_string(regex);

  gd_match_entries(D, regexp, fragment, type, flags, &fl);

  if (fl == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, fl, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_entry_type)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  gd_entype_t entype;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  entype = gd_entry_type(D, field_code);

  if (entype == GD_NO_ENTRY)
    GDPHP_RETURN_F;

  dreturn("%i", entype);
  RETURN_LONG(entype);
}

PHP_FUNCTION(gd_eof)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  long eof;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  eof = (long)gd_eof64(D, field_code);

  if (eof < 0)
    GDPHP_RETURN_F;

  dreturn("%li", eof);
  RETURN_LONG(eof);
}

PHP_FUNCTION(gd_error)
{
  long e;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED_ONLY();

  e = gd_error(D);

  dreturn("%li", e);
  RETURN_LONG(e);
}

PHP_FUNCTION(gd_error_string)
{
  char *s;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED_ONLY();

  s = gd_error_string(D, NULL, 0);

  if (s) {
    GDPHP_RETVAL_STRING(s);
    dreturn("\"%s\"", s);
    free(s);
  } else
    GDPHP_RETURN_F;
}

PHP_FUNCTION(gd_field_list)
{
  DIRFILE *D;
  const char **fl;

  dtracephp();

  GDPHP_PARSED_ONLY();

  fl = gd_field_list(D);

  if (fl == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, fl, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_field_list_by_type)
{
  DIRFILE *D;
  const char **fl;
  GDPHP_LONG type;

  dtracephp();

  GDPHP_PARSED("l", &type);

  fl = gd_field_list_by_type(D, type);

  if (fl == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, fl, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_flags)
{
  GDPHP_LONG set = 0, reset = 0;
  unsigned long flags;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("|ll", &set, &reset);

  flags = gd_flags(D, set, reset);

  dreturn("0x%lX", flags);
  RETURN_LONG(flags);
}
  
PHP_FUNCTION(gd_flush)
{
  char *field_code = NULL;
  GDPHP_SLEN field_code_len;

  char *field_codep;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("|s", &field_code, &field_code_len);

  field_codep = gdphp_check_null_string(field_code);

  GDPHP_RETURN_BOOL(gd_flush(D, field_codep));
}

PHP_FUNCTION(gd_fragment_namespace)
{
  GDPHP_LONG index;
  const char *nsin = NULL;
  GDPHP_SLEN nsin_len;
  const char *nsout;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("l|s", &index, &nsin, &nsin_len);

  nsout = gd_fragment_namespace(D, index, nsin);

  if (nsout) {
    dreturn("\"%s\"", nsout);
    GDPHP_RET_STR_COPY(nsout);
  } else
    GDPHP_RETURN_F;
}

PHP_FUNCTION(gd_fragment_affixes)
{
  GDPHP_LONG index;

  DIRFILE *D;
  char *affixes[2] = {NULL, NULL};

  dtracephp();

  GDPHP_PARSED("l", &index);

  if (gd_fragment_affixes(D, index, affixes + 0, affixes + 1))
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, (const char**)affixes, 2);
  dreturn("{\"%s\", \"%s\"}", affixes[0], affixes[1]);
  free(affixes[0]);
  free(affixes[1]);
}

PHP_FUNCTION(gd_fragment_index)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  int i;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  i = gd_fragment_index(D, field_code);

  if (i < 0)
    GDPHP_RETURN_F;

  dreturn("%i", i);
  RETURN_LONG(i);
}

PHP_FUNCTION(gd_fragmentname)
{
  GDPHP_LONG i;
  const char *s;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("l", &i);

  s = gd_fragmentname(D, i);

  dreturn("\"%s\"", s);
  GDPHP_RET_STR_COPY(s);
}

PHP_FUNCTION(gd_frameoffset)
{
  GDPHP_LONG i = 0;
  
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("|l", &i);

  i = gd_frameoffset64(D, i);

  dreturn("%li", i);
  RETURN_LONG(i);
}

PHP_FUNCTION(gd_framenum)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  double value;
  GDPHP_LONG start = 0, stop = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sd|ll", &field_code, &field_code_len, &value, &start, &stop);

  value = gd_framenum_subset64(D, field_code, value, start, stop);

  /* check for NAN */
  if (value != value)
    GDPHP_RETURN_F;

  dreturn("%g", value);
  RETURN_DOUBLE(value);
}

PHP_FUNCTION(gd_get_carray)
{
  GDPHP_LONG data_type;
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG start = 0;
  zval *zlen = NULL, *zunpack = NULL;

  long len;
  void *data = NULL;
  DIRFILE *D;
  zend_bool unpack;
  GDPHP_CONTEXTp(ctx,3);

  dtracephp();

  GDPHP_PARSED("sl|lzz", &field_code, &field_code_len, &data_type, &start,
      &zlen, &zunpack);

  len = gdphp_long_from_zval_null(zlen, -1);
  unpack = gdphp_unpack(zunpack);

  if (data_type == GD_NULL) {
    if (len == -1)
      GDPHP_RETURN_BOOL(gd_get_carray(D, field_code, GD_NULL, NULL));
    else
      GDPHP_RETURN_BOOL(gd_get_carray_slice(D, field_code, start, len, GD_NULL,
            NULL));
  }

  if (len == -1) {
    len = gd_array_len(D, field_code);
    if (len == 0) /* error */
      GDPHP_RETURN_F;
		len -= start;
  }

  if (len <= 0) { /* explicit request for no data */
    dreturnvoid();
    if (unpack) {
      array_init(return_value);
      return;
    } else
      RETURN_EMPTY_STRING();
  }

  gdphp_validate_type(data_type, &ctx);
  data = emalloc(len * GD_SIZE(data_type));

  if (gd_get_carray_slice(D, field_code, start, len, data_type, data)) {
    efree(data);
    GDPHP_RETURN_F;
  }

  gdphp_from_data(return_value, len, data_type, data, 0, unpack);
  dreturn("%li", len);
}

PHP_FUNCTION(gd_get_sarray)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG start = 0;
  zval *zlen = NULL;

  long len;
  const char **data = NULL;
  DIRFILE *D;
  GDPHP_CONTEXTp(ctx,3);

  dtracephp();

  GDPHP_PARSED("s|lz", &field_code, &field_code_len, &start, &zlen);

  len = gdphp_long_from_zval_null(zlen, -1);

  if (len == -1) {
    len = gd_array_len(D, field_code);
    if (len == 0) /* error */
      GDPHP_RETURN_F;
		len -= start;
  }

	if (len <= 0) { /* explicit request for no data */
    dreturnvoid();
    array_init(return_value);
    return;
  }

  data = emalloc(len * sizeof(*data));

  if (gd_get_sarray_slice(D, field_code, start, len, data)) {
    efree(data);
    GDPHP_RETURN_F;
  }

  gdphp_to_string_array(return_value, data, len);
  efree(data);
  dreturn("%li", len);
}

PHP_FUNCTION(gd_get_constant)
{
  GDPHP_LONG data_type;
  char *field_code;
  GDPHP_SLEN field_code_len;

  char datum[16];
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sl", &field_code, &field_code_len, &data_type);

  if (gd_get_constant(D, field_code, data_type, datum))
    GDPHP_RETURN_F;

  if (data_type == GD_NULL)
    GDPHP_RETURN_T;

  gdphp_from_datum(return_value, datum, data_type);
  dreturnvoid();
}

PHP_FUNCTION(gd_get_string)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

#if ZEND_MODULE_API_NO >= 20151012
  zend_string *s;
#else
  char *s;
#endif
  char *p;

  size_t len;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  /* get length */
  len = gd_get_string(D, field_code, 0, NULL);

  if (gd_error(D))
    GDPHP_RETURN_F;
  else if (len == 0) {
    dreturn("%s", "");
    RETURN_EMPTY_STRING();
  }

  /* allocate a buffer and get the string */
#if ZEND_MODULE_API_NO >= 20151012
  s = zend_string_alloc(len - 1, 0);
  p = ZSTR_VAL(s);
#else
  p = s = emalloc(len);
#endif

  if (gd_get_string(D, field_code, len, p) != len) {
#if ZEND_MODULE_API_NO >= 20151012
    zend_string_release(s);
#else
    efree(s);
#endif

    GDPHP_RETURN_F;
  }

  dreturn("\"%s\"", p);
#if ZEND_MODULE_API_NO >= 20151012
  RETURN_STR(s);
#else
  RETURN_STRINGL(s, len - 1, 0);
#endif
}

PHP_FUNCTION(gd_getdata)
{
  GDPHP_LONG first_frame, first_sample, num_frames, num_samples;
  char *field_code;
  GDPHP_SLEN field_code_len;
  size_t ns;
  zval *zunpack = NULL;

  GDPHP_LONG data_type = GD_UNKNOWN;
  DIRFILE *D;
  zend_bool unpack;
  size_t n = 0;
  GDPHP_CONTEXTp(ctx,6);
  void *data;

  dtracephp();

  GDPHP_PARSED("sllll|lz", &field_code, &field_code_len, &first_frame,
      &first_sample, &num_frames, &num_samples, &data_type, &zunpack);

  unpack = gdphp_unpack(zunpack);

  if (data_type == GD_NULL) {
    n = gd_getdata(D, field_code, first_frame, first_sample, num_frames,
        num_samples, GD_NULL, NULL);

    GDPHP_CHECK_ERROR(D);

    dreturn("%" PRIuSIZE, n);
    RETURN_LONG(n);
  } else {
    /* figure out how much data we have */
    if (num_frames > 0) {
      unsigned spf = gd_spf(D, field_code);
      if (spf == 0)
        GDPHP_RETURN_F;
      ns = num_frames * spf + num_samples;
    } else
      ns = num_samples;

		if (gd_entry_type(D, field_code) == GD_SINDIR_ENTRY) {
			const char **data = emalloc(ns * sizeof(*data));
			n = gd_getdata(D, field_code, first_frame, first_sample, 0, ns, GD_STRING,
				data);

			if (gd_error(D)) {
				efree(data);
				GDPHP_RETURN_F;
      }

      gdphp_to_string_array(return_value, data, n);
      efree(data);
    } else {
      /* get the type, if needed */
      if (data_type == GD_UNKNOWN)
        data_type = gd_native_type(D, field_code);

      /* allocate a buffer */
      gdphp_validate_type(data_type, &ctx);
      data = emalloc(ns * GD_SIZE(data_type));

      n = gd_getdata(D, field_code, first_frame, first_sample, 0, ns, data_type,
          data);

      if (gd_error(D)) {
        efree(data);
        GDPHP_RETURN_F;
      }

      gdphp_from_data(return_value, n, data_type, data, 0, unpack);
    }
  }

  dreturn("%" PRIuSIZE, n);
}

PHP_FUNCTION(gd_hidden)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  int hidden;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  hidden = gd_hidden(D, field_code);

  if (hidden < 0) {
    dreturnvoid();
    RETURN_NULL();
  }

  GDPHP_RETURN_BOOL(!hidden);
}

PHP_FUNCTION(gd_hide)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  GDPHP_RETURN_BOOL(gd_hide(D, field_code));
}

PHP_FUNCTION(gd_include_affix)
{
  char *path, *prefix = NULL, *suffix = NULL;
  GDPHP_SLEN path_len, prefix_len, suffix_len;
  GDPHP_LONG i, parent, flags = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sl|ssl", &path, &path_len, &parent, &prefix, &prefix_len,
      &suffix, &suffix_len, &flags);

  i = gd_include_affix(D, path, parent, prefix, suffix, flags);

  if (i < 0)
    GDPHP_RETURN_F;

  dreturn("%li", i);
  RETURN_LONG(i);
}

PHP_FUNCTION(gd_include)
{
  char *path, *ns = NULL;
  GDPHP_SLEN path_len, ns_len;
  GDPHP_LONG i, parent, flags = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sl|sl", &path, &path_len, &parent, &ns, &ns_len, &flags);

  i = gd_include_ns(D, path, parent, ns, flags);

  if (i < 0)
    GDPHP_RETURN_F;

  dreturn("%li", i);
  RETURN_LONG(i);
}

PHP_FUNCTION(gd_invalid_dirfile)
{
  gdphp_dirfile *r;

  dtracephp();

  if (zend_parse_parameters_none() != SUCCESS) {
    GDPHP_RETURN_F;
  }

  /* create the resource */
  r = emalloc(sizeof(gdphp_dirfile));
  memset(r, 0, sizeof(gdphp_dirfile));

  r->D = gd_invalid_dirfile();
  r->persist = 0;

  GDPHP_RET_RES(r, le_gdphp_dirfile);
  dreturn("%p", r);
}

PHP_FUNCTION(gd_linterp_tablename)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  char *s;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  s = gd_linterp_tablename(D, field_code);

  if (s) {
    GDPHP_RETVAL_STRING(s);
    dreturn("\"%s\"", s);
    free(s);
  } else
    GDPHP_RETURN_F;
}

PHP_FUNCTION(gd_madd)
{
  char *parent;
  GDPHP_SLEN parent_len;
  zval *z;

  gd_entry_t E;
  DIRFILE *D;

  GDPHP_CONTEXT(ctx);

  dtracephp();

  GDPHP_PARSED("as", &z, &parent, &parent_len);

  ctx.p = 2;
  gdphp_to_entry(&E, z, NULL, 1, &ctx);

  /* no need to free entry strings */
  GDPHP_RETURN_BOOL(gd_madd(D, &E, parent));
}

PHP_FUNCTION(gd_madd_alias)
{
  char *field_code, *target, *parent;
  GDPHP_SLEN field_code_len, target_len, parent_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sss", &parent, &parent_len, &field_code, &field_code_len,
      &target, &target_len);

  GDPHP_RETURN_BOOL(gd_madd_alias(D, parent, field_code, target));
}

PHP_FUNCTION(gd_madd_bit)
{
  char *field_code, *in_field, *parent;
  GDPHP_SLEN field_code_len, in_field_len, parent_len;
  GDPHP_LONG bitnum;
  long numbits;
  zval *znumbits;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sssl|z", &parent, &parent_len, &field_code, &field_code_len,
      &in_field, &in_field_len, &bitnum, &znumbits);

  numbits = gdphp_long_from_zval_null(znumbits, 1);

  GDPHP_RETURN_BOOL(gd_madd_bit(D, parent, field_code, in_field, bitnum,
        numbits));
}

PHP_FUNCTION(gd_madd_carray)
{
  char *field_code, *parent;
  GDPHP_SLEN field_code_len, parent_len;
  GDPHP_LONG data_type;
  zval *zdata1, *zdata2 = NULL;
  DIRFILE *D;

  int r;
  struct gdphp_din din;

  dtracephp();

  GDPHP_PARSED("sslz|z", &parent, &parent_len, &field_code, &field_code_len,
      &data_type, &zdata1, &zdata2);

  din = gdphp_convert_data(zdata1, zdata2, 4, 5);

  r = gd_madd_carray(D, parent, field_code, data_type, din.ns, din.type,
      din.data);

  if (din.free_din)
    efree(din.data);

  GDPHP_RETURN_BOOL(r);
}

PHP_FUNCTION(gd_madd_sarray)
{
  char *field_code, *parent;
  GDPHP_SLEN field_code_len, parent_len;
  zval *zdata;
  DIRFILE *D;

  int r;
  size_t n;
  const char **d;

  dtracephp();

  GDPHP_PARSED("ssz", &parent, &parent_len, &field_code, &field_code_len,
      &zdata);

  n = gdphp_convert_sarray(&d, zdata, 2);

  r = gd_madd_sarray(D, parent, field_code, n, d);
  efree(d);

  GDPHP_RETURN_BOOL(r);
}

PHP_FUNCTION(gd_madd_const)
{
  char *field_code, *parent;
  GDPHP_SLEN field_code_len, parent_len;
  GDPHP_LONG data_type;
  zval *z;

  DIRFILE *D;
  char data[16];
  GDPHP_CONTEXTp(ctx,3);

  dtracephp();

  GDPHP_PARSED("sslz", &parent, &parent_len, &field_code, &field_code_len,
      &data_type, &z);

  /* might as well convert to the storage type now */
  gdphp_to_datum(data, data_type, z, 1, &ctx);

  GDPHP_RETURN_BOOL(gd_madd_const(D, parent, field_code, data_type, data_type,
        data));
}

PHP_FUNCTION(gd_madd_divide)
{
  char *field_code, *in1, *in2, *parent;
  GDPHP_SLEN field_code_len, in1_len, in2_len, parent_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ssss", &parent, &parent_len, &field_code, &field_code_len, &in1,
      &in1_len, &in2, &in2_len);

  GDPHP_RETURN_BOOL(gd_madd_divide(D, parent, field_code, in1, in2));
}

PHP_FUNCTION(gd_madd_indir)
{
  char *field_code, *in1, *in2, *parent;
  GDPHP_SLEN field_code_len, in1_len, in2_len, parent_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ssss", &parent, &parent_len, &field_code, &field_code_len, &in1,
      &in1_len, &in2, &in2_len);

  GDPHP_RETURN_BOOL(gd_madd_indir(D, parent, field_code, in1, in2));
}

PHP_FUNCTION(gd_madd_sindir)
{
  char *field_code, *in1, *in2, *parent;
  GDPHP_SLEN field_code_len, in1_len, in2_len, parent_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ssss", &parent, &parent_len, &field_code, &field_code_len, &in1,
      &in1_len, &in2, &in2_len);

  GDPHP_RETURN_BOOL(gd_madd_sindir(D, parent, field_code, in1, in2));
}

PHP_FUNCTION(gd_madd_lincom)
{
  char *field_code, *parent;
  GDPHP_SLEN field_code_len, parent_len;
  zval *zin, *zm, *zb;

  char *in[GD_MAX_LINCOM];
  double m[GD_MAX_LINCOM * 2];
  double b[GD_MAX_LINCOM * 2];
  int n;

  DIRFILE *D;
  GDPHP_CONTEXT(ctx);

  dtracephp();

  GDPHP_PARSED("ssaaa", &parent, &parent_len, &field_code, &field_code_len,
      &zin, &zm, &zb);

  /* these don't return on error */
  ctx.p = 2;
  n = gdphp_convert_nsarray((char**)in, zin, 1, GD_MAX_LINCOM, &ctx);
  ctx.p = 3;
  gdphp_convert_cmparray(m, zm, n, n, 0, &ctx);
  ctx.p = 4;
  gdphp_convert_cmparray(b, zb, n, n, 0, &ctx);

  GDPHP_RETURN_BOOL(gd_madd_clincom(D, parent, field_code, n, (const char**)in,
        (GD_DCOMPLEXP_t)m, (GD_DCOMPLEXP_t)b));
}

PHP_FUNCTION(gd_madd_linterp)
{
  char *field_code, *in_field, *table, *parent;
  GDPHP_SLEN field_code_len, in_field_len, table_len, parent_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ssss", &parent, &parent_len, &field_code, &field_code_len,
      &in_field, &in_field_len, &table, &table_len);

  GDPHP_RETURN_BOOL(gd_madd_linterp(D, parent, field_code, in_field, table));
}

PHP_FUNCTION(gd_madd_mplex)
{
  char *field_code, *in1, *in2, *parent;
  GDPHP_SLEN field_code_len, in1_len, in2_len, parent_len;
  GDPHP_LONG count, period = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ssssl|l", &parent, &parent_len, &field_code, &field_code_len,
      &in1, &in1_len, &in2, &in2_len, &count, &period);

  GDPHP_RETURN_BOOL(gd_madd_mplex(D, parent, field_code, in1, in2, count,
        period));
}

PHP_FUNCTION(gd_madd_multiply)
{
  char *field_code, *in1, *in2, *parent;
  GDPHP_SLEN field_code_len, in1_len, in2_len, parent_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ssss", &parent, &parent_len, &field_code, &field_code_len, &in1,
      &in1_len, &in2, &in2_len);

  GDPHP_RETURN_BOOL(gd_madd_multiply(D, parent, field_code, in1, in2));
}

PHP_FUNCTION(gd_madd_phase)
{
  char *field_code, *in_field, *parent;
  GDPHP_SLEN field_code_len, in_field_len, parent_len;
  GDPHP_LONG shift;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sssl", &parent, &parent_len, &field_code, &field_code_len,
      &in_field, &in_field_len, &shift);

  GDPHP_RETURN_BOOL(gd_madd_phase(D, parent, field_code, in_field, shift));
}

PHP_FUNCTION(gd_madd_polynom)
{
  char *field_code, *in_field, *parent;
  GDPHP_SLEN field_code_len, in_field_len, parent_len;
  zval *za;

  double a[2 * (GD_MAX_POLYORD + 1)];
  int o;

  DIRFILE *D;
  GDPHP_CONTEXTp(ctx,3);

  dtracephp();

  GDPHP_PARSED("sssa", &parent, &parent_len, &field_code, &field_code_len,
      &in_field, &in_field_len, &za);

  /* doesn't return on error */
  o = gdphp_convert_cmparray(a, za, 2, GD_MAX_POLYORD + 1, 0, &ctx) - 1;

  GDPHP_RETURN_BOOL(gd_madd_cpolynom(D, parent, field_code, o, in_field,
        (GD_DCOMPLEXP_t)a));
}

PHP_FUNCTION(gd_madd_recip)
{
  char *field_code, *in_field, *parent;
  GDPHP_SLEN field_code_len, in_field_len, parent_len;
  zval *zdividend;

  double dividend[2];
  DIRFILE *D;
  GDPHP_CONTEXTp(ctx,4);

  dtracephp();

  GDPHP_PARSED("sssz", &parent, &parent_len, &field_code, &field_code_len,
      &in_field, &in_field_len, &zdividend);

  gdphp_to_datum(dividend, GD_COMPLEX128, zdividend, 1, &ctx);

  GDPHP_RETURN_BOOL(gd_madd_crecip89(D, parent, field_code, in_field,
        dividend));
}

PHP_FUNCTION(gd_madd_sbit)
{
  char *field_code, *in_field, *parent;
  GDPHP_SLEN field_code_len, in_field_len, parent_len;
  GDPHP_LONG bitnum;
  long numbits;
  zval *znumbits;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sssl|z", &parent, &parent_len, &field_code, &field_code_len,
      &in_field, &in_field_len, &bitnum, &znumbits);

  numbits = gdphp_long_from_zval_null(znumbits, 1);

  GDPHP_RETURN_BOOL(gd_madd_sbit(D, parent, field_code, in_field, bitnum,
        numbits));
}

PHP_FUNCTION(gd_madd_spec)
{
  char *spec, *parent;
  GDPHP_SLEN spec_len, parent_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ss", &spec, &spec_len, &parent, &parent_len);

  GDPHP_RETURN_BOOL(gd_madd_spec(D, spec, parent));
}

PHP_FUNCTION(gd_madd_string)
{
  char *field_code, *value, *parent;
  GDPHP_SLEN field_code_len, value_len, parent_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sss", &parent, &parent_len, &field_code, &field_code_len,
      &value, &value_len);

  GDPHP_RETURN_BOOL(gd_madd_string(D, parent, field_code, value));
}

PHP_FUNCTION(gd_madd_window)
{
  char *field_code, *in1, *in2, *parent;
  GDPHP_SLEN field_code_len, in1_len, in2_len, parent_len;
  GDPHP_LONG windop;
  zval *zthreshold;

  gd_triplet_t threshold;
  DIRFILE *D;
  GDPHP_CONTEXTp(ctx,6);

  dtracephp();

  GDPHP_PARSED("sssslz", &parent, &parent_len, &field_code, &field_code_len,
      &in1, &in1_len, &in2, &in2_len, &windop, &zthreshold);

  gdphp_to_threshold(&threshold, windop, zthreshold, &ctx);

  GDPHP_RETURN_BOOL(gd_madd_window(D, parent, field_code, in1, in2, windop,
        threshold));
}

PHP_FUNCTION(gd_malter_spec)
{
  char *spec, *parent;
  GDPHP_SLEN spec_len, parent_len;
  zend_bool recode = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ss|b", &spec, &spec_len, &parent, &parent_len, &recode);

  GDPHP_RETURN_BOOL(gd_malter_spec(D, spec, parent, recode));
}

PHP_FUNCTION(gd_mcarrays)
{
  char *parent;
  GDPHP_SLEN parent_len;
  zval *zunpack = NULL;
  GDPHP_LONG data_type;
  GDPHP_ZVAL_NULL(tmp);

  DIRFILE *D;
  int unpack, i;
  const gd_carray_t *c;

  dtracephp();

  GDPHP_PARSED("sl|z", &parent, &parent_len, &data_type, &zunpack);

  unpack = gdphp_unpack(zunpack);

  c = gd_mcarrays(D, parent, data_type);

  if (c == NULL)
    GDPHP_RETURN_F;

  /* convert */
  array_init(return_value);

  for (i = 0; c[i].n; ++i)
    add_index_zval(return_value, i, gdphp_from_data(RZ(tmp), c[i].n, data_type,
          c[i].d, 1, unpack));

  dreturnvoid();
}

PHP_FUNCTION(gd_msarrays)
{
  char *parent;
  GDPHP_SLEN parent_len;
	GDPHP_ZVAL_NULL(tmp);

  DIRFILE *D;
  int i;
  const char ***s;

  dtracephp();

  GDPHP_PARSED("s", &parent, &parent_len);

  s = gd_msarrays(D, parent);

  if (s == NULL)
    GDPHP_RETURN_F;

  /* convert */
  array_init(return_value);

  for (i = 0; s[i]; ++i)
    add_index_zval(return_value, i, gdphp_to_string_array(RZ(tmp), s[i], 0));

  dreturnvoid();
}

PHP_FUNCTION(gd_mconstants)
{
  char *parent;
  GDPHP_SLEN parent_len;
  GDPHP_LONG data_type;
  zval *zunpack = NULL;

  void *data;
  unsigned n;
  DIRFILE *D;
  zend_bool unpack;
  GDPHP_CONTEXTp(ctx,2);

  dtracephp();

  GDPHP_PARSED("sl|z", &parent, &parent_len, &data_type, &zunpack);

  unpack = gdphp_unpack(zunpack);
  gdphp_validate_type(data_type, &ctx);

  data = (void*)gd_mconstants(D, parent, data_type);
  
  GDPHP_CHECK_ERROR(D);

  n = gd_nmfields_by_type(D, parent, GD_CONST_ENTRY);

  GDPHP_CHECK_ERROR(D);

  gdphp_from_data(return_value, n, data_type, data, 1, unpack);
  dreturn("%i", n);
}

PHP_FUNCTION(gd_metaflush)
{
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED_ONLY();

  GDPHP_RETURN_BOOL(gd_metaflush(D));
}

PHP_FUNCTION(gd_mfield_list)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  const char **fl;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  fl = gd_mfield_list(D, field_code);

  if (fl == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, fl, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_mfield_list_by_type)
{
  GDPHP_LONG type;
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  const char **fl;

  dtracephp();

  GDPHP_PARSED("sl", &field_code, &field_code_len, &type);

  fl = gd_mfield_list_by_type(D, field_code, type);

  if (fl == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, fl, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_move)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG new_fragment;
  GDPHP_LONG flags = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sl|l", &field_code, &field_code_len, &new_fragment, &flags);

  GDPHP_RETURN_BOOL(gd_move(D, field_code, new_fragment, flags));
}

PHP_FUNCTION(gd_mplex_lookback)
{
  GDPHP_LONG lookback;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("l", &lookback);

  gd_mplex_lookback(D, lookback);

  GDPHP_RETURN_T;
}

PHP_FUNCTION(gd_mstrings)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  const char **l;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  l = gd_mstrings(D, field_code);

  if (l == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, l, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_mvector_list)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  const char **fl;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  fl = gd_mvector_list(D, field_code);

  if (fl == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, fl, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_naliases)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  long n;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  n = gd_naliases(D, field_code);

  if (n == 0)
    GDPHP_RETURN_F;

  dreturn("%li", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_native_type)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  gd_type_t t;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  t = gd_native_type(D, field_code);

  if (t == GD_UNKNOWN)
    GDPHP_RETURN_F;

  dreturn("%u", t);
  RETURN_LONG(t);
}

PHP_FUNCTION(gd_nentries)
{
  char *parent;
  GDPHP_SLEN parent_len;
  GDPHP_LONG n, type = 0, flags = 0;
  DIRFILE *D;

  char *parentp;
  dtracephp();

  GDPHP_PARSED("|sll", &parent, &parent_len, &type, &flags);

  parentp = gdphp_check_null_string(parent);

  n = gd_nentries(D, parentp, type, flags);

  GDPHP_CHECK_ERROR(D);

  dreturn("%li", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_nfields)
{
  long n;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED_ONLY();

  n = gd_nfields(D);

  GDPHP_CHECK_ERROR(D);

  dreturn("%li", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_nfields_by_type)
{
  GDPHP_LONG n, type;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("l", &type);

  n = gd_nfields_by_type(D, type);

  GDPHP_CHECK_ERROR(D);

  dreturn("%li", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_nfragments)
{
  long n;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED_ONLY();

  n = gd_nfragments(D);

  GDPHP_CHECK_ERROR(D);

  dreturn("%li", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_nframes)
{
  long n;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED_ONLY();

  n = gd_nframes(D);

  GDPHP_CHECK_ERROR(D);

  dreturn("%li", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_nmfields)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  long n;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  n = gd_nmfields(D, field_code);

  GDPHP_CHECK_ERROR(D);

  dreturn("%li", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_nmfields_by_type)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  GDPHP_LONG n, type;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("sl", &field_code, &field_code_len, &type);

  n = gd_nmfields_by_type(D, field_code, type);

  GDPHP_CHECK_ERROR(D);

  dreturn("%li", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_nmvectors)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  long n;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  n = gd_nmvectors(D, field_code);

  GDPHP_CHECK_ERROR(D);

  dreturn("%li", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_nvectors)
{
  long n;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED_ONLY();

  n = gd_nvectors(D);

  GDPHP_CHECK_ERROR(D);

  dreturn("%li", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_open)
{
  GDPHP_LONG flags = GD_RDONLY;
  char *dirfilename;
  GDPHP_SLEN len;
  gdphp_dirfile *r;
  char *callback = NULL;
  GDPHP_SLEN callback_len = -1;
  GDPHP_ZVALP callback_data = NULL;

  dtracephp();

  /* parse input */
  GDPHP_PARSE("s|ls" ZPP_Z, &dirfilename, &len, &flags, &callback,
      &callback_len, &callback_data);

  r = gdphp_open(dirfilename, len, flags, callback, callback_len,
      callback_data, 0);

  /* return */
  GDPHP_RET_RES(r, le_gdphp_dirfile);
  dreturn("%p", r);
}

PHP_FUNCTION(gd_parent_fragment)
{
  GDPHP_LONG i;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("l", &i);

  i = gd_parent_fragment(D, i);

  if (i < 0)
    GDPHP_RETURN_F;

  dreturn("%lx", i);
  RETURN_LONG(i);
}

PHP_FUNCTION(gd_popen)
{
  GDPHP_LONG flags = GD_RDONLY;
  char *dirfilename;
  GDPHP_SLEN len;
  gdphp_dirfile *r;
  GDPHP_RESOURCE *le, new_le;
  char *callback = NULL;
  GDPHP_SLEN callback_len;
  GDPHP_ZVALP callback_data = NULL;

  dtracephp();

  /* parse input */
  GDPHP_PARSE("s|ls" ZPP_Z, &dirfilename, &len, &flags, &callback,
      &callback_len, &callback_data);

  /* look for an existing dirfile */
  if (GDPHP_FIND_RES(le, &EG(persistent_list), dirfilename, len)) {
    GDPHP_RET_RES(le->ptr, le_gdphp_dirfile_persist);
    dreturn("%p", le->ptr);
    return;
  }

  /* nope, open */
  r = gdphp_open(dirfilename, len, flags, callback, callback_len,
      callback_data, 1);

  /* register and store */
  GDPHP_RET_RES(r, le_gdphp_dirfile_persist);
  new_le.ptr = r;
  new_le.type = le_gdphp_dirfile_persist;
  GDPHP_HASH_ADD(&EG(persistent_list), dirfilename, len, new_le);

  dreturn("%p", r);
}

PHP_FUNCTION(gd_protection)
{
  GDPHP_LONG i;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("l", &i);

  i = gd_protection(D, i);

  if (i == 0)
    GDPHP_RETURN_F;

  dreturn("%lx", i);
  RETURN_LONG(i);
}

PHP_FUNCTION(gd_put_carray)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  zval *z1, *z2 = NULL, *z3 = NULL;

  DIRFILE *D;
  int r;
  long start = 0;
  struct gdphp_din din = { NULL, GD_NULL, 0, 0};

  dtracephp();

  GDPHP_PARSED("sz|zz", &field_code, &field_code_len, &z1, &z2, &z3);

  /* allowed types for the last three parameters:
   *
   * 1: anything: just data
   * 2: long, string: just data
   * 3: long, other, [anything]: start and data
   * 4: null, anything, [anything]: just data
   * 5: other, anything, anything: forbidden
   * 6: other, anything: just data
   */
  if (z2 == NULL)  /* 1 */
    din = gdphp_convert_data(z1, NULL, 2, 3);
  else if (Z_TYPE_P(z1) == IS_LONG) {
    if (z2 && Z_TYPE_P(z2) == IS_STRING && z3 == NULL) { /* 2 */
      din = gdphp_convert_data(z1, z2, 2, 3);
    } else { /* 3 */
      start = Z_LVAL_P(z1);
      din = gdphp_convert_data(z2, z3, 3, 4);
    }
  } else if (Z_TYPE_P(z1) == IS_NULL) /* 4 */
    din = gdphp_convert_data(z2, z3, 3, 4);
  else if (z3) { /* 5 */
    GDPHP_CONTEXTp(ctx, 2);
    GDPHP_DIE(&ctx, "expected starting index or input data");
  } else /* 6 */
    din = gdphp_convert_data(z1, z2, 2, 3);

  r = gd_put_carray_slice(D, field_code, start, din.ns, din.type, din.data);

  if (din.free_din)
    efree(din.data);

  GDPHP_RETURN_BOOL(r);
}

PHP_FUNCTION(gd_put_sarray)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  zval *z1, *z2 = NULL;

  DIRFILE *D;
  int r;
  long start = 0;
  size_t n;
  const char **d;

  dtracephp();

  GDPHP_PARSED("sz|z", &field_code, &field_code_len, &z1, &z2);

  /* if z2 exists, it's the data and z1 is the start, otherwise z1 is the data
   */
  if (z2 == NULL)
    n = gdphp_convert_sarray(&d, z1, 2);
  else {
    start = Z_LVAL_P(z1);
    n = gdphp_convert_sarray(&d, z2, 3);
  }

  r = gd_put_sarray_slice(D, field_code, start, n, d);
  efree(d);

  GDPHP_RETURN_BOOL(r);
}

PHP_FUNCTION(gd_put_constant)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  zval *z;

  DIRFILE *D;
  char datum[16];
  gd_type_t t;
  GDPHP_CONTEXTp(ctx, 2);

  dtracephp();

  GDPHP_PARSED("sz", &field_code, &field_code_len, &z);

  t = gdphp_to_datum_and_type(datum, z, &ctx);

  GDPHP_RETURN_BOOL(gd_put_constant(D, field_code, t, datum));
}

PHP_FUNCTION(gd_put_string)
{
  char *field_code, *value;
  GDPHP_SLEN field_code_len, value_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ss", &field_code, &field_code_len, &value, &value_len);

  gd_put_string(D, field_code, value);
  GDPHP_RETURN_ERROR(D);
}

PHP_FUNCTION(gd_putdata)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG first_frame, first_sample;
  zval *zdata1, *zdata2 = NULL;
  DIRFILE *D;
  size_t n;

  struct gdphp_din din;

  dtracephp();

  GDPHP_PARSED("sllz|z", &field_code, &field_code_len, &first_frame,
      &first_sample, &zdata1, &zdata2);

  din = gdphp_convert_data(zdata1, zdata2, 4, 5);

  n = gd_putdata(D, field_code, first_frame, first_sample, 0, din.ns, din.type,
      din.data);

  if (din.free_din)
    efree(din.data);

  GDPHP_CHECK_ERROR(D);

  dreturn("%zu", n);
  RETURN_LONG(n);
}

PHP_FUNCTION(gd_raw_close)
{
  char *field_code = NULL;
  GDPHP_SLEN field_code_len;

  char *field_codep;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("|s", &field_code, &field_code_len);

  field_codep = gdphp_check_null_string(field_code);

  GDPHP_RETURN_BOOL(gd_raw_close(D, field_codep));
}

PHP_FUNCTION(gd_raw_filename)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  char *s;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  s = gd_raw_filename(D, field_code);

  if (s) {
    GDPHP_RETVAL_STRING(s);
    dreturn("\"%s\"", s);
    free(s);
  } else
    GDPHP_RETURN_F;
}

PHP_FUNCTION(gd_reference)
{
  const char *field_code = NULL;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  const char *s;

  dtracephp();

  GDPHP_PARSED("|s", &field_code, &field_code_len);
  s = gd_reference(D, field_code);

  GDPHP_CHECK_ERROR(D);

  dreturn("\"%s\"", s);
  if (s == NULL)
    RETURN_NULL();

  GDPHP_RET_STR_COPY(s);
}

PHP_FUNCTION(gd_rename)
{
  const char *old_code, *new_name;
  GDPHP_SLEN old_code_len, new_name_len;
  GDPHP_LONG flags = 0;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("ss|l", &old_code, &old_code_len, &new_name, &new_name_len,
      &flags);

  GDPHP_RETURN_BOOL(gd_rename(D, old_code, new_name, flags));
}

PHP_FUNCTION(gd_rewrite_fragment)
{
  zval *zi = NULL;

  long i;

  DIRFILE *D;

  dtracephp();
  
  GDPHP_PARSED("|z", &zi);

  i = gdphp_long_from_zval_null(zi, GD_ALL_FRAGMENTS);

  GDPHP_RETURN_BOOL(gd_rewrite_fragment(D, i));
}

PHP_FUNCTION(gd_seek)
{
  char *field_code;
  GDPHP_SLEN field_code_len;
  GDPHP_LONG frame_num, sample_num, flags = 0;

  DIRFILE *D;
  long pos;

  dtracephp();

  GDPHP_PARSED("sll|l", &field_code, &field_code_len, &frame_num, &sample_num,
      &flags);

  pos = gd_seek(D, field_code, frame_num, sample_num, flags);

  if (pos < 0)
    GDPHP_RETURN_F;

  dreturn("%li", pos);
  RETURN_LONG(pos);
}

PHP_FUNCTION(gd_spf)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  unsigned spf;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  spf = gd_spf(D, field_code);

  if (spf == 0)
    GDPHP_RETURN_F;

  dreturn("%u", spf);
  RETURN_LONG(spf);
}

PHP_FUNCTION(gd_strings)
{
  DIRFILE *D;
  const char **l;

  dtracephp();

  GDPHP_PARSED_ONLY();

  l = gd_strings(D);

  if (l == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, l, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_strtok)
{
  char *s, *p;
  GDPHP_SLEN slen;
  int i = 0;
  
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s", &s, &slen);

  array_init(return_value);

  for (p = gd_strtok(D, s); p; p = gd_strtok(D, NULL)) {
    gdphp_add_index_string(return_value, i++, p);
    free(p);
  }

  GDPHP_CHECK_ERROR(D);
  dreturn("%i", i);
}

PHP_FUNCTION(gd_sync)
{
  char *field_code = NULL;
  GDPHP_SLEN field_code_len;

  char *field_codep;
  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("|s", &field_code, &field_code_len);

  field_codep = gdphp_check_null_string(field_code);

  GDPHP_RETURN_BOOL(gd_sync(D, field_codep));
}

PHP_FUNCTION(gd_tell)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;
  long pos;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  pos = gd_tell(D, field_code);

  if (pos < 0)
    GDPHP_RETURN_F;

  dreturn("%li", pos);
  RETURN_LONG(pos);
}

PHP_FUNCTION(gd_unhide)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  GDPHP_RETURN_BOOL(gd_unhide(D, field_code));
}

PHP_FUNCTION(gd_uninclude)
{
  DIRFILE *D;
  GDPHP_LONG i;
  zend_bool del = 0;

  dtracephp();

  GDPHP_PARSED("l|b", &i, &del);

  GDPHP_RETURN_BOOL(gd_uninclude(D, i, del));
}

PHP_FUNCTION(gd_validate)
{
  char *field_code;
  GDPHP_SLEN field_code_len;

  DIRFILE *D;

  dtracephp();

  GDPHP_PARSED("s", &field_code, &field_code_len);

  GDPHP_RETURN_BOOL(gd_validate(D, field_code));
}

PHP_FUNCTION(gd_vector_list)
{
  DIRFILE *D;
  const char **fl;

  dtracephp();

  GDPHP_PARSED_ONLY();

  fl = gd_vector_list(D);

  if (fl == NULL)
    GDPHP_RETURN_F;

  gdphp_to_string_array(return_value, fl, 0);

  dreturnvoid();
}

PHP_FUNCTION(gd_verbose_prefix)
{
  char *prefix = NULL;
  GDPHP_SLEN prefix_len;

  char *prefixp;
  DIRFILE *D;
  
  dtracephp();

  GDPHP_PARSED("|s", &prefix, &prefix_len);

  prefixp = gdphp_check_null_string(prefix);

  GDPHP_RETURN_BOOL(gd_verbose_prefix(D, prefixp));
}

PHP_FUNCTION(gd_encoding_support)
{
  GDPHP_LONG e;
  long r;

  dtracephp();

  GDPHP_PARSE("l", &e);

  r = gd_encoding_support(e);

  if (r == -1)
    GDPHP_RETURN_F;

  dreturn("%li", r);
  RETURN_LONG(r);
}

/* MODULE DECLARATIONS */
static const zend_function_entry getdata_functions[] = {
  PHP_FE(gd_add, NULL)
    PHP_FE(gd_add_alias, NULL)
    PHP_FE(gd_add_bit, NULL)
    PHP_FE(gd_add_carray, NULL)
    PHP_FE(gd_add_const, NULL)
    PHP_FE(gd_add_divide, NULL)
		PHP_FE(gd_add_indir, NULL)
    PHP_FE(gd_add_lincom, NULL)
    PHP_FE(gd_add_linterp, NULL)
    PHP_FE(gd_add_mplex, NULL)
    PHP_FE(gd_add_multiply, NULL)
    PHP_FE(gd_add_phase, NULL)
    PHP_FE(gd_add_polynom, NULL)
    PHP_FE(gd_add_raw, NULL)
    PHP_FE(gd_add_recip, NULL)
    PHP_FE(gd_add_sarray, NULL)
    PHP_FE(gd_add_sbit, NULL)
		PHP_FE(gd_add_sindir, NULL)
    PHP_FE(gd_add_spec, NULL)
    PHP_FE(gd_add_string, NULL)
    PHP_FE(gd_add_window, NULL)
    PHP_FE(gd_alias_target, NULL)
    PHP_FE(gd_aliases, NULL)
    PHP_FE(gd_alter_affixes, NULL)
    PHP_FE(gd_alter_bit, NULL)
    PHP_FE(gd_alter_carray, NULL)
    PHP_FE(gd_alter_const, NULL)
    PHP_FE(gd_alter_divide, NULL)
    PHP_FE(gd_alter_encoding, NULL)
    PHP_FE(gd_alter_endianness, NULL)
    PHP_FE(gd_alter_entry, NULL)
    PHP_FE(gd_alter_frameoffset, NULL)
    PHP_FE(gd_alter_indir, NULL)
    PHP_FE(gd_alter_lincom, NULL)
    PHP_FE(gd_alter_linterp, NULL)
    PHP_FE(gd_alter_mplex, NULL)
    PHP_FE(gd_alter_multiply, NULL)
    PHP_FE(gd_alter_phase, NULL)
    PHP_FE(gd_alter_polynom, NULL)
    PHP_FE(gd_alter_protection, NULL)
    PHP_FE(gd_alter_raw, NULL)
    PHP_FE(gd_alter_recip, NULL)
    PHP_FE(gd_alter_sarray, NULL)
    PHP_FE(gd_alter_sbit, NULL)
    PHP_FE(gd_alter_sindir, NULL)
    PHP_FE(gd_alter_spec, NULL)
    PHP_FE(gd_alter_window, NULL)
    PHP_FE(gd_array_len, NULL)
    PHP_FE(gd_bof, NULL)
    PHP_FE(gd_carrays, NULL)
    PHP_FE(gd_carray_len, NULL)
    PHP_FE(gd_close, NULL)
    PHP_FE(gd_constants, NULL)
    PHP_FE(gd_delete, NULL)
    PHP_FE(gd_desync, NULL)
    PHP_FE(gd_dirfile_standards, NULL)
    PHP_FE(gd_dirfilekey, NULL)
    PHP_FE(gd_dirfilename, NULL)
    PHP_FE(gd_discard, NULL)
    PHP_FE(gd_encoding, NULL)
    PHP_FE(gd_encoding_support, NULL)
    PHP_FE(gd_endianness, NULL)
    PHP_FE(gd_entry, NULL)
    PHP_FE(gd_entry_list, NULL)
    PHP_FE(gd_entry_type, NULL)
    PHP_FE(gd_eof, NULL)
    PHP_FE(gd_error, NULL)
    PHP_FE(gd_error_string, NULL)
    PHP_FE(gd_field_list, NULL)
    PHP_FE(gd_field_list_by_type, NULL)
    PHP_FE(gd_flags, NULL)
    PHP_FE(gd_flush, NULL)
    PHP_FE(gd_fragment_affixes, NULL)
    PHP_FE(gd_fragment_index, NULL)
    PHP_FE(gd_fragment_namespace, NULL)
    PHP_FE(gd_fragmentname, NULL)
    PHP_FE(gd_framenum, NULL)
    PHP_FE(gd_frameoffset, NULL)
    PHP_FE(gd_get_carray, NULL)
    PHP_FE(gd_get_constant, NULL)
    PHP_FE(gd_get_sarray, NULL)
    PHP_FE(gd_get_string, NULL)
    PHP_FE(gd_getdata, NULL)
    PHP_FE(gd_hidden, NULL)
    PHP_FE(gd_hide, NULL)
    PHP_FE(gd_include, NULL)
    PHP_FE(gd_include_affix, NULL)
    PHP_FE(gd_invalid_dirfile, NULL)
    PHP_FE(gd_linterp_tablename, NULL)
    PHP_FE(gd_madd, NULL)
    PHP_FE(gd_madd_alias, NULL)
    PHP_FE(gd_madd_bit, NULL)
    PHP_FE(gd_madd_carray, NULL)
    PHP_FE(gd_madd_const, NULL)
    PHP_FE(gd_madd_divide, NULL)
    PHP_FE(gd_madd_indir, NULL)
    PHP_FE(gd_madd_lincom, NULL)
    PHP_FE(gd_madd_linterp, NULL)
    PHP_FE(gd_madd_mplex, NULL)
    PHP_FE(gd_madd_multiply, NULL)
    PHP_FE(gd_madd_phase, NULL)
    PHP_FE(gd_madd_polynom, NULL)
    PHP_FE(gd_madd_recip, NULL)
    PHP_FE(gd_madd_sarray, NULL)
    PHP_FE(gd_madd_sbit, NULL)
    PHP_FE(gd_madd_sindir, NULL)
    PHP_FE(gd_madd_spec, NULL)
    PHP_FE(gd_madd_string, NULL)
    PHP_FE(gd_madd_window, NULL)
    PHP_FE(gd_malter_spec, NULL)
    PHP_FE(gd_match_entries, NULL)
    PHP_FE(gd_mcarrays, NULL)
    PHP_FE(gd_mconstants, NULL)
    PHP_FE(gd_metaflush, NULL)
    PHP_FE(gd_mfield_list, NULL)
    PHP_FE(gd_mfield_list_by_type, NULL)
    PHP_FE(gd_move, NULL)
    PHP_FE(gd_mplex_lookback, NULL)
    PHP_FE(gd_msarrays, NULL)
    PHP_FE(gd_mstrings, NULL)
    PHP_FE(gd_mvector_list, NULL)
    PHP_FE(gd_naliases, NULL)
    PHP_FE(gd_native_type, NULL)
    PHP_FE(gd_nentries, NULL)
    PHP_FE(gd_nfields, NULL)
    PHP_FE(gd_nfields_by_type, NULL)
    PHP_FE(gd_nfragments, NULL)
    PHP_FE(gd_nframes, NULL)
    PHP_FE(gd_nmfields, NULL)
    PHP_FE(gd_nmfields_by_type, NULL)
    PHP_FE(gd_nmvectors, NULL)
    PHP_FE(gd_nvectors, NULL)
    PHP_FE(gd_open, NULL)
    PHP_FE(gd_parent_fragment, NULL)
    PHP_FE(gd_popen, NULL)
    PHP_FE(gd_protection, NULL)
    PHP_FE(gd_put_carray, NULL)
    PHP_FE(gd_put_constant, NULL)
    PHP_FE(gd_put_sarray, NULL)
    PHP_FE(gd_put_string, NULL)
    PHP_FE(gd_putdata, NULL)
    PHP_FE(gd_raw_close, NULL)
    PHP_FE(gd_raw_filename, NULL)
    PHP_FE(gd_reference, NULL)
    PHP_FE(gd_rename, NULL)
    PHP_FE(gd_rewrite_fragment, NULL)
    PHP_FE(gd_sarrays, NULL)
    PHP_FE(gd_seek, NULL)
    PHP_FE(gd_spf, NULL)
    PHP_FE(gd_strings, NULL)
    PHP_FE(gd_strtok, NULL)
    PHP_FE(gd_sync, NULL)
    PHP_FE(gd_tell, NULL)
    PHP_FE(gd_unhide, NULL)
    PHP_FE(gd_uninclude, NULL)
    PHP_FE(gd_validate, NULL)
    PHP_FE(gd_vector_list, NULL)
    PHP_FE(gd_verbose_prefix, NULL)
    PHP_FE_END
};

zend_module_entry getdata_module_entry = {
#if ZEND_MODULE_API_NO >= 20010901
  STANDARD_MODULE_HEADER,
#endif
  "GetData",
  getdata_functions,
  PHP_MINIT(getdata),
  PHP_MSHUTDOWN(getdata),
  NULL,
  NULL,
  PHP_MINFO(getdata),
#if ZEND_MODULE_API_NO >= 20010901
  GD_GETDATA_VERSION,
#endif
  STANDARD_MODULE_PROPERTIES
};

ZEND_GET_MODULE(getdata);
