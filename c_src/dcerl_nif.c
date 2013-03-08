#include "erl_nif.h"
#include <stdio.h>
#include <string.h>
#include "dcerl.h"
#include "common.h"

#define CHERLY_RES_TYPE "dcerl_res"

static ERL_NIF_TERM dcerl_nif_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dcerl_nif_stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dcerl_nif_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dcerl_nif_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dcerl_nif_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dcerl_nif_eldest(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dcerl_nif_iterator(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dcerl_nif_iterator_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dcerl_nif_iterator_has_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dcerl_nif_items(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
  {
    {"start",  0, dcerl_nif_init},
    {"stop",   1, dcerl_nif_stop},
    {"get" ,   2, dcerl_nif_get},
    {"put" ,   2, dcerl_nif_put},
    {"remove", 2, dcerl_nif_remove},
    {"eldest", 1, dcerl_nif_eldest},
    {"iterator", 1, dcerl_nif_iterator},
    {"iterator_next", 1, dcerl_nif_iterator_next},
    {"iterator_has_next", 1, dcerl_nif_iterator_has_next},
    {"items" , 1, dcerl_nif_items}
  };

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_oom;
static ERL_NIF_TERM atom_not_found;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM tuple_error_oom;

/**
 * Initialize
 */
static ERL_NIF_TERM dcerl_nif_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM term;
  ErlNifResourceType* pert;
  dcerl_t* obj;

  pert = (ErlNifResourceType*)enif_priv_data(env);
  obj = enif_alloc_resource(pert, sizeof(dcerl_t));

  term = enif_make_resource(env, obj);
  dcerl_init(obj);
  enif_release_resource(obj);

  return enif_make_tuple2(env, atom_ok, term);
}


/**
 * Stop
 */
static ERL_NIF_TERM dcerl_nif_stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  dcerl_t *obj;
  ErlNifResourceType* pert;

  if (argc < 1) {
    return enif_make_badarg(env);
  }

  pert = (ErlNifResourceType*)enif_priv_data(env);

  if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
    return enif_make_badarg(env);
  }

  dcerl_destroy(obj);
  return atom_ok;
}


/**
 * Retrieve an object from LRU-Storage
 */
static ERL_NIF_TERM dcerl_nif_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  dcerl_t *obj;
  void* value;

  ErlNifResourceType* pert;
  ErlNifBinary keybin;

  if (argc < 2) {
    return enif_make_badarg(env);
  }

  pert = (ErlNifResourceType*)enif_priv_data(env);
  if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_binary(env, argv[1], &keybin)) {
    return enif_make_badarg(env);
  }

  if (keybin.size <= 0) {
    return enif_make_badarg(env);
  }

  value = dcerl_get(obj, keybin.data, keybin.size);

  if (value == NULL) {
    return atom_not_found;
  }

  return atom_ok;

}


/**
 * Insert an object into LRU-Storage
 */
static ERL_NIF_TERM dcerl_nif_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  dcerl_t *obj;
  ErlNifResourceType* pert;
  ErlNifBinary keybin;
  bool ret;

  if (argc < 2) {
    return enif_make_badarg(env);
  }

  pert = (ErlNifResourceType*)enif_priv_data(env);

  if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
    return enif_make_badarg(env);
  }
  if (!enif_inspect_binary(env, argv[1], &keybin)) {
    return enif_make_badarg(env);
  }
  if (keybin.size <= 0) {
    return enif_make_badarg(env);
  }

  ret = dcerl_put(obj, keybin.data, keybin.size);
  return ret ? atom_ok : tuple_error_oom;
}


/**
 * Remove an object from LRU-Storage
 */
static ERL_NIF_TERM dcerl_nif_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  dcerl_t *obj;
  ErlNifResourceType* pert;
  ErlNifBinary keybin;

  if (argc < 2) {
    return enif_make_badarg(env);
  }

  pert = (ErlNifResourceType*)enif_priv_data(env);

  if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_binary(env, argv[1], &keybin)) {
    return enif_make_badarg(env);
  }
  if (keybin.size <= 0) {
    return enif_make_badarg(env);
  }

  dcerl_remove(obj, keybin.data, keybin.size);
  return atom_ok;
}

static ERL_NIF_TERM dcerl_nif_eldest(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  dcerl_t *obj;
  ErlNifResourceType* pert;
  ErlNifBinary keybin;
  void* key;
  int keylen;

  if (argc < 1) {
    return enif_make_badarg(env);
  }

  pert = (ErlNifResourceType*)enif_priv_data(env);

  if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
    return enif_make_badarg(env);
  }

  key = dcerl_eldest(obj, &keylen);
  if (key == NULL) {
    return atom_not_found;
  }
  if (!enif_alloc_binary(keylen, &keybin)) {
    return enif_make_badarg(env);
  }

  memcpy(keybin.data, key, keylen);
  return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &keybin));
}

static ERL_NIF_TERM dcerl_nif_iterator(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  dcerl_t *obj;
  ErlNifResourceType* pert;
  ErlNifBinary keybin;
  void* key;
  int keylen;

  if (argc < 1) {
    return enif_make_badarg(env);
  }

  pert = (ErlNifResourceType*)enif_priv_data(env);

  if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
    return enif_make_badarg(env);
  }

  key = dcerl_iterator(obj, &keylen);
  if (key == NULL) {
    return atom_not_found;
  }
  if (!enif_alloc_binary(keylen, &keybin)) {
    return enif_make_badarg(env);
  }

  memcpy(keybin.data, key, keylen);
  return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &keybin));
}

static ERL_NIF_TERM dcerl_nif_iterator_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  dcerl_t *obj;
  ErlNifResourceType* pert;
  ErlNifBinary keybin;
  void* key;
  int keylen;

  if (argc < 1) {
    return enif_make_badarg(env);
  }

  pert = (ErlNifResourceType*)enif_priv_data(env);

  if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
    return enif_make_badarg(env);
  }

  key = dcerl_iterator_next(obj, &keylen);
  if (key == NULL) {
    return atom_not_found;
  }
  if (!enif_alloc_binary(keylen, &keybin)) {
    return enif_make_badarg(env);
  }

  memcpy(keybin.data, key, keylen);
  return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &keybin));
}

static ERL_NIF_TERM dcerl_nif_iterator_has_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  dcerl_t *obj;
  ErlNifResourceType* pert;

  if (argc < 1) {
    return enif_make_badarg(env);
  }

  pert = (ErlNifResourceType*)enif_priv_data(env);

  if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
    return enif_make_badarg(env);
  }

  if (dcerl_iterator_has_next(obj)) {
    return atom_true;
  } else {
    return atom_false;
  }
}

/**
 * Retrieve total of objects
 */
static ERL_NIF_TERM dcerl_nif_items(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  dcerl_t *obj;
  ErlNifResourceType* pert;
  ErlNifUInt64 len;

  if (argc < 1) {
    return enif_make_badarg(env);
  }

  pert = (ErlNifResourceType*)enif_priv_data(env);

  if (!enif_get_resource(env, argv[0], pert, (void**)&obj)) {
    return enif_make_badarg(env);
  }

  len = dcerl_items_length(obj);
  return enif_make_tuple2(env, atom_ok, enif_make_uint64(env, len));
}


/**
 * When calling onload or uggrade
 */
static int onload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  ErlNifResourceFlags erf = ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER;
  ErlNifResourceType* pert = enif_open_resource_type(env, NULL, CHERLY_RES_TYPE, NULL, erf, &erf);

  if (pert == NULL) {
    return 1;
  }

  *priv_data = (void*)pert;
  atom_ok = enif_make_atom(env, "ok");
  atom_error = enif_make_atom(env, "error");
  atom_oom = enif_make_atom(env, "oom");
  atom_not_found = enif_make_atom(env, "not_found");
  atom_true = enif_make_atom(env, "true");
  atom_false = enif_make_atom(env, "false");
  tuple_error_oom = enif_make_tuple2(env, atom_error, atom_oom);
  return 0;
}

/**
 *  Onload
 */
int dcerl_nif_onload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  return onload(env, priv_data, load_info);
}


/**
 * Upgrade
 */
int dcerl_nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
  return onload(env, priv_data, load_info);
}


ERL_NIF_INIT(dcerl, nif_funcs, dcerl_nif_onload, NULL, dcerl_nif_upgrade, NULL)

