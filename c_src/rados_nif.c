// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "rados_nif.h"

ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;

ErlNifResourceType *rados_connection;

static ERL_NIF_TERM mk_atom(ErlNifEnv *env, const char *atom) {
  ERL_NIF_TERM ret;
  if (!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
    return enif_make_atom(env, atom);
  }
  return ret;
}

static void init_atoms(ErlNifEnv *env) {
  atom_ok = mk_atom(env, "ok");
  atom_error = mk_atom(env, "error");
}

static void connection_dtor(ErlNifEnv *env, void *obj) {
  connection_t *connection = (connection_t *)obj;

  if (connection->cluster) {
    rados_shutdown(connection->cluster);
  }
  if (connection->mutex) {
    enif_mutex_destroy(connection->mutex);
  }
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  init_atoms(env);
  rados_connection = enif_open_resource_type(env, NULL, "rados_connection", connection_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
  if (!rados_connection) {
    return -1;
  }
  return 0;
}

static void unload(ErlNifEnv *env, void *priv_data) { 
  
}

static ErlNifFunc nif_funcs[] = {
    {"version", 0, erlang_rados_version},
    {"connect", 1, erlang_rados_connect, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"shutdown", 1, erlang_rados_shutdown, ERL_NIF_DIRTY_JOB_IO_BOUND}};

ERL_NIF_INIT(rados, nif_funcs, &load, NULL, NULL, &unload);
