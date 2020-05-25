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

#include "connection.h"
#include "atoms.h"
#include "rados/librados.h"
#include "resources.h"
#include <string.h>

static void free_buffer(char **buf) { free(*buf); }

ERL_NIF_TERM
connect_to_cluster(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary cluster_name_bin, pool_name_bin, user_name_bin;
  __attribute__((cleanup(free_buffer))) char *cluster_name, *pool_name,
      *user_name;
  cluster_name = NULL;
  pool_name = NULL;
  user_name = NULL;

  if (argc != 4) {
    return enif_make_badarg(env);
  }

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  }

  if (!enif_is_binary(env, argv[1])) {
    return enif_make_badarg(env);
  }

  if (!enif_is_binary(env, argv[2])) {
    return enif_make_badarg(env);
  }

  if (!enif_is_map(env, argv[3])) {
    return enif_make_badarg(env);
  }

  if (enif_inspect_binary(env, argv[0], &cluster_name_bin)) {
    if (!cluster_name_bin.size || cluster_name_bin.data == NULL) {
      return enif_make_badarg(env);
    } else {
      cluster_name = (char *)malloc(cluster_name_bin.size + 1);
      memcpy(cluster_name, cluster_name_bin.data, cluster_name_bin.size);
      *(cluster_name + cluster_name_bin.size) = '\0';
    }
  } else {
    return enif_make_badarg(env);
  }

  if (enif_inspect_binary(env, argv[1], &pool_name_bin)) {
    if (!pool_name_bin.size || pool_name_bin.data == NULL) {
      return enif_make_badarg(env);
    } else {
      pool_name = (char *)malloc(pool_name_bin.size + 1);
      memcpy(pool_name, pool_name_bin.data, pool_name_bin.size);
      *(pool_name + pool_name_bin.size) = '\0';
    }
  } else {
    return enif_make_badarg(env);
  }

  if (enif_inspect_binary(env, argv[2], &user_name_bin)) {
    if (!user_name_bin.size || user_name_bin.data == NULL) {
      return enif_make_badarg(env);
    } else {
      user_name = (char *)malloc(user_name_bin.size + 1);
      memcpy(user_name, user_name_bin.data, user_name_bin.size);
      *(user_name + user_name_bin.size) = '\0';
    }
  } else {
    return enif_make_badarg(env);
  }

  //  enif_fprintf(stdout, "cluster '%s', pool '%s', user '%s'\n", cluster_name,
  //  pool_name, user_name);

  connection_t *conn_res =
      enif_alloc_resource(connection_res, sizeof(connection_t));

  // TODO Init connection: set async, cluster, io, completion, etc

  rados_t *cluster = NULL;
  uint64_t flags = 0;

  /* Initialize the cluster handle  */
  int err;
  cluster = (rados_t *)malloc(sizeof(rados_t));
  err = rados_create2(cluster, cluster_name, user_name, flags);
  if (err < 0) {
    free(cluster);
    //       enif_fprintf(stderr, "Couldn't create the cluster handle: %s\n",
    //       strerror(-err));
    return enif_make_badarg(env);
  }

  conn_res->cluster = cluster;

  //       enif_fprintf(stdout, "conn_res->cluster = %p\n", conn_res->cluster);

  // TODO Init connection: set async, cluster, io, completion, etc

  ERL_NIF_TERM term = enif_make_resource(env, conn_res);
  enif_release_resource(conn_res);

  return enif_make_tuple2(env, atom_ok, term);
}

ERL_NIF_TERM close_connection(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {

  connection_t *conn_res = NULL;

  if (argc != 1) {
    return enif_make_badarg(env);
  }

  if (!enif_get_resource(env, argv[0], connection_res, (void **)&conn_res)) {
    return enif_make_badarg(env);
  }
  /* conn_res->cluster can be shutdown at this point by other process */
  if (conn_res->cluster) {
    rados_shutdown(*(conn_res->cluster));
    free(conn_res->cluster);
    conn_res->cluster = NULL;
  }
  enif_release_resource(conn_res);

  return atom_ok;
}
