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

#include "cluster.h"
#include "atoms.h"
#include "resources.h"
#include <string.h>

ERL_NIF_TERM
connect_to_cluster(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary cluster_name_bin, pool_name_bin;
  unsigned char *cluster_name, *pool_name;
  cluster_name = NULL;
  pool_name = NULL;

  if (argc != 3) {
    return enif_make_badarg(env);
  }

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  }

  if (!enif_is_binary(env, argv[1])) {
    return enif_make_badarg(env);
  }

  if (!enif_is_map(env, argv[2])) {
    return enif_make_badarg(env);
  }

  if (enif_inspect_binary(env, argv[0], &cluster_name_bin)) {
    if (!cluster_name_bin.size || cluster_name_bin.data == NULL) {
      return enif_make_badarg(env);
    } else {
      cluster_name = (unsigned char *)malloc(cluster_name_bin.size + 1);
      memcpy(cluster_name, cluster_name_bin.data, cluster_name_bin.size);
      *(cluster_name + cluster_name_bin.size) = '\0';
    }
  } else {
    return enif_make_badarg(env);
  }

  if (enif_inspect_binary(env, argv[1], &pool_name_bin)) {
    if (!pool_name_bin.size || pool_name_bin.data == NULL) {
      free(cluster_name);
      return enif_make_badarg(env);
    } else {
      pool_name = (unsigned char *)malloc(pool_name_bin.size + 1);
      memcpy(pool_name, pool_name_bin.data, pool_name_bin.size);
      *(pool_name + pool_name_bin.size) = '\0';
    }
  } else {
    free(cluster_name);
    return enif_make_badarg(env);
  }

  enif_fprintf(stdout, "cluster '%s', pool '%s'\n", cluster_name, pool_name);

  connection_t *conn_res =
      enif_alloc_resource(connection_res, sizeof(connection_t));
  // TODO Init connection: set async, cluster, io, completion, etc
  ERL_NIF_TERM term = enif_make_resource(env, conn_res);
  enif_release_resource(conn_res);
  free(cluster_name);
  free(pool_name);

  return enif_make_tuple2(env, atom_ok, term);
}
