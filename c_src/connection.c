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
#include <string.h>

ERL_NIF_TERM
erlang_rados_connect(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary user_name_bin;
  char *user_name;
  connection_t *connection;
  rados_t cluster;
  int err;

  if (!enif_inspect_binary(env, argv[0], &user_name_bin)) {
    return enif_make_badarg(env);
  }

  user_name = (char *)enif_alloc(user_name_bin.size + 1);
  memcpy(user_name, user_name_bin.data, user_name_bin.size);
  user_name[user_name_bin.size] = 0;

  /* Initialize the cluster handle  */
  err = rados_create(&cluster, user_name);
  enif_free(user_name);

  err = rados_conf_read_file(cluster,
                             "/var/snap/microceph/current/conf/ceph.conf");
  if (err < 0) {
    enif_fprintf(stderr, "Couldn't read ceph.conf file: %s\n", strerror(-err));
    return enif_make_badarg(env);
  }

  err = rados_connect(cluster);
  if (err < 0) {
    enif_fprintf(stderr, "Couldn't connect to cluster handle: %s\n",
                 strerror(-err));
    return enif_make_badarg(env);
  }

  connection = enif_alloc_resource(rados_connection, sizeof(connection_t));
  connection->cluster = cluster;
  connection->mutex = enif_mutex_create("rados_connection");

  ERL_NIF_TERM term = enif_make_resource(env, connection);
  enif_release_resource(connection);

  return enif_make_tuple2(env, atom_ok, term);
}

ERL_NIF_TERM erlang_rados_shutdown(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {

  connection_t *connection;

  if (argc != 1) {
    return enif_make_badarg(env);
  }

  if (!enif_get_resource(env, argv[0], rados_connection, (void **)&connection)) {
    return enif_make_badarg(env);
  }

  enif_mutex_lock(connection->mutex);
  if (connection->cluster) {
    rados_shutdown(connection->cluster);
    connection->cluster = NULL;
  }
  enif_mutex_unlock(connection->mutex);
  if (connection->mutex) {
    enif_mutex_destroy(connection->mutex);
    connection->mutex = NULL;
  }
  enif_release_resource(connection);

  return atom_ok;
}
