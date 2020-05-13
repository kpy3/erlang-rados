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
#include "atoms.h"
#include "cluster.h"
#include "resources.h"

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    init_atoms(env);
    if(open_resources(env) == -1) {
        return -1;
    }
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
	close_resources(env);
}

static ErlNifFunc nif_funcs[] =    {
    {"connect", 3, connect_to_cluster, ERL_NIF_DIRTY_JOB_IO_BOUND}
};

ERL_NIF_INIT(rados, nif_funcs,  &load, NULL, NULL, &unload);
