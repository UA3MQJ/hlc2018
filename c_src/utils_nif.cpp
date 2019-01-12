#include <erl_nif.h>
#include <string.h>
#include <stdio.h>

// static ERL_NIF_TERM
// jaro(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     unsigned int list_length1, list_length2;

//     enif_get_list_length(env, argv[0], &list_length1);
//     enif_get_list_length(env, argv[1], &list_length2);

//     int chars1[list_length1], chars2[list_length2];

//     fill_buffer(env, argv[0], chars1, list_length1);
//     fill_buffer(env, argv[1], chars2, list_length2);

//     double distance = jaro_winkler_distance(chars1, list_length1, chars2, list_length2);

//     return enif_make_double(env, distance);
// }

static ERL_NIF_TERM
test(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int some_result = 666;

    return enif_make_int(env, some_result);
}

/*
 * Load the nif. Initialize some stuff and such
 */
static int on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int on_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"test", 0, test}
};

ERL_NIF_INIT(Elixir.UtilsNif, nif_funcs, on_load, on_reload, on_upgrade, NULL)
