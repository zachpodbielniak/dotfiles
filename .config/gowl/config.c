/*
 * gowl user configuration
 *
 * Compile args are taken from the GOWL_BUILD_ARGS define if present,
 * otherwise pkg-config is used. This file is compiled to a .so and
 * loaded at startup. On compile failure, defaults are used.
 *
 * Optional build args override:
 * #define GOWL_BUILD_ARGS "-I/custom/path"
 */

#include <gowl/gowl.h>

/*
 * Extern references to compositor objects.
 * These are resolved at dlopen time from the running compositor.
 */
extern GowlCompositor *gowl_compositor;
extern GowlConfig     *gowl_config;

/*
 * spawn_gowlbar:
 *
 * Locates the 'gowlbar' binary in $PATH and spawns it as an async
 * child process. Errors are reported to stderr; the compositor
 * continues regardless of whether the spawn succeeds.
 */
static void
spawn_gowlbar(void)
{
    g_autofree gchar  *gowlbar_path = NULL;
    gchar             *argv[]       = { NULL, NULL };
    GError            *error        = NULL;

    /* Resolve gowlbar from $PATH */
    gowlbar_path = g_find_program_in_path("gowlbar");
    if (gowlbar_path == NULL) {
        g_printerr("gowl_config_init: 'gowlbar' not found in $PATH\n");
        return;
    }

    argv[0] = gowlbar_path;

    /* Spawn asynchronously — compositor does not wait on gowlbar */
    if (!g_spawn_async(
            NULL,                        /* inherit cwd */
            argv,
            NULL,                        /* inherit env */
            G_SPAWN_DEFAULT,
            NULL,                        /* no child setup */
            NULL,                        /* no child setup data */
            NULL,                        /* don't track child pid */
            &error)) {
        g_printerr("gowl_config_init: failed to spawn gowlbar: %s\n",
                   error->message);
        g_clear_error(&error);
    }
}

/*
 * gowl_config_init:
 *
 * Called after YAML config is loaded but before compositor starts.
 * Override or supplement YAML values here.
 * Return TRUE on success, FALSE to fall back to defaults.
 */
G_MODULE_EXPORT gboolean
gowl_config_init(void)
{
    /* Apply compositor configuration values */
    g_object_set(gowl_config,
        "border-width", 3,
        "mfact", 0.55,
        NULL);

    /* Launch the status bar */
    spawn_gowlbar();

    return TRUE;
}
