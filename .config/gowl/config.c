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
 * gowl_config_init:
 *
 * Called after YAML config is loaded but before compositor starts.
 * Override or supplement YAML values here.
 * Return TRUE on success, FALSE to fall back to defaults.
 */
G_MODULE_EXPORT gboolean
gowl_config_init(void)
{
    /* Example: override border width */
    g_object_set(gowl_config,
        "border-width", 3,
        "mfact", 0.55,
        NULL);

    return TRUE;
}
