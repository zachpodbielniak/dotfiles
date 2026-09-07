/*
 * gowl user configuration
 *
 * Compiled to a .so and loaded at startup; on a compile failure gowl
 * falls back to its defaults.
 *
 * Almost nothing lives here.  The session is `emacs --gowl', so the
 * compositor is configured from Elisp in ~/.config/doom/+gowl.el --
 * wallpaper, gaps, opacity, window rules, dropdowns, monitors and both
 * status bars.  Keeping a second source of truth in C is how the two
 * end up disagreeing.
 *
 * What is left is the handful of things that have to happen before the
 * Lisp VM is up.
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
 * Called after the YAML config is loaded but before the compositor
 * starts.  Return TRUE on success, FALSE to fall back to defaults.
 */
G_MODULE_EXPORT gboolean
gowl_config_init(void)
{
    g_object_set(gowl_config,
        "border-width", 3,
        NULL);

    return TRUE;
}

/*
 * configure_monitors:
 *
 * Hostname-conditional monitor configuration.  On libreclaw-00 the
 * VM's preferred mode is only 1280x800, so ask for the real one.
 *
 * Goes through gowl_monitor_set_mode() rather than wlroots directly:
 * this file used to include <wlr/types/wlr_output.h> and pin
 * CRISPY_PARAMS to wlroots-0.19, which meant a config object built
 * against 0.19 headers being dlopened into a compositor linked against
 * 0.20.  That is an ABI mismatch waiting for a crash, and it bought
 * nothing libgowl does not already expose.
 */
static void
configure_monitors(void)
{
    GList *monitors;
    GList *l;

    if (g_strcmp0(g_get_host_name(), "libreclaw-00") != 0)
        return;

    monitors = gowl_compositor_get_monitors(gowl_compositor);
    for (l = monitors; l != NULL; l = l->next) {
        GowlMonitor *mon = GOWL_MONITOR(l->data);

        if (g_strcmp0(gowl_monitor_get_name(mon), "Virtual-1") == 0) {
            gowl_monitor_set_mode(mon, 1920, 1200, 60000);
            break;
        }
    }
}

/*
 * gowl_config_ready:
 *
 * Called once the compositor is fully started and the Wayland display
 * is accepting clients.
 *
 * This used to spawn the standalone `gowlbar' binary.  That is the
 * wrong bar: under `emacs --gowl' the bar is the in-process bar module
 * (tags, title, panels, toasts), and spawning the standalone client as
 * well put a second, near-empty bar on the screen.
 */
G_MODULE_EXPORT void
gowl_config_ready(void)
{
    configure_monitors();
}
