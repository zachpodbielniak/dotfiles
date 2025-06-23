module UserSettings where
import Settings.Types (AppSettings(..))
import Plugin.Types (registerMessagePlugin, registerSystemPlugin, registerProcessPlugin)
import qualified Plugins.PluginAiChats as PluginAiChats

-- | User settings override function
-- This function takes the default settings and returns modified settings
-- These settings apply globally but can be overridden by per-project settings
userSettingsOverride :: AppSettings -> AppSettings
userSettingsOverride defaults = defaults
    { appPort = 3001                             -- Test dynamic WebSocket port
    , appEnableWebSocketLogging = False           -- Enable WebSocket debugging
    , appWebSocketVerboseLogging = True          -- Enable verbose WebSocket debugging
    , appKeepUploadedFiles = True                -- Keep uploaded files for debugging  
    , appUploadDir = "./uploads"                 -- Use local uploads directory (global default)
    , appDefaultProject = Just "/var/home/zach/Documents/notes" -- Set default project
    , appProjectRelativeUploads = True          -- Store uploads relative to selected project
    , appVerboseLogging = True                   -- Enable verbose logging
    , appStderrLogging = True                    -- Enable stderr logging
    , appPluginVerboseLogging = True             -- Plugin debug output enabled for debugging
    
    -- Plugin Configuration - Enable AiChats plugin
    , appPluginRegistry = registerMessagePlugin PluginAiChats.aiChatsPlugin $
                         registerSystemPlugin PluginAiChats.aiChatsPlugin $
                         registerProcessPlugin PluginAiChats.aiChatsPlugin $
                         appPluginRegistry defaults  -- Build on top of defaults
    }

-- ========================================================================
-- PLUGIN CONFIGURATION EXAMPLES
-- ========================================================================
-- Once plugins are working, you can add them like this:
--
-- import Plugin.Types (registerMessagePlugin, registerSystemPlugin)
-- import qualified Plugins.PluginAiChats as PluginAiChats
--
-- , appPluginRegistry = registerMessagePlugin PluginAiChats.aiChatsPlugin $
--                      registerSystemPlugin PluginAiChats.aiChatsPlugin $
--                      appPluginRegistry defaults  -- Build on top of defaults
--
-- ========================================================================
-- PROJECT-SPECIFIC SETTINGS EXAMPLES
-- ========================================================================
-- Per-project settings are configured in Settings.hs in the getProjectSettings function.
-- Here are some examples of how you might configure different projects:
--
-- EXAMPLE 1: Exact path matching for a specific project
-- "/home/zach/Source/Projects/my-important-app" -> defaultProjectSettings
--     { projectUploadDir = Just ".temp"                -- Use .temp folder for uploads
--     , projectKeepUploadedFiles = Just False          -- Auto-delete uploads after processing
--     , projectVerboseLogging = Just False             -- Reduce logging noise
--     , projectAllowedTools = Just ["Edit", "Read", "Git"] -- Restrict to essential tools only
--     }