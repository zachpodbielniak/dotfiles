{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module PluginExample where

import Plugin.Types
import Import.NoFoundation hiding (atomically)
import qualified Data.Text as T
import Control.Concurrent.STM (atomically)

-- | Example plugin that demonstrates message transformation
data ExamplePlugin = ExamplePlugin

-- | Create the plugin instance
examplePlugin :: ExamplePlugin
examplePlugin = ExamplePlugin

instance Plugin ExamplePlugin where
    pluginName :: ExamplePlugin -> Text
    pluginName _ = "Example Plugin"
    
    pluginVersion :: ExamplePlugin -> Text
    pluginVersion _ = "1.0.0"
    
    pluginDescription :: ExamplePlugin -> Text
    pluginDescription _ = "A demonstration plugin that shows how to transform messages"
    
    pluginHooks :: ExamplePlugin -> [PluginHook]
    pluginHooks _ = [OnUserMessage, OnClaudeResponse, OnApplicationStart]
    
    pluginInitialize :: ExamplePlugin -> PluginContext -> IO (PluginResult ())
    pluginInitialize _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: Example Plugin initialized!"
        return $ pluginWithLogs ["Example Plugin startup complete"] $ pluginOk ()
    
    pluginCleanup :: ExamplePlugin -> PluginContext -> IO (PluginResult ())
    pluginCleanup _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: Example Plugin shutting down..."
        return $ pluginWithLogs ["Example Plugin cleanup complete"] $ pluginOk ()

instance MessagePlugin ExamplePlugin where
    transformUserMessage :: ExamplePlugin -> PluginContext -> Text -> IO (PluginResult Text)
    transformUserMessage _ _ message = do
        -- Add a friendly prefix to user messages
        let transformed = if T.null message
                         then message
                         else "Hey Claude, " <> message
        return $ pluginWithLogs ["Transformed user message"] $ pluginOk transformed
    
    transformClaudeResponse :: ExamplePlugin -> PluginContext -> Text -> IO (PluginResult Text)
    transformClaudeResponse _ _ response = do
        -- Add a signature to Claude responses
        let transformed = if T.null response
                         then response
                         else response <> "\n\n*Enhanced by Example Plugin*"
        return $ pluginWithLogs ["Enhanced Claude response"] $ pluginOk transformed

instance SystemPlugin ExamplePlugin where
    onApplicationStart :: ExamplePlugin -> PluginContext -> IO (PluginResult ())
    onApplicationStart _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: Example Plugin detected application startup!"
        return $ pluginOk ()
    
    onProjectChange :: ExamplePlugin -> PluginContext -> Maybe Text -> Maybe Text -> IO (PluginResult ())
    onProjectChange _ ctx oldProject newProject = do
        let msg = case (oldProject, newProject) of
                    (Nothing, Just new) -> "system: Example Plugin: Switched to project " <> new
                    (Just old, Just new) -> "system: Example Plugin: Changed project from " <> old <> " to " <> new
                    (Just old, Nothing) -> "system: Example Plugin: Left project " <> old
                    (Nothing, Nothing) -> "system: Example Plugin: No project change"
        atomically $ writeTChan (pluginMessages ctx) msg
        return $ pluginOk ()