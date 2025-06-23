{-# LANGUAGE OverloadedStrings #-}

module PluginTest where

import Plugin.Types
import qualified Data.Text as T
import Control.Concurrent.STM (atomically, writeTChan)

-- | Test plugin that demonstrates all plugin capabilities
data TestPlugin = TestPlugin

-- | Create the plugin instance
testPlugin :: TestPlugin
testPlugin = TestPlugin

instance Plugin TestPlugin where
    pluginName _ = "Test Plugin"
    pluginVersion _ = "1.0.0"
    pluginDescription _ = "A test plugin that demonstrates all plugin functionality"
    pluginHooks _ = [OnUserMessage, OnClaudeResponse, OnApplicationStart, OnProjectChange, OnFileUpload, OnProcessStart, OnProcessEnd, OnWorkflowStart, OnWorkflowStep, OnWorkflowEnd]
    
    pluginInitialize _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: 🔌 Test Plugin initialized successfully!"
        return $ pluginWithLogs ["Test Plugin startup complete"] $ pluginOk ()
    
    pluginCleanup _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: 🔌 Test Plugin shutting down..."
        return $ pluginWithLogs ["Test Plugin cleanup complete"] $ pluginOk ()

instance MessagePlugin TestPlugin where
    transformUserMessage _ _ message = do
        -- Add emoji to user messages
        let transformed = if T.null message
                         then message
                         else "💬 " <> message
        return $ pluginWithLogs ["Added emoji to user message"] $ pluginOk transformed
    
    transformClaudeResponse _ _ response = do
        -- Add a watermark to Claude responses
        let transformed = if T.null response
                         then response
                         else response <> "\n\n🤖 *Response enhanced by Test Plugin*"
        return $ pluginWithLogs ["Added watermark to Claude response"] $ pluginOk transformed

instance ProcessPlugin TestPlugin where
    onProcessStart _ ctx args = do
        atomically $ writeTChan (pluginMessages ctx) "system: 🔌 Test Plugin: Claude process starting"
        return $ pluginWithLogs ["Logged process start"] $ pluginOk args
    
    onProcessEnd _ ctx exitCode = do
        let msg = "system: 🔌 Test Plugin: Claude process ended with code " <> T.pack (show exitCode)
        atomically $ writeTChan (pluginMessages ctx) msg
        return $ pluginWithLogs ["Logged process end"] $ pluginOk ()

instance WorkflowPlugin TestPlugin where
    onWorkflowStart _ ctx workflowName = do
        let msg = "system: 🔌 Test Plugin: Workflow '" <> workflowName <> "' started"
        atomically $ writeTChan (pluginMessages ctx) msg
        return $ pluginOk ()
    
    onWorkflowStep _ ctx workflowName stepNum = do
        let msg = "system: 🔌 Test Plugin: Workflow '" <> workflowName <> "' executing step " <> T.pack (show stepNum)
        atomically $ writeTChan (pluginMessages ctx) msg
        return $ pluginOk ()
    
    onWorkflowEnd _ ctx workflowName success = do
        let status = if success then "completed successfully" else "failed"
        let msg = "system: 🔌 Test Plugin: Workflow '" <> workflowName <> "' " <> status
        atomically $ writeTChan (pluginMessages ctx) msg
        return $ pluginOk ()

instance SystemPlugin TestPlugin where
    onApplicationStart _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: 🔌 Test Plugin detected application startup!"
        return $ pluginOk ()
    
    onProjectChange _ ctx oldProject newProject = do
        let msg = case (oldProject, newProject) of
                    (Nothing, Just new) -> "system: 🔌 Test Plugin: Switched to project " <> new
                    (Just old, Just new) -> "system: 🔌 Test Plugin: Changed project from " <> old <> " to " <> new
                    (Just old, Nothing) -> "system: 🔌 Test Plugin: Left project " <> old
                    (Nothing, Nothing) -> "system: 🔌 Test Plugin: No project change"
        atomically $ writeTChan (pluginMessages ctx) msg
        return $ pluginOk ()
    
    onFileUpload _ ctx files = do
        let fileCount = length files
        let msg = "system: 🔌 Test Plugin: " <> T.pack (show fileCount) <> " files uploaded"
        atomically $ writeTChan (pluginMessages ctx) msg
        return $ pluginOk files