{-# LANGUAGE OverloadedStrings #-}

module PluginSimple where

import Plugin.Types
import qualified Data.Text as T
import Control.Concurrent.STM (atomically, writeTChan)

-- | Simple test plugin
data SimplePlugin = SimplePlugin

-- | Create the plugin instance
simplePlugin :: SimplePlugin
simplePlugin = SimplePlugin

instance Plugin SimplePlugin where
    pluginName _ = "Simple Test Plugin"
    pluginVersion _ = "1.0.0"
    pluginDescription _ = "Simple test plugin for debugging"
    pluginHooks _ = [OnUserMessage, OnClaudeResponse]
    
    pluginInitialize _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: 🔧 Simple Test Plugin initialized"
        return $ pluginOk ()
    
    pluginCleanup _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: 🔧 Simple Test Plugin shutting down"
        return $ pluginOk ()

instance MessagePlugin SimplePlugin where
    transformUserMessage _ ctx message = do
        atomically $ writeTChan (pluginMessages ctx) $ "system: 🔧 Simple plugin captured user message: " <> T.take 50 message
        return $ pluginOk message
    
    transformClaudeResponse _ ctx response = do
        atomically $ writeTChan (pluginMessages ctx) $ "system: 🔧 Simple plugin saw Claude response: " <> T.take 50 response
        return $ pluginOk response