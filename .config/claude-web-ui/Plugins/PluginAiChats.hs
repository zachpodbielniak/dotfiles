{-# LANGUAGE OverloadedStrings #-}

module PluginAiChats where

import Plugin.Types
import qualified Data.Text as T
import Control.Concurrent.STM (atomically, writeTChan, newTVarIO, readTVarIO, writeTVar, TVar)
import Control.Exception (catch, SomeException)
import Control.Monad (when)
import System.Process (readProcess)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.Posix ((=~))
import Data.Maybe (listToMaybe)

-- | AI Chats plugin that stores conversations in the ai_chats database
data AiChatsPlugin = AiChatsPlugin

-- | Create the plugin instance
aiChatsPlugin :: AiChatsPlugin
aiChatsPlugin = AiChatsPlugin

-- | Global state to store the current conversation state
{-# NOINLINE conversationState #-}
conversationState :: TVar (Maybe (T.Text, [T.Text], Maybe T.Text))  -- (user_message, accumulated_response_chunks, session_id)
conversationState = unsafePerformIO $ newTVarIO Nothing

instance Plugin AiChatsPlugin where
    pluginName _ = "AI Chats Database Logger"
    pluginVersion _ = "1.0.0"
    pluginDescription _ = "Stores chat conversations in ai_chats PostgreSQL database using session UUIDs"
    pluginHooks _ = [OnUserMessage, OnClaudeResponse, OnApplicationStart, OnProcessEnd]
    
    pluginInitialize _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: 🗄️ AI Chats Database Logger initialized"
        return $ pluginWithLogs ["AI Chats Database Logger startup complete"] $ pluginOk ()
    
    pluginCleanup _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: 🗄️ AI Chats Database Logger shutting down..."
        return $ pluginWithLogs ["AI Chats Database Logger cleanup complete"] $ pluginOk ()

instance MessagePlugin AiChatsPlugin where
    transformUserMessage _ ctx message = do
        -- Start a new conversation: store user message and reset response chunks and session ID
        atomically $ writeTVar conversationState (Just (message, [], Nothing))
        when (pluginVerboseLogging ctx) $
            atomically $ writeTChan (pluginMessages ctx) $ "system: 🐛 DEBUG: AiChats started new conversation: " <> T.take 100 message <> "..."
        return $ pluginWithLogs ["Started new conversation"] $ pluginOk message
    
    transformClaudeResponse _ ctx response = do
        -- Accumulate response chunks and extract session ID from raw JSON if available
        when (pluginVerboseLogging ctx) $
            atomically $ writeTChan (pluginMessages ctx) $ "system: 🐛 DEBUG: AiChats accumulating response chunk: " <> T.take 100 response <> "..."
        maybeConversation <- readTVarIO conversationState
        case maybeConversation of
            Nothing -> do
                atomically $ writeTChan (pluginMessages ctx) "system: 🗄️ Warning: No active conversation for this response chunk"
                return $ pluginWithLogs ["No active conversation"] $ pluginOk response
            Just (userMsg, existingChunks, currentSessionId) -> do
                -- Try to extract session ID from raw JSON if we don't have one yet
                newSessionId <- case (currentSessionId, pluginRawJson ctx) of
                    (Nothing, Just rawJson) -> 
                        case extractSessionFromJson rawJson of
                            Just sessionId -> do
                                when (pluginVerboseLogging ctx) $
                                    atomically $ writeTChan (pluginMessages ctx) $ "system: 🐛 DEBUG: Extracted session ID from raw JSON: " <> sessionId
                                return $ Just sessionId
                            Nothing -> return Nothing
                    (existing, _) -> return existing
                
                -- Add this chunk to the accumulated chunks
                let updatedChunks = existingChunks ++ [response]
                atomically $ writeTVar conversationState (Just (userMsg, updatedChunks, newSessionId))
                when (pluginVerboseLogging ctx) $
                    atomically $ writeTChan (pluginMessages ctx) $ "system: 🐛 DEBUG: Accumulated chunk " <> T.pack (show (length updatedChunks)) <> " for conversation"
                return $ pluginWithLogs ["Accumulated response chunk"] $ pluginOk response

instance SystemPlugin AiChatsPlugin where
    onApplicationStart _ ctx = do
        atomically $ writeTChan (pluginMessages ctx) "system: 🗄️ AI Chats Database Logger ready to log conversations"
        return $ pluginOk ()

instance ProcessPlugin AiChatsPlugin where
    onProcessStart _ _ args = do
        -- Don't modify process args
        return $ pluginOk args
    
    onProcessEnd _ ctx exitCode = do
        -- Store the accumulated conversation when Claude process ends
        maybeConversation <- readTVarIO conversationState
        case maybeConversation of
            Nothing -> do
                when (pluginVerboseLogging ctx) $
                    atomically $ writeTChan (pluginMessages ctx) "system: 🐛 DEBUG: No conversation to store on process end"
                return $ pluginOk ()
            Just (userMsg, responseChunks, sessionId) -> do
                if null responseChunks then do
                    when (pluginVerboseLogging ctx) $
                        atomically $ writeTChan (pluginMessages ctx) "system: 🐛 DEBUG: No response chunks to store"
                    return $ pluginOk ()
                else do
                    -- Combine all response chunks into final response
                    let fullResponse = T.concat responseChunks
                    when (pluginVerboseLogging ctx) $
                        atomically $ writeTChan (pluginMessages ctx) $ "system: 🐛 DEBUG: Storing complete conversation with " <> T.pack (show (length responseChunks)) <> " chunks, total length: " <> T.pack (show (T.length fullResponse))
                    
                    when (pluginVerboseLogging ctx) $ case sessionId of
                        Nothing -> atomically $ writeTChan (pluginMessages ctx) "system: 🐛 DEBUG: No session ID captured during conversation"
                        Just sid -> atomically $ writeTChan (pluginMessages ctx) $ "system: 🐛 DEBUG: Using captured session ID: " <> sid
                    
                    -- Store the complete interaction with captured session ID
                    storeCompleteInteractionWithSessionId ctx userMsg fullResponse sessionId
                    
                    -- Clear the conversation state for next interaction
                    atomically $ writeTVar conversationState Nothing
                    return $ pluginOk ()

-- | Store complete interaction (user message + Claude response) in the database
storeCompleteInteraction :: PluginContext -> T.Text -> T.Text -> IO ()
storeCompleteInteraction ctx userMsg response = do
    atomically $ writeTChan (pluginMessages ctx) "system: 🐛 DEBUG: Starting storeCompleteInteraction"
    result <- catch (storeInDatabase ctx userMsg response) handleError
    case result of
        Left err -> do
            atomically $ writeTChan (pluginMessages ctx) $ "system: 🗄️ Error storing chat: " <> T.pack err
        Right _ -> do
            atomically $ writeTChan (pluginMessages ctx) "system: 🗄️ Successfully stored interaction in ai_chats database"
  where
    handleError :: SomeException -> IO (Either String ())
    handleError e = return $ Left $ show e

-- | Store complete interaction with pre-captured session ID
storeCompleteInteractionWithSessionId :: PluginContext -> T.Text -> T.Text -> Maybe T.Text -> IO ()
storeCompleteInteractionWithSessionId ctx userMsg response sessionId = do
    when (pluginVerboseLogging ctx) $
        atomically $ writeTChan (pluginMessages ctx) "system: 🐛 DEBUG: Starting storeCompleteInteractionWithSessionId"
    result <- catch (storeInDatabaseWithSessionId ctx userMsg response sessionId) handleError
    case result of
        Left err -> do
            atomically $ writeTChan (pluginMessages ctx) $ "system: 🗄️ Error storing chat: " <> T.pack err
        Right _ -> do
            atomically $ writeTChan (pluginMessages ctx) "system: 🗄️ Successfully stored interaction in ai_chats database"
  where
    handleError :: SomeException -> IO (Either String ())
    handleError e = return $ Left $ show e

-- | Store the interaction in the database 
storeInDatabase :: PluginContext -> T.Text -> T.Text -> IO (Either String ())
storeInDatabase ctx userMsg response = do
    atomically $ writeTChan (pluginMessages ctx) "system: 🐛 DEBUG: Extracting session ID from context"
    -- Try to get session ID from plugin context first (if available)
    let maybeSessionId = case pluginRawJson ctx of
            Just rawJson -> extractSessionFromJson rawJson
            Nothing -> pluginSessionId ctx
    
    case maybeSessionId of
        Nothing -> do
            atomically $ writeTChan (pluginMessages ctx) "system: 🐛 DEBUG: No session ID available in context"
            return $ Left "No session ID available"
        Just sessionId -> do
            atomically $ writeTChan (pluginMessages ctx) $ "system: 🐛 DEBUG: Using session ID: " <> sessionId
            
            -- Get current project name
            let projectName = case pluginProject ctx of
                    Nothing -> "unknown"
                    Just proj -> T.takeWhileEnd (/= '/') proj -- Get last part of path
            
            -- Create tags with claude-code and project name
            let tags = ["claude-code", projectName]
            let provider = "claude"
            let model = "claude-sonnet-4" 
            
            -- Call database storage
            result <- catch (callAipy sessionId userMsg response provider model tags) handleAipyError
            return result
  where
    handleAipyError :: SomeException -> IO (Either String ())
    handleAipyError e = return $ Left $ "aipy error: " ++ show e

-- | Store the interaction in the database with pre-captured session ID
storeInDatabaseWithSessionId :: PluginContext -> T.Text -> T.Text -> Maybe T.Text -> IO (Either String ())
storeInDatabaseWithSessionId ctx userMsg response maybeSessionId = do
    when (pluginVerboseLogging ctx) $
        atomically $ writeTChan (pluginMessages ctx) "system: 🐛 DEBUG: Using pre-captured session ID"
    
    case maybeSessionId of
        Nothing -> do
            when (pluginVerboseLogging ctx) $
                atomically $ writeTChan (pluginMessages ctx) "system: 🐛 DEBUG: No session ID was captured during conversation"
            return $ Left "No session ID was captured"
        Just sessionId -> do
            when (pluginVerboseLogging ctx) $
                atomically $ writeTChan (pluginMessages ctx) $ "system: 🐛 DEBUG: Using captured session ID: " <> sessionId
            
            -- Get current project name
            let projectName = case pluginProject ctx of
                    Nothing -> "unknown"
                    Just proj -> T.takeWhileEnd (/= '/') proj -- Get last part of path
            
            -- Create tags with claude-code and project name
            let tags = ["claude-code", projectName]
            let provider = "claude"
            let model = "claude-sonnet-4" 
            
            -- Call database storage
            result <- catch (callAipy sessionId userMsg response provider model tags) handleAipyError
            return result
  where
    handleAipyError :: SomeException -> IO (Either String ())
    handleAipyError e = return $ Left $ "aipy error: " ++ show e

-- | Extract Claude session UUID from raw JSON response
extractSessionFromJson :: T.Text -> Maybe T.Text
extractSessionFromJson rawJson = 
    let jsonText = T.unpack rawJson
        pattern = "\"session_id\"\\s*:\\s*\"([a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12})\"" :: String
    in case jsonText =~ pattern :: [[String]] of
        ((_:sessionId:_):_) -> Just $ T.pack sessionId
        _ -> Nothing

-- | Simple hash function for text
hashText :: T.Text -> T.Text
hashText text = T.pack $ show $ hash $ T.unpack text
  where
    hash :: String -> Int
    hash = foldr (\c acc -> fromEnum c + acc * 31) 0

-- | Call database storage
callAipy :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> [T.Text] -> IO (Either String ())
callAipy sessionId prompt response provider model tags = do
    let dbName = "ai_chats"
    let dbUser = "postgres"
    
    let tagsArray = "{" ++ T.unpack (T.intercalate "," (map (\t -> "\"" <> t <> "\"") tags)) ++ "}"
    let promptHash = T.take 16 $ hashText prompt
    let responseHash = T.take 16 $ hashText response
    
    let insertSQL = "INSERT INTO ai_chats (context_uuid, prompt, response, provider, model, prompt_hash, response_hash, tags, metadata) VALUES ('" 
                   ++ T.unpack sessionId ++ "', '" 
                   ++ escapeSql (T.unpack prompt) ++ "', '" 
                   ++ escapeSql (T.unpack response) ++ "', '" 
                   ++ T.unpack provider ++ "', '" 
                   ++ T.unpack model ++ "', '" 
                   ++ T.unpack promptHash ++ "', '" 
                   ++ T.unpack responseHash ++ "', '" 
                   ++ tagsArray ++ "', '{}');"
    
    let contextSQL = "INSERT INTO ai_chat_contexts (context_uuid, name, tags) VALUES ('" 
                    ++ T.unpack sessionId ++ "', 'Claude Web UI Session', '" 
                    ++ tagsArray ++ "') ON CONFLICT (context_uuid) DO NOTHING;"
    
    result <- catch (do
        -- Connect to PostgreSQL running in quadlet container with trust auth
        _ <- readProcess "psql" ["-h", "127.0.0.1", "-p", "5432", "-U", dbUser, "-d", dbName, "-c", contextSQL] ""
        _ <- readProcess "psql" ["-h", "127.0.0.1", "-p", "5432", "-U", dbUser, "-d", dbName, "-c", insertSQL] ""
        return $ Right ()
        ) (\e -> return $ Left $ "Failed to insert into database: " ++ show (e :: SomeException))
    
    return result
  where
    escapeSql :: String -> String
    escapeSql = concatMap (\c -> if c == '\'' then "''" else [c])