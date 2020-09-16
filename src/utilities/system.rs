{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE UnboxedTuples          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}

module HulkanEngine3D.Utilities.System where

import GHC.Base
import GHC.Exts
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.Hashable
import qualified Data.Time.Clock.System as SystemTime
import qualified Data.Vector.Mutable as MVector
import qualified Data.HashTable.IO as HashTable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Directory
import System.FilePath.Posix

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

instance Show (IORef a) where
  show _ = "<ioref>"

instance Show (MVector.IOVector a) where
    show a = "MVector<" ++ show (MVector.length a) ++ ">"

-- | Use this to throw all exceptions in this project
data VulkanException
  = VulkanException
  { vkeCode    :: Maybe VkResult
  , vkeMessage :: String
  } deriving (Eq, Show, Read)

instance Exception VulkanException where
  displayException (VulkanException Nothing msg)
    = unlines
    [ ""
    , "Vulkan exception:"
    , "*** " ++ msg
    ]
  displayException (VulkanException (Just c) msg)
    = unlines
    [ ""
    , "Vulkan error: " ++ show c
    , "*** " ++ msg
    ]


-- | Low latency time in seconds since the start
getSystemTime :: IO Double
getSystemTime = do
    now <- SystemTime.getSystemTime
    let seconds = SystemTime.systemSeconds now
        -- Have to nanoseconds convert from Word64 before subtraction to allow negative delta.
        nanoseconds :: Int64 = fromIntegral (SystemTime.systemNanoseconds now)
        -- Seconds in Double keep at least microsecond-precision for 285 years.
        -- Float is not good enough even for millisecond-precision over more than a few hours.
        result :: Double = fromIntegral seconds + fromIntegral nanoseconds / 1e9
    return result

-- | Handle any error and return default value
handleAllErrors :: a -> SomeException -> IO a
handleAllErrors a (SomeException e)
  = a <$ putStrLn (displayException e)

-- | Throw VulkanException if something goes wrong
validationVK :: VkResult
             -> String
             -> IO ()
validationVK result msg = do
  when (result < VK_SUCCESS) $ throwIO $ VulkanException (Just result) msg

throwingVK :: String
           -> IO VkResult
           -> IO ()
throwingVK msg f = do
  vkRez <- f
  validationVK vkRez msg

-- | Throw VulkanException without error code
throwVKMsg :: String -> IO a
throwVKMsg msg = throwIO $ VulkanException Nothing msg

-- | Use list of haskell strings as @Ptr CString@
withCStringList :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStringList [] f = f 0 nullPtr
withCStringList xs f = go xs [] 0
  where
    go [] pts n     = withArray (reverse pts) (f n)
    go (s:ss) pts n = withCString s (\p -> go ss (p:pts) (n+1))

-- | Get size of action output and then get the result,
--   performing data copy.
asListVK :: Storable x
         => (Ptr Word32 -> Ptr x -> IO ())
         -> IO [x]
asListVK action = alloca $ \counterPtr -> do
  action counterPtr VK_NULL_HANDLE
  counter <- fromIntegral <$> peek counterPtr
  if counter <= 0
  then pure []
  else allocaArray counter $ \valPtr -> do
    action counterPtr valPtr
    peekArray counter valPtr

-- | Prevent earlier GC of given value
touch :: a -> IO ()
touch x = GHC.Base.IO $ \s -> case GHC.Base.touch# x s of s' -> (# s', () #)
{-# INLINE touch #-}

-- | This should probably be in Graphics.Vulkan.Marshal
withVkArrayLen :: (Storable a, VulkanMarshal a) => [a] -> (Word32 -> Ptr a -> IO b) -> IO b
withVkArrayLen xs pf = do
  ret <- withArrayLen xs (pf . fromIntegral)
  touch xs
  return ret
{-# INLINE withVkArrayLen #-}

-- Any is a type to which any type can be safely unsafeCoerced to.
aToWord# :: Any -> Word#
aToWord# a = let !mb = a in case unsafeCoerce# mb :: Word of W# addr -> addr

unsafeAddr :: a -> Int
unsafeAddr a = I# (word2Int# (aToWord# (unsafeCoerce# a)))

unsafeToPtr  :: forall a. Storable a => a -> Ptr a
unsafeToPtr a = Ptr (unsafeCoerce# a)
{-# INLINE unsafeToPtr #-}

ptrAtIndex :: forall a. Storable a => Ptr a -> Int -> Ptr a
ptrAtIndex ptr i = ptr `plusPtr` (i * sizeOf @a undefined)

allocaPeek :: Storable a => (Ptr a -> IO b) -> IO a
allocaPeek action = alloca $ \ptr -> do
  action ptr >> peek ptr

allocaPeekArray :: Storable a => Int -> (Ptr a -> IO b) -> IO [a]
allocaPeekArray count action = allocaArray count $ \arrayPtr -> do
   action arrayPtr
   peekArray count arrayPtr


walkDirectory :: FilePath -> [String] -> IO [FilePath]
walkDirectory currentDirectory filter = do
    contents <- getDirectoryContents currentDirectory
    filepaths <- forM [x | x <- contents, not (elem x [".", ".."])] (\content -> do
        let currentContent = joinPath [currentDirectory, content]
        isDir <- doesDirectoryExist currentContent
        if isDir then do
            walkDirectory currentContent filter
        else do
            let ext = map toLower (snd . splitExtension $ currentContent)
            if elem ext filter || null filter then
                return [currentContent]
            else
                return []
        )
    return $ concat [filepath | filepath <- filepaths, not (null filepath)]

generateUniqueName :: HashTable.BasicHashTable Text.Text v -> Text.Text -> IO Text.Text
generateUniqueName objectMap objectName = do
    objectData <- HashTable.lookup objectMap objectName
    case objectData of
        Nothing -> return objectName
        otherwise -> generator objectMap objectName (0::Int)
    where
        generator sceneManagerData objectName index = do
            let newObjectName = Text.append objectName $ Text.append (Text.pack "_") (Text.pack . show $ index)
            objectData <- HashTable.lookup objectMap newObjectName
            case objectData of
                Nothing -> return newObjectName
                otherwise -> generator objectMap objectName (index + 1)

clearHashTable :: (Eq k, Hashable k) => HashTable.BasicHashTable k a -> ((k, a) -> IO ()) -> IO ()
clearHashTable hashTable action = do
    HashTable.mapM_ action hashTable
    resourcesList <- HashTable.toList hashTable
    mapM_ (\(k, v) -> HashTable.delete hashTable k) resourcesList

lookupWithDefaultMap :: (Eq k, Hashable k) => k -> HashMap.HashMap k v -> HashMap.HashMap k v -> Maybe v
lookupWithDefaultMap key map defaultMap =
    let maybeValue = HashMap.lookup key map
    in case maybeValue of
        Nothing -> HashMap.lookup key defaultMap
        otherwise -> maybeValue

toText :: (Show a) => a -> Text.Text
toText = Text.pack . show

fromText :: (Read a) => Text.Text -> a
fromText = read . Text.unpack