module System.Linux.Kvm.IoCtl.Types.Utils where

import Data.Array.IArray
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr

mkArray :: (Storable e, IArray a e) => Int -> Ptr e -> IO (a Int e)
mkArray len ptr = do
                    arr <- zip [0,1..] <$> peekArray len ptr
                    return $ array (0, len-1) arr
