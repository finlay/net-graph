import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Database.CDB
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import qualified Data.Array.Repa as R
import Data.List as L

inputfile = "morepork-dropout-3e116ed1-i15-h399-o2-b1-8000Hz-w512.net"

getInt = fromIntegral <$> getWord32le
size :: CDB -> (Int, Int, Int)
size cdb =  
    let bigendian = runGet getInt . BL.fromStrict
        i_size  = maybe (error "i_size not defined")  bigendian $ cdbGet cdb "net.i_size"
        h_size  = maybe (error "h_size not defined")  bigendian $ cdbGet cdb "net.h_size"
        ih_size = maybe (error "ih_size not defined") bigendian $ cdbGet cdb "net.ih_size"
    in  (i_size, h_size, ih_size)

getArray :: Get (R.Array R.U R.DIM1 Float)
getArray = 
    let getfloats = do 
              empty <- isEmpty
              if empty 
                  then return []
                  else do
                      f <- getFloat32le
                      fs <- getfloats 
                      return (f:fs)
    in do
        fs <- getfloats
        return $ R.fromListUnboxed (R.Z R.:. length fs) fs


weights :: CDB -> R.Array R.U R.DIM1 Float
weights cdb = maybe (error "ih_weights not defined") (runGet getArray . BL.fromStrict) $ cdbGet cdb "net.ih_weights"

main :: IO ()
main = do
    cdb <- cdbInit inputfile
    let (i_size, h_size, ih_size) = size cdb 
    let w' = weights cdb
    let w = R.reshape ((R.Z R.:. i_size R.:. h_size)::R.DIM2) w'
    print $ R.extent w
