import           Text.GLTF.Loader as Gltf
import           Lens.Micro
import           Linear.V3
import qualified Data.Vector as V

getVertices :: Gltf -> V.Vector (V3 Float)
getVertices gltf = V.concatMap getVertices' (gltf ^. _meshes)
  where getVertices' mesh = V.concatMap (^. _meshPrimitivePositions) (mesh ^. _meshPrimitives)
  
main = undefined
