import numpy as np
import pygltflib
import pathlib
import struct
from pygltflib import GLTF2

def Main():

    fname  = pathlib.Path("./Model.gltf")
    gltf   = GLTF2().load(fname)
    mesh0  = gltf.meshes[gltf.scenes[gltf.scene].nodes[0]]
    prim0  = mesh0.primitives[0]
     
    for primitive in mesh0.primitives:

        accessor = gltf.accessors[primitive.indices]
        bufferView = gltf.bufferViews[accessor.bufferView]
        buffer = gltf.buffers[bufferView.buffer]
        data = gltf.get_data_from_buffer_uri(buffer.uri)

        indices = []
        for i in range(accessor.count):
            index = bufferView.byteOffset + accessor.byteOffset + i*12  # the location in the buffer of this vertex
            d   = data[index:index+12]  # the vertex data
            idx = struct.unpack("<fff", d)   # convert from base64 to three floats
            indices.append(idx)
            print(i, idx)
        

     
        # # get the binary data for this mesh primitive from the buffer
        # accessor = gltf.accessors[primitive.attributes.POSITION]
        # bufferView = gltf.bufferViews[accessor.bufferView]
        # buffer = gltf.buffers[bufferView.buffer]
        # data = gltf.get_data_from_buffer_uri(buffer.uri)
     
        # # pull each vertex from the binary buffer and convert it into a tuple of python floats
        # vertices = []
        # for i in range(accessor.count):
        #     index = bufferView.byteOffset + accessor.byteOffset + i*12  # the location in the buffer of this vertex
        #     d = data[index:index+12]  # the vertex data
        #     v = struct.unpack("<fff", d)   # convert from base64 to three floats
        #     vertices.append(v)
        #     print(i, v)

#if __name__ == "__main__"
if __name__ == "__main__":
    Main()
