module SDM

-- this is uninstalled hackery for development

%flag C "-L./lib/build/rtl"
%flag C "-I./lib/rtl"
%flag C "-I./lib/vspace"

-- this looks portable if we install the libs/headers in a common path

%include C "sdm.h"
%lib C "sdm"

-- low level SDM functions and types

-- some type aliases for foreign pointers

Space : Type
Space = Ptr
    

--  could vector space be a type provider for n vectors of length m?
-- so that we can put Fin contraints on relevant function args

vspace : IO Space
vspace = foreign FFI_C "sdm_vspace" (IO Space)

vspaceAlloc : Int -> Space -> IO Int
vspaceAlloc n vs = foreign FFI_C "sdm_vspace_allocate" (Space -> Int -> IO Int) vs n

vspaceFree : Space -> IO ()
vspaceFree vs = foreign FFI_C "sdm_vspace_free" (Ptr -> IO ()) vs

-- alias types...
 
Vector : Type
Vector = Ptr

vspaceVector : Int -> Space -> IO Vector
vspaceVector x vs = foreign FFI_C "sdm_vspace_vector" (Space -> Int -> IO Vector) vs x


-- vectors --

vectorSuperpose : Vector -> Vector -> IO ()
vectorSuperpose u v = foreign FFI_C "sdm_vector_superpose" (Vector -> Vector -> IO ()) u v


-- watch this space...

vectorNorm : Vector -> IO Int
vectorNorm u = foreign FFI_C "sdm_vector_norm" (Vector -> IO Int) u
