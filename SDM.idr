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
-- TODO make these types more constrained

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
-- destructive vector ops 

vectorOnes : Vector -> IO ()
vectorOnes = foreign FFI_C "sdm_vector_ones" (Vector -> IO ())

vectorZero : Vector -> IO ()
vectorZero = foreign FFI_C "sdm_vector_zero" (Vector -> IO ())

vectorRandom : Vector -> IO ()
vectorRandom = foreign FFI_C "sdm_vector_random" (Vector -> IO ())

vectorSuperpose : Vector -> Vector -> IO ()
vectorSuperpose = foreign FFI_C "sdm_vector_superpose" (Vector -> Vector -> IO ())

vectorSubtract : Vector -> Vector -> IO ()
vectorSubtract = foreign FFI_C "sdm_vector_subtract" (Vector -> Vector -> IO ())

vectorMultiply : Vector -> Vector -> IO ()
vectorMultiply = foreign FFI_C "sdm_vector_subtract" (Vector -> Vector -> IO ())


-- vector functions 

vectorNorm : Vector -> IO Int
vectorNorm = foreign FFI_C "sdm_vector_norm" (Vector -> IO Int)

{-

const size_t sdm_vector_distance(const vector restrict u, const vector restrict v);
const size_t sdm_vector_inner(const vector restrict u, const vector restrict v);
const size_t sdm_vector_countsum(const vector restrict u, const vector restrict v);
const float sdm_vector_similarity(const vector restrict u, const vector restrict v);
const float sdm_vector_density(const vector u);
-}
