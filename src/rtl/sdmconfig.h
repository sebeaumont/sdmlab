#pragma once // XX may have to use trad methods for c90 
/* Copyright (c) 2012-2017 Simon Beaumont
 * This is the fundamental config file for compile time (baked in)
 * implementation constants. DO NOT VIOLATE THESE BOUNDS.
 */
/* xx can/should we make this a macro? */
//typedef unsigned long long SDM_VECTOR_ELEMENT_TYPE;
#define SDM_VECTOR_ELEMENT_TYPE unsigned long long 
#define SDM_VECTOR_ELEMS 256
 /* xxx rename to BASIS_SIZE and adjust terminology/interface throughout */
#define SDM_VECTOR_BASIS_SIZE 16
#define SDM_VECTOR_PAYLOAD_SIZE sizeof(SDM_VECTOR_ELEMENT_TYPE)*SDM_VECTOR_ELEMS
/*
 There may be more...
*/
