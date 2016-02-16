#include "binary_vectorarray.hpp"

// wrapping of c++ api for simple c abi/api for dll based ffi

extern "C" dsm::binary_vectorarray* create_vectorarray(const char* path, const std::size_t els) {
  dsm_trace("create_vectorarray: %s %lu", path, els);
  try {
    return new dsm::binary_vectorarray(path, els);
  } catch (const dsm::error& e) {
    dsm_error("%s", e.what());
    return NULL;
  } catch (...) {
    dsm_fatal("unhandled exception");
    return NULL;
  }
}

extern "C" dsm::binary_vectorarray* load_vectorarray(const char* path) {
  dsm_trace("load_vectorarray: %s", path);
  try {
    return new dsm::binary_vectorarray(path);
  } catch (const std::exception& e) {
    dsm_error("%s", e.what());
    return NULL;
  }
}

extern "C" void extend_vectorarray(dsm::binary_vectorarray* va, std::size_t els) {
  dsm_trace("extend_vectorarray: %p %lu", va, els);
  try {
    va->extend(els);
  } catch (dsm::error& e) {
    dsm_error("ERROR: %s", e.what());
  } catch (...) {
    dsm_fatal("unhandled exception");
  }
}


extern "C" void unmap_vectorarray(dsm::binary_vectorarray* va) {
  dsm_trace("unmap_vectorarray: %p", va);
  try {
    delete va;
  } catch (dsm::error& e) {
    dsm_error("ERROR: %s", e.what());
  } catch (...) {
    dsm_fatal("unhandled exception");
  }
}

