//
//  vectorspace.hpp
//
//  Created by Simon Beaumont on 27/06/2012.
//  Copyright (c) 2012, 2013 Simon Beaumont All rights reserved.
//  Copyright (c) 2013 Datalligator Ltd. All Rights Reserved.

#ifndef __VECTORARRAY_HPP__
#define __VECTORARRAY_HPP__

#include "unistd_fix.h"
#include "utils.h"

#include <cstddef>
#include <iostream>
#include <stdexcept>

#include <fcntl.h>

#include <limits.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <iostream>
#include <stdexcept>
#include <new>

#include "runtime_exceptions.hpp"
#include <boost/lexical_cast.hpp>

namespace dsm {

  /*
   * A generic vectorarray can have vectors of different types.
   *
   * All vectors are allocated from a fixed mmap region
   *
   * Whilst this template allows for vectors to be of any type and size
   * there are practical considerations that usally see the vector_size coming out 
   * sized as single system page (usually 4kB on unix).
   */ 
  
  template <typename T, std::size_t N>
  class vectorarray {

  public:
    
    /* defined types */
    typedef T vector_element_t;
    typedef vector_element_t* vector_t;
    
    /* constants in class */
    static const std::size_t vector_els = N;
    static const std::size_t vector_size = N * sizeof(T);
    
    /* computed in constructors */
    std::size_t n_vectors;
    bool flags;

    /* destructor */
    virtual ~vectorarray();
    
    /* default constructor */
    vectorarray();
    
    /* create a new vectorarray for els */
    vectorarray(const char* file, const std::size_t els);
    
    /* open an existing vectorarray */
    vectorarray(const char* file);
    
    /* bounds checked */
    vector_t get_vector(const std::size_t i) const;
    
    /* extend the array */
    void extend(const std::size_t n);

  protected:

    /* could implement some std::iterator based on these */
    const vector_t begin() const { return m_begin; }
    const vector_t end() const { return m_end; }
    
    /* overload the subscript operator */
    vector_t operator[] (const std::size_t i) const;

    /* computed in constructors */
    std::size_t region_size;
    std::string region_name;
    
  private:

    int m_fd;
    vector_t m_begin;
    vector_t m_end;
    
  };


  /////////////////////////////////////
  // method implementation templates //
  /////////////////////////////////////

  /* subscript operator: naked address arithmetic with no bounds checking */
  
  template <typename T, std::size_t N>
  inline typename vectorarray<T,N>::vector_t vectorarray<T,N>::operator[] (const std::size_t i) const {
    return (m_begin + (i * vector_els));
  }

  
  /* bounds checked subscript for those of a nervous disposition */
  
  template <typename T, std::size_t N>
  inline typename vectorarray<T,N>::vector_t vectorarray<T,N>::get_vector(const std::size_t i) const {
    if (i < 0 || i > n_vectors) throw dsm::error("out of range vector id: "+ boost::lexical_cast<std::string>(i));
    else return (*this)[i];
  }
  
  
  /* new named vectorarray */
  
  template <typename T, std::size_t N>
  vectorarray<T,N>::vectorarray(const char* file, const std::size_t els) {

    //readonly = false;
    n_vectors = els;
    region_name = std::string(file);
    
    // compute required memory 
    std::size_t required_size = els * vector_size; 
    
    /* allocate the actual vectorarray memory */
    
    // make sure we map a mutiple of pagesize
    //long pagesize = sysconf(_SC_PAGESIZE);
    
    // in case we are experimenting with non-pagesized vectors
    region_size = (size_t) ROUND_UP(required_size, vector_size);
    
    // preallocate a file for the region
    m_fd = open(file, O_RDWR|O_CREAT|O_EXCL, 0600);
    if (m_fd < 0) {throw dsm::error("vectorarray:open:" + boost::lexical_cast<std::string>(file));}
    
    int sts = ftruncate(m_fd, (off_t) region_size);
    if (sts < 0) {throw dsm::error("vectorarray:extend");}
    
    // make sure file is extended 
    sts = (int)lseek(m_fd, (off_t)-1, SEEK_END);
    if (sts < 0) {throw dsm::error("vectorarray:seek");} 
    
    sts = (int) write(m_fd, "\0", 1);
    if (sts < 0) {throw dsm::error("vectorarray:write");}
    
    // mmap
    m_begin = (vector_t) mmap(NULL, region_size, PROT_READ|PROT_WRITE, MAP_SHARED, m_fd, 0);
    
    if (m_begin == MAP_FAILED) {throw dsm::error("vectorarray:mmap");}
    
    // we are going to do random reads during training
    if (madvise((void*) m_begin, region_size, MADV_RANDOM|MADV_WILLNEED) < 0) {
      throw dsm::error("vectorarray:madvise");
    }
    
    //end_ = (*this)[n_vectors];
    m_end = m_begin + region_size;
  }
  
  
  /* open existing vectorarray map */

  template <typename T, std::size_t N>
  vectorarray<T,N>::vectorarray(const char* file) {

    //readonly = true;
    region_name = std::string(file);
    
    // open and stat the file provided 
    struct stat fs;
    
    m_fd = open(file, O_RDWR);
    if (m_fd < 0) {throw dsm::error("vectorarray:open:" + boost::lexical_cast<std::string>(file));}
    fstat(m_fd, &fs);
    
    // make sure we map a mutiple of pagesize
    //long pagesize = sysconf(_SC_PAGESIZE);
    
    region_size = (size_t) ROUND_DOWN(fs.st_size, vector_size);
    //region_size = fs.st_size;

    // we reserve one vector
    n_vectors = region_size / vector_size;

    // mmap
    m_begin = (vector_t) mmap(NULL, region_size, PROT_READ|PROT_WRITE, MAP_SHARED, m_fd, 0);
    
    if (m_begin == MAP_FAILED) {throw dsm::error("vectorarray:mmap");}
    
    // we are going to do sequential reads during search
    if (madvise((void*) m_begin, region_size, MADV_SEQUENTIAL|MADV_WILLNEED) < 0) {
      throw dsm::error("vectorarray:madvise"); 
    }
    
    //end_ = (*this)[n_vectors];
    m_end = m_begin + region_size;
  }

  
  /* destructor */
  
  template <typename T, std::size_t N>
  vectorarray<T, N>::~vectorarray() {
    
    if (m_begin != 0) {
      msync(m_begin, region_size, MS_ASYNC);
      munmap(m_begin, region_size);
      close(m_fd);
      m_begin= 0, m_end= 0, region_size = 0, n_vectors = 0, m_fd = 0;
    }
  }


  /* UC: extend region -- if this works then we might not need multi-segment arrays*/

  template <typename T, std::size_t N>
  void vectorarray<T,N>::extend(std::size_t m) {
    
    /* try and extend the exisitng mapping by m vectors firstly we try and extend the
       region into contiguous virtual address space so we dont have to remap the whole array */
    std::size_t extend_size = m * vector_size;
    
    // extend the file first -- if this fails then all bets are off...
    int sts = ftruncate(m_fd, (off_t) region_size + extend_size);
    if (sts < 0) {throw dsm::error("vectorarray:extend:ftruncate");}

    void* new_segment = mmap(m_end, extend_size, PROT_READ|PROT_WRITE, MAP_FIXED|MAP_SHARED, m_fd, 0);
    if (new_segment == MAP_FAILED) {throw dsm::error("vectorarray:extend:mmap:strategy1");} // we can try and remap if this fails!

    // TODO madvise...
    // update array extents
    m_end += extend_size;
    n_vectors += m; 
  }
    
}      // namespace dsm
#endif // __VECTORARRAY_HPP__
