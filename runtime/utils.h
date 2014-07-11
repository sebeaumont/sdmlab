 /**
 * 扬子鳄 The Smaller Alligator
 * Copyright (c) Simon Beaumont 2013-2014 - All Rights Reserved.
 * 
 * This file defines the implementation utilities and constants
 */

#ifndef __UTILS_H__
#define __UTILS_H__

#include <sys/errno.h>
#include <string.h>

/**
 * logging and errors -- TODO -- make this much nicer!
 */

#define ERRTXT_SIZE 512
static char last_error[ERRTXT_SIZE];

// write systemerror text into last_error 
#define sys_error() strerror_r(errno, last_error, ERRTXT_SIZE)

#define dsm_log(format, ...) {fprintf(stdout, format, ## __VA_ARGS__);fprintf(stdout, "\n");}
//#define dsm_log(format, ...) snprintf(last_error, ERRTXT_SIZE, format, ## __VA_ARGS__)

#define dsm_error(format, ...) {dsm_log(format, ## __VA_ARGS__);}

#define dsm_fatal(format, ...) {dsm_log(format, ## __VA_ARGS__);}

#define dsm_warn(format, ...) {dsm_log(format, ## __VA_ARGS__);}

#ifdef DEBUG
#define dsm_trace(format, ...) {dsm_log(format, ## __VA_ARGS__);}
#else
#define dsm_trace(format, ...)
#endif

// Fast arithmetic and bit twiddling.

#define ROUND_UP(N,S) ((((N)) + (S)-1)  & (~((S)-1)))
#define ROUND_DOWN(N,S) (((N) / (S)) * (S))

#define MB(X) (X)/(double)(1024*1024)
#define ROTL(r,n) (((r)<<(n)) | ((r)>>((8*sizeof(r))-(n))))


#endif 
