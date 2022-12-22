// -*-c++-*-
#include <iostream>
#define MIN(a,b) a < b ? a : b;

template <int Dummy=0>
void skipBits(uint64_t &cursor, int nbits) {
  cursor += nbits;
}

template <int Dummy=0>
bool exists(uint64_t cursor, uint64_t stop, int nbits) {
  return cursor + nbits < stop;
}

template <typename T>
T lookaheadBits(const uint8_t *buffer, uint64_t cursor, int nbits) {
  T val = 0;
  uint64_t byte_idx = cursor / 8;
  uint64_t bit_idx = cursor % 8;
  if (bit_idx == 0) {
    // this is aligned--read whole byte chunks first
    int idx = 0;
    int bits_left = nbits;
    for (int i = 0; i < nbits-8; i+=8) {
      uint8_t byte = buffer[byte_idx++];
      val <<= idx * 8;
      val |= byte;
      bits_left -= 8;
      idx = 1;
    }
    // now get any stragglers
    if (bits_left > 0) {
      uint8_t byte = buffer[byte_idx++];
      val <<= bits_left;
      byte >>= (8-bits_left);
      val |= byte;
    }    
  } else {
    // this is unaligned
    // first, read up to N bits, where N is how many bits til aligned
    int bits_left = nbits;
    uint8_t byte = buffer[byte_idx++];
    int N = MIN(bits_left, 8 - bit_idx);
    byte >>= (8-N);
    val |= byte;
    bits_left -= N;
    // now read whole byte chunks
    for (int i = 0; i < bits_left-8; i+=8) {
      uint8_t byte = buffer[byte_idx++];
      val <<= 8;
      val |= byte;
      bits_left -= 8;
    }
    // now get any stragglers
    if (bits_left > 0) {
      uint8_t byte = buffer[byte_idx++];
      val <<= bits_left;
      byte >>= (8-bits_left);
      val |= byte;
    }    
  }
  return val;
}

template <typename T>
T parseBits(const uint8_t *buffer, uint64_t &cursor, int nbits) {
  T val = lookaheadBits<T>(buffer, cursor, nbits);
  skipBits(cursor, nbits);
  return val;
}


template <typename T, typename USER>
T syntaxParse(USER &user, uint8_t *buffer, uint64_t cursor, uint64_t stop) {
  return T::parse(user, buffer, cursor, stop);
}

template <typename T, typename USER, typename...Args>
void templateParse(USER &user, uint8_t *buffer, uint64_t cursor, uint64_t stop, Args...args) {
  T::parse(user, buffer, cursor, stop, args...);
}

template <int Dummy=0>
void fatal(const std::string &msg) {
  std::cerr << "[FATAL] " << msg << std::endl;
  exit(-1);
}

