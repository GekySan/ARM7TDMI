#pragma once

#include <cstdint>
#include <cstring>
#include "CPU.hpp"

constexpr std::uint32_t IWRAM_SIZE = 0x8000;
constexpr std::uint32_t IWRAM_BASE = 0x03000000;

struct GBA {
    Arm7Tdmi cpu;
    union {
        std::uint8_t  bytes[IWRAM_SIZE];
        std::uint16_t halves[IWRAM_SIZE / 2];
        std::uint32_t words[IWRAM_SIZE / 4];
    } iwram;
};

inline void InitGba(GBA* pGba, void*) {
    std::memset(pGba, 0, sizeof(*pGba));
    pGba->cpu.gbaSystem = pGba;
}

static inline bool IsInIWRam(std::uint32_t address) {
    return (address >> 24) == 0x03;
}

inline std::uint8_t GbaReadByte(GBA* pGba, std::uint32_t address, int*) {
    return IsInIWRam(address) ? pGba->iwram.bytes[address & (IWRAM_SIZE - 1)] : 0;
}
inline std::uint16_t GbaReadHalf(GBA* pGba, std::uint32_t address, int*) {
    return IsInIWRam(address)
        ? pGba->iwram.halves[(address & (IWRAM_SIZE - 1)) >> 1]
        : 0;
}
inline std::uint32_t GbaReadWord(GBA* pGba, std::uint32_t address, int*) {
    return IsInIWRam(address)
        ? pGba->iwram.words[(address & (IWRAM_SIZE - 1)) >> 2]
        : 0;
}

inline void GbaWriteByte(GBA* pGba, std::uint32_t address, std::uint8_t value, int*) {
    if (IsInIWRam(address)) {
        pGba->iwram.bytes[address & (IWRAM_SIZE - 1)] = value;
    }
}
inline void GbaWriteHalf(GBA* pGba, std::uint32_t address, std::uint16_t value, int*) {
    if (IsInIWRam(address)) {
        pGba->iwram.halves[(address & (IWRAM_SIZE - 1)) >> 1] = value;
    }
}
inline void GbaWriteWord(GBA* pGba, std::uint32_t address, std::uint32_t value, int*) {
    if (IsInIWRam(address)) {
        pGba->iwram.words[(address & (IWRAM_SIZE - 1)) >> 2] = value;
    }
}
