#include <iostream>
#include <iomanip>
#include <cstring>

#include "gba.hpp"
#include "CPU.hpp"

void DumpRegisters(const Arm7Tdmi& cpu, int step)
{
    static std::uint32_t prevRegs[16]{};
    static bool prevN = false, prevZ = false, prevC = false, prevV = false;
    static bool firstTime = true;

    std::ios::fmtflags oldFlags(std::cout.flags());

    std::cout << "\n=== Etape " << step << " ===\n";

    for (int rIndex = 0; rIndex < 16; ++rIndex) {
        if (firstTime || cpu.regs[rIndex] != prevRegs[rIndex]) {
            std::cout << "R" << std::dec << rIndex
                << " = 0x" << std::hex
                << std::setw(8) << std::setfill('0')
                << cpu.regs[rIndex]
                << (rIndex == 15 ? " (PC)" : "")
                    << '\n';
                prevRegs[rIndex] = cpu.regs[rIndex];
        }
    }

    bool n = cpu.cpsr.negativeFlag, z = cpu.cpsr.zeroFlag,
        c = cpu.cpsr.carryFlag, v = cpu.cpsr.overflowFlag;

    if (firstTime || n != prevN || z != prevZ || c != prevC || v != prevV) {
        std::cout << std::dec
            << "N=" << n << " Z=" << z
            << " C=" << c << " V=" << v << '\n';
        prevN = n; prevZ = z; prevC = c; prevV = v;
    }

    firstTime = false;
    std::cout.flags(oldFlags);
}

constexpr std::uint32_t gProgram[] = {
    0xE3A00000,   // MOV   R0, #0
    0xE2901000,   // ADDS  R1, R0, #0
    0xE3E0200F,   // MVN   R2, #0x0F
    0xE2923010,   // ADDS  R3, R2, #0x10
    0xE2D14001,   // SBCS  R4, R1, #1
    0xE2745000,   // RSBS  R5, R4, #0
    0xE3A06101,   // MOV   R6, #0x40000000
    0xE0967006,   // ADDS  R7, R6, R6
    0xE0978007,   // ADDS  R8, R7, R7
    0xE0190696,   // MULS  R9, R6, R6
    0xE3A0A001,   // MOV   R10, #1
    0xE2BAA000,   // ADCS  R10, R10, #0
    0xEAFFFFFE    // B     .
};

int main()
{
    GBA gbaSys{};
    InitGba(&gbaSys, nullptr);

    std::memcpy(gbaSys.iwram.words, gProgram, sizeof(gProgram));

    gbaSys.cpu.programCounter = 0x03000000;
    gbaSys.cpu.cpsr.thumbState = 0;
    CpuFlush(&gbaSys.cpu);

    constexpr int STEPS_TO_SHOW = 18;
    for (int step = 0; step < STEPS_TO_SHOW; ++step) {
        ArmExecuteInstruction(&gbaSys.cpu);
        DumpRegisters(gbaSys.cpu, step);
    }
    return 0;
}
