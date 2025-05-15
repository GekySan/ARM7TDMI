#pragma once

#include <cstdint>

struct GBA;

enum ConditionCode {
    COND_EQ,
    COND_NE,
    COND_CS,
    COND_CC,
    COND_MI,
    COND_PL,
    COND_VS,
    COND_VC,
    COND_HI,
    COND_LS,
    COND_GE,
    COND_LT,
    COND_GT,
    COND_LE,
    COND_AL
};

enum DataProcessingOpcode {
    OPC_AND,
    OPC_EOR,
    OPC_SUB,
    OPC_RSB,
    OPC_ADD,
    OPC_ADC,
    OPC_SBC,
    OPC_RSC,
    OPC_TST,
    OPC_TEQ,
    OPC_CMP,
    OPC_CMN,
    OPC_ORR,
    OPC_MOV,
    OPC_BIC,
    OPC_MVN
};

enum ShiftType {
    SHIFT_LSL,
    SHIFT_LSR,
    SHIFT_ASR,
    SHIFT_ROR
};

enum ThumbAluOpcode {
    THUMB_AND,
    THUMB_EOR,
    THUMB_LSL,
    THUMB_LSR,
    THUMB_ASR,
    THUMB_ADC,
    THUMB_SBC,
    THUMB_ROR,
    THUMB_TST,
    THUMB_NEG,
    THUMB_CMP,
    THUMB_CMN,
    THUMB_ORR,
    THUMB_MUL,
    THUMB_BIC,
    THUMB_MVN
};

union ArmInstruction {
    std::uint32_t raw;
    struct {
        std::uint32_t bits28 : 28;
        std::uint32_t conditionCode : 4;
    };
    struct {
        std::uint32_t operand2 : 12;
        std::uint32_t destReg : 4;
        std::uint32_t operand1Reg : 4;
        std::uint32_t setConditionFlags : 1;
        std::uint32_t operationCode : 4;
        std::uint32_t immediateFlag : 1;
        std::uint32_t decodeBits1 : 2;
        std::uint32_t conditionCode : 4;
    } data_proc;
    struct {
        std::uint32_t rm : 4;
        std::uint32_t decodeBits2 : 4;
        std::uint32_t rs : 4;
        std::uint32_t rn : 4;
        std::uint32_t rd : 4;
        std::uint32_t setConditionFlags : 1;
        std::uint32_t accumulate : 1;
        std::uint32_t decodeBits1 : 6;
        std::uint32_t conditionCode : 4;
    } multiply;
    struct {
        std::uint32_t rm : 4;
        std::uint32_t decodeBits2 : 4;
        std::uint32_t rs : 4;
        std::uint32_t rdLo : 4;
        std::uint32_t rdHi : 4;
        std::uint32_t setConditionFlags : 1;
        std::uint32_t accumulate : 1;
        std::uint32_t signedMultiply : 1;
        std::uint32_t decodeBits1 : 5;
        std::uint32_t conditionCode : 4;
    } multiply_long;
    struct {
        std::uint32_t operand2 : 12;
        std::uint32_t destReg : 4;
        std::uint32_t c : 1;
        std::uint32_t x : 1;
        std::uint32_t s : 1;
        std::uint32_t f : 1;
        std::uint32_t decodeBits2 : 1;
        std::uint32_t op : 1;
        std::uint32_t p : 1;
        std::uint32_t decodeBits1 : 2;
        std::uint32_t immediateFlag : 1;
        std::uint32_t decodeBits0 : 2;
        std::uint32_t conditionCode : 4;
    } psr_trans;
    struct {
        std::uint32_t rm : 4;
        std::uint32_t decodeBits2 : 8;
        std::uint32_t rd : 4;
        std::uint32_t rn : 4;
        std::uint32_t decodeBits1 : 2;
        std::uint32_t byteMode : 1;
        std::uint32_t decodeBits0 : 5;
        std::uint32_t conditionCode : 4;
    } swap;
    struct {
        std::uint32_t rn : 4;
        std::uint32_t decodeBits0 : 24;
        std::uint32_t conditionCode : 4;
    } branch_ex;
    struct {
        std::uint32_t rm : 4;
        std::uint32_t decodeBits3 : 1;
        std::uint32_t highHalf : 1;
        std::uint32_t signExtend : 1;
        std::uint32_t decodeBits2 : 5;
        std::uint32_t rd : 4;
        std::uint32_t rn : 4;
        std::uint32_t load : 1;
        std::uint32_t writeBack : 1;
        std::uint32_t immediate : 1;
        std::uint32_t up : 1;
        std::uint32_t preIndex : 1;
        std::uint32_t decodeBits1 : 3;
        std::uint32_t conditionCode : 4;
    } half_transr;
    struct {
        std::uint32_t offLow : 4;
        std::uint32_t decodeBits3 : 1;
        std::uint32_t highHalf : 1;
        std::uint32_t signExtend : 1;
        std::uint32_t decodeBits2 : 1;
        std::uint32_t offHigh : 4;
        std::uint32_t rd : 4;
        std::uint32_t rn : 4;
        std::uint32_t load : 1;
        std::uint32_t writeBack : 1;
        std::uint32_t immediate : 1;
        std::uint32_t up : 1;
        std::uint32_t preIndex : 1;
        std::uint32_t decodeBits1 : 3;
        std::uint32_t conditionCode : 4;
    } half_transi;
    struct {
        std::uint32_t offset : 12;
        std::uint32_t destReg : 4;
        std::uint32_t baseReg : 4;
        std::uint32_t load : 1;
        std::uint32_t writeBack : 1;
        std::uint32_t byteTransfer : 1;
        std::uint32_t up : 1;
        std::uint32_t preIndex : 1;
        std::uint32_t immediate : 1;
        std::uint32_t decodeBits1 : 2;
        std::uint32_t conditionCode : 4;
    } single_trans;
    struct {
        std::uint32_t unused2 : 4;
        std::uint32_t decodeBits2 : 1;
        std::uint32_t unused1 : 20;
        std::uint32_t decodeBits1 : 3;
        std::uint32_t conditionCode : 4;
    } undefined;
    struct {
        std::uint32_t regList : 16;
        std::uint32_t baseReg : 4;
        std::uint32_t load : 1;
        std::uint32_t writeBack : 1;
        std::uint32_t sBit : 1;
        std::uint32_t up : 1;
        std::uint32_t preIndex : 1;
        std::uint32_t decodeBits1 : 3;
        std::uint32_t conditionCode : 4;
    } block_trans;
    struct {
        std::uint32_t offset : 24;
        std::uint32_t link : 1;
        std::uint32_t decodeBits1 : 3;
        std::uint32_t conditionCode : 4;
    } branch;
    struct {
        std::uint32_t argument : 24;
        std::uint32_t decodeBits1 : 4;
        std::uint32_t conditionCode : 4;
    } sw_intr;
};

union ThumbInstruction {
    std::uint16_t raw;
    struct {
        std::uint16_t lowNibble : 4;
        std::uint16_t nextNibble : 4;
        std::uint16_t next2Nibble : 4;
        std::uint16_t highNibble : 4;
    };
    struct {
        std::uint16_t destReg : 3;
        std::uint16_t sourceReg : 3;
        std::uint16_t shiftOffset : 5;
        std::uint16_t shiftOp : 2;
        std::uint16_t decodeBits1 : 3;
    } shift;
    struct {
        std::uint16_t destReg : 3;
        std::uint16_t sourceReg : 3;
        std::uint16_t op2 : 3;
        std::uint16_t subOp : 1;
        std::uint16_t immediateFlag : 1;
        std::uint16_t decodeBits1 : 5;
    } add;
    struct {
        std::uint16_t immediateValue : 8;
        std::uint16_t destReg : 3;
        std::uint16_t aluOp : 2;
        std::uint16_t decodeBits1 : 3;
    } alu_imm;
    struct {
        std::uint16_t destReg : 3;
        std::uint16_t sourceReg : 3;
        std::uint16_t opcode : 4;
        std::uint16_t decodeBits1 : 6;
    } alu;
    struct {
        std::uint16_t destReg : 3;
        std::uint16_t sourceReg : 3;
        std::uint16_t h2 : 1;
        std::uint16_t h1 : 1;
        std::uint16_t op : 2;
        std::uint16_t decodeBits1 : 6;
    } hi_ops;
    struct {
        std::uint16_t offset : 8;
        std::uint16_t destReg : 3;
        std::uint16_t decodeBits1 : 5;
    } ldst_pc;
    struct {
        std::uint16_t destReg : 3;
        std::uint16_t baseReg : 3;
        std::uint16_t offsetReg : 3;
        std::uint16_t decodeBits2 : 1;
        std::uint16_t byteMode : 1;
        std::uint16_t load : 1;
        std::uint16_t decodeBits1 : 4;
    } ldst_reg;
    struct {
        std::uint16_t destReg : 3;
        std::uint16_t baseReg : 3;
        std::uint16_t offsetReg : 3;
        std::uint16_t decodeBits2 : 1;
        std::uint16_t signExtend : 1;
        std::uint16_t half : 1;
        std::uint16_t decodeBits1 : 4;
    } ldst_s;
    struct {
        std::uint16_t destReg : 3;
        std::uint16_t baseReg : 3;
        std::uint16_t offset : 5;
        std::uint16_t load : 1;
        std::uint16_t byteMode : 1;
        std::uint16_t decodeBits1 : 3;
    } ldst_imm;
    struct {
        std::uint16_t destReg : 3;
        std::uint16_t baseReg : 3;
        std::uint16_t offset : 5;
        std::uint16_t load : 1;
        std::uint16_t decodeBits1 : 4;
    } ldst_h;
    struct {
        std::uint16_t offset : 8;
        std::uint16_t destReg : 3;
        std::uint16_t load : 1;
        std::uint16_t decodeBits1 : 4;
    } ldst_sp;
    struct {
        std::uint16_t offset : 8;
        std::uint16_t destReg : 3;
        std::uint16_t sp : 1;
        std::uint16_t decodeBits1 : 4;
    } ldst_addr;
    struct {
        std::uint16_t offset : 7;
        std::uint16_t subtract : 1;
        std::uint16_t decodeBits1 : 8;
    } add_sp;
    struct {
        std::uint16_t regList : 8;
        std::uint16_t rBit : 1;
        std::uint16_t decodeBits2 : 2;
        std::uint16_t load : 1;
        std::uint16_t decodeBits1 : 4;
    } push_pop;
    struct {
        std::uint16_t regList : 8;
        std::uint16_t baseReg : 3;
        std::uint16_t load : 1;
        std::uint16_t decodeBits1 : 4;
    } ldst_m;
    struct {
        std::uint16_t offset : 8;
        std::uint16_t condition : 4;
        std::uint16_t decodeBits1 : 4;
    } b_cond;
    struct {
        std::uint16_t argument : 8;
        std::uint16_t decodeBits1 : 8;
    } swi;
    struct {
        std::uint16_t offset : 11;
        std::uint16_t decodeBits1 : 5;
    } branch;
    struct {
        std::uint16_t offset : 11;
        std::uint16_t h : 1;
        std::uint16_t decodeBits1 : 4;
    } branch_l;
};

ArmInstruction ThumbDecodeInstruction(ThumbInstruction instr);

enum CpuMode : std::uint32_t {
    MODE_USER = 0b10000,
    MODE_FIQ = 0b10001,
    MODE_IRQ = 0b10010,
    MODE_SVC = 0b10011,
    MODE_ABT = 0b10111,
    MODE_UND = 0b11011,
    MODE_SYSTEM = 0b11111
};

enum CpuInterrupt {
    INT_RESET,
    INT_UND,
    INT_SWI,
    INT_PABT,
    INT_DABT,
    INT_ADDR,
    INT_IRQ,
    INT_FIQ
};

enum RegisterBank {
    BANK_USER,
    BANK_FIQ,
    BANK_SVC,
    BANK_ABT,
    BANK_IRQ,
    BANK_UND,
    BANK_COUNT
};

struct Arm7Tdmi
{
    GBA* gbaSystem;

    union {
        std::uint32_t regs[16];
        struct {
            std::uint32_t generalRegs[13];
            std::uint32_t stackPointer;
            std::uint32_t linkRegister;
            std::uint32_t programCounter;
        };
    };

    union {
        std::uint32_t raw;
        struct {
            std::uint32_t modeBits : 5;
            std::uint32_t thumbState : 1;
            std::uint32_t fiqDisable : 1;
            std::uint32_t irqDisable : 1;
            std::uint32_t reserved : 20;
            std::uint32_t overflowFlag : 1;
            std::uint32_t carryFlag : 1;
            std::uint32_t zeroFlag : 1;
            std::uint32_t negativeFlag : 1;
        };
    } cpsr;

    std::uint32_t savedSpsr;

    std::uint32_t bankedRegsR8To12[2][5];
    std::uint32_t bankedStackPointers[BANK_COUNT];
    std::uint32_t bankedLinkRegisters[BANK_COUNT];
    std::uint32_t bankedSavedSpsr[BANK_COUNT];

    ArmInstruction currentArmInstruction;
    ArmInstruction nextArmInstruction;

    bool thumbBranchLinkState;
    int cycleCount;
};

bool EvaluateCondition(Arm7Tdmi* cpu, ArmInstruction instruction);
void ArmExecuteInstruction(Arm7Tdmi* cpu);
void ExecuteArmDataProc(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmMultiply(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmMultiplyLong(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmPsrTransfer(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmSwap(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmBranchExchange(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmHalfTransfer(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmSingleTransfer(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmUndefined(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmBlockTransfer(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmBranch(Arm7Tdmi* cpu, ArmInstruction instruction);
void ExecuteArmSoftwareInterrupt(Arm7Tdmi* cpu, ArmInstruction instruction);
void CpuFetch(Arm7Tdmi* cpu);
void CpuFlush(Arm7Tdmi* cpu);
void CpuUpdateMode(Arm7Tdmi* cpu, CpuMode oldMode);
void CpuHandleInterrupt(Arm7Tdmi* cpu, CpuInterrupt intr);
std::uint8_t  CpuReadByte(Arm7Tdmi* cpu, std::uint32_t addr);
std::uint16_t CpuReadHalf(Arm7Tdmi* cpu, std::uint32_t addr);
std::uint32_t CpuReadWord(Arm7Tdmi* cpu, std::uint32_t addr);
void CpuWriteByte(Arm7Tdmi* cpu, std::uint32_t addr, std::uint8_t  data);
void CpuWriteHalf(Arm7Tdmi* cpu, std::uint32_t addr, std::uint16_t data);
void CpuWriteWord(Arm7Tdmi* cpu, std::uint32_t addr, std::uint32_t data);
void CpuInternalCycle(Arm7Tdmi* cpu);
