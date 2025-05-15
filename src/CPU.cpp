#include <cstdio>

#include "CPU.hpp"
#include "GBA.hpp"

bool EvaluateCondition(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    switch (instruction.conditionCode) {
    case COND_EQ: return cpu->cpsr.zeroFlag;
    case COND_NE: return !cpu->cpsr.zeroFlag;
    case COND_CS: return cpu->cpsr.carryFlag;
    case COND_CC: return !cpu->cpsr.carryFlag;
    case COND_MI: return cpu->cpsr.negativeFlag;
    case COND_PL: return !cpu->cpsr.negativeFlag;
    case COND_VS: return cpu->cpsr.overflowFlag;
    case COND_VC: return !cpu->cpsr.overflowFlag;
    case COND_HI: return cpu->cpsr.carryFlag && !cpu->cpsr.zeroFlag;
    case COND_LS: return !cpu->cpsr.carryFlag || cpu->cpsr.zeroFlag;
    case COND_GE: return cpu->cpsr.negativeFlag == cpu->cpsr.overflowFlag;
    case COND_LT: return cpu->cpsr.negativeFlag != cpu->cpsr.overflowFlag;
    case COND_GT: return !cpu->cpsr.zeroFlag && (cpu->cpsr.negativeFlag == cpu->cpsr.overflowFlag);
    case COND_LE: return cpu->cpsr.zeroFlag || (cpu->cpsr.negativeFlag != cpu->cpsr.overflowFlag);
    case COND_AL: return true;
    default: return true;
    }
}

void ArmExecuteInstruction(Arm7Tdmi* cpu)
{
    ArmInstruction instruction = cpu->currentArmInstruction;
    if (!EvaluateCondition(cpu, instruction)) {
        CpuFetch(cpu);
        return;
    }

    // Software interrupt
    if (instruction.sw_intr.decodeBits1 == 0b1111) {
        ExecuteArmSoftwareInterrupt(cpu, instruction);
    }
    // Branch
    else if (instruction.branch.decodeBits1 == 0b101) {
        ExecuteArmBranch(cpu, instruction);
    }
    // Block transfer
    else if (instruction.block_trans.decodeBits1 == 0b100) {
        ExecuteArmBlockTransfer(cpu, instruction);
    }
    // Undefined
    else if (instruction.undefined.decodeBits1 == 0b011 && instruction.undefined.decodeBits2 == 1) {
        ExecuteArmUndefined(cpu, instruction);
    }
    // Single data transfer
    else if (instruction.single_trans.decodeBits1 == 0b01) {
        ExecuteArmSingleTransfer(cpu, instruction);
    }
    // Branch exchange (special pattern)
    else if (instruction.branch_ex.decodeBits0 == 0b000100101111111111110001) {
        ExecuteArmBranchExchange(cpu, instruction);
    }
    // Swap (special pattern)
    else if (instruction.swap.decodeBits0 == 0b00010 && instruction.swap.decodeBits2 == 0b00 &&
        instruction.swap.decodeBits1 == 0b00001001)
    {
        ExecuteArmSwap(cpu, instruction);
    }
    // Multiply
    else if (instruction.multiply.decodeBits1 == 0b000000 && instruction.multiply.decodeBits2 == 0b1001) {
        ExecuteArmMultiply(cpu, instruction);
    }
    // Multiply long
    else if (instruction.multiply_long.decodeBits1 == 0b00001 &&
        instruction.multiply_long.decodeBits2 == 0b1001)
    {
        ExecuteArmMultiplyLong(cpu, instruction);
    }
    // Halfword transfer register
    else if (instruction.half_transr.decodeBits1 == 0b000 && instruction.half_transr.immediate == 0 &&
        instruction.half_transr.decodeBits2 == 0b00001 && instruction.half_transr.decodeBits3 == 1)
    {
        ExecuteArmHalfTransfer(cpu, instruction);
    }
    // Halfword transfer immediate
    else if (instruction.half_transi.decodeBits1 == 0b000 && instruction.half_transi.immediate == 1 &&
        instruction.half_transi.decodeBits2 == 1 && instruction.half_transi.decodeBits3 == 1)
    {
        ExecuteArmHalfTransfer(cpu, instruction);
    }
    // PSR transfer
    else if (instruction.psr_trans.decodeBits0 == 0b00 && instruction.psr_trans.decodeBits2 == 0b10 &&
        instruction.psr_trans.decodeBits2 == 0)
    {
        ExecuteArmPsrTransfer(cpu, instruction);
    }
    // Data processing
    else if (instruction.data_proc.decodeBits1 == 0b00) {
        ExecuteArmDataProc(cpu, instruction);
    }
}

void ExecuteArmDataProc(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    std::uint32_t operand2Value;
    bool shiftRegisterUsed = false;
    std::uint32_t carryVal = cpu->cpsr.carryFlag;
    std::uint32_t overflowVal = cpu->cpsr.overflowFlag;
    std::uint32_t zeroVal, negativeVal;

    // Immediate
    if (instruction.data_proc.immediateFlag) {
        operand2Value = instruction.data_proc.operand2 & 0xFF;
        std::uint32_t shiftAmount = instruction.data_proc.operand2 >> 8;
        shiftAmount *= 2;
        carryVal = (operand2Value >> (shiftAmount - 1)) & 1;
        operand2Value = (operand2Value >> shiftAmount) | (operand2Value << (32 - shiftAmount));
    }
    else {
        // Register-based shift
        std::uint32_t shiftField = instruction.data_proc.operand2 >> 4;
        std::uint32_t shiftKind = (shiftField >> 1) & 0b11;
        std::uint32_t shiftAmount;
        if (shiftField & 1) {
            CpuFetch(cpu);
            shiftRegisterUsed = true;
            std::uint32_t rsIndex = shiftField >> 4;
            shiftAmount = cpu->regs[rsIndex] & 0xFF;
            CpuInternalCycle(cpu);
        }
        else {
            shiftAmount = shiftField >> 3;
        }
        std::uint32_t rmIndex = instruction.data_proc.operand2 & 0xF;
        operand2Value = cpu->regs[rmIndex];

        if (!(shiftAmount == 0 && shiftRegisterUsed)) {
            switch (shiftKind) {
            case SHIFT_LSL:
                if (shiftAmount > 32) carryVal = 0;
                else if (shiftAmount > 0) carryVal = (operand2Value >> (32 - shiftAmount)) & 1;
                if (shiftAmount >= 32) operand2Value = 0;
                else operand2Value <<= shiftAmount;
                break;
            case SHIFT_LSR:
                if (shiftAmount == 0) shiftAmount = 32;
                if (shiftAmount > 32) carryVal = 0;
                else carryVal = (operand2Value >> (shiftAmount - 1)) & 1;
                if (shiftAmount >= 32) operand2Value = 0;
                else operand2Value >>= shiftAmount;
                break;
            case SHIFT_ASR: {
                if (shiftAmount == 0) shiftAmount = 32;
                if (shiftAmount > 32) shiftAmount = 32;
                std::int32_t signedOp2 = static_cast<std::int32_t>(operand2Value);
                carryVal = (signedOp2 >> (shiftAmount - 1)) & 1;
                if (shiftAmount == 32) {
                    signedOp2 = (carryVal) ? -1 : 0;
                }
                else {
                    signedOp2 >>= shiftAmount;
                }
                operand2Value = static_cast<std::uint32_t>(signedOp2);
                break;
            }
            case SHIFT_ROR:
                if (shiftAmount == 0) {
                    carryVal = operand2Value & 1;
                    operand2Value >>= 1;
                    operand2Value |= (cpu->cpsr.carryFlag << 31);
                }
                else {
                    carryVal = (operand2Value >> (shiftAmount - 1)) & 1;
                    operand2Value = (operand2Value >> shiftAmount) | (operand2Value << (32 - shiftAmount));
                }
                break;
            }
        }
    }

    std::uint32_t op1 = cpu->regs[instruction.data_proc.operand1Reg];
    if (instruction.data_proc.operand1Reg == 15 && instruction.data_proc.destReg != 15) {
        op1 &= ~0b10;
    }
    if (!shiftRegisterUsed) {
        CpuFetch(cpu);
    }

    std::uint32_t result = 0;
    bool isArithmetic = false;
    bool isSubtraction = false;
    bool isReverseSub = false;
    bool useCarry = false;
    bool writeResult = true;

    switch (instruction.data_proc.operationCode) {
    case OPC_AND: result = op1 & operand2Value; break;
    case OPC_EOR: result = op1 ^ operand2Value; break;
    case OPC_SUB: isArithmetic = true; isSubtraction = true; break;
    case OPC_RSB: isArithmetic = true; isSubtraction = true; isReverseSub = true; break;
    case OPC_ADD: isArithmetic = true; break;
    case OPC_ADC: isArithmetic = true; useCarry = true; break;
    case OPC_SBC: isArithmetic = true; isSubtraction = true; useCarry = true; break;
    case OPC_RSC: isArithmetic = true; isSubtraction = true; isReverseSub = true; useCarry = true; break;
    case OPC_TST: result = op1 & operand2Value; writeResult = false; break;
    case OPC_TEQ: result = op1 ^ operand2Value; writeResult = false; break;
    case OPC_CMP: isArithmetic = true; isSubtraction = true; writeResult = false; break;
    case OPC_CMN: isArithmetic = true; writeResult = false; break;
    case OPC_ORR: result = op1 | operand2Value; break;
    case OPC_MOV: result = operand2Value; break;
    case OPC_BIC: result = op1 & ~operand2Value; break;
    case OPC_MVN: result = ~operand2Value; break;
    }

    if (isArithmetic) {
        if (isReverseSub) {
            std::uint32_t tmp = op1;
            op1 = operand2Value;
            operand2Value = tmp;
        }
        if (isSubtraction) {
            operand2Value = ~operand2Value;
        }
        std::uint64_t wideOp1 = op1;
        std::uint64_t wideOp2 = operand2Value;
        if ((isSubtraction && (!useCarry || cpu->cpsr.carryFlag)) ||
            (!isSubtraction && useCarry && cpu->cpsr.carryFlag))
        {
            wideOp2++;
        }
        std::uint64_t wideResult = wideOp1 + wideOp2;
        result = static_cast<std::uint32_t>(wideResult);
        carryVal = (wideResult > 0xFFFFFFFFULL) ? 1 : 0;
        overflowVal = ((op1 >> 31) == (operand2Value >> 31)) && ((op1 >> 31) != (result >> 31));
    }

    zeroVal = (result == 0) ? 1 : 0;
    negativeVal = (result >> 31) & 1;

    if (writeResult) {
        cpu->regs[instruction.data_proc.destReg] = result;
        if (instruction.data_proc.destReg == 15) {
            CpuFlush(cpu);
        }
    }

    if (instruction.data_proc.setConditionFlags) {
        if (instruction.data_proc.destReg == 15) {
            CpuMode currentMode = static_cast<CpuMode>(cpu->cpsr.modeBits);
            if (!(currentMode == MODE_USER || currentMode == MODE_SYSTEM)) {
                cpu->cpsr.raw = cpu->savedSpsr;
                CpuUpdateMode(cpu, currentMode);
            }
        }
        else {
            cpu->cpsr.zeroFlag = zeroVal;
            cpu->cpsr.negativeFlag = negativeVal;
            cpu->cpsr.carryFlag = carryVal;
            cpu->cpsr.overflowFlag = overflowVal;
        }
    }
}

void ExecuteArmMultiply(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    CpuFetch(cpu);
    std::int32_t operandVal = static_cast<std::int32_t>(cpu->regs[instruction.multiply.rs]);
    int localCycles = 0;
    for (int i = 0; i < 4; i++) {
        operandVal >>= 8;
        localCycles++;
        if (operandVal == 0 || operandVal == -1) break;
    }
    std::uint32_t mulResult = cpu->regs[instruction.multiply.rm] * cpu->regs[instruction.multiply.rs];
    if (instruction.multiply.accumulate) {
        localCycles++;
        mulResult += cpu->regs[instruction.multiply.rn];
    }
    CpuInternalCycle(cpu);
    cpu->regs[instruction.multiply.rd] = mulResult;
    if (instruction.multiply.setConditionFlags) {
        cpu->cpsr.zeroFlag = (mulResult == 0);
        cpu->cpsr.negativeFlag = (mulResult >> 31) & 1;
    }
}

void ExecuteArmMultiplyLong(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    CpuFetch(cpu);
    CpuInternalCycle(cpu);
    std::int32_t operandVal = static_cast<std::int32_t>(cpu->regs[instruction.multiply_long.rs]);
    int localCycles = 0;
    for (int i = 1; i <= 4; i++) {
        operandVal >>= 8;
        localCycles++;
        if (operandVal == 0 || (operandVal == -1 && instruction.multiply_long.signedMultiply)) break;
    }
    std::uint64_t resultLong;
    if (instruction.multiply_long.signedMultiply) {
        std::int64_t sres =
            static_cast<std::int64_t>(static_cast<std::int32_t>(cpu->regs[instruction.multiply_long.rm])) *
            static_cast<std::int64_t>(static_cast<std::int32_t>(cpu->regs[instruction.multiply_long.rs]));
        resultLong = static_cast<std::uint64_t>(sres);
    }
    else {
        resultLong = static_cast<std::uint64_t>(cpu->regs[instruction.multiply_long.rm]) *
            static_cast<std::uint64_t>(cpu->regs[instruction.multiply_long.rs]);
    }
    if (instruction.multiply_long.accumulate) {
        localCycles++;
        std::uint64_t addVal =
            static_cast<std::uint64_t>(cpu->regs[instruction.multiply_long.rdLo]) |
            (static_cast<std::uint64_t>(cpu->regs[instruction.multiply_long.rdHi]) << 32);
        resultLong += addVal;
    }
    CpuInternalCycle(cpu);
    if (instruction.multiply_long.setConditionFlags) {
        cpu->cpsr.zeroFlag = (resultLong == 0);
        cpu->cpsr.negativeFlag = (resultLong >> 63) & 1;
    }
    cpu->regs[instruction.multiply_long.rdLo] = static_cast<std::uint32_t>(resultLong);
    cpu->regs[instruction.multiply_long.rdHi] = static_cast<std::uint32_t>(resultLong >> 32);
}

void ExecuteArmPsrTransfer(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    if (instruction.psr_trans.op) {
        std::uint32_t op2Val;
        if (instruction.psr_trans.immediateFlag) {
            op2Val = instruction.psr_trans.operand2 & 0xFF;
            std::uint32_t rot = instruction.psr_trans.operand2 >> 7;
            op2Val = (op2Val >> rot) | (op2Val << (32 - rot));
        }
        else {
            std::uint32_t rmIndex = instruction.psr_trans.operand2 & 0xF;
            op2Val = cpu->regs[rmIndex];
        }
        std::uint32_t mask = 0;
        if (instruction.psr_trans.f) mask |= 0xFF000000;
        if (instruction.psr_trans.s) mask |= 0x00FF0000;
        if (instruction.psr_trans.x) mask |= 0x0000FF00;
        if (instruction.psr_trans.c) mask |= 0x000000FF;
        if (cpu->cpsr.modeBits == MODE_USER) mask &= 0xF0000000;
        op2Val &= mask;
        if (instruction.psr_trans.p) {
            cpu->savedSpsr &= ~mask;
            cpu->savedSpsr |= op2Val;
        }
        else {
            CpuMode currentMode = static_cast<CpuMode>(cpu->cpsr.modeBits);
            cpu->cpsr.raw &= ~mask;
            cpu->cpsr.raw |= op2Val;
            CpuUpdateMode(cpu, currentMode);
        }
    }
    else {
        std::uint32_t psrVal;
        if (instruction.psr_trans.p) {
            psrVal = cpu->savedSpsr;
        }
        else {
            psrVal = cpu->cpsr.raw;
        }
        cpu->regs[instruction.psr_trans.destReg] = psrVal;
    }
    CpuFetch(cpu);
}

void ExecuteArmSwap(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    std::uint32_t address = cpu->regs[instruction.swap.rn];
    CpuFetch(cpu);
    if (instruction.swap.byteMode) {
        CpuInternalCycle(cpu);
        std::uint8_t dataVal = CpuReadByte(cpu, address);
        std::uint8_t tmp = dataVal;
        dataVal = static_cast<std::uint8_t>(cpu->regs[instruction.swap.rm]);
        CpuWriteByte(cpu, address, dataVal);
        cpu->regs[instruction.swap.rd] = tmp;
    }
    else {
        CpuInternalCycle(cpu);
        std::uint32_t dataVal = CpuReadWord(cpu, address);
        std::uint32_t rotation = (address % 4) * 8;
        dataVal = (dataVal >> rotation) | (dataVal << (32 - rotation));
        std::uint32_t tmp = dataVal;
        dataVal = cpu->regs[instruction.swap.rm];
        CpuWriteWord(cpu, address, dataVal);
        cpu->regs[instruction.swap.rd] = tmp;
    }
}

void ExecuteArmBranchExchange(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    CpuFetch(cpu);
    cpu->programCounter = cpu->regs[instruction.branch_ex.rn];
    cpu->cpsr.thumbState = cpu->regs[instruction.branch_ex.rn] & 1;
    CpuFlush(cpu);
}

void ExecuteArmHalfTransfer(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    std::uint32_t address = cpu->regs[instruction.half_transi.rn];
    std::uint32_t offsetVal;

    if (instruction.half_transi.immediate) {
        offsetVal = instruction.half_transi.offLow | (instruction.half_transi.offHigh << 4);
    }
    else {
        offsetVal = cpu->regs[instruction.half_transr.rm];
    }
    CpuFetch(cpu);

    if (!instruction.half_transi.up) {
        offsetVal = static_cast<std::uint32_t>(-static_cast<int>(offsetVal));
    }
    std::uint32_t writeBackVal = address + offsetVal;
    if (instruction.half_transi.preIndex) {
        address = writeBackVal;
    }

    if (instruction.half_transi.signExtend) {
        if (instruction.half_transi.load) {
            CpuInternalCycle(cpu);
            if (instruction.half_transi.writeBack || !instruction.half_transi.preIndex) {
                cpu->regs[instruction.half_transi.rn] = writeBackVal;
            }
            if (instruction.half_transi.highHalf) {
                std::int16_t dataIn = static_cast<std::int16_t>(CpuReadHalf(cpu, address));
                std::uint32_t rotation = (address % 2) * 8;
                dataIn >>= rotation;
                cpu->regs[instruction.half_transi.rd] = static_cast<std::uint32_t>(dataIn);
            }
            else {
                std::int8_t dataIn = static_cast<std::int8_t>(CpuReadByte(cpu, address));
                cpu->regs[instruction.half_transi.rd] = static_cast<std::uint32_t>(dataIn);
            }
        }
    }
    else if (instruction.half_transi.highHalf) {
        if (instruction.half_transi.load) {
            CpuInternalCycle(cpu);
            if (instruction.half_transi.writeBack || !instruction.half_transi.preIndex) {
                cpu->regs[instruction.half_transi.rn] = writeBackVal;
            }
            std::uint32_t dataIn = CpuReadHalf(cpu, address);
            std::uint32_t rotation = (address % 2) * 8;
            dataIn = (dataIn >> rotation) | (dataIn << (32 - rotation));
            cpu->regs[instruction.half_transi.rd] = dataIn;
        }
        else {
            std::uint32_t dataOut = cpu->regs[instruction.half_transi.rd];
            CpuWriteHalf(cpu, address, static_cast<std::uint16_t>(dataOut));
            if (instruction.half_transi.writeBack || !instruction.half_transi.preIndex) {
                cpu->regs[instruction.half_transi.rn] = writeBackVal;
            }
        }
    }

    if (instruction.half_transi.rd == 15 && instruction.half_transi.load) {
        CpuFlush(cpu);
    }
}

void ExecuteArmSingleTransfer(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    std::uint32_t address = cpu->regs[instruction.single_trans.baseReg];
    if (instruction.single_trans.baseReg == 15) {
        address &= ~0b10;
    }
    std::uint32_t offsetVal;
    if (instruction.single_trans.immediate) {
        std::uint32_t rmIndex = instruction.single_trans.offset & 0xF;
        offsetVal = cpu->regs[rmIndex];
        std::uint32_t shiftField = instruction.single_trans.offset >> 4;
        std::uint32_t shiftKind = (shiftField >> 1) & 0b11;
        std::uint32_t shiftAmount = shiftField >> 3;
        switch (shiftKind) {
        case SHIFT_LSL:
            offsetVal <<= shiftAmount;
            break;
        case SHIFT_LSR:
            if (shiftAmount == 0) offsetVal = 0;
            else offsetVal >>= shiftAmount;
            break;
        case SHIFT_ASR:
            if (shiftAmount == 0) {
                offsetVal = (offsetVal & 0x80000000) ? 0xFFFFFFFF : 0;
            }
            else {
                std::int32_t signedVal = static_cast<std::int32_t>(offsetVal);
                signedVal >>= shiftAmount;
                offsetVal = static_cast<std::uint32_t>(signedVal);
            }
            break;
        case SHIFT_ROR:
            if (shiftAmount == 0) {
                offsetVal >>= 1;
                offsetVal |= (cpu->cpsr.carryFlag << 31);
            }
            else {
                offsetVal = (offsetVal >> shiftAmount) | (offsetVal << (32 - shiftAmount));
            }
            break;
        }
    }
    else {
        offsetVal = instruction.single_trans.offset;
    }

    CpuFetch(cpu);

    if (!instruction.single_trans.up) {
        offsetVal = static_cast<std::uint32_t>(-static_cast<int>(offsetVal));
    }
    std::uint32_t writeBackVal = address + offsetVal;
    if (instruction.single_trans.preIndex) {
        address = writeBackVal;
    }

    if (instruction.single_trans.byteTransfer) {
        if (instruction.single_trans.load) {
            CpuInternalCycle(cpu);
            if (instruction.single_trans.writeBack || !instruction.single_trans.preIndex) {
                cpu->regs[instruction.single_trans.baseReg] = writeBackVal;
            }
            std::uint8_t dataIn = CpuReadByte(cpu, address);
            cpu->regs[instruction.single_trans.destReg] = dataIn;
        }
        else {
            std::uint8_t dataOut = static_cast<std::uint8_t>(cpu->regs[instruction.single_trans.destReg]);
            CpuWriteByte(cpu, address, dataOut);
            if (instruction.single_trans.writeBack || !instruction.single_trans.preIndex) {
                cpu->regs[instruction.single_trans.baseReg] = writeBackVal;
            }
        }
    }
    else {
        if (instruction.single_trans.load) {
            CpuInternalCycle(cpu);
            if (instruction.single_trans.writeBack || !instruction.single_trans.preIndex) {
                cpu->regs[instruction.single_trans.baseReg] = writeBackVal;
            }
            std::uint32_t dataIn = CpuReadWord(cpu, address);
            std::uint32_t rotation = (address % 4) * 8;
            dataIn = (dataIn >> rotation) | (dataIn << (32 - rotation));
            cpu->regs[instruction.single_trans.destReg] = dataIn;
        }
        else {
            std::uint32_t dataOut = cpu->regs[instruction.single_trans.destReg];
            CpuWriteWord(cpu, address, dataOut);
            if (instruction.single_trans.writeBack || !instruction.single_trans.preIndex) {
                cpu->regs[instruction.single_trans.baseReg] = writeBackVal;
            }
        }
    }

    if (instruction.single_trans.destReg == 15 && instruction.single_trans.load) {
        CpuFlush(cpu);
    }
}

void ExecuteArmUndefined(Arm7Tdmi* cpu, ArmInstruction /*instruction*/)
{
    CpuHandleInterrupt(cpu, INT_UND);
}

void ExecuteArmBlockTransfer(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    int regCount = 0;
    int usedIndices[16];
    std::uint32_t address = cpu->regs[instruction.block_trans.baseReg];
    std::uint32_t writeBackVal = address;

    if (instruction.block_trans.regList) {
        for (int i = 0; i < 16; i++) {
            if (instruction.block_trans.regList & (1 << i)) {
                usedIndices[regCount++] = i;
            }
        }
        if (instruction.block_trans.up) {
            writeBackVal += 4 * regCount;
        }
        else {
            writeBackVal -= 4 * regCount;
            address = writeBackVal;
        }
    }
    else {
        regCount = 1;
        usedIndices[0] = 15;
        if (instruction.block_trans.up) {
            writeBackVal += 0x40;
        }
        else {
            writeBackVal -= 0x40;
            address = writeBackVal;
        }
    }

    if (instruction.block_trans.preIndex == instruction.block_trans.up) {
        address += 4;
    }
    CpuFetch(cpu);

    bool userModeTransfer =
        (instruction.block_trans.sBit != 0) &&
        ((instruction.block_trans.regList & (1 << 15)) == 0 || !instruction.block_trans.load);

    CpuMode oldMode = static_cast<CpuMode>(cpu->cpsr.modeBits);
    if (userModeTransfer) {
        cpu->cpsr.modeBits = MODE_USER;
        CpuUpdateMode(cpu, oldMode);
    }

    if (instruction.block_trans.load) {
        CpuInternalCycle(cpu);
        if (instruction.block_trans.writeBack) {
            cpu->regs[instruction.block_trans.baseReg] = writeBackVal;
        }
    }

    for (int i = 0; i < regCount; i++, address += 4) {
        if (instruction.block_trans.load) {
            std::uint32_t dataIn = CpuReadWord(cpu, address);
            cpu->regs[usedIndices[i]] = dataIn;
            if (usedIndices[i] == 15) {
                CpuFlush(cpu);
            }
        }
        else {
            std::uint32_t dataOut = cpu->regs[usedIndices[i]];
            CpuWriteWord(cpu, address, dataOut);
            if (i == 0 && instruction.block_trans.writeBack) {
                cpu->regs[instruction.block_trans.baseReg] = writeBackVal;
            }
        }
    }

    if (userModeTransfer) {
        cpu->cpsr.modeBits = oldMode;
        CpuUpdateMode(cpu, MODE_USER);
    }

    if ((instruction.block_trans.regList & (1 << 15)) && instruction.block_trans.load) {
        if (instruction.block_trans.sBit) {
            CpuMode currentMode = static_cast<CpuMode>(cpu->cpsr.modeBits);
            if (!(currentMode == MODE_USER || currentMode == MODE_SYSTEM)) {
                cpu->cpsr.raw = cpu->savedSpsr;
                CpuUpdateMode(cpu, currentMode);
            }
        }
    }
}

void ExecuteArmBranch(Arm7Tdmi* cpu, ArmInstruction instruction)
{
    std::uint32_t offsetVal = instruction.branch.offset;
    if (offsetVal & (1 << 23)) {
        offsetVal |= 0xFF000000;
    }
    if (cpu->cpsr.thumbState) {
        offsetVal <<= 1;
    }
    else {
        offsetVal <<= 2;
    }
    std::uint32_t destination = cpu->programCounter + offsetVal;

    if (instruction.branch.link) {
        if (cpu->cpsr.thumbState) {
            if (cpu->thumbBranchLinkState) {
                cpu->thumbBranchLinkState = false;
                cpu->linkRegister += offsetVal;
                destination = cpu->linkRegister;
                cpu->linkRegister = (cpu->programCounter - 2) | 1;
            }
            else {
                cpu->thumbBranchLinkState = true;
                cpu->linkRegister = destination;
                CpuFetch(cpu);
                return;
            }
        }
        else {
            cpu->linkRegister = (cpu->programCounter - 4) & ~0b11;
        }
    }

    CpuFetch(cpu);
    cpu->programCounter = destination;
    CpuFlush(cpu);
}

void ExecuteArmSoftwareInterrupt(Arm7Tdmi* cpu, ArmInstruction /*instruction*/)
{
    CpuHandleInterrupt(cpu, INT_SWI);
}

ArmInstruction ThumbDecodeInstruction(ThumbInstruction instr)
{
    ArmInstruction decoded{};
    decoded.conditionCode = COND_AL;
    decoded.bits28 = 0;

    switch (instr.highNibble) {
    case 0:
    case 1:
        if (instr.shift.shiftOp < 0b11) {
            decoded.data_proc.decodeBits1 = 0b00;
            decoded.data_proc.immediateFlag = 0;
            decoded.data_proc.operationCode = OPC_MOV;
            decoded.data_proc.setConditionFlags = 1;
            decoded.data_proc.destReg = instr.shift.destReg;
            decoded.data_proc.operand2 = instr.shift.sourceReg;
            decoded.data_proc.operand2 |= (instr.shift.shiftOp << 5);
            decoded.data_proc.operand2 |= (instr.shift.shiftOffset << 7);
        }
        else {
            decoded.data_proc.decodeBits1 = 0b00;
            decoded.data_proc.immediateFlag = instr.add.immediateFlag;
            decoded.data_proc.operationCode = (instr.add.subOp) ? OPC_SUB : OPC_ADD;
            decoded.data_proc.setConditionFlags = 1;
            decoded.data_proc.operand1Reg = instr.add.sourceReg;
            decoded.data_proc.destReg = instr.add.destReg;
            decoded.data_proc.operand2 = instr.add.op2;
        }
        break;
    case 2:
    case 3:
        decoded.data_proc.decodeBits1 = 0b00;
        decoded.data_proc.immediateFlag = 1;
        decoded.data_proc.setConditionFlags = 1;
        decoded.data_proc.operand1Reg = instr.alu_imm.destReg;
        decoded.data_proc.destReg = instr.alu_imm.destReg;
        decoded.data_proc.operand2 = instr.alu_imm.immediateValue;
        switch (instr.alu_imm.aluOp) {
        case 0: decoded.data_proc.operationCode = OPC_MOV; break;
        case 1: decoded.data_proc.operationCode = OPC_CMP; break;
        case 2: decoded.data_proc.operationCode = OPC_ADD; break;
        case 3: decoded.data_proc.operationCode = OPC_SUB; break;
        }
        break;
    case 4: {
        std::uint32_t topNib = instr.next2Nibble >> 2;
        switch (topNib) {
        case 0:
            decoded.data_proc.decodeBits1 = 0b00;
            decoded.data_proc.immediateFlag = 0;
            decoded.data_proc.operationCode = instr.alu.opcode;
            decoded.data_proc.setConditionFlags = 1;
            decoded.data_proc.operand1Reg = instr.alu.destReg;
            decoded.data_proc.destReg = instr.alu.destReg;
            decoded.data_proc.operand2 = instr.alu.sourceReg;
            switch (instr.alu.opcode) {
            case THUMB_LSL:
            case THUMB_LSR:
            case THUMB_ASR:
            case THUMB_ROR: {
                decoded.data_proc.operationCode = OPC_MOV;
                decoded.data_proc.operand2 = instr.alu.destReg;
                decoded.data_proc.operand2 |= (1 << 4);
                std::uint32_t st = 0;
                switch (instr.alu.opcode) {
                case THUMB_LSL: st = SHIFT_LSL; break;
                case THUMB_LSR: st = SHIFT_LSR; break;
                case THUMB_ASR: st = SHIFT_ASR; break;
                case THUMB_ROR: st = SHIFT_ROR; break;
                }
                decoded.data_proc.operand2 |= (st << 5);
                decoded.data_proc.operand2 |= (instr.alu.sourceReg << 8);
                break;
            }
            case THUMB_NEG:
                decoded.data_proc.immediateFlag = 1;
                decoded.data_proc.operationCode = OPC_RSB;
                decoded.data_proc.operand1Reg = instr.alu.sourceReg;
                decoded.data_proc.operand2 = 0;
                break;
            case THUMB_MUL:
                decoded.multiply.decodeBits1 = 0;
                decoded.multiply.accumulate = 0;
                decoded.multiply.setConditionFlags = 1;
                decoded.multiply.rd = instr.alu.destReg;
                decoded.multiply.rn = 0;
                decoded.multiply.rs = instr.alu.sourceReg;
                decoded.multiply.decodeBits2 = 0b1001;
                decoded.multiply.rm = instr.alu.destReg;
                break;
            default:
                break;
            }
            break;
        case 1:
            decoded.data_proc.decodeBits1 = 0b00;
            decoded.data_proc.immediateFlag = 0;
            decoded.data_proc.setConditionFlags = 0;
            decoded.data_proc.operand1Reg = instr.hi_ops.destReg | (instr.hi_ops.h1 << 3);
            decoded.data_proc.destReg = instr.hi_ops.destReg | (instr.hi_ops.h1 << 3);
            decoded.data_proc.operand2 = instr.hi_ops.sourceReg | (instr.hi_ops.h2 << 3);
            switch (instr.hi_ops.op) {
            case 0:
                decoded.data_proc.operationCode = OPC_ADD;
                break;
            case 1:
                decoded.data_proc.operationCode = OPC_CMP;
                decoded.data_proc.setConditionFlags = 1;
                break;
            case 2:
                decoded.data_proc.operationCode = OPC_MOV;
                break;
            case 3:
                decoded.branch_ex.decodeBits0 = 0b000100101111111111110001;
                decoded.branch_ex.rn = instr.hi_ops.sourceReg | (instr.hi_ops.h2 << 3);
                break;
            }
            break;
        case 2:
        case 3:
            decoded.single_trans.decodeBits1 = 0b01;
            decoded.single_trans.immediate = 0;
            decoded.single_trans.preIndex = 1;
            decoded.single_trans.up = 1;
            decoded.single_trans.writeBack = 0;
            decoded.single_trans.byteTransfer = 0;
            decoded.single_trans.load = 1;
            decoded.single_trans.baseReg = 15;
            decoded.single_trans.destReg = instr.ldst_pc.destReg;
            decoded.single_trans.offset = instr.ldst_pc.offset << 2;
            break;
        }
        break;
    }
    case 5:
        if (instr.ldst_reg.decodeBits2 == 0) {
            decoded.single_trans.decodeBits1 = 0b01;
            decoded.single_trans.immediate = 1;
            decoded.single_trans.preIndex = 1;
            decoded.single_trans.up = 1;
            decoded.single_trans.writeBack = 0;
            decoded.single_trans.byteTransfer = instr.ldst_reg.byteMode;
            decoded.single_trans.load = instr.ldst_reg.load;
            decoded.single_trans.baseReg = instr.ldst_reg.baseReg;
            decoded.single_trans.destReg = instr.ldst_reg.destReg;
            decoded.single_trans.offset = instr.ldst_reg.offsetReg;
        }
        else {
            decoded.half_transr.decodeBits1 = 0b000;
            decoded.half_transr.preIndex = 1;
            decoded.half_transr.up = 1;
            decoded.half_transr.immediate = 0;
            decoded.half_transr.writeBack = 0;
            decoded.half_transr.load = (instr.ldst_s.signExtend || instr.ldst_s.half);
            decoded.half_transr.rn = instr.ldst_s.baseReg;
            decoded.half_transr.rd = instr.ldst_s.destReg;
            decoded.half_transr.decodeBits2 = 0b00001;
            decoded.half_transr.signExtend = instr.ldst_s.signExtend;
            decoded.half_transr.highHalf = (~instr.ldst_s.signExtend) | instr.ldst_s.half;
            decoded.half_transr.decodeBits3 = 1;
            decoded.half_transr.rm = instr.ldst_s.offsetReg;
        }
        break;
    case 6:
    case 7:
        decoded.single_trans.decodeBits1 = 0b01;
        decoded.single_trans.immediate = 0;
        decoded.single_trans.preIndex = 1;
        decoded.single_trans.up = 1;
        decoded.single_trans.writeBack = 0;
        decoded.single_trans.byteTransfer = instr.ldst_imm.byteMode;
        decoded.single_trans.load = instr.ldst_imm.load;
        decoded.single_trans.baseReg = instr.ldst_imm.baseReg;
        decoded.single_trans.destReg = instr.ldst_imm.destReg;
        if (instr.ldst_imm.byteMode) {
            decoded.single_trans.offset = instr.ldst_imm.offset;
        }
        else {
            decoded.single_trans.offset = instr.ldst_imm.offset << 2;
        }
        break;
    case 8:
        decoded.half_transi.decodeBits1 = 0b000;
        decoded.half_transi.preIndex = 1;
        decoded.half_transi.up = 1;
        decoded.half_transi.immediate = 1;
        decoded.half_transi.writeBack = 0;
        decoded.half_transi.load = instr.ldst_h.load;
        decoded.half_transi.rn = instr.ldst_h.baseReg;
        decoded.half_transi.rd = instr.ldst_h.destReg;
        decoded.half_transi.offHigh = instr.ldst_h.offset >> 4;
        decoded.half_transi.decodeBits2 = 1;
        decoded.half_transi.signExtend = 0;
        decoded.half_transi.highHalf = 1;
        decoded.half_transi.decodeBits3 = 1;
        decoded.half_transi.offLow = instr.ldst_h.offset << 1;
        break;
    case 9:
        decoded.single_trans.decodeBits1 = 0b01;
        decoded.single_trans.immediate = 1;
        decoded.single_trans.preIndex = 1;
        decoded.single_trans.up = 1;
        decoded.single_trans.writeBack = 0;
        decoded.single_trans.byteTransfer = 0;
        decoded.single_trans.load = instr.ldst_sp.load;
        decoded.single_trans.baseReg = 13;
        decoded.single_trans.destReg = instr.ldst_sp.destReg;
        decoded.single_trans.offset = instr.ldst_sp.offset << 2;
        break;
    case 10:
        decoded.data_proc.decodeBits1 = 0b00;
        decoded.data_proc.immediateFlag = 1;
        decoded.data_proc.operationCode = OPC_ADD;
        decoded.data_proc.setConditionFlags = 0;
        decoded.data_proc.destReg = instr.ldst_addr.destReg;
        decoded.data_proc.operand1Reg = (instr.ldst_addr.sp) ? 13 : 15;
        decoded.data_proc.operand2 = instr.ldst_addr.offset << 2;
        break;
    case 11:
        if (instr.add_sp.decodeBits1 == 0b10110000) {
            decoded.data_proc.decodeBits1 = 0b00;
            decoded.data_proc.immediateFlag = 1;
            decoded.data_proc.operationCode = (instr.add_sp.subtract) ? OPC_SUB : OPC_ADD;
            decoded.data_proc.setConditionFlags = 0;
            decoded.data_proc.destReg = 13;
            decoded.data_proc.operand1Reg = 13;
            decoded.data_proc.operand2 = instr.add_sp.offset << 2;
        }
        else {
            decoded.block_trans.decodeBits1 = 0b100;
            decoded.block_trans.preIndex = (instr.push_pop.load) ? 0 : 1;
            decoded.block_trans.up = instr.push_pop.load;
            decoded.block_trans.sBit = 0;
            decoded.block_trans.writeBack = 1;
            decoded.block_trans.load = instr.push_pop.load;
            decoded.block_trans.baseReg = 13;
            decoded.block_trans.regList = instr.push_pop.regList;
            if (instr.push_pop.rBit) {
                if (instr.push_pop.load) {
                    decoded.block_trans.regList |= (1 << 15);
                }
                else {
                    decoded.block_trans.regList |= (1 << 14);
                }
            }
        }
        break;
    case 12:
        decoded.block_trans.decodeBits1 = 0b100;
        decoded.block_trans.preIndex = 0;
        decoded.block_trans.up = 1;
        decoded.block_trans.sBit = 0;
        decoded.block_trans.writeBack = 1;
        decoded.block_trans.load = instr.ldst_m.load;
        decoded.block_trans.baseReg = instr.ldst_m.baseReg;
        decoded.block_trans.regList = instr.ldst_m.regList;
        break;
    case 13:
        if (instr.b_cond.condition < 0b1111) {
            decoded.conditionCode = static_cast<ConditionCode>(instr.b_cond.condition);
            decoded.branch.decodeBits1 = 0b101;
            decoded.branch.link = 0;
            std::uint32_t off = instr.b_cond.offset;
            if (off & (1 << 7)) {
                off |= 0xFFFFFF00;
            }
            decoded.branch.offset = off;
        }
        else {
            decoded.sw_intr.decodeBits1 = 0b1111;
        }
        break;
    case 14: {
        decoded.branch.decodeBits1 = 0b101;
        decoded.branch.link = 0;
        std::uint32_t off = instr.branch.offset;
        if (off & (1 << 10)) {
            off |= 0xFFFFF800;
        }
        decoded.branch.offset = off;
        break;
    }
    case 15:
        decoded.branch.decodeBits1 = 0b101;
        decoded.branch.link = 1;
        if (instr.branch_l.h) {
            decoded.branch.offset = instr.branch_l.offset;
        }
        else {
            std::uint32_t off = instr.branch_l.offset;
            if (off & (1 << 10)) {
                off |= 0xFFFFF800;
            }
            decoded.branch.offset = off << 11;
        }
        break;
    }

    return decoded;
}

void CpuFetch(Arm7Tdmi* cpu)
{
    cpu->currentArmInstruction = cpu->nextArmInstruction;
    if (cpu->cpsr.thumbState) {
        ThumbInstruction thumbInstr;
        thumbInstr.raw = CpuReadHalf(cpu, cpu->programCounter);
        cpu->nextArmInstruction = ThumbDecodeInstruction(thumbInstr);
        cpu->programCounter += 2;
    }
    else {
        cpu->nextArmInstruction.raw = CpuReadWord(cpu, cpu->programCounter);
        cpu->programCounter += 4;
    }
}

void CpuFlush(Arm7Tdmi* cpu)
{
    if (cpu->cpsr.thumbState) {
        cpu->programCounter &= ~1U;
        {
            ThumbInstruction thumbInstr;
            thumbInstr.raw = CpuReadHalf(cpu, cpu->programCounter);
            cpu->currentArmInstruction = ThumbDecodeInstruction(thumbInstr);
        }
        cpu->programCounter += 2;
        {
            ThumbInstruction thumbInstr;
            thumbInstr.raw = CpuReadHalf(cpu, cpu->programCounter);
            cpu->nextArmInstruction = ThumbDecodeInstruction(thumbInstr);
        }
        cpu->programCounter += 2;
    }
    else {
        cpu->programCounter &= ~0b11U;
        cpu->currentArmInstruction.raw = CpuReadWord(cpu, cpu->programCounter);
        cpu->programCounter += 4;
        cpu->nextArmInstruction.raw = CpuReadWord(cpu, cpu->programCounter);
        cpu->programCounter += 4;
    }
}

static RegisterBank GetRegisterBank(CpuMode mode)
{
    switch (mode) {
    case MODE_USER:   return BANK_USER;
    case MODE_FIQ:    return BANK_FIQ;
    case MODE_IRQ:    return BANK_IRQ;
    case MODE_SVC:    return BANK_SVC;
    case MODE_ABT:    return BANK_ABT;
    case MODE_UND:    return BANK_UND;
    case MODE_SYSTEM: return BANK_USER;
    default:          return BANK_USER;
    }
}

void CpuUpdateMode(Arm7Tdmi* cpu, CpuMode oldMode)
{
    RegisterBank oldBank = GetRegisterBank(oldMode);
    cpu->bankedStackPointers[oldBank] = cpu->stackPointer;
    cpu->bankedLinkRegisters[oldBank] = cpu->linkRegister;
    cpu->bankedSavedSpsr[oldBank] = cpu->savedSpsr;

    RegisterBank newBank = GetRegisterBank(static_cast<CpuMode>(cpu->cpsr.modeBits));
    cpu->stackPointer = cpu->bankedStackPointers[newBank];
    cpu->linkRegister = cpu->bankedLinkRegisters[newBank];
    cpu->savedSpsr = cpu->bankedSavedSpsr[newBank];

    if (oldMode == MODE_FIQ && cpu->cpsr.modeBits != MODE_FIQ) {
        for (int i = 0; i < 5; i++) {
            cpu->bankedRegsR8To12[1][i] = cpu->regs[8 + i];
            cpu->regs[8 + i] = cpu->bankedRegsR8To12[0][i];
        }
    }
    if (oldMode != MODE_FIQ && cpu->cpsr.modeBits == MODE_FIQ) {
        for (int i = 0; i < 5; i++) {
            cpu->bankedRegsR8To12[0][i] = cpu->regs[8 + i];
            cpu->regs[8 + i] = cpu->bankedRegsR8To12[1][i];
        }
    }
}

void CpuHandleInterrupt(Arm7Tdmi* cpu, CpuInterrupt intr)
{
    CpuMode oldMode = static_cast<CpuMode>(cpu->cpsr.modeBits);
    std::uint32_t oldSpsr = cpu->cpsr.raw;
    switch (intr) {
    case INT_RESET:
    case INT_SWI:
    case INT_ADDR:
        cpu->cpsr.modeBits = MODE_SVC;
        break;
    case INT_PABT:
    case INT_DABT:
        cpu->cpsr.modeBits = MODE_ABT;
        break;
    case INT_UND:
        cpu->cpsr.modeBits = MODE_UND;
        break;
    case INT_IRQ:
        cpu->cpsr.modeBits = MODE_IRQ;
        break;
    case INT_FIQ:
        cpu->cpsr.modeBits = MODE_FIQ;
        break;
    }
    CpuUpdateMode(cpu, oldMode);

    cpu->savedSpsr = oldSpsr;
    cpu->linkRegister = cpu->programCounter;
    if (!cpu->cpsr.thumbState) {
        cpu->linkRegister -= 4;
    }
    CpuFetch(cpu);
    cpu->cpsr.thumbState = 0;
    cpu->cpsr.irqDisable = 1;
    cpu->programCounter = 4 * intr;
    CpuFlush(cpu);
}

std::uint8_t CpuReadByte(Arm7Tdmi* cpu, std::uint32_t addr)
{
    cpu->cycleCount++;
    return GbaReadByte(cpu->gbaSystem, addr, &cpu->cycleCount);
}

std::uint16_t CpuReadHalf(Arm7Tdmi* cpu, std::uint32_t addr)
{
    cpu->cycleCount++;
    return GbaReadHalf(cpu->gbaSystem, addr, &cpu->cycleCount);
}

std::uint32_t CpuReadWord(Arm7Tdmi* cpu, std::uint32_t addr)
{
    cpu->cycleCount++;
    return GbaReadWord(cpu->gbaSystem, addr, &cpu->cycleCount);
}

void CpuWriteByte(Arm7Tdmi* cpu, std::uint32_t addr, std::uint8_t data)
{
    cpu->cycleCount++;
    GbaWriteByte(cpu->gbaSystem, addr, data, &cpu->cycleCount);
}

void CpuWriteHalf(Arm7Tdmi* cpu, std::uint32_t addr, std::uint16_t data)
{
    cpu->cycleCount++;
    GbaWriteHalf(cpu->gbaSystem, addr, data, &cpu->cycleCount);
}

void CpuWriteWord(Arm7Tdmi* cpu, std::uint32_t addr, std::uint32_t data)
{
    cpu->cycleCount++;
    GbaWriteWord(cpu->gbaSystem, addr, data, &cpu->cycleCount);
}

void CpuInternalCycle(Arm7Tdmi* cpu)
{
    cpu->cycleCount++;
}
