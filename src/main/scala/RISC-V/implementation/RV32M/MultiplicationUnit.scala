package RISCV.implementation.RV32M

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class MultiplicationUnit extends AbstractExecutionUnit {

    io.misa := "b01__0000__0_00000_00000_00100_00000_00000".U

    io.valid := false.B    
    io.stall := STALL_REASON.NO_STALL

    io_data <> DontCare
    io_reg <> DontCare
    io_pc <> DontCare
    io_reset <> DontCare
    
    //TODO: Your solution to Problem 2.4 should go here
    val rs1 = io.instr(19, 15)
    val rs2 = io.instr(24, 20)
    val rd = io.instr(11, 7)
    val funct7 = RISCV_FUNCT7(io.instr(31, 25))
    val funct3 = RISCV_FUNCT3(io.instr(14, 12))
    val opcode = RISCV_OP(io.instr(6, 0))

    io_reg.reg_rs1 := rs1
    io_reg.reg_rs2 := rs2
    io_reg.reg_rd := rd

    val leftoperand = io_reg.reg_read_data1
    val rightoperand = io_reg.reg_read_data2

    val result = Wire(UInt(32.W))
    result := 0.U

    when(funct7 === RISCV_FUNCT7.MULDIV && opcode===RISCV_OP.OP){
    switch(funct3){
        is(RISCV_FUNCT3.F000){  //MUL *page 575 of the specification
            io.valid := true.B
            result := ((leftoperand.asSInt * rightoperand.asSInt).asSInt)(31, 0)
            io_reg.reg_write_data := result
        }
        is(RISCV_FUNCT3.F001){  //MULH
            io.valid := true.B
            result := ((leftoperand.asSInt * rightoperand.asSInt).asSInt)(63, 32)
            io_reg.reg_write_data := result
        }
        is(RISCV_FUNCT3.F010){  //MULHSU
            io.valid := true.B
            result := ((leftoperand.asSInt * rightoperand.asUInt).asSInt)(63, 32)
            io_reg.reg_write_data := result
        }
        is(RISCV_FUNCT3.F011){  //MULHU
            io.valid := true.B
            result := ((leftoperand.asUInt * rightoperand.asUInt).asUInt)(63, 32)
            io_reg.reg_write_data := result
        }
    } }

    io_reg.reg_write_en := true.B
    io_pc.pc_we := true.B
    io_pc.pc_wdata := io_pc.pc + 4.U
}