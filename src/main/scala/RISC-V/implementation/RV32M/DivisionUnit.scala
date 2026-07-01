package RISCV.implementation.RV32M

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._


class DivisionUnit extends AbstractExecutionUnit {

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

    val dividend = io_reg.reg_read_data1
    val divisor = io_reg.reg_read_data2

    val result = Wire(UInt(32.W))
    result := 0.U

    //the most-negative integer divided by -1
    val overflow = (dividend === (1 << 31).S.asUInt) && (divisor === -1.S.asUInt) 

    when(funct7 === RISCV_FUNCT7.MULDIV && opcode===RISCV_OP.OP){
    switch(funct3){
        is(RISCV_FUNCT3.F100){  //DIV *page 575 of the specification
            io.valid := true.B
            when(divisor === 0.U){
                result := (-1).S.asUInt
            } .elsewhen(overflow){
                result := dividend 
            } .otherwise{
                result := (dividend.asSInt / divisor.asSInt).asUInt
            }
        }
        is(RISCV_FUNCT3.F101){  //DIVU
            io.valid := true.B
            when (divisor === 0.U){
                result := (~0.U(32.W))
            } .otherwise{
                result := (dividend.asUInt / divisor.asUInt)
            }
        }
        is(RISCV_FUNCT3.F110){  //REM
            io.valid := true.B
            when(divisor === 0.U){
                result := dividend
            } .elsewhen(overflow){
                result := 0.U
            } .otherwise{
                result := (dividend.asSInt % divisor.asSInt).asUInt
            }
        }
        is(RISCV_FUNCT3.F111){  //REMU
            io.valid := true.B
            when(divisor === 0.U){
                result := dividend
            } .otherwise{
                result := (dividend.asUInt % divisor.asUInt)
            }
        }
    }
    }

    io_reg.reg_write_en := true.B
    io_reg.reg_write_data := result 
    io_pc.pc_we := true.B
    io_pc.pc_wdata := io_pc.pc + 4.U
}