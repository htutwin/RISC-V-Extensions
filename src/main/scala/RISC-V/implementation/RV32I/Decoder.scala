package RISCV.implementation.RV32I

import chisel3._
import chisel3.util._

import RISCV.model._
import RISCV.interfaces.RV32I.AbstractDecoder

class Decoder extends AbstractDecoder {
  val opcode = RISCV_OP(io_decoder.instr(6, 0))
  val funct3 = RISCV_FUNCT3(io_decoder.instr(14, 12))
  val funct7 = RISCV_FUNCT7(io_decoder.instr(31, 25))
  val funct12 = RISCV_FUNCT12(io_decoder.instr(31, 20))

  val RS1 = io_decoder.instr(19, 15)
  val RS2 = io_decoder.instr(24, 20)
  val RD = io_decoder.instr(11, 7)

  // define default values
  io_decoder.valid := false.B
  io_decoder.instr_type := RISCV_TYPE.addi
  io_decoder.rs1 := 0.U
  io_decoder.rs2 := 0.U
  io_decoder.rd := 0.U
  io_decoder.imm := 0.U

  switch(opcode) {
    is(RISCV_OP.OP) {
      io_decoder.instr_type := RISCV_TYPE(opcode.asUInt ## funct3.asUInt ## funct7.asUInt)
      io_decoder.valid := opcode =/= RISCV_OP.UNKNOWN && funct7 =/= RISCV_FUNCT7.UNKNOWN && io_decoder.instr_type =/= RISCV_TYPE.UNKNOWN
      io_decoder.rs1 := RS1
      io_decoder.rs2 := RS2
      io_decoder.rd := RD
      io_decoder.imm := Fill(32, 0.U)
    }
    is(RISCV_OP.OP_IMM) {
      io_decoder.instr_type := RISCV_TYPE(opcode.asUInt ## funct3.asUInt ## Fill(7, 0.U))
      io_decoder.valid := opcode =/= RISCV_OP.UNKNOWN && io_decoder.instr_type =/= RISCV_TYPE.UNKNOWN
      io_decoder.rs1 := RS1
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ## io_decoder.instr(31, 20)
    }
    is(RISCV_OP.LUI) {
      io_decoder.instr_type := RISCV_TYPE(opcode.asUInt ## Fill(3, 0.U) ## Fill(7, 0.U))
      io_decoder.valid := opcode =/= RISCV_OP.UNKNOWN && io_decoder.instr_type =/= RISCV_TYPE.UNKNOWN
      io_decoder.rs1 := 0.U
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := io_decoder.instr(31, 12) ## Fill(12, 0.U)
    }
    is(RISCV_OP.AUIPC) {
      io_decoder.instr_type := RISCV_TYPE(opcode.asUInt ## Fill(3, 0.U) ## Fill(7, 0.U))
      io_decoder.valid := opcode =/= RISCV_OP.UNKNOWN && io_decoder.instr_type =/= RISCV_TYPE.UNKNOWN
      io_decoder.rs1 := 0.U
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := io_decoder.instr(31, 12) ## Fill(12, 0.U)
    }
    is(RISCV_OP.BRANCH) {
      io_decoder.instr_type := RISCV_TYPE(opcode.asUInt ## funct3.asUInt ## Fill(7, 0.U))
      io_decoder.valid := opcode =/= RISCV_OP.UNKNOWN && io_decoder.instr_type =/= RISCV_TYPE.UNKNOWN
      io_decoder.rs1 := RS1
      io_decoder.rs2 := RS2
      io_decoder.rd := 0.U
      io_decoder.imm := Fill(19, io_decoder.instr(31)) ## io_decoder.instr(31) ## io_decoder.instr(7) ## io_decoder.instr(30, 25) ## io_decoder.instr(11, 8) ## Fill(1, 0.U)
    }
    is(RISCV_OP.STORE) {
      io_decoder.instr_type := RISCV_TYPE(opcode.asUInt ## funct3.asUInt ## Fill(7, 0.U))
      io_decoder.valid := opcode =/= RISCV_OP.UNKNOWN && io_decoder.instr_type =/= RISCV_TYPE.UNKNOWN
      io_decoder.rs1 := RS1
      io_decoder.rs2 := RS2
      io_decoder.rd := 0.U
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ## io_decoder.instr(31, 25) ## io_decoder.instr(11, 7)
    }


    // JAL code decoder
    is(RISCV_OP.JAL) {
      io_decoder.instr_type := RISCV_TYPE(opcode.asUInt ## Fill(3, 0.U) ## Fill(7, 0.U)) // JAL is J-type
      io_decoder.valid := opcode =/= RISCV_OP.UNKNOWN // valid if opcode is not unknown
      io_decoder.rd := RD
      // we are setting the imm value to
      io_decoder.imm := Fill(11, io_decoder.instr(31)) ## io_decoder.instr(31) ## io_decoder.instr(19, 12) ## io_decoder.instr(20) ## io_decoder.instr(30, 21) ## Fill(1, 0.U)
    }

    // JALR code decoder
    is(RISCV_OP.JALR) {
      io_decoder.instr_type := RISCV_TYPE(opcode.asUInt ## funct3.asUInt ## Fill(7, 0.U)) // JALR is I-type
      io_decoder.valid := opcode =/= RISCV_OP.UNKNOWN && io_decoder.instr_type =/= RISCV_TYPE.UNKNOWN // valid if opcode and instr_type are not unknown
      io_decoder.rs1 := RS1
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      // we are setting the imm value to
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ## io_decoder.instr(31, 20) 
    } 

    // load instructions code decoder
    is(RISCV_OP.LOAD) {
      io_decoder.instr_type := RISCV_TYPE(opcode.asUInt ## funct3.asUInt ## Fill(7, 0.U)) // LOAD is I-type
      io_decoder.valid := opcode =/= RISCV_OP.UNKNOWN && io_decoder.instr_type =/= RISCV_TYPE.UNKNOWN // valid if opcode and instr_type are not unknown
      io_decoder.rs1 := RS1
      io_decoder.rd := RD
      // we are setting the imm value to
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ## io_decoder.instr(31, 20)
    }

  }

    
}
