package arithmetic

import chisel3._
import scala.reflect.ClassTag
import chisel3.util._
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}

abstract class ComputationalUnit(width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val c = Output(UInt(width.W))
    })  
}

class ParallelUnit(vectorSize: Int, arraySize: Int, unitWidth: Int, comp : (Int) => ComputationalUnit) extends Module {
    require(vectorSize % arraySize == 0)
    val io = IO(new Bundle {
        val a = Input(Vec(vectorSize, UInt(unitWidth.W)))
        val b = Input(Vec(vectorSize, UInt(unitWidth.W)))
        val start = Input(Bool())
        val done = Output(Bool())
        val c = Output(Vec(vectorSize, UInt(unitWidth.W)))
    })

    val units = Seq.fill(arraySize)(Module(comp(unitWidth)))
    
    // TODO: implement Problem 1.3 here
    for(i <- 0 until arraySize){
        units(i).io <> DontCare 
    }

    val inputA = Reg(Vec(vectorSize, UInt(unitWidth.W)))
    val inputB = Reg(Vec(vectorSize, UInt(unitWidth.W)))
    val outputC = Reg(Vec(vectorSize, UInt(unitWidth.W)))

    io.c := DontCare
    io.done := false.B

    val numCycles = vectorSize / arraySize  // how many set of arraySize
    val cnt = RegInit(0.U(numCycles.W))     // totally numCycles' round

    object states extends ChiselEnum{
        val inCycle, done = Value
    }
    val state = RegInit(states.done)

    when(io.start){

        inputA := io.a  // initialization part
        inputB := io.b
        cnt := 0.U
        state := states.inCycle

    } .otherwise{
        switch(state){

            is(states.inCycle){
                for(i <- 0 until arraySize){      // until is exclusive
                    
                    units(i).io.a := inputA(cnt * arraySize.U + i.U)    // cnt start from 0 to numCycles - 1
                    units(i).io.b := inputB(cnt * arraySize.U + i.U)
                    outputC(cnt * arraySize.U + i.U) := units(i).io.c   // no need to cnt - 1, if so, will go to negative
                }

                cnt := cnt + 1.U
        
                when(cnt === (numCycles - 1).U) {   // because cnt start from 0, so numCycles - 1 
                    state := states.done
                }
            }

            is(states.done){
                io.c := outputC
                io.done := true.B
            }
        }
    }
        
}
