package arithmetic

import chisel3._
import chisel3.util._

class Divider(bitWidth: Int) extends Module {
    val io = IO(new Bundle {
        val start = Input(Bool())
        val done = Output(Bool())
        val dividend = Input(UInt(bitWidth.W))
        val divisor = Input(UInt(bitWidth.W))
        val quotient = Output(UInt(bitWidth.W))
        val remainder = Output(UInt(bitWidth.W))
    })

    val remainder = RegInit(0.U(bitWidth.W))       //current remainder
    val quotient = RegInit(VecInit(Seq.fill(bitWidth)(0.U(1.W))))   //= {dividend[i:0], quotient[N−1:i+1]}, where dividend is the input dividend and quotient is the final output quotient, and i is the current cycle
    val divisor = RegInit(0.U(bitWidth.W))         //divisor
    
    // TODO: implement Problem 1.1 here
    
    var cnt = RegInit(bitWidth.U - 1.U)
    io.done := false.B
    io.quotient := DontCare
    io.remainder := DontCare
    
    object breakPoint extends ChiselEnum{
        val compute, done = Value
    } 
    val state = RegInit(breakPoint.done) // initialize the state with idle

    /* 
    algorithm:
        for i = n-1 to 0
         R' = 2*R + A[i]
         if(R' < B) then Q[i] = 0, R = R'
         else Q[i] = 1, R = R' - B
    */ 

    when(io.start){
        /* 
            since io.dividend and io.divisor might change every clock rise, 
           we assign io.dividend and io.divisor to divisor and quotient respectively,
           such that the they won't change during the calculation if the clock raise in the middle
        */
        state := breakPoint.compute
        divisor := io.divisor   // initialize remainder and quotient
        
        remainder := 0.U 

        // quotient is a vector, so we assign io.dividend bit by bit
        for(i <- 0 until bitWidth){     
            quotient(i) := io.dividend(i)
        }
        
        cnt := (bitWidth).U
        
    }.otherwise {
        switch(state){
            is(breakPoint.compute){
       
                when(divisor === 0.U){

                    for(i <- 0 until bitWidth){     // x/0 -> quotient = Maximum value of that bitWidth (bitWidth = 6 -> Max = <111111> = 63 )
                        quotient(i) := 1.U 
                    }
                    remainder := quotient.asUInt
                    state := breakPoint.done

                } .otherwise{
                    when(cnt === 0.U){
                        
                        state := breakPoint.done

                    } .otherwise{
                        val RmulTwo = remainder << 1      // RmulTwo = r * 2
                        val Rprime = RmulTwo + quotient(cnt - 1.U)     //R' = RmulTwo + the counter bit of dividend

                        quotient(cnt - 1.U) := Mux(Rprime < divisor, 0.U, 1.U)     // if R' < B then Q[i] = 0, else 1
                        val RPminusB = Rprime - divisor
                        remainder := Mux(Rprime < divisor, Rprime, RPminusB) // if R' < B then R = R', else R = R' - B
        
                        cnt := cnt - 1.U
                    }
                }
            } 
            is(breakPoint.done){
                io.done := true.B
                io.quotient := quotient.asUInt
                io.remainder := remainder
            }
        }
    }      
}
