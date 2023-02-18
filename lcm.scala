import Chisel._
import freechips.rocketchip.tile._ 
import freechips.rocketchip.config._ 
import freechips.rocketchip.diplomacy._ 

class LCM(val w : Int) extends Module{
    val io = IO(new Bundle{
        val in1 = Flipped(Valid(UInt(w.W)))
        val in2 = Flipped(Valid(UInt(w.W)))
        val out = Decoupled(UInt(w.W))  //解耦接口，加out.ready和out.valid信号,Chisel学习记录（2） - 知乎 (zhihu.com)
    })
//用于数据处理
    val x = Reg(UInt(w.W))
    val y = Reg(UInt(w.W))
    val a = Reg(UInt(w.W))
    val b = Reg(UInt(w.W))   
    
    val s_idle::s_dataIn::s_gcdComp::s_lcmComp::Nil = Enum(4)
    val state = RegInit(s_idle)
//状态机
    state := MuxCase(state,Seq(
        (((state===s_idle)&&io.in1.valid&&io.in2.valid) -> s_dataIn),
        ((state===s_dataIn) -> s_gcdComp),
        (((state===s_gcdComp)&&(x===y)) -> s_lcmComp),
        (((state===s_lcmComp)&&io.out.ready) -> s_idle)))
//数据输入ROCC内寄存器
    when(state===s_dataIn){
        x := io.in1.bits
        y := io.in2.bits
        a := io.in1.bits
        b := io.in2.bits
    }
//寻找最大公约数=x=y
    when(state===s_gcdComp){
        when(x>=y){ // 相等表明找到了最大公约数
            x := y
            y := x
        }.otherwise{
            y := y - x
        }       
    }
 //计算最小公倍数
    io.out.bits := a * b / x
    io.out.valid := state===s_lcmComp 
}

class  LCMAccelerator(opcodes: OpcodeSet)
    (implicit p: Parameters) extends LazyRoCC(opcodes) {
 override lazy val module = new LCMAcceleratorModule(this) 
} 

class LCMAcceleratorModule(outer: LCMAccelerator)
extends LazyRoCCModuleImp(outer)
{ 
  val rd = RegInit(0.U(5.W))        
  val rs1Value = RegInit(0.U(w.W))
  val rs1Enable = RegInit(false.B)
  val rs2Value = RegInit(0.U(w.W))
  val rs2Enable = RegInit(false.B)
   //ROCC接口信号
   val busy = RegInit(false.B)
   val canResp = RegInit(false.B)
   io.cmd.ready := !busy  //cmd初始为ture
   io.busy := busy   //busy初始为不忙
    val canDecode = io.cmd.fire() && (io.cmd.bits.inst.funct===0.U)  //判断交火

    when(canDecode){ // 每当fire时候会Rocket-core送一条指令过来，把相应的值传入寄存器
            busy := true.B
            rs1Value := io.cmd.bits.rs1
            rs1Enable := true.B 
            rs2Value := io.cmd.bits.rs2
            rs2Enable := true.B
            rd := io.cmd.bits.inst.rd
    }
   val lcm = Module(new LCM(w))  //实例化计算模块
   
   //计算值传入
    lcm.io.in1.bits := rs1Value
    lcm.io.in2.bits := rs2Value
    lcm.io.in1.valid :=  rs1Enable
    lcm.io.in2.valid := rs2Enable
    
    //计算值传出
     val lcmRes = RegInit(0.U(w.W))
              
    lcm.io.out.ready := Mux(lcm.io.out.valid, true.B, false.B)
    when(lcm.io.out.valid){
        lcmRes := lcm.io.out.bits
        canResp := true.B
}
  
 class WithLCMAccelerator extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => LazyModule(
    new LCMAccelerator(OpcodeSet.custom0 | OpcodeSet.custom1)(p)))
})

//另一写法
class WithLCMRoCCAccel extends Config((site,here,up) => {
    case BuildRoCC => Seq(       
        (p:Parameters) => {
            val regWidth = 64 // 寄存器位宽
            val lcmAccel = LazyModule(new LCMRoCCAccel(OpcodeSet.all, regWidth)(p))
            lcmAccel
        }
    )
})
//自定义指令编码
object OpcodeSet {
  def custom0 = new OpcodeSet(Seq(Bits("b0001011")))
  def custom1 = new OpcodeSet(Seq(Bits("b0101011")))
  def custom2 = new OpcodeSet(Seq(Bits("b1011011")))
  def custom3 = new OpcodeSet(Seq(Bits("b1111011")))
  def all = custom0 | custom1 | custom2 | custom3
}

//顶层配置
class LCMAcceleratorConfig extends Config( 
new WithCustomAccelerator ++ 
new RocketConfig)


