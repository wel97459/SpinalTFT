package TFT_Driver

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateDelay, StateMachine}

import scala.util.control.Breaks

object TFT_ILI9341 {
    def apply(cycles: BigInt) : TFT_ILI9341 = new TFT_ILI9341(cycles)
    def apply(time: TimeNumber) : TFT_ILI9341 = new TFT_ILI9341((time * ClockDomain.current.frequency.getValue).toBigInt())
}

class TFT_ILI9341(val Delay: BigInt) extends Component {
    val io = new Bundle {
        val data = in Bits (9 bits) //Data to be placed into FIFO
        val data_clk = in Bool()

        val ready = out Bool()
        val sending = out Bool()
        val fifo_full = out Bool() //High when less than two byte are available in the fifo
        val displayReady = out Bool()

        val SPI_SCL = out Bool()
        val SPI_SDA = out Bool()
        val SPI_DC = out Bool()
        val SPI_RST = out Bool()
        val SPI_CS = out Bool()
    }

    val displayReady = Reg(Bool) init (false)
    io.displayReady := displayReady

    val data_clk = Reg(Bool) init (false)
    val data = Reg(Bits(9 bits))

    val spi = new TFT_SPI(Delay)
    io.SPI_SCL := spi.io.SPI_SCL
    io.SPI_SDA := spi.io.SPI_SDA
    io.SPI_RST := spi.io.SPI_RST
    io.SPI_DC := spi.io.SPI_DC
    io.SPI_CS := spi.io.SPI_CS

    when(displayReady) {
        spi.io.data := io.data
        spi.io.data_clk := io.data_clk
    }otherwise{
        spi.io.data := data
        spi.io.data_clk := data_clk
    }

    io.ready := spi.io.ready
    io.sending := spi.io.sending
    io.fifo_full := spi.io.fifo_full

    val initParamsList = List(
        0x201, //SWRESET with a Wait
        0X0CB, 0x139, 0x12C, 0x100, 0x134, 0x102, //POWER CONTROL A
        0x0CF, 0x100, 0x1C1, 0x130, //POWER CONTROL B
        0x0E8, 0x185, 0x100, 0x178, //DRIVER TIMING CONTROL A
        0x0EA, 0x100, 0x100, //DRIVER TIMING CONTROL B
        0x0ED, 0x164, 0x103, 0x112, 0x181, //POWER ON SEQUENCE CONTROL
        0x0F7, 0x120, //PUMP RATIO CONTROL
        0x0C0, 0x123, //POWER CONTROL,VRH[5:0]
        0x0C1, 0x110, //POWER CONTROL,SAP[2:0];BT[3:0]
        0x0C5, 0x13E, 0x128, //VCM CONTROL
        0x0C7, 0x186, //VCM CONTROL 2
        //0x036, 0x148, //SCREEN_VERTICAL_1
        //0x036, 0x128, //SCREEN_HORIZONTAL_1
        //0x036, 0x188, //SCREEN_VERTICAL_2
        0x036, 0x1E8, //SCREEN_HORIZONTAL_2
        0x03A, 0x155, //PIXEL FORMAT
        0x0B1, 0x100, 0x118, //FRAME RATIO CONTROL, STANDARD RGB COLOR
        0x0B6, 0x108, 0x182, 0x127, //DISPLAY FUNCTION CONTROL
        0x0F2, 0x100, //3GAMMA FUNCTION DISABLE
        0x026, 0x101, //GAMMA CURVE SELECTED
        0x0E0, 0x10F, 0x131, 0x12B, 0x10C, 0x10E, 0x108, 0x14E, 0x1F1, 0x137, 0x107, 0x110, 0x103, 0x10E, 0x109, 0x100, //POSITIVE GAMMA CORRECTION
        0x0E1, 0x100, 0x10E, 0x114, 0x103, 0x111, 0x107, 0x131, 0x1C1, 0x148, 0x108, 0x10F, 0x10C, 0x131, 0x136, 0x10F, //NEGATIVE GAMMA CORRECTION
        0x211, //EXIT SLEEP
        0x029, //TURN ON DISPLAY
        0x3ff  //END
    )
    val initParamsRom = Mem(UInt(10 bits), initParamsList.map(U(_, 10 bits)))

    val fsm = new StateMachine {

        val initParamsPointer = new Counter(start = 0, end = initParamsList.length)

        val paramData = initParamsRom(initParamsPointer)

        val delayTimer = Timeout(Delay)

        val Init: State = new StateDelay(Delay) with EntryPoint {
            whenIsActive {
                initParamsPointer.clear()
                when(spi.io.ready) {
                    goto(LoadParams)
                }
            }
        }

        val LoadParams: State = new State{
            whenIsActive {
                data := paramData.asBits(8 downto 0)

                when(paramData === 0x3ff){
                    goto(Done)
                }elsewhen(paramData.asBits(9) && !spi.io.sending) {
                    data_clk := True;
                    delayTimer.clear()
                    goto(SendData)
                }elsewhen(!paramData.asBits(9)){
                    data_clk := True;
                    goto(SendData)
                }
            }
        }

        val SendData: State = new State{
            whenIsActive {
                data_clk := False;

                when(delayTimer && !spi.io.fifo_full){
                    initParamsPointer.increment()
                    goto(LoadParams)
                }
            }
        }

        val Done: State = new State{
            whenIsActive {
                displayReady := True
            }
        }
    }
}

object ILI9341_Test {
    def main(args: Array[String]) {
        SimConfig.withWave.compile{
            val dut = TFT_ILI9341(100)
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)

            dut.clockDomain.waitRisingEdge()
            var t = 0
            var tt = 0
            val loop = new Breaks;
            loop.breakable {
                while (true) {

                    if(dut.io.displayReady.toBoolean && tt == 0){
                        dut.io.data #= 0xf0
                        dut.io.data_clk #= true
                        tt=1
                    }else{
                        dut.io.data_clk #= false
                    }

                    if(t >= 10000) loop.break;
                    t+=1
                    dut.clockDomain.waitRisingEdge()
                }
            }
        }
    }
}