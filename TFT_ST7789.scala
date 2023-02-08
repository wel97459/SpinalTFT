package TFT_Driver

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateDelay, StateMachine}

import scala.util.control.Breaks

object TFT_ST7789 {
    def apply(cycles: BigInt) : TFT_ST7789 = new TFT_ST7789(cycles)
    def apply(time: TimeNumber) : TFT_ST7789 = new TFT_ST7789((time * ClockDomain.current.frequency.getValue).toBigInt())
 }

class TFT_ST7789(val Delay: BigInt) extends Component {
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
        0x211, //SLPOUT with a Wait
        0x03A, 0x155, //COLMOD
        0x0E0, 0x1d0, 0x100, 0x102, 0x107, 0x10a, 0x128, 0x132, 0x144, 0x142, 0x106, 0x10e, 0x112, 0x114, 0x117, //PVGAMCTRL
        0x0E1, 0x1d0, 0x100, 0x102, 0x107, 0x10a, 0x128, 0x131, 0x154, 0x147, 0x10e, 0x11c, 0x117, 0x11b, 0x11e, //NVGAMCTRL
        0x02A, 0x100, 0x100, 0x100, 0x1f0, //CASET
        0x02B, 0x100, 0x100, 0x100, 0x1f0, //RASET
        0x0DF, 0x15a, 0x169, 0x102, 0x101, //CMD2EN
        0x0C6, 0x101, //TFT_FRCTR2
        0x221, //INVON
        0x213, //NORON
        0x229, //DISPON
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

object TFT_ST7789_Test {
    def main(args: Array[String]) {
        SimConfig.withWave.compile{
            val dut = TFT_ST7789(100)
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