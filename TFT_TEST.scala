package TFT_Driver

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import TFT_Driver._
import MySpinalHardware._
class TFT_TEST(val delay: BigInt) extends Component {
    var io = new Bundle()
    {
        val lcd_rst = out Bool()
        val lcd_dc = out Bool()
        val lcd_sdo = out Bool()
        val lcd_sck = out Bool()
    }

    val data_clk = False
    val data = B"9'h000"

    val lcd = TFT_ILI9341(delay)
    io.lcd_sck := lcd.io.SPI_SCL
    io.lcd_sdo := lcd.io.SPI_SDA
    io.lcd_rst := lcd.io.SPI_RST
    io.lcd_dc := lcd.io.SPI_DC


    lcd.io.data := data
    lcd.io.data_clk := data_clk

    //draw area setting for 0x0 by 320x240
    val initParamsList = List(
        0x02A, 0x100, 0x100, 0x101, 0x13f, //CASET
        0x02B, 0x100, 0x100, 0x100, 0x1ef, //RASET
        0x02C, //RAMWR
        0x1FF  //END
    )
    val ParamsRom = Mem(UInt(9 bits), initParamsList.map(U(_, 9 bits)))
    val ParamsPointer = new Counter(start = 0, end = initParamsList.length)
    val paramData = ParamsRom(ParamsPointer)

    val ScreenX = Counter(start = 0, end = 319)
    val ScreenY = Counter(start = 0, end = 239)
    val ScreenDone = RegNext(ScreenX.willOverflowIfInc && ScreenY.willOverflowIfInc)

    val colorHighByte = False
    val colorOut = B"16'h0000"

    val ball = False
    val grid = False

    val ballX = Reg(UInt(9 bits)) init(0)
    val ballY = Reg(UInt(8 bits)) init(10)
    val ballXDir = Reg(Bool) init(True)
    val ballYDir = Reg(Bool) init(True)

    when(ScreenX(4) ^ ScreenY(4)) {
        grid := True
    }

    when(ScreenX >= ballX && ScreenX <= ballX+10 && ScreenY >= ballY && ScreenY <= ballY+10){
        ball := True
    }

    when(ball){
        colorOut := B"16'hf800"
    }elsewhen(grid){
        colorOut := B"16'h39C7"
    }

    //mux between the parameters and pixel data in the color space of 565
    when(paramData === 0x1FF){
        when(colorHighByte){
            data := B"1" ## colorOut(15 downto 8)
        }otherwise{
            data := B"1" ## colorOut(7 downto 0)
        }
    }otherwise(data := paramData.asBits(8 downto 0))

    val fsm = new StateMachine {
        val Init: State = new State with EntryPoint {
            whenIsActive {
                data_clk := False
                goto(Wait)
            }
        }

        //Wait for SPI reset
        val Wait: State = new State {
            whenIsActive{
                when(lcd.io.displayReady){
                    goto(Done)
                }
            }
        }

        //Send the paramData till we hit 0x1ff then start sending pixel data
        val StartFrame: State = new State{
            whenIsActive {
                when(paramData === 0x1ff){
                    goto(SendData)
                }otherwise{
                    data_clk := True;
                    goto(SendData)
                }
            }
        }

        val SendData: State = new State{
            whenIsActive {
                data_clk := False;
                when(!lcd.io.fifo_full){
                    when(paramData =/= 0x1ff){
                        ParamsPointer.increment()
                        goto(StartFrame)
                    }otherwise{
                        goto(LoadColorLowByte)
                    }
                }
            }
        }
        val LoadColorLowByte: State = new State {
            onEntry{
                data_clk := True;
                colorHighByte := True
            }
            whenIsActive {
                data_clk := False;
                goto(LoadColorHighByte)
            }
        }

        val LoadColorHighByte: State = new State {
            onEntry{
                data_clk := True;
                colorHighByte := False
            }
            whenIsActive  {
                data_clk := False;
                when(ScreenDone){
                    goto(Finish)
                }otherwise{
                    ScreenX.increment()
                    when (ScreenX.willOverflowIfInc) {
                        ScreenY.increment()
                    }
                    goto(SendData)
                }
            }
        }
        
        val Finish: State = new State {
            whenIsActive {
                colorHighByte :=  False;

                when(ballX < 1){
                    ballXDir := True
                } elsewhen(ballX >= 310){
                    ballXDir := False
                }

                when(ballY < 1) {
                    ballYDir := True
                } elsewhen(ballY >= 230){
                    ballYDir := False
                }

                goto(Done)
            }
        }
        //Reset everything and wait for next frame
        val Done: State = new StateDelay(delay) {
            whenIsActive {
                data_clk := False;
                ScreenY.clear()
                ScreenX.clear()
                ParamsPointer.clear()
                colorHighByte := False
            }
            whenCompleted{

                when(ballXDir) {
                    ballX := ballX + 1
                } otherwise (ballX := ballX - 1)

                when(ballYDir) {
                    ballY := ballY + 1
                } otherwise (ballY := ballY - 1)

                goto(StartFrame)
            }
        }
    }
}

object TFT_TESTConfig extends SpinalConfig(targetDirectory = "/home/winston/Projects/C/nodeUI/src/TestNodes/", defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

//Generate the MyTopLevel's Verilog using the above custom configuration.
object TFT_TESTGen {
    def main(args: Array[String]) {
        TFT_TESTConfig.generateVerilog(new TFT_TEST(10))
    }
}