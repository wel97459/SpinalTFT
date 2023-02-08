package TFT_Driver

import spinal.core._
import spinal.lib.{Counter, Stream, StreamFifo}
import spinal.lib.fsm.{EntryPoint, State, StateDelay, StateMachine}

object TFT_SPI {
    def apply(cycles: BigInt) : TFT_SPI = new TFT_SPI(cycles)
    def apply(time: TimeNumber) : TFT_SPI = new TFT_SPI((time * ClockDomain.current.frequency.getValue).toBigInt())
}

class TFT_SPI(val Delay: BigInt) extends Component {
    val io = new Bundle {
            val data = in Bits(9 bits) //Data to be placed into FIFO
            val data_clk = in Bool()

            val ready = out Bool()
            val sending = out Bool()

            val fifo_full = out Bool() //High when less than two byte are available in the fifo

            val SPI_SCL = out Bool()
            val SPI_SDA = out Bool()
            val SPI_DC = out Bool()
            val SPI_RST = out Bool()
            val SPI_CS = out Bool()
    }

    val source,sink = Stream(Bits(9 bits))

    val spiFiFo = StreamFifo(
        dataType = Bits(9 bits),
        depth    = 16
    )

    spiFiFo.io.push << source
    spiFiFo.io.pop  >> sink
    sink.ready := False

    val validRegSource = Reg(Bool) init(False)
    source.valid := io.data_clk
    source.payload := io.data

    val fifo_full = RegNext(spiFiFo.io.availability < 2) init(False)
    io.fifo_full := fifo_full

    val ready = Reg(Bool) init(False)
    io.ready := ready

    val sending = Reg(Bool) init(False)
    io.sending := sending

    val bitCount = Counter(8)
    val shiftReg = Reg(Bits(8 bits)) init(0)
    val SPI_SCL = Reg(Bool) init(True)
    val SPI_SDA = Reg(Bool) init(False)
    val SPI_DC = Reg(Bool) init(False)
    val SPI_RST = Reg(Bool) init(False)
    val SPI_CS = Reg(Bool) init(True)

    io.SPI_SCL := SPI_SCL
    io.SPI_SDA := SPI_SDA
    io.SPI_DC := SPI_DC
    io.SPI_RST := SPI_RST
    io.SPI_CS := SPI_CS

    val fsm = new StateMachine {

        val Init: State = new StateDelay(Delay) with EntryPoint {
            whenIsActive {
                SPI_RST := False
            }

            whenCompleted {
                goto(ResetDisplay)
            }
        }

        val ResetDisplay: State = new StateDelay(Delay) {
            whenIsActive {
                SPI_RST := True
            }

            whenCompleted {
                ready := True
                goto(WaitForData)
            }
        }

        val WaitForData: State = new State {
            whenIsActive {
                when(sink.valid) {
                    shiftReg := sink.payload(7 downto 0)
                    SPI_DC := sink.payload(8)
                    SPI_CS := False
                    bitCount.clear()
                    sink.ready := True
                    sending := True
                    goto(OutData)
                }otherwise(sending := False)
            }
        }

        val OutData: State = new State {
            whenIsActive {
                sink.ready := False
                SPI_SDA := shiftReg(7)
                SPI_SCL := False
                goto(ShiftData)
            }
        }

        val ShiftData: State = new State {
            whenIsActive {
                bitCount.increment()
                shiftReg := shiftReg |<< 1
                SPI_SCL := True
                when(bitCount === 7) {
                    when(sink.valid){
                        shiftReg := sink.payload(7 downto 0)
                        SPI_DC := sink.payload(8)
                        bitCount.clear()
                        sink.ready := True
                        goto(OutData)
                    }otherwise {
                        SPI_CS := True
                        goto(WaitForData)
                    }
                } otherwise(goto(OutData))
            }
        }
    }
}