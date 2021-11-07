package net.hotelling.harold.audiowidget

import javax.sound.sampled.DataLine.Info
import javax.sound.sampled._

import scala.collection.mutable.ArrayBuffer

/**
  * Convert raw data coming in into a sequence of amplitude values.
  */
class Audio(val format: AudioFormat, val timeWindowMillis: Double = 50.0) {
  if (format.getEncoding != AudioFormat.Encoding.PCM_UNSIGNED &&
    format.getEncoding != AudioFormat.Encoding.PCM_SIGNED) {
    throw new RuntimeException(s"Not sure how to deal with audio format with unsupported encoding: $format")
  }

  val bitsPerFrame: Int = format.getSampleSizeInBits * format.getChannels
  if (bitsPerFrame % 8 != 0) {
    // Frames may span across several adjacent bytes so we need to split up data that way.
    throw new RuntimeException(s"Weird sized audio input frames are not supported: $format")
  }

  val framesInWindow: Int = (format.getFrameRate * timeWindowMillis / 1000.0).toInt

  val bufferSize: Int = {
    val desiredSize =(framesInWindow * bitsPerFrame / 16.0).toInt
    // Make sure we ask for a multiple of the frame size in bytes:
    (desiredSize / format.getFrameSize) * format.getFrameSize
  }

  private[this] val buffer: Array[Byte] = Array.fill[Byte](bufferSize)(0)

  def printInfo() {
    println(s"format: $format")
    println(s"bitsPerFrame: $bitsPerFrame")
    println(s"framesIn50ms: $framesInWindow")
    println(s"buffer size:  $bufferSize")
  }

  def readFrames(line: TargetDataLine): Array[Int] = {
    // By monitoring line.available I discovered that the backlog of input data
    // kept growing and growing and I needed to flush the data each time.  This
    // way each sample is current and the UI does not being to lag behind.
    line.flush

    line.start()
    val bytesRead = line.read(buffer, 0, bufferSize)
    line.stop()

    decode(buffer, bytesRead)
  }

  /** Decode the input samples into Int values.
    *
    * Only takes in the first channel, i.e. the left channel for stereo data.
    * Compensates for 8- or 16-bit samples, big- or little-endian 16 bit values,
    * and
    * TODO: signed versus unsigned values.
    */
  def decode(data: Array[Byte], numBytes: Int): Array[Int] = {
    val result = new ArrayBuffer[Int]()
    // Each frame is an integer number of bytes so we can just split up the bytes into frames.
    var offsetBytes: Int = 0
    while (offsetBytes < numBytes - format.getFrameSize) {
      offsetBytes += format.getFrameSize

      // For now, assume we're working with 8 bit or 16 bit data.
      val sample: Int = if (format.getSampleSizeInBits == 16) {
        // Just take 1 channel for now
        val byte1 = data(offsetBytes)
        val byte2 = data(offsetBytes + 1)
        if (format.isBigEndian) (byte1 << 8) + byte2 else (byte2 << 8) + byte1
      } else if (format.getSampleSizeInBits == 8) {
        // Just take 1 channel for now
        data(offsetBytes).toInt
      } else {
        throw new RuntimeException(s"Not sure how to deal with audio format: $format")
      }
      result.append(sample)
    }
    result.toArray
  }
}

object Audio {
  def getMixerByName(name: String = "Primary Sound Capture Driver"): Mixer = {
    val mixerInfo = AudioSystem.getMixerInfo.filter { _.getName == name }.head
    println("Found mixer info: " + mixerInfo)
    AudioSystem.getMixer(mixerInfo)
  }

  def getSomeInputLine(mixer: Mixer): TargetDataLine = {
    val line = mixer.getTargetLineInfo.map(mixer.getLine)
      .filter(_.isInstanceOf[TargetDataLine]).map(_.asInstanceOf[TargetDataLine]).head
    line.open()
    line
  }

  def getLineFormats(line: TargetDataLine): Array[AudioFormat] = {
    line.getLineInfo.asInstanceOf[DataLine.Info].getFormats
  }

  def supportsInput(mixer: Mixer, lineInfo: Line.Info): Boolean = {
    val line = mixer.getLine(lineInfo)
    line match {
      case tdl: TargetDataLine => !tdl.getLineInfo.asInstanceOf[Info].getFormats.isEmpty
      case _ => false
    }
  }

  def supportsInput(mixerInfo: Mixer.Info): Boolean = {
    val mixer: Mixer = AudioSystem.getMixer(mixerInfo)
    mixer.getTargetLineInfo.exists(supportsInput(mixer, _))
  }

  def mixerNamesWithInputSupport(): Array[String] =
    AudioSystem.getMixerInfo.toList.filter(supportsInput).map(_.getName).toArray

  def dumpInfo(): Unit = {
    val mixers = AudioSystem.getMixerInfo
    mixers.foreach { mixerInfo =>
      println(mixerInfo.getName)
      println(mixerInfo.getDescription)
      println(mixerInfo.getVendor)
      println(mixerInfo.getVersion)

      val mixer: Mixer = AudioSystem.getMixer(mixerInfo)
      mixer.getSourceLineInfo.foreach { sourceLineInfo =>
        println(s"  $sourceLineInfo")
        val line = mixer.getLine(sourceLineInfo)

        line match {
          case sdl: SourceDataLine =>
            sdl.getLineInfo.asInstanceOf[Info].getFormats.foreach { format =>
              println(s"    output: $format")
            }
          case _ =>
        }
      }
      mixer.getTargetLineInfo.foreach { targetLineInfo =>
        println(s"  $targetLineInfo")

        val line = mixer.getLine(targetLineInfo)
        line match {
          case tdl: TargetDataLine =>
            tdl.getLineInfo.asInstanceOf[Info].getFormats.foreach { format =>
              println(s"    input: $format")
            }
          case _ =>
        }
      }

      println()

      val targetLines = mixer.getTargetLines
      targetLines.foreach { targetLine => println(targetLine.getLineInfo) }
      println(s"targetLines: ${targetLines.size}")
      println()
    }

    val mixer = AudioSystem.getMixer(mixers(0))
    val targetLines = mixer.getTargetLines
    targetLines.foreach { targetLine => println(targetLine.getLineInfo) }
    println(s"targetLines: ${targetLines.size}")
  }
}
