package net.hotelling.harold.audiowidget

import javax.sound.sampled.DataLine.Info
import javax.sound.sampled._

import scala.collection.mutable.ArrayBuffer

/**
  * Convert raw data coming in into a sequence of amplitude values.
  */
class Audio(val format: AudioFormat, val timeWindowMillis: Double = 50.0) {
  val bitsPerFrame: Int = format.getSampleSizeInBits * format.getChannels
  val framesInWindow: Int = (format.getFrameRate * timeWindowMillis / 1000.0).toInt
  val bufferSize: Int = {
    val desiredSize =(framesInWindow * bitsPerFrame / 16.0).toInt
    // Make sure we ask for a multiple of the frame size in bytes:
    (desiredSize / format.getFrameSize) * format.getFrameSize
  }

  private[this] val buffer = Array.fill[Byte](bufferSize)(0)

  def printInfo() {
    println(s"format: $format")
    println(s"bitsPerFrame: $bitsPerFrame")
    println(s"framesIn50ms: $framesInWindow")
    println(s"buffer size:  $bufferSize")
  }

  def readFrames(line: TargetDataLine): Array[Int] = {
    val result = new ArrayBuffer[Int]()

    line.start()
    val bytesRead = line.read(buffer, 0, bufferSize)
    line.stop()

    if (bitsPerFrame % 8 == 0) {
      // For now, assume we're working with 16 bit stereo data.
      if (format.getFrameSize != 4) throw new RuntimeException(s"Not sure how to deal with audio format: $format")

      // Each frame is an integer number of bytes so we can just split up the bytes into frames.
      var offsetBytes: Int = 0
      while (offsetBytes < bytesRead - format.getFrameSize) {
        offsetBytes += format.getFrameSize

        // Just take 1 channel for now
        val byte1 = buffer(offsetBytes)
        val byte2 = buffer(offsetBytes + 1)
        val sample: Int = if (format.isBigEndian) (byte1 << 8) + byte2 else (byte2 << 8) + byte1
        result.append(sample)
      }
    } else {
      // Frames may span across several adjacent bytes so we need to split up data that way.
      throw new RuntimeException(s"weird sized audio input frames are not supported... $format")
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