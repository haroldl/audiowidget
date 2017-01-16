package net.hotelling.harold.audiowidget

import javax.sound.sampled.DataLine.Info
import javax.sound.sampled._


object AudioWidget {

  def main(args: Array[String]) {
    //dumpInfo()

    val window = new GraphWindow()
    window.display()

    //val mixer = getMixerByName("Microphone (9- USB VoIP Device)")
    val mixer = getMixerByName()
    val line = getSomeInputLine(mixer)
    val parser = new AudioParser(line.getFormat)
    while (true) {
      val data = parser.readFrames(line)
      window.setData(rescaleData(data, window.getMaxYValueForOscilloscope.getOrElse(1000).toFloat))
      val intensityData = DFT.dft(data, 44100d, 40, 10000).map { v => (v * 10).toInt }
      window.setIntensity(rescaleData(intensityData, window.getMaxYValueForFrequencies.getOrElse(1000).toFloat))
    }
  }

  def rescaleData(data: Array[Int], desiredMaxAmplitude: Float): Array[Int] = {
    val (min, max) = (data.min, data.max)
    val amplitude = Math.max(Math.abs(min), Math.abs(max))
    val scale = desiredMaxAmplitude / amplitude.toFloat
    data.map(_ * scale).map(_.toInt)
  }

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

  def readSomeData(line: TargetDataLine): Unit = {
    new AudioParser(line.getFormat).printInfo()

    println(s"isActive? ${line.isActive} ; available bytes: ${line.available}")
    line.start()
    println(s"isActive? ${line.isActive} ; available bytes: ${line.available}")
    val bufSize = 1024
    val buffer = Array.fill[Byte](bufSize)(0)
    var bytesRead = line.read(buffer, 0, bufSize)
    println(s"isActive? ${line.isActive} ; available bytes: ${line.available}")
    println(s"${System.currentTimeMillis} Was able to read $bytesRead bytes.")
    val startTime = System.currentTimeMillis()
    (1 to 1000) foreach { _ =>
      bytesRead = line.read(buffer, 0, bufSize)
      //println(s"${System.currentTimeMillis} Was able to read $bytesRead bytes.")
      if (bytesRead != 1024) {
        println(s"${System.currentTimeMillis} Was able to read $bytesRead bytes.")
      }
    }
    line.stop()
    val elapsedTime: Long = System.currentTimeMillis() - startTime
    println(s"Did 1000 1k reads in $elapsedTime ms")
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
