package net.hotelling.harold.audiowidget


object AudioWidget {

  def main(args: Array[String]) {
    val window = new GraphWindow()
    window.display()

    val mixer = Audio.getMixerByName( /* "Microphone (9- USB VoIP Device)" */ )
    val line = Audio.getSomeInputLine(mixer)
    val parser = new Audio(line.getFormat)
    while (true) {
      val data = parser.readFrames(line)
      window.setData(rescaleData(data, window.getMaxYValueForOscilloscope.getOrElse(1000).toFloat))
      val intensityData = DFT.dft(data, parser.format.getFrameRate, 40, 5000).map { v => (v * 10).toInt }
      window.setIntensity(rescaleData(intensityData, window.getMaxYValueForFrequencies.getOrElse(1000).toFloat))
    }
  }

  def rescaleData(data: Array[Int], desiredMaxAmplitude: Float): Array[Int] = {
    val (min, max) = (data.min, data.max)
    val amplitude = Math.max(Math.abs(min), Math.abs(max))
    val scale = desiredMaxAmplitude / amplitude.toFloat
    data.map(_ * scale).map(_.toInt)
  }
}
