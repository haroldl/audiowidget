package net.hotelling.harold.audiowidget

/**
  * Discrete Fourier Transform
  *
  * Naive O(n^^2) just to play around for now.
  */
object DFT {

  /**
    * Take the raw data points and test for the strength of the signal
    * at various frequencies going from minFreq to maxFreq in increments
    * of stepFreq.
    */
  def dft(data: Array[Int], samplesPerSecond: Double,
          minFreqHz: Int = 20, maxFreqHz: Int = 20000, stepFreqHz: Int = 15): Array[Double] = {
    // Each sampled data point will be the magnitude of a polar coordinate
    // where the angle goes through 0 to 2 * pi once each second for each
    // Hertz of frequency we're testing for.
    val angleDeltaRadians: Double = 2 * Math.PI / samplesPerSecond

    // Spin the data around the origin at the given frequency
    def spin(freq: Int): Point = {
      data.zipWithIndex.map({ case (value, i) =>
        val angle = angleDeltaRadians * freq * i
        val magnitude = data(i).toDouble
        // Convert polar coordinates to cartesian:
        Point(magnitude * Math.cos(angle), magnitude * Math.sin(angle))
      }).reduce(_ + _)
    }

    // Test the strength of the signal (magnitude) at each frequency of interest.
    // We're ignoring the phase (angle) of the points because we're not interested
    // in how the different frequencies align.
    (minFreqHz until maxFreqHz by stepFreqHz).map { freq =>
      spin(freq).magnitude
    }.toArray
  }

  case class Point(x: Double, y: Double) {
    def +(that: Point): Point = Point(this.x + that.x, this.y + that.y)
    def magnitude: Double = Math.sqrt((x * x) + (y * y))
  }
}
