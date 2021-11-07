AudioWidget
===========

Just some playing around with audio data.

Try `./gradlew runme`.

So far:
* Shows raw audio capture data in the top graph.
* Shows strength of signal for frequencies in the bottom graph.
* Placeholder piano keyboard for showing which notes are being played.

Next steps:
* Add drop-down to choose a mixer (among those with input capabilities).
* For each note, calculate the set of frequencies to test.
* Display a mini-graph of those frequencies in the piano JFrame.
* Average the per-note scores to get overall signal strength for that note.
* Convert per-note signals to z-scores and find positive outliers, i.e. the notes being played.
* Detect chord patterns.
* Per note, see if the center-of-mass value is sharp or flat and by how much.
* Add a music staff and show notes being played in a scrolling fashion.

![Sample Screenshot](screenshot.png)
