package samples.zenguitar

/* === InstConfig =============================== START ============================================================= */
object InstConfig {
  private var DEFAULT_NUM_STRINGS           = 8   // If not explicitly defined otherwise, the instrument has 8 strings.
  private var CHINESE_GUITAR_NUM_STRINGS    = 4   // The Chinese Guitar has only 4 strings.
  private var DEFAULT_OPEN_NOTE_VALS        = List(46, 41, 37, 32, 27, 22, 17, 12)
  private var CHINESE_GUITAR_OPEN_NOTE_VALS = List(51, 46, 44, 39)

  /* The InstConfig Interface --------------------------------------------------------------------------------------- */
  def config4Midi(midiInstNum: Int) = {           // Delivers the instrument's major configuration.
    if (midiInstNum == 47)                        // Instruments number 47 is the Chinese Guitar.
      new InstConfig(CHINESE_GUITAR_NUM_STRINGS , CHINESE_GUITAR_OPEN_NOTE_VALS , true) else  // Chinese Guitar.
      new InstConfig(DEFAULT_NUM_STRINGS        , DEFAULT_OPEN_NOTE_VALS        , true)       // Any other instrument.
  }

  case class InstConfig(                          // Holds the individual Instrument's main parameters.
   val numStrings   : Int       ,                 // .. The number of Strings.
   val openNoteVals : List[Int] ,                 // .. Tone ..
   val bendEnabled  : Boolean   )                 // .. Should it support "bending".

  val midis = List(                               // Holds the Index-numbers of the different Instruments.
    46,  // Pizzicato Strings
    47,  // Orchestral Harp
    48,  // Timpani
    57,  // Trumpet
    58,  // Trombone
    61,  // French Horn
    67,  // Tenor Sax
    72,  // Clarinet
    23,  // Harmonica 76,  // Pan Flute
    106, // Banjo
    1,   // Acoustic Grand Piano
    5,   // Electric Piano 1
    14,  // Xylophone
    22,  // Accordion
    25,  // Acoustic Guitar (nylon)
    31,  // Distortion Guitar
    33,  // Acoustic Bass
    41,  // Violin
    43,  // Cello
    47
  )
  /* ................................................................................................................ */
}
/* === InstConfig =============================== END =============================================================== */