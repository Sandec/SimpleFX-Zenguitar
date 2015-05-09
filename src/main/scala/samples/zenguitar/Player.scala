package samples.zenguitar

import org.jfugue.theory.Note
import simplefx.all._
import simplefx.core._
import org.jfugue.realtime.RealTimePlayer

/* === Player =================================== START ============================================================= */
class Player {

  /* The Player Interface ------------------------------------------------------------------------------------------- */
  @Bind var instrument = 0                                            // The instrument index.
  @Bind var note       = 0                                            // The note.
  @Bind var softAttack = false                                        // The Soft-attack indicator.
  @Bind var pitch      = 0                                            // The value of the pitch-wheel.
  @Bind var playing    = false                                        // The Player's status.

  def play = internalPlay                                             // To execute the play-action.
  def stop = internalStop                                             // To stop    the play-action.
  /* ................................................................................................................ */


  /* Creating the Link to the RealTimePlayer ------------------------------------------------------------------------ */
  def init: Unit = player
  private lazy val player = new RealTimePlayer {                      // Creates the link to the midi-player.
    instrument --> { this.changeInstrument(_)                      }  // Bind instrument index to player's index.
    pitch      --> { this.changePitchWheel(0.toByte, pitch.toByte) }  // Sets the midi-player's pitch-wheel.
  }
  /* ................................................................................................................ */


  /* The internal Play and Stop Commands ---------------------------------------------------------------------------- */
  private var disp = Disposer.empty                                   // Creates an empty Disposer.

  private def notePattern(note: Int, softAttack: Boolean):String = {  // Generates am appropriate note-pattern,
    new Note(note) {                                                  // .. using the midi-note.
      if (softAttack) setAttackVelocity((getAttackVelocity() * 0.75).toByte) // if softAttack, downgrade attackVelocity.
    }.getPattern.toString
  }

  private def internalPlay: Unit = {
    stop                                                              // Don't allow parallel playing.
    playing = true                                                    // Sets the Player's status to "playing".
    if(note != 0) {                                                   // If there is something to play ..
      player.play(notePattern(note, softAttack) + "s-")               // .. start player with the correct pattern.
      val dispPattern = notePattern(note, false) + "-s"               // .. get the pattern for the stop.
      disp = Disposer{                                                // .. creates a disposer for later disposing.
        pitch = 0                                                     // .. reset the pitch.
        player.play(dispPattern)                                      // .. Provide the "stop-pattern".
      }
    }
  }

  private def internalStop: Unit = {
    playing = false                                                   // Sets the Player's status to "not playing".
    disp.dispose                                                      // Disposes the player's .. ??
    disp = Disposer.empty                                             // Why is this required - is dispose not enough???
  }
  /* ................................................................................................................ */
}
/* === Player =================================== END =============================================================== */
