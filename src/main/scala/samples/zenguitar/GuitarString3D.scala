package samples.zenguitar

import simplefx.all._
import simplefx.core._
import simplefx.experimental._

/* === GuitarString3D =========================== START ============================================================= */
class GuitarString3D(openNoteValue: Int, numFrets: Int, pwidth: Double, pheight: Double, zenGuitar3D: ZenGuitar3D)
  extends Pane {
  import zenGuitar3D._

  /* Global constants and declarations ------------------------------------------------------------------------------ */
  lazy private val FRET_MAT   = new PhongMaterial(Color.rgb(222, 215, 165))     // Color of the FretCylinders.
  lazy private val STRING_MAT = new PhongMaterial(Color.rgb(220, 220, 220))     // Color of the StringCylinder.
  lazy private val noteWidth  = pwidth / numFrets                               // The width of one "cell".
  lazy private val player     = new Player { instrument <-- (midiInstNum - 1) } // The wrapped RealTimePlayer.
  lazy private val fretCyls   = for(i <- 0 to numFrets) yield newFretCylinder(i)// Generates all the FretCylinders.
  lazy private val strTrans   = new Translate(pwidth / 2, pheight / 2, -15)     // The String-Cylinder's Positioning.
  lazy private val strRotate  = new Rotate(90, Rotate.Z_AXIS)                   // The String-Cylinder's Positioning.
  /* ................................................................................................................ */


  /* Private Bindables ---------------------------------------------------------------------------------------------- */
  /* ................................................................................................................ */


  /* The Constructor ------------------------------------------------------------------------------------------------ */
  updated {                                                             // The main constructor.
    prefWidthProp  = pwidth                                             // Setting the preferred width.
    prefHeightProp = pheight                                            // Setting the preferred height.
    <++(newStringCyl)                                                   // Pins the String-Cylinder.
    <++(fretCyls: _*)                                                   // Pins all Fret-Cylinders.
  }
  /* ................................................................................................................ */


  /* Invariants and Events Declarations ----------------------------------------------------------------------------- */
  when (localTouchCount >= 3 ) --> { muteMode = false       }           // Turns mute off when 3 or more fingers.
  when ( muteMode            ) --> { player.stop            }           // Stop when mute is off.
  when (!muteMode            ) ==> {                                    // When mute is off, define some events ..
    when(localTouchCount >= 4) --> { muteMode       = true  }           // .. When 4 or more fingers, turn mute on.
    when(totalTouchCount >= 5) --> { showMidiPicker = true  }           // .. When 5 or more fingers, show MidiPicker.
    lowestTouchPoint           --> { useLowestTouchPoint(_) }           // .. When lowest changes, update player.
  }
  /* ................................................................................................................ */


  /* Factory-methods for the cylinders ------------------------------------------------------------------------------ */
  private def newStringCyl = new Cylinder(5, pwidth) {                  // Factory-method for a new String-Cylinder.
    transforms   = List(strTrans, strRotate)                            // .. Positioning the cylinder.
    material     = STRING_MAT                                           // .. Setting the Color.
    translateY <-- (if(player.playing) 2 * cos(time / (0.1 ms)) else 0)  // .. If playing, let the string vibrate. (??)
  }

  private def newFretCylinder(i: Int) = {                               // Factory-method for a new Fret-Cylinder.
    new Cylinder(if (i == 1) 6 else 3, pheight) {                       // .. for the second(!??), use radius = 6.
      translateXY = (i * noteWidth, pheight / 2)                        // .. set the horizontal position.
      material    = FRET_MAT                                            // .. set the color.
    }
  }
  /* ................................................................................................................ */


  /* Methods to update the Touch-points ----------------------------------------------------------------------------- */
  @Bind var useMouse = true                                               // Indicates whether mouse or touch.
  when(!useMouse && touchPoints.isEmpty) ==> {                            // Defines an invariant, when not touching
    runIn(1 s) { useMouse = true }                                        // then set mouse usage after 1 second.
  }                                                                       // The Imply operator automatically
                                                                          // disposes the runIn when condition false.
  when(!touchPoints.isEmpty) --> { useMouse = false }                     // When using touch, disable mouse.

  @Bind private var noteValuePosY = 0.0
  @Bind private val lowestTouchPoint: Option[Double2] = <-- {             // The rightmost point (or None).
    val points = if(useMouse) mouseLikeTouchPoint else touchPoints        // Avoid simultaneous use of both.
    lazy val minPos = points.map{_.currentPos.x}.reduce{_ max _}          // .. Lazy to avoid NPE.
    points.map{_.currentPos}.filter{_.x == minPos}.headOption             // .. Returns either the Head or None.
  }

  private def useLowestTouchPoint(point: Option[Double2]) = point match { // Consumes an empty or real point ...
    case None =>                                                          // .. when empty, stop playing.
      player.stop
      player.note = 0
    case Some(pos) =>                                                     // .. when Point provided ...
      val noteValue: Int = computeNoteValue(pos.x)                        // .. .. get a new note-value.
      if(!player.playing) {                                               // .. .. when not playing ..
        noteValuePosY     = pos.y                                         // .. .. .. play
        player.note       = noteValue
        player.softAttack = false
        player.play
      } else {                                                            // else, when point NOT provided ..
        if (noteValue != player.note) {                                   // when note changed ...
          player.note       = noteValue                                   // .. play the new note ..
          player.softAttack = true                                        // .. with the softAttack ...
          player.play
        } else {                                                          // else, when note NOT changed ..
          if(bendEnabled) {                                               // if bendEnabled adjust the pitch.
            val bendDist: Double = (pos.y - noteValuePosY).abs            // ..
            val bendPct : Double = bendDist / pheight
            if (bendEnabled && bendPct > .25) {                           // .. if bending more than 25% ..
              player.pitch = Math.min(64 * (bendPct - .25), 64).toByte    // .. .. set the pitch.
            }
          }
        }
      }
  }

  private def computeNoteValue(stringPosX: Double): Int = {               // Computes a new note-value.
    openNoteValue + (stringPosX / noteWidth).toInt + 2
  }
  /* ................................................................................................................ */
}
/* === GuitarString3D =========================== END =============================================================== */