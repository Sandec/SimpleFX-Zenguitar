package samples.zenguitar

import simplefx.core._
import simplefx.all._
import simplefx.all.Color._
import simplefx.all.KeyCode._
import simplefx.experimental._

/* === ZenGuitar3DApp =========================== START ============================================================= */
object ZenGuitar3DApp extends App
class  ZenGuitar3D {                                                  // Holds the main guitar's parameters.
  @Bind var muteMode       = false                                    // Currently muted or not.
  @Bind var midiInstNum    = 0                                        // Current midi instrument (not the list-index).
  @Bind var showMidiPicker = true                                     // Is the MidiPicker currently visible or not.
  @Bind val numStrings     = <--(instConfig.numStrings)               // The current instrument's no strings.
  @Bind var bendEnabled    = <--(instConfig.bendEnabled)              // Is the current instrument bendable or not.
  @Bind val instConfig     = <--(InstConfig.config4Midi(midiInstNum)) // The configuration of the current instrument.
}

/* === ZenGuitar3DApp =========================== ZenGuitar3DApp ==================================================== */
@SimpleFXApp class ZenGuitar3DApp { THIS =>

  title = "The ZenGuitar3D - Invented by James Weaver, written in SimpleFX by Florian Kirmaier(SANDEC)"

  /* Global declarations -------------------------------------------------------------------------------------------- */
  object Guitar extends ZenGuitar3D
  import Guitar._

  lazy val GUITAR_RESET_DURATION   : Time   = (1 s)                   // Duration of reset for the entire "guitar".
  lazy val INITIAL_INST_PICKER_FACE: Int    = 14                      // The Face of the initial instrument.
  lazy val LOW_NOTE                : Int    = 18                      // The lowest possible note-value.
  lazy val NUM_FRETS               : Int    = 15                      // The number of Frets to support.
  lazy val STRING_W                : Double = 1900                    // The width of the guitar string.
  lazy val GUITAR_INITIAL_Z        : Double = 40                      // The initial Z-position.
  lazy val NECK_H                  : Double = 900                     // The Neck's height.
  lazy val NECK_DEPTH              : Double = 100                     // The Neck's depth.

  lazy val NECK_MAT       = new PhongMaterial() {diffuseMap = neckDiffuseMap} // The Neck's material.
  lazy val FRET_MAT       = new PhongMaterial(rgb(222, 215, 165))             // The Fret's material/color.
  lazy val neckDiffuseMap = new Image("/samples/zenguitar/wood.jpeg")         // The Neck's diffuse-map.
  lazy val cpDiffuseMap   = new Image("/samples/zenguitar/20-instruments-w-pipa.png") // The cylinder-picker's diff-map.
  /* ................................................................................................................ */


  /* Declaring the UI components etc -------------------------------------------------------------------------------- */
  lazy val pin        = new Group
  lazy val guitar     = new Group { translateXYZ = (STRING_W/2, NECK_H/2, GUITAR_INITIAL_Z) }
  lazy val theCamera  = new PerspectiveCamera (false)
  lazy val pointLight = new PointLight (WHITE) {translateXYZ = (STRING_W * 0.33, NECK_H * 0.33, -2000.0)}
  lazy val neck       = new Box (STRING_W, NECK_H, NECK_DEPTH) {material = NECK_MAT; translateZ = NECK_DEPTH/2}

  def fm (i:Int, j:Int) = new FretMarker(i,j)                                 // Factory method for the FretMarker.

  class FretMarker (fretNum: Int, height: Int) extends Cylinder(STRING_W / NUM_FRETS / 3, 1) { // The knobs ...
    material      =  FRET_MAT                                                 // The Fret-material.
    transform     =  Rotate(90, Rotate.X_AXIS)                                // The positioning.
    translateX    =  (STRING_W / NUM_FRETS) * (fretNum + 0.5) - STRING_W / 2  // The x-position.
    translateY  <-- height * NECK_H / numStrings                              // The y-position.
  }

  object MidiPicker extends CylinderPicker (300, 20, INITIAL_INST_PICKER_FACE, cpDiffuseMap, Guitar) {
    midiInstNum <-- InstConfig.midis(curFace)                                 // A new instrument is picked.
    enabled     <-- (!muteMode && showMidiPicker)                             // No mute and picker visible.
    enabled     --> { translateZ := (if(enabled) -100 else 400) in (1 s) }    // When enabled, zoom out else in.
  }

  lazy val guitarStringsContainer = new VBox {            // Note: the ==> operator automatically un-pins for you.
    def note (i:Int) = LOW_NOTE + instConfig.openNoteVals(i - 1)              // Notes configuration ...
    translateXY      = - (STRING_W, NECK_H) / 2                               // Positions the string.
    numStrings     ==> { for(i <- 1 to numStrings)                            // Generate and pin all strings.
      <++ (new GuitarString3D ( note(i), NUM_FRETS, STRING_W, NECK_H / numStrings, Guitar ))    // .. pin.
    }
  }
  /* ................................................................................................................ */


  /* Pinning onto the Scene-Graph ----------------------------------------------------------------------------------- */
  guitar <++ (neck, guitarStringsContainer, fm(3,0), fm(5,0), fm(7,0), fm(9,0), fm(12,1), fm(12,-1), MidiPicker )
  pin    <++ (new HBox {
    <++ (new Button{text = "3d Mode" ; onAction --> { muteMode       = !muteMode} })
    <++ (new Button{text = "Zylinder"; onAction --> { muteMode = false; showMidiPicker = !showMidiPicker} })
    <++ (new Button{text = "Reset"   ; onAction --> { reset } })
  } )
  pin    <++ (guitar, pointLight, theCamera)

  scene = new Scene(pin, STRING_W.toInt, NECK_H.toInt, true, null) {
    fill                  =  WHITE
    camera                =  theCamera
    on(CONTROL).pressed --> { muteMode       = !muteMode       }
    on(SPACE  ).pressed --> { showMidiPicker = !showMidiPicker }
  }
  /* ................................................................................................................ */


  /* Defining the overall 3D rotates -------------------------------------------------------------------------------- */
  @Bind var guitarAngleXYZ : Double3 = <-> (guitarAngleX, guitarAngleY, guitarAngleZ)
  @Bind var guitarAngleX   : Double  = 0.0
  @Bind var guitarAngleY   : Double  = 0.0
  @Bind var guitarAngleZ   : Double  = 0.0
  @Bind var curZoomFactor  : Double  = 1.0

  guitar.transform <-- (Rotate(guitarAngleX, Rotate.X_AXIS)  *
                        Rotate(guitarAngleY, Rotate.Y_AXIS)  *
                        Rotate(guitarAngleZ, Rotate.Z_AXIS))
  /* ................................................................................................................ */


  /* Declaring the invariants, used when scrolling and zooming ------------------------------------------------------ */
  when(muteMode) ==> { // the following bindings/events are enabled, as long as muteMode is true
    Δ(guitarAngleX) <-- (Δ(pin.dragDistance.y + pin.touchScrollDistance.y) /  3)
    Δ(guitarAngleY) <-- (Δ(pin.dragDistance.x + pin.touchScrollDistance.x) / -3)
    Δ(guitarAngleZ) <--  Δ(pin.rotateAngle)
    guitar.scaleXYZ <-- (prev(guitar.scaleX) *                      // Defining Scale based upon the new Δ-distance and
                        (Δ(pin.mouseWheelDistance.y) / 400 + 1) *   // the relation between the last zoom-factor.
                        guitar.zoomFactor / prev(guitar.zoomFactor)).to3D

    when(guitar.scaleX < .25) --> { reset }                               // When the x-scale goes below 25%, do a reset.
  }
  def reset = {
      muteMode = false                                              // .. muting is set to off.
      guitarAngleXYZ    := (0,0,0)  in GUITAR_RESET_DURATION        // .. any rotation is reset.
      guitar.scaleXYZ   := 1.0.to3D in GUITAR_RESET_DURATION        // .. any scaling  is reset.
  }
  /* ................................................................................................................ */
}
/* === ZenGuitar3DApp =========================== END =============================================================== */