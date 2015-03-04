package samples.zenguitar

import simplefx.core._
import simplefx.all._
import simplefx.experimental._
import javafx.scene.input.{MouseEvent => ME}
import java.lang.Math._

/* === CylinderPicker =========================== START ============================================================= */
class CylinderPicker( pradius           : Double      ,                   // The radius of the cylinder-picker.
                      numFaces          : Int         ,                   // The number of Faces (or Instruments).
                      initialFace       : Int         ,                   // The Instrument to start with.
                      cylinderDiffuseMap: Image       ,                   // The Material of the cylinder-picker.
                      guitar            : ZenGuitar3D ) extends Group {   // The main parameters.

  /* The CylinderPicker Interface ----------------------------------------------------------------------------------- */
  @Bind var curFace  = <--(face4angle(cylinder.rotate))                   // Midi-index of current instrument.
  @Bind var enabled  = true                                               // Ready for adjustments or not.
  /* ................................................................................................................ */


 /* UI Components --------------------------------------------------------------------------------------------------- */
  lazy private val cylinder : Cylinder = new Cylinder {
    radius            = pradius                                           // The radius of the cylinder-picker.
    height            = pradius * Math.sin(360 / (numFaces * 2))          // The height of the cylinder-picker.
    material          = new PhongMaterial{diffuseMap = cylinderDiffuseMap}// Defines the Material / ("surface").
    rotationAxis      = Rotate.Y_AXIS                                     // Rotate around Y.
    def released      = onMouseReleased || onTouchReleased                // Released = mouse or touch released.
    def selFace(e:ME) = face4point(e.getPickResult.getIntersectedPoint)   // The selected face, depending on 3D click.
    def touchAndDrag  = touchScrollDistance.x + dragDistance.x            // The x-distance of the scroll + drag.

    when(enabled) ==> {                                                   // Whenever enables is set to true ..

      Δ(rotate) <-- ( Δ(mouseWheelDistance.y + touchAndDrag / -7))        // Rotate with wheel, touch and drag.

      when (released             ) --> { rotate2face(curFace, 0.1 s)    } // On release, select the closest instrument.
      when (touchPoints.size >= 3) --> { guitar.showMidiPicker = false  } // Hide MidiPicker when 3 or more fingers.
      when (onDoubleClick        ) --> { e:ME => rotate2face(selFace(e))} // Rotate selected instrument to front.
    }
  }
  /* ................................................................................................................ */


  /* Some Help-methods ---------------------------------------------------------------------------------------------- */
  private def face4point (p3d  : Point3D) = face4angle (180 - atan2toDegrees(p3d.getX, p3d.getZ))
  private def face4angle (angle: Double ) = floor (normalizeDegrees(angle) / (360.0/numFaces)).toInt

  private def rotate2face(face: Int, duration: Time = 1 s) {              // Rotate, depending on face.
    val toAngle     = normalizeDegrees((face + 0.5) * (360.0/numFaces))   // Set the "target-position" for the rotation.
    val distance    = cylinder.rotate - toAngle                           // Set the distance from current.
    val startAngle  = if     (distance >  180) cylinder.rotate - 360      // We rotate with the shortest distance.
                      else if(distance < -180) cylinder.rotate + 360
                      else                     cylinder.rotate

    cylinder.rotate := toAngle in duration startAt startAngle             // Rotate the cylinder, progressively.
  }
  /* ................................................................................................................ */


  /* Pin the Cylinder to the Scene-Graph ---------------------------------------------------------------------------- */
  updated {
    transform = Rotate(-8, Rotate.X_AXIS)                                 // Position the cylinder.
    rotate2face (initialFace, 0 s)                                        // Rotate to the start position.
    <++(cylinder)                                                         // Pin the cylinder.
  }
  /* ................................................................................................................ */
}
/* === CylinderPicker =========================== END =============================================================== */
