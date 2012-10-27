
import scala.actors.Actor
import java.io.File

class SignatureMaker(imPath: String) extends Actor {
	def act() {
		receive {
			case (file: File, sig: File) => {

			}
		}
	}
}
