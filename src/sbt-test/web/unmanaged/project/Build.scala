import sbt._
import com.github.siasia.WebPlugin._
import Keys._

object MyBuild extends Build {
	override def projects = Seq(root)

	lazy val root = Project("root", file("."), settings = Defaults.defaultSettings ++ webSettings ++ rootSettings)

	lazy val rootSettings = Seq(
		webappUnmanaged <<= temporaryWarPath( _ / "WEB-INF" / "appengine-generated" *** )
	)
}
