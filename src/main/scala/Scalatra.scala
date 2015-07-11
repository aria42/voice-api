import org.scalatra.LifeCycle
import javax.servlet.ServletContext
import com.pragmaticideal.voiceapi.{JsonAPIServlet, WebappServlet}

class ScalatraBootstrap extends LifeCycle {

  override def init(ctx: ServletContext) {
    ctx.mount(new JsonAPIServlet, "/api/0.1/*")
    ctx.mount(new WebappServlet, "/app/*")
  }
}