import akka.event.NoLogging
import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jperezsl on 5/4/17.
  */
class FridgeBotControllerTest extends FlatSpec with Matchers with ScalatestRouteTest with Service {

  override def testConfigSource = "akka.loglevel = WARNING"
  override def config = testConfig
  override val logger = NoLogging

  fridgeBotController.addItem(new Item(IngredientType.TOMATO, notExpiredDate, 2));
  fridgeBotController.addItem(new Item(IngredientType.CHEESE, notExpiredDate, 3));
  fridgeBotController.addItem(new Item(IngredientType.DOUGH, notExpiredDate, 5));
  fridgeBotController.addItem(new Item(IngredientType.MUSHROOMS, notExpiredDate, 2));

  "The service" should "Say hi when hi is requested" in {
      Get("/hi") ~> routes ~> check {
        status shouldBe OK
        //contentType shouldBe `application/json`
        //responseAs[IpInfo] shouldBe ip1Info
        responseAs[String] shouldBe "Fridgebot says hi"
      }
    }

  it should "Respond to getAll" in {
    Get("/getAll") ~> routes ~> check {
      status shouldBe OK

    }
  }

  it should "Add the ingredients of the Pizza to the fridge" in {
    Post("/addItem/{$item}") ~> routes ~> check {

    }
  }
}
