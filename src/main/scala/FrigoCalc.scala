/**
  * Created by jperezsl on 5/1/17.
  */
object FrigoCalc extends App {

  import IngredientType._

  val frigo = List(Cucumber, Dough, Tomato, Mushrooms, Pasta, Sausage, Cheese, Chicken, Carrot)
  type Frigo = List[IngredientType]

  val recipies = List(STANDARD, PIZZA, HEALTHY, FANCY)


  def canBeCooked(recipe: Recipe, fridge: Frigo): Boolean = {
    recipe.ingredients.map(ing => fridge.contains(ing)).fold(true)(_&&_)
  }

  def useIngredients(recipe: Recipe, fridge: Frigo) : Frigo = {
    if (canBeCooked(recipe, fridge)) {
      fridge.diff(recipe.ingredients)
    } else { fridge }
  }

  def cookUntilFridgeIsEmpty(fridge: Frigo, acc: List[Recipe]) : (List[Recipe], Int) = fridge match {
    case Nil => (acc, 0)
    case _ =>
      val canbecooked = recipies.map{ r => (r, canBeCooked(r, fridge)) }.filter( _._2 )

      if (canbecooked.isEmpty) (acc, fridge.size)
      else
        canbecooked
          .map{ case(r, _) => cookUntilFridgeIsEmpty(useIngredients(r, fridge), r :: acc ) }
          .minBy(_._2)
  }

  val frigo2 = List(Sausage, Pasta, Sausage, Pasta, Dough, Tomato, Cheese, Mushrooms, Cucumber, Chicken, Tomato, Pasta, Mushrooms, Tomato)

  println(useIngredients(PIZZA, frigo2))

  println(cookUntilFridgeIsEmpty(frigo2, Nil))


}
