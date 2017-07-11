/**
  * Created by jperezsl on 5/1/17.
  */

import IngredientType._

sealed abstract class Recipe(val name: String, val ingredients: List[IngredientType])
case object STANDARD extends Recipe("Standard", List(Pasta, Sausage))
case object PIZZA extends Recipe("Pizza", List(Dough, Tomato, Cheese, Mushrooms))
case object HEALTHY extends Recipe("Healthy", List(Carrot, Cucumber, Tomato, Chicken))
case object FANCY extends Recipe("Fancy", List(Chicken, Pasta, Mushrooms, Tomato))


