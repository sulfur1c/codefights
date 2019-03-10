package dishes

import scala.collection.GenTraversableOnce

object DishesAgrrupator extends DishesAgrrupator {

  def main(args: Array[String]): Unit = {
    val result: Unit = println(groupingDishes(Array(Array("Salad","Tomato","Cucumber","Salad","Sauce"),
      Array("Pizza","Tomato","Sausage","Sauce","Dough"),
      Array("Quesadilla","Chicken","Cheese","Sauce"),
      Array("Sandwich","Salad","Bread","Tomato","Cheese"))))
  }
}

class DishesAgrrupator {

  def groupingDishes(dishes: Array[Array[String]]): Array[Array[String]] = {
    val dishTuple = dishes.map(dish => (dish.head, dish.tail))
    val toppingsAndDish = dishTuple.flatMap(dish => dish._2.map(topping => (topping, dish._1))).sorted
    val toppingsAndDishGrouped = toppingsAndDish.groupBy(_._1)
    val dishesByToppingFiltered = toppingsAndDishGrouped.filter(element => element._2.length > 1)
                                                        .groupBy(_._1).values
                                                        .flatMap(element => element.values)
    val dishesByTopping = dishesByToppingFiltered.map(element => populateArray(element))
    dishesByTopping.foreach(element => element.foreach(element => println(element)))
    dishesByTopping.toArray
  }

  def populateArray(element: Array[(String, String)]): Array[String] = {
    var result = Array(element(0)._1)
    element.foreach(element => result = result :+ element._2)
    result
  }

}