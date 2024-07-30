object Q1 {

	type product = (String,Int,Double)
	
	def main(args : Array[String]) : Unit = {
	
		val inventory1: Map[Int,product] = Map (
		101 -> ("Laptop",12,299999.99),
		102 -> ("Smartphone",25,59900.00),
		103 -> ("Tablet",6,70000.00),
		104 -> ("Monitor",4,8000.00),
		105 -> ("Keyboard",10,999.99),
		106 -> ("Mouse",26,900.00)
		)
		
		val inventory2: Map[Int,product] = Map (
		102 -> ("Smartphone",15,49900.00),
		103 -> ("Tablet",10,70000.00),
		107 -> ("Headphones",32,690.00),
		108 -> ("Air prod",11,4000.00)
		)
		
		println("I. All product names in inventory1:")
		val productName = inventory1.values.map(_._1)
		productName.foreach(println)
		
		val totalValue = inventory1.values.map {
			case (_,quantity,price) => quantity*price
			}.sum
		
		println()
		println("II.Total value of inventory1: " + totalValue)
		
		println()
		println("III. Is inventory1 empty? " + {inventory1.isEmpty})
		
		val mergedInventory = (inventory1.keySet ++ inventory2.keySet).map { id =>
			val product1 = inventory1.get(id)
			val product2 = inventory2.get(id)
			
			(product1,product2) match {
			case (Some((name1,qty1,price1)), Some((_,qty2,price2))) => id -> (name1,qty1 + qty2, price1.max(price2))
			case (Some(product), None) => id -> product
			case (None, Some(product)) => id -> product
			case _ => throw new IllegalStateException("This should never happen")
			}
		}.toMap
		
		println("\nIV. Merged Inventory: ")
		mergedInventory.foreach { 
			case (id, (name, quantity, price)) =>
			println("ID: " + id," Name: " + name + " Quantity: " + quantity + "  price: " + price)
		}
		
		println("\nEnter the product ID to check: ")
        val productIdToCheck = readInt()
        println(s"\nV. Checking for product with ID $productIdToCheck")
        inventory1.get(productIdToCheck) match {
            case Some((name, quantity, price)) =>
                println(s"Product found - Name: $name, Quantity: $quantity, Price: $price")
            case None =>
                println(s"Product with ID $productIdToCheck not found in inventory1")
			}	
		}		
	}