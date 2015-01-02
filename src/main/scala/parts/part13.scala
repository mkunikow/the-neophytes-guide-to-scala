package parts
import common.printer.printHeader

/**
 * Created by michal on 1/2/15.
 * The Neophyte's Guide to Scala Part 13: Path-dependent Types
 */
object part13 extends App {

  //The problem
  {
    printHeader("The problem")

    object Franchise {
      case class Character(name: String)
    }

    class Franchise(name: String) {
      import Franchise.Character
      def createFanFiction(lovestruck: Character, objectOfDesire: Character): (Character, Character) =
        (lovestruck, objectOfDesire)
    }

    val starTrek = new Franchise("Star Trek")
    val starWars = new Franchise("Star Wars")

    val quark = Franchise.Character("Quark")
    val jadzia = Franchise.Character("Jadzia Dax")

    val luke = Franchise.Character("Luke Skywalker")
    val yoda = Franchise.Character("Yoda")

    //Unfortunately, at the moment we are unable to prevent bad things from happening:
    starTrek.createFanFiction(lovestruck = jadzia, objectOfDesire = luke)

    {
      object Franchise {
        case class Character(name: String, franchise: Franchise)
      }
      class Franchise(name: String) {
        import Franchise.Character
        def createFanFiction(
                              lovestruck: Character,
                              objectOfDesire: Character): (Character, Character) = {
          require(lovestruck.franchise == objectOfDesire.franchise)
          (lovestruck, objectOfDesire)
        }
      }
    }
  }

  //Safer fiction with path-dependent types
  {
    printHeader("Safer fiction with path-dependent types")

    {
      class A {
        class B
        var b: Option[B] = None
      }
      val a1 = new A
      val a2 = new A
      val b1 = new a1.B
      val b2 = new a2.B
      a1.b = Some(b1)
      //    a2.b = Some(b1) // does not compile
    }


    class Franchise(name: String) {
      case class Character(name: String)
      def createFanFictionWith(lovestruck: Character, objectOfDesire: Character): (Character, Character) =
        (lovestruck, objectOfDesire)
    }

    val starTrek = new Franchise("Star Trek")
    val starWars = new Franchise("Star Wars")

    val quark = starTrek.Character("Quark")
    val jadzia = starTrek.Character("Jadzia Dax")

    val luke = starWars.Character("Luke Skywalker")
    val yoda = starWars.Character("Yoda")

    starTrek.createFanFictionWith(lovestruck = quark, objectOfDesire = jadzia)
    starWars.createFanFictionWith(lovestruck = luke, objectOfDesire = yoda)

//    starTrek.createFanFictionWith(lovestruck = jadzia, objectOfDesire = luke) // compilation error found   : starWars.Character, required: starTrek.Character

    def createFanFiction(f: Franchise)(lovestruck: f.Character, objectOfDesire: f.Character) =
      (lovestruck, objectOfDesire)
  }

  //Abstract type members
  {
    printHeader("Abstract type members")

    object AwesomeDB {
      abstract class Key(name: String) {
        type Value
      }
    }

    import AwesomeDB.Key
    class AwesomeDB {
      import collection.mutable.Map
      val data = Map.empty[Key, Any]
      def get(key: Key): Option[key.Value] = data.get(key).asInstanceOf[Option[key.Value]]
      def set(key: Key)(value: key.Value): Unit = data.update(key, value)
    }

    trait IntValued extends Key {
      type Value = Int
    }

    trait StringValued extends Key {
      type Value = String
    }

    object Keys {
      val foo = new Key("foo") with IntValued
      val bar = new Key("bar") with StringValued
    }

    val dataStore = new AwesomeDB
    dataStore.set(Keys.foo)(23)

    val i: Option[Int] = dataStore.get(Keys.foo)
//    dataStore.set(Keys.foo)("23") // does not compile
  }

}
