import scala.math.{pow, sqrt}
import scala.util.control.Breaks._

object Problem_List {
  def problem1(n: Number): Int = {
    var sum = 0;
    for (i <- 0 until n.intValue()) {
      if (i % 3 == 0 || i % 5 == 0) {
        sum += i;
      }
    }
    return sum
  }

  def problem2(n: Number): Int = {
    var sum = 2;
    var fibo_1 = 1;
    var fibo_2 = 2;
    var fibo_3 = 0;

    var loop = false;

    while (!loop) {
      if (fibo_3 > n.intValue()) loop = true
      fibo_3 = fibo_1 + fibo_2;
      if (fibo_3 % 2 == 0) sum += fibo_3
      fibo_1 = fibo_2;
      fibo_2 = fibo_3;
    }
    return sum;
  }

  /**
   * 풀이
   *
   * For문 사용 시 Long타입의 Number가 적용 안됨
   * 따라서 재귀함수를 사용하여 인자값을 소인수로 나눈 값을 넘김
   * 최종적으로 인자값이 1이 될때의 해당 소인수값을 리턴하게 됌.
   */
  def problem3(num: Long, i: Long): Long = {
    if (num == 1)
      i - 1
    else if (num % i == 0)
      problem3(num / i, i + 1)
    else
      problem3(num, i + 1)
  }

  def problem4(): Int = {
    var sum = 0;
    var reverseSum = ""
    var maxSum = 0;
    for (i <- 100 until 999; j <- 100 until 999) {
      sum = i * j;
      reverseSum = sum.toString.reverse
      if (sum.toString == reverseSum && sum > maxSum) {
        maxSum = sum;
      }
    }
    return maxSum;
  }

  // TODO: 리팩토링 고려해보기
  def problem5(n: Long): Long = {
    def max(a: Long, b: Long): Long = {
      if (b == 0) a
      else max(b, a % b)
    }

    var b: Long = 1
    for (i <- 2L to n) {
      b = b * i / max(b, i)
    }
    b
  }

  def problem6() = {
    var sumOfSquares: BigInt = 0;
    var sum = 0;
    for (i <- 1 until 101) {
      sumOfSquares = sumOfSquares + BigInt(i).pow(2);
      sum += i;
    }
    var squaredSum = BigInt(sum).pow(2)
    squaredSum - sumOfSquares
  }

  def problem7() = {
    var cnt = 0;
    var decimal = 2;
    var result = 0;
    var loop = false;

    def isPrime(num: Int): Boolean =
      (num > 1) && !(2 to scala.math.sqrt(num).toInt).exists(x => num % x == 0)

    while (!loop) {
      if (isPrime(decimal)) {
        cnt += 1;
        if (cnt == 10001) {
          result += decimal;
          loop = true
        }
      }
      decimal += 1;
    }
    result;
  }

  def problem8() = {
    var result = 0
    var arg = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    var splitArg = arg.split("")
    for (i <- 0 until splitArg.length - 4) {
      var sum = splitArg(i).toInt *
        splitArg(i + 1).toInt *
        splitArg(i + 2).toInt *
        splitArg(i + 3).toInt *
        splitArg(i + 4).toInt
      if (result < sum) result = sum
    }
    result
  }

  def problem9(n: Number): Int = {
    var result: Double = 0;
    for (a <- 1 until n.intValue(); b <- 1 until n.intValue()) {
      var c = sqrt(pow(a, 2) + pow(b, 2))
      var check = a + b + c
      if (c == c.toInt && check == n) {
        result = a * b * c;
      }
    }
    result.toInt;
  }

  // TODO: 리팩토링해보기
  def problem10(n: Int) = {
    val tmpset = scala.collection.mutable.BitSet(1)
    val limit = Math.sqrt(n).toInt
    for (i <- 2 to limit) {
      if (!tmpset(i)) {
        for (j <- 2 to n / i) {
          tmpset += (j * i)
        }
      }
    }

    val result = for (i <- 2 to n if (!tmpset(i))) yield i
    result.toList.map(_.toLong).sum
  }
}
