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
    def isPrime(num:Int):Boolean =
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
}
