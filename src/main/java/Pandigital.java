import fj.*;
import fj.data.List;
import fj.data.Stream;
import fj.function.Integers;

import static fj.data.Stream.*;
import static fj.Show.*;

public class Pandigital {
    public static void main(String[] args) {
        List<Integer> list = count().toList().nub();
        intShow.println(Integers.sum(list));
        listShow(intShow).println(list);
    }

    static Stream<Stream<Character>> permute(Stream<Character> stream) {
        return stream.bind(x -> {
            Stream<Character> s = stream.filter(a -> !a.equals(x));
            if(s.isEmpty())
                return single(single(x));
            else
                return permute(s).map(xs -> cons(x, () -> xs));
        });
    }

    static Stream<Integer> count() {
        Stream<Stream<Character>> streams = permute(fromString("123456789"));
        return streams.map(Pandigital::digitToInt)
                .map(i -> quotRem(i, 100000))
                .filter(p2 -> check(10, p2) || check(100, p2))
                .map(P2::_1);
    }

    static int digitToInt(Stream<Character> stream) {
        return stream.foldLeft((s, n) -> s * 10 + Character.getNumericValue(n), 0);
    }

    static P2<Integer, Integer> quotRem(Integer x, Integer y) {
        return P.lazy(() -> x / y, () -> x % y);
    }

    static boolean check(int n, P2<Integer, Integer> p2) {
        P2<Integer, Integer> p = quotRem(p2._2(), n);
        return p2._1() == p._1() * p._2();
    }
}
