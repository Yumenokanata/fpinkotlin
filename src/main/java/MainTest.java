/**
 * Created by yume on 16-10-20.
 */
public class MainTest {
    public static void main(String[] args) {
        short i = 0x7fff;
        short index = 0x0;
        int type = saveIndex(index, i);
        System.out.println(Integer.toBinaryString(type));
        System.out.println("ori: " + index + " | " + getIndex(type));
    }

    public static int saveIndex(short index, short num) {
        return ((int) index) << 16 | num;
    }

    public static short getIndex(int type) {
        return (short) (type >> 16);
    }
}
