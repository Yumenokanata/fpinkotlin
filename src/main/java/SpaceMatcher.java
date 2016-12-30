import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by yume on 16-12-14.
 */
public class SpaceMatcher {
    public static void main(String[] args) {
        String string="12 34  56 　7";
        Matcher matcher = Pattern.compile("([\\s\\p{Zs}]+)").matcher(string);
        System.out.println(matcher.replaceAll("+"));
//        if(matcher.find()) {
//            System.out.println(matcher.group(1));
//
//            System.out.println(matcher.replaceAll("+"));
//        }
    }
}
